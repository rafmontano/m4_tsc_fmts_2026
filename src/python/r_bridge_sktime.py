# File: src/python/r_bridge_sktime.py
# Purpose:
#   RDS -> Python -> model-ready sktime bridge.
#
# Final contract:
#   - RotF -> 2D tabular dataset
#   - DTW, EUCLIDEAN, ROCKET, InceptionTime, HIVECOTEV2 -> numpy3D dataset

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List

import numpy as np
import pandas as pd
import rdata
from sktime.datatypes import convert_to
from sktime.transformations.panel.padder import PaddingTransformer


def _sanitize_feature_df(df: pd.DataFrame, name: str) -> pd.DataFrame:
    out = df.copy()

    # force numeric
    out = out.apply(pd.to_numeric, errors="coerce")

    # replace inf/-inf with NaN
    out = out.replace([np.inf, -np.inf], np.nan)

    # optional clip to avoid float32 overflow / absurd magnitudes
    out = out.clip(lower=-1e12, upper=1e12)

    # fill NaN with column medians, fallback to 0
    med = out.median(axis=0, numeric_only=True)
    med = med.fillna(0.0)
    out = out.fillna(med).fillna(0.0)

    bad_left = ~np.isfinite(out.to_numpy(dtype=float))
    if bad_left.any():
        print(f"[WARN] {name}: non-finite values remained after sanitization; forcing to 0")
        arr = out.to_numpy(dtype=float)
        arr[~np.isfinite(arr)] = 0.0
        out = pd.DataFrame(arr, columns=out.columns, index=out.index)

    return out


# --------------------------------------------------
# Shared path helpers
# --------------------------------------------------

PROJECT_ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = PROJECT_ROOT / "data"
MODELS_DIR = PROJECT_ROOT / "models"
RESULTS_DIR = PROJECT_ROOT / "results"


def freq_tag(period_or_tag: str) -> str:
    value = str(period_or_tag).strip()

    mapping = {
        "Yearly": "y",
        "Quarterly": "q",
        "Monthly": "m",
        "Weekly": "w",
        "Daily": "d",
        "Hourly": "h",
        "y": "y",
        "q": "q",
        "m": "m",
        "w": "w",
        "d": "d",
        "h": "h",
    }

    return mapping[value]


def window_tag(window_mode: str) -> str:
    value = str(window_mode).strip().lower()

    mapping = {
        "small": "s",
        "default": "d",
        "large": "l",
        "full": "f",
    }

    if value not in mapping:
        raise ValueError("Invalid window_mode: {}".format(window_mode))

    return mapping[value]


@dataclass
class FrequencyPaths:
    tag: str
    window_mode: str
    window_tag: str
    raw_rds: Path
    labeled_rds: Path
    features_rds: Path
    split_index_rds: Path
    real_eval_rds: Path
    subset_clean_rds: Path


def get_paths(period_or_tag: str, window_mode: str = "default") -> FrequencyPaths:
    tag = freq_tag(period_or_tag)
    wtag = window_tag(window_mode)

    return FrequencyPaths(
        tag=tag,
        window_mode=window_mode,
        window_tag=wtag,
        raw_rds=DATA_DIR / f"all_windows_raw_{tag}_{wtag}.rds",
        labeled_rds=DATA_DIR / f"all_windows_labeled_{tag}_{wtag}.rds",
        features_rds=DATA_DIR / f"all_windows_with_features_{tag}_{wtag}.rds",
        split_index_rds=DATA_DIR / f"split_index_{tag}_{wtag}.rds",
        real_eval_rds=DATA_DIR / f"real_eval_with_features_{tag}_{wtag}.rds",
        subset_clean_rds=DATA_DIR / "M4_subset_clean.rds",
    )


# --------------------------------------------------
# Low-level readers
# --------------------------------------------------

def read_rds(path: Path) -> Any:
    return rdata.read_rds(str(path))


# --------------------------------------------------
# Normalisation helpers
# --------------------------------------------------

def _to_numpy_1d(x: Any, dtype=float) -> np.ndarray:
    if isinstance(x, np.ndarray):
        return x.astype(dtype).reshape(-1)

    if isinstance(x, (list, tuple)):
        return np.asarray(x, dtype=dtype).reshape(-1)

    return np.asarray(x, dtype=dtype).reshape(-1)


def _normalise_list_column(col: pd.Series) -> List[np.ndarray]:
    return [_to_numpy_1d(v, dtype=float) for v in col.tolist()]


def _normalise_labels_column(col: pd.Series) -> List[np.ndarray]:
    return [_to_numpy_1d(v, dtype=int) for v in col.tolist()]


def _normalise_eval_df(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()

    if "x" in out.columns:
        out["x"] = _normalise_list_column(out["x"])

    if "xx" in out.columns:
        out["xx"] = _normalise_list_column(out["xx"])

    if "labels" in out.columns:
        out["labels"] = _normalise_labels_column(out["labels"])

    return out


def _to_zero_based_index(x: Any) -> np.ndarray:
    arr = np.asarray(x, dtype=int).reshape(-1)
    return arr - 1


# --------------------------------------------------
# Split helpers
# --------------------------------------------------

#def get_model_split(split_index: Dict[str, Any], model_id: str) -> Dict[str, np.ndarray]:
#    if model_id in split_index:
#        split_use = split_index[model_id]
#    else:
#        split_use = split_index["uncap"]
#
#    return {
#        "train": _to_zero_based_index(split_use["train"]),
#        "test": _to_zero_based_index(split_use["test"]),
#    }

def get_model_split(split_index: Dict[str, Any], model_id: str) -> Dict[str, np.ndarray]:
    if model_id in split_index:
        split_use = split_index[model_id]
    else:
        split_use = split_index["uncap"]

    train_idx = _to_zero_based_index(split_use["train"])
    test_idx = _to_zero_based_index(split_use["test"])

    if str(model_id).strip() == "HIVECOTEV2":
        train_fraction = 0.5

        n_before = len(train_idx)
        n_keep = int(n_before * train_fraction)

        train_idx = train_idx[:n_keep]

        print(
            f"[HIVECOTEV2] Training index first-half subset: "
            f"{n_before} -> {len(train_idx)} rows "
            f"({train_fraction:.2f})"
        )

    return {
        "train": train_idx,
        "test": test_idx,
    }

# --------------------------------------------------
# Final dataset builders
# --------------------------------------------------

def build_features_dataset(features_dataset: Dict[str, Any]) -> Dict[str, Any]:
    train_df = features_dataset["X_train"]
    test_df = features_dataset["X_test"]
    real_df = features_dataset["X_real"]

    y_train = features_dataset["y_train"]
    y_test = features_dataset["y_test"]
    y_real = features_dataset["y_real"]

    drop_cols = ["x", "xx", "labels", "series_id", "series_name"]

    predictor_cols = [c for c in train_df.columns if c not in drop_cols]

    common_cols = [
        c for c in predictor_cols
        if c in test_df.columns and c in real_df.columns
    ]

    missing_test = [c for c in predictor_cols if c not in test_df.columns]
    missing_real = [c for c in predictor_cols if c not in real_df.columns]

    if missing_test:
        print(f"[WARN] Missing columns in X_test, dropping: {missing_test}")

    if missing_real:
        print(f"[WARN] Missing columns in X_real, dropping: {missing_real}")

    X_train = train_df[common_cols]
    X_test = test_df[common_cols]
    X_real = real_df[common_cols]

    X_train = _sanitize_feature_df(X_train, "X_train")
    X_test = _sanitize_feature_df(X_test, "X_test")
    X_real = _sanitize_feature_df(X_real, "X_real")

    return {
        "X_train": X_train,
        "y_train": y_train,
        "X_test": X_test,
        "y_test": y_test,
        "X_real": X_real,
        "y_real": y_real,
    }


def _series_list_to_df_list(series_list: List[np.ndarray]) -> List[pd.DataFrame]:
    df_list: List[pd.DataFrame] = []

    for s in series_list:
        arr = np.asarray(s, dtype=float).reshape(-1)
        df_i = pd.DataFrame({"value": arr})
        df_list.append(df_i)

    return df_list


def build_series_dataset(series_dataset: Dict[str, Any]) -> Dict[str, Any]:
    x_train = series_dataset["X_train"]
    y_train = series_dataset["y_train"]

    x_test = series_dataset["X_test"]
    y_test = series_dataset["y_test"]

    x_real = series_dataset["X_real"]
    y_real = series_dataset["y_real"]

    X_train_df_list = _series_list_to_df_list(x_train)
    X_test_df_list = _series_list_to_df_list(x_test)
    X_real_df_list = _series_list_to_df_list(x_real)

    all_lengths = [len(v) for v in x_train] + [len(v) for v in x_test] + [len(v) for v in x_real]
    target_len = int(max(all_lengths))

    padder = PaddingTransformer(pad_length=target_len)

    X_train_pad = padder.fit_transform(X_train_df_list)
    X_test_pad = padder.transform(X_test_df_list)
    X_real_pad = padder.transform(X_real_df_list)

    X_train_np3d = convert_to(X_train_pad, to_type="numpy3D", as_scitype="Panel")
    X_test_np3d = convert_to(X_test_pad, to_type="numpy3D", as_scitype="Panel")
    X_real_np3d = convert_to(X_real_pad, to_type="numpy3D", as_scitype="Panel")

    return {
        "X_train": X_train_np3d,
        "y_train": y_train,
        "X_test": X_test_np3d,
        "y_test": y_test,
        "X_real": X_real_np3d,
        "y_real": y_real,
    }


# --------------------------------------------------
# Public entrypoints
# --------------------------------------------------

def get_model_data(
    freq_tag: str,
    model_id: str,
    horizon_id: int,
    window_mode: str = "default",
) -> Dict[str, Any]:
    paths = get_paths(freq_tag, window_mode=window_mode)

    all_windows_df = _normalise_eval_df(read_rds(paths.features_rds).copy())
    split_index = read_rds(paths.split_index_rds)
    real_eval_df = _normalise_eval_df(read_rds(paths.real_eval_rds).copy())

    split_use = get_model_split(split_index, model_id=model_id)

    train_idx = split_use["train"]
    test_idx = split_use["test"]

    train_df = all_windows_df.iloc[train_idx].reset_index(drop=True)
    test_df = all_windows_df.iloc[test_idx].reset_index(drop=True)
    real_df = real_eval_df.reset_index(drop=True)

    h0 = int(horizon_id) - 1

    y_train = np.asarray([int(v[h0]) for v in train_df["labels"]], dtype=int)
    y_test = np.asarray([int(v[h0]) for v in test_df["labels"]], dtype=int)
    y_real = np.asarray([int(v[h0]) for v in real_df["labels"]], dtype=int)

    features_dataset = {
        "X_train": train_df,
        "y_train": y_train,
        "X_test": test_df,
        "y_test": y_test,
        "X_real": real_df,
        "y_real": y_real,
    }

    series_dataset = {
        "X_train": train_df["x"].tolist(),
        "y_train": y_train,
        "X_test": test_df["x"].tolist(),
        "y_test": y_test,
        "X_real": real_df["x"].tolist(),
        "y_real": y_real,
    }

    if str(model_id).strip() == "RotF":
        return build_features_dataset(features_dataset)

    return build_series_dataset(series_dataset)


# --------------------------------------------------
# Raw-series loader for Mantis
# Returns raw unpadded rolling windows and labels
# --------------------------------------------------

def get_mantis_data(
    freq_tag: str,
    model_id: str,
    horizon_id: int,
    window_mode: str = "default",
) -> Dict[str, Any]:
    paths = get_paths(freq_tag, window_mode=window_mode)

    all_windows_df = _normalise_eval_df(read_rds(paths.features_rds).copy())
    split_index = read_rds(paths.split_index_rds)
    real_eval_df = _normalise_eval_df(read_rds(paths.real_eval_rds).copy())

    split_use = get_model_split(split_index, model_id=model_id)

    train_idx = split_use["train"]
    test_idx = split_use["test"]

    train_df = all_windows_df.iloc[train_idx].reset_index(drop=True)
    test_df = all_windows_df.iloc[test_idx].reset_index(drop=True)
    real_df = real_eval_df.reset_index(drop=True)

    h0 = int(horizon_id) - 1

    y_train = np.asarray([int(v[h0]) for v in train_df["labels"]], dtype=int)
    y_test = np.asarray([int(v[h0]) for v in test_df["labels"]], dtype=int)
    y_real = np.asarray([int(v[h0]) for v in real_df["labels"]], dtype=int)

    X_train = [_to_numpy_1d(v, dtype=float) for v in train_df["x"].tolist()]
    X_test = [_to_numpy_1d(v, dtype=float) for v in test_df["x"].tolist()]
    X_real = [_to_numpy_1d(v, dtype=float) for v in real_df["x"].tolist()]

    return {
        "X_train": X_train,
        "y_train": y_train,
        "X_test": X_test,
        "y_test": y_test,
        "X_real": X_real,
        "y_real": y_real,
    }

def get_chronos_data(
    freq_tag: str,
    model_id: str,
    window_mode: str = "default",
) -> Dict[str, Any]:
    paths = get_paths(freq_tag, window_mode=window_mode)

    all_df = _normalise_eval_df(read_rds(paths.labeled_rds).copy())
    real_df = _normalise_eval_df(read_rds(paths.real_eval_rds).copy())

    for col in ["x", "labels"]:
        if col not in all_df.columns:
            raise ValueError(f"Missing column '{col}' in {paths.labeled_rds}")
        if col not in real_df.columns:
            raise ValueError(f"Missing column '{col}' in {paths.real_eval_rds}")

    if str(window_mode).strip().lower() == "full":
        test_df = all_df.reset_index(drop=True)
    else:
        split_index = read_rds(paths.split_index_rds)
        split_use = get_model_split(split_index, model_id=model_id)
        test_idx = split_use["test"]
        test_df = all_df.iloc[test_idx].reset_index(drop=True)

    real_df = real_df.reset_index(drop=True)

    return {
        "X_test": [_to_numpy_1d(v, dtype=float) for v in test_df["x"].tolist()],
        "y_test": [_to_numpy_1d(v, dtype=int) for v in test_df["labels"].tolist()],
        "X_real": [_to_numpy_1d(v, dtype=float) for v in real_df["x"].tolist()],
        "y_real": [_to_numpy_1d(v, dtype=int) for v in real_df["labels"].tolist()],
        "series_name_test": test_df["series_name"].tolist() if "series_name" in test_df.columns else None,
        "series_name_real": real_df["series_name"].tolist() if "series_name" in real_df.columns else None,
    }