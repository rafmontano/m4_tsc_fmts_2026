# ==============================================================================
# run_mantis_experiment.py
#
# Purpose:
#   Train and evaluate Mantis directional classifiers.
# Inputs:
#   R pipeline artifacts, split indices, M4 series, and Mantis configuration.
# Outputs:
#   Fitted models, evaluation RDS files, and horizon-level predictions.
# Run from:
#   Project root using the foundation-model Python environment.
# ==============================================================================

import os
import warnings
from typing import Any, Dict, List

import numpy as np
import pandas as pd
import rpy2.robjects as ro

from src.python.eval_reports import (
    build_r_consolidated_eval_object,
    compute_r_binary_eval,
    get_r_summary_accuracy,
    init_r_eval_containers,
    save_r_consolidated_eval_object,
    store_r_real_result,
    store_r_test_result,
)
from src.python.mantis_models import (
    fit_mantis_rf,
    predict_mantis_rf,
)
from src.python.model_io import save_models
from src.python.r_bridge_sktime import (
    MODELS_DIR,
    PROJECT_ROOT,
    RESULTS_DIR,
    _normalise_eval_df,
    get_mantis_data,
    get_paths,
    read_rds,
    window_tag,
)


warnings.filterwarnings("ignore")
warnings.filterwarnings(
    "ignore",
    category=FutureWarning,
)

os.environ["JOBLIB_TEMP_FOLDER"] = str(
    PROJECT_ROOT / "tmp_joblib"
)
(PROJECT_ROOT / "tmp_joblib").mkdir(
    parents=True,
    exist_ok=True,
)


FREQ_TAGS = [
    "w",
    "h",
    "d",
    "y",
    "q",
    "m",
]

MODEL_ID = "MANTIS"

# WINDOW_MODES = [
#     "small",
#     "default",
#     "large",
#     "full",
# ]
WINDOW_MODES = ["default"]

MANTIS_CHECKPOINT = "paris-noah/Mantis-8M"
MANTIS_RESIZE_TO = 512

RF_N_ESTIMATORS = 200
RF_RANDOM_STATE = 42
RF_N_JOBS = -1

MANTIS_DA_OUTPUT = (
    PROJECT_ROOT
    / "data"
    / "fct"
    / "mantis_da_horizon.csv"
)
M4_FCT_TABLE6_RDS = (
    PROJECT_ROOT
    / "data"
    / "fct"
    / "M4_fct_table6.rds"
)

PERIOD_BY_TAG = {
    "y": "Yearly",
    "q": "Quarterly",
    "m": "Monthly",
    "w": "Weekly",
    "d": "Daily",
    "h": "Hourly",
}


def _safe_shape(x: Any):
    return getattr(
        x,
        "shape",
        (len(x),),
    )


def get_available_device() -> str:
    try:
        import torch

        if torch.cuda.is_available():
            return "cuda"

        if getattr(torch.backends, "mps", None) is not None:
            if torch.backends.mps.is_available():
                return "mps"

    except Exception:
        pass

    return "cpu"


MANTIS_DEVICE = get_available_device()


def _get_n_horizons(
    freq_tag: str,
    window_mode: str,
) -> int:
    paths = get_paths(
        freq_tag,
        window_mode=window_mode,
    )
    df = _normalise_eval_df(
        read_rds(paths.features_rds).copy()
    )

    return int(len(df["labels"].iloc[0]))


def _get_m4_real_series_ids(
    freq_tag: str,
    n_expected: int,
) -> List[str]:
    if not M4_FCT_TABLE6_RDS.exists():
        return [
            f"{freq_tag}_{index + 1}"
            for index in range(n_expected)
        ]

    read_rds_r = ro.r["readRDS"]
    m4_obj = read_rds_r(str(M4_FCT_TABLE6_RDS))

    ro.globalenv["m4_obj_tmp"] = m4_obj
    ro.globalenv["period_target_tmp"] = PERIOD_BY_TAG[freq_tag]

    ids = list(
        ro.r(
            """
            out <- character(0)
            for (i in seq_along(m4_obj_tmp)) {
              p <- as.character(m4_obj_tmp[[i]]$period)
              if (identical(p, period_target_tmp)) {
                out <- c(out, as.character(m4_obj_tmp[[i]]$st))
              }
            }
            out
            """
        )
    )

    if len(ids) != n_expected:
        print(
            f"[WARN] Series ID count mismatch for freq={freq_tag}: "
            f"ids={len(ids)}, expected={n_expected}. "
            "Using fallback IDs."
        )

        return [
            f"{freq_tag}_{index + 1}"
            for index in range(n_expected)
        ]

    return ids


def _append_mantis_da_records(
    records: List[Dict[str, Any]],
    freq_tag: str,
    window_mode: str,
    horizon_id: int,
    y_real: np.ndarray,
    y_real_pred: np.ndarray,
    series_name_real=None,
) -> None:
    y_real = np.asarray(y_real).astype(int).reshape(-1)
    y_real_pred = np.asarray(y_real_pred).astype(int).reshape(-1)

    if len(y_real) != len(y_real_pred):
        raise ValueError(
            f"REAL prediction length mismatch for freq={freq_tag}, "
            f"window={window_mode}, h={horizon_id}: "
            f"y_real={len(y_real)}, y_pred={len(y_real_pred)}"
        )

    if series_name_real is None:
        series_ids = _get_m4_real_series_ids(
            freq_tag,
            len(y_real_pred),
        )
    else:
        series_ids = [
            str(series_name)
            for series_name in series_name_real
        ]

        if len(series_ids) != len(y_real_pred):
            raise ValueError(
                f"Series ID count mismatch for freq={freq_tag}: "
                f"ids={len(series_ids)}, "
                f"predictions={len(y_real_pred)}"
            )

    for st, actual, prediction in zip(
        series_ids,
        y_real,
        y_real_pred,
    ):
        records.append(
            {
                "st": st,
                "freq_tag": freq_tag,
                "window_mode": window_mode,
                "horizon_id": horizon_id,
                "mantis_da": int(prediction),
                "actual_da": int(actual),
            }
        )


def _save_mantis_da_records(
    records: List[Dict[str, Any]],
) -> None:
    MANTIS_DA_OUTPUT.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    df = pd.DataFrame(records)

    if MANTIS_DA_OUTPUT.exists():
        old = pd.read_csv(MANTIS_DA_OUTPUT)
        df = pd.concat(
            [old, df],
            ignore_index=True,
        )
        df = df.drop_duplicates(
            subset=[
                "st",
                "freq_tag",
                "window_mode",
                "horizon_id",
            ],
            keep="last",
        )

    df.to_csv(
        MANTIS_DA_OUTPUT,
        index=False,
    )

    print(
        f"[SAVE] Mantis DA CSV: {MANTIS_DA_OUTPUT} "
        f"| rows={len(df)}"
    )


def run_one_frequency(
    freq_tag: str,
    window_mode: str,
    horizon_id: int,
    containers,
    da_records: List[Dict[str, Any]],
):
    paths = get_paths(
        freq_tag,
        window_mode=window_mode,
    )
    model_key = str(MODEL_ID).strip().lower()
    wtag = window_tag(window_mode)

    print("\n==================================================")
    print(
        f" MANTIS EXPERIMENT | freq={freq_tag} "
        f"| mode={window_mode} "
        f"| model={MODEL_ID} "
        f"| h={horizon_id}"
    )
    print("==================================================")
    print(f"Features : {paths.features_rds}")
    print(f"Split    : {paths.split_index_rds}")
    print(f"REAL     : {paths.real_eval_rds}")
    print(f"Subset   : {paths.subset_clean_rds}")

    results_dir = RESULTS_DIR / model_key
    models_dir = (
        MODELS_DIR
        / model_key
        / f"{freq_tag}_{wtag}"
    )

    results_dir.mkdir(
        parents=True,
        exist_ok=True,
    )
    models_dir.mkdir(
        parents=True,
        exist_ok=True,
    )

    dataset = get_mantis_data(
        freq_tag=freq_tag,
        model_id=MODEL_ID,
        horizon_id=horizon_id,
        window_mode=window_mode,
    )

    X_train = dataset["X_train"]
    y_train = dataset["y_train"]
    X_test = dataset["X_test"]
    y_test = dataset["y_test"]
    X_real = dataset["X_real"]
    y_real = dataset["y_real"]
    series_name_real = dataset.get(
        "series_name_real",
        None,
    )

    print(
        f"[LOAD] X_train={_safe_shape(X_train)}, "
        f"y_train={_safe_shape(y_train)}"
    )
    print(
        f"[LOAD] X_test={_safe_shape(X_test)}, "
        f"y_test={_safe_shape(y_test)}"
    )
    print(
        f"[REAL] X_real={_safe_shape(X_real)}, "
        f"y_real={_safe_shape(y_real)}"
    )

    print("\n-----------------------------------------------")
    print(f"[FIT] Mantis + RF | h={horizon_id}")
    print("-----------------------------------------------")

    fit_obj = fit_mantis_rf(
        X_train=X_train,
        y_train=y_train,
        device=MANTIS_DEVICE,
        checkpoint=MANTIS_CHECKPOINT,
        resize_to=MANTIS_RESIZE_TO,
        n_estimators=RF_N_ESTIMATORS,
        random_state=RF_RANDOM_STATE,
        n_jobs=RF_N_JOBS,
    )

    trainer = fit_obj["trainer"]
    predictor = fit_obj["predictor"]
    Z_train = fit_obj["Z_train"]

    print(f"[EMBED] Z_train={_safe_shape(Z_train)}")

    y_pred, Z_test = predict_mantis_rf(
        X=X_test,
        trainer=trainer,
        predictor=predictor,
        resize_to=MANTIS_RESIZE_TO,
    )

    print(f"[EMBED] Z_test={_safe_shape(Z_test)}")

    res_test = compute_r_binary_eval(
        y_true=y_test,
        y_pred=y_pred,
        freq_tag=freq_tag,
        horizon_id=horizon_id,
        eval_type="test",
    )

    containers = store_r_test_result(
        containers,
        horizon_id,
        res_test,
    )

    print("\n=== TEST Accuracy Summary ===")
    print(
        f"{model_key}_{window_mode}_h{horizon_id:02d}: "
        f"{get_r_summary_accuracy(res_test):.4f}"
    )

    print("\n[SAVE] Writing fitted model...")

    save_models(
        {f"{model_key}_rf": predictor},
        models_dir,
        freq_tag=freq_tag,
        horizon_id=horizon_id,
    )

    y_real_pred, Z_real = predict_mantis_rf(
        X=X_real,
        trainer=trainer,
        predictor=predictor,
        resize_to=MANTIS_RESIZE_TO,
    )

    print(f"[EMBED] Z_real={_safe_shape(Z_real)}")

    res_real = compute_r_binary_eval(
        y_true=y_real,
        y_pred=y_real_pred,
        freq_tag=freq_tag,
        horizon_id=horizon_id,
        eval_type="real",
    )

    containers = store_r_real_result(
        containers,
        horizon_id,
        res_real,
        series_name_real,
    )

    _append_mantis_da_records(
        records=da_records,
        freq_tag=freq_tag,
        window_mode=window_mode,
        horizon_id=horizon_id,
        y_real=y_real,
        y_real_pred=y_real_pred,
        series_name_real=series_name_real,
    )

    print("\n[DONE] Frequency completed.")

    return containers, da_records


def main():
    print("\n===============================================")
    print(" Running Mantis experiments (R -> Python)")
    print("===============================================")
    print(f"PROJECT_ROOT       : {PROJECT_ROOT}")
    print(f"FREQ_TAGS          : {FREQ_TAGS}")
    print(f"MODEL_ID           : {MODEL_ID}")
    print(f"WINDOW_MODES       : {WINDOW_MODES}")
    print(f"MANTIS_DEVICE      : {MANTIS_DEVICE}")
    print(f"MANTIS_CHECKPOINT  : {MANTIS_CHECKPOINT}")
    print(f"MANTIS_RESIZE_TO   : {MANTIS_RESIZE_TO}")
    print(f"RF_N_ESTIMATORS    : {RF_N_ESTIMATORS}")
    print(f"RF_RANDOM_STATE    : {RF_RANDOM_STATE}")
    print(f"RF_N_JOBS          : {RF_N_JOBS}")
    print(f"MANTIS_DA_OUTPUT   : {MANTIS_DA_OUTPUT}")

    da_records: List[Dict[str, Any]] = []

    for freq_tag in FREQ_TAGS:
        for window_mode in WINDOW_MODES:
            try:
                n_horizons = _get_n_horizons(
                    freq_tag,
                    window_mode,
                )
            except Exception as error:
                print(
                    f"[FAILED] freq={freq_tag} "
                    f"mode={window_mode} "
                    f"| reason: {error}"
                )
                raise

            containers = init_r_eval_containers(
                n_horizons
            )

            for horizon_id in range(
                1,
                n_horizons + 1,
            ):
                try:
                    containers, da_records = run_one_frequency(
                        freq_tag=freq_tag,
                        window_mode=window_mode,
                        horizon_id=horizon_id,
                        containers=containers,
                        da_records=da_records,
                    )

                    _save_mantis_da_records(da_records)
                    da_records = []

                except Exception as error:
                    print(
                        f"[FAILED] freq={freq_tag} "
                        f"mode={window_mode} "
                        f"model={MODEL_ID} "
                        f"h={horizon_id} "
                        f"| reason: {error}"
                    )
                    raise

            eval_obj = build_r_consolidated_eval_object(
                containers
            )
            eval_rds_path = str(
                RESULTS_DIR
                / str(MODEL_ID).strip().lower()
                / (
                    f"{str(MODEL_ID).strip().lower()}"
                    f"_eval_{freq_tag}.rds"
                )
            )

            save_r_consolidated_eval_object(
                eval_obj,
                eval_rds_path,
                window_mode=window_mode,
            )

    print("\n===============================================")
    print(" All requested experiments completed.")
    print("===============================================")


if __name__ == "__main__":
    main()
