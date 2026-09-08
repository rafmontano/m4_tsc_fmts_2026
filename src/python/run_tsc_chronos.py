# File: src/python/run_tsc_chronos.py
# Purpose:
#   Chronos forecasting experiment runner.
#   Same high-level process as the TSC pipeline:
#     1) Load data
#     2) Init model
#     3) Skip training
#     4) Predict full horizon
#     5) Evaluate per horizon
#     6) Save consolidated results

import os
import warnings
from typing import Any, Dict, List

import numpy as np
from sktime.forecasting.base import ForecastingHorizon

from src.python.r_bridge_sktime import (
    PROJECT_ROOT,
    RESULTS_DIR,
    MODELS_DIR,
    get_paths,
    get_chronos_data,
    window_tag,
)
from src.python.get_sktime_models import get_sktime_models, get_available_model_ids
from src.python.eval_reports import (
    init_r_eval_containers,
    compute_r_binary_eval,
    store_r_test_result,
    store_r_real_result,
    build_r_consolidated_eval_object,
    save_r_consolidated_eval_object,
    get_r_summary_accuracy,
    compute_label_vector,
)

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
warnings.filterwarnings("ignore")
warnings.filterwarnings("ignore", category=FutureWarning)

try:
    from numba.core.errors import NumbaTypeSafetyWarning
    warnings.filterwarnings("ignore", category=NumbaTypeSafetyWarning)
except Exception:
    pass


# ==================================================
# USER CONSTANTS
# ==================================================

FREQ_TAGS = ["w", "h", "y", "q", "m", "d"]

MODEL_ID = ["CHRONOS"]

# Start conservative. Add/remove modes manually as needed.
WINDOW_MODES = ["small", "default", "large", "full"]


def detect_device() -> str:
    import torch

    if torch.cuda.is_available():
        return "cuda"
    if hasattr(torch.backends, "mps") and torch.backends.mps.is_available():
        return "mps"
    return "cpu"


def _safe_len(x: Any) -> int:
    try:
        return len(x)
    except Exception:
        return -1


def _get_n_horizons_from_dataset(dataset: Dict[str, Any]) -> int:
    y_test = dataset["y_test"]
    y_real = dataset["y_real"]

    if len(y_test) > 0:
        return int(len(y_test[0]))

    if len(y_real) > 0:
        return int(len(y_real[0]))

    raise ValueError("Cannot infer horizon length from empty Chronos dataset.")


def _forecast_chronos_labels(
    series_list: List[np.ndarray],
    model,
    n_horizons: int,
) -> np.ndarray:
    fh = ForecastingHorizon(np.arange(1, n_horizons + 1), is_relative=True)

    all_labels = []

    for i, x_hist in enumerate(series_list):
        x_hist = np.asarray(x_hist, dtype=float).reshape(-1)

        if x_hist.size == 0:
            all_labels.append(np.zeros(n_horizons, dtype=np.int32))
            continue

        try:
            model.fit(x_hist, fh=fh)
            xx_pred = model.predict(fh)
            labels = compute_label_vector(x_hist, xx_pred)

            labels = np.asarray(labels, dtype=np.int32).reshape(-1)

            if labels.size != n_horizons:
                fixed = np.zeros(n_horizons, dtype=np.int32)
                m = min(labels.size, n_horizons)
                fixed[:m] = labels[:m]
                labels = fixed

        except Exception as e:
            print(f"[WARN] Chronos failed for series index={i}; using zero labels. Reason: {e}")
            labels = np.zeros(n_horizons, dtype=np.int32)

        all_labels.append(labels)

    return np.asarray(all_labels, dtype=np.int32)


def _evaluate_all_horizons(
    *,
    freq_tag: str,
    window_mode: str,
    model_key: str,
    containers,
    y_test_all,
    pred_test_all: np.ndarray,
    y_real_all,
    pred_real_all: np.ndarray,
    series_name_real=None,
):
    n_horizons = pred_test_all.shape[1]

    for h in range(n_horizons):
        horizon_id = h + 1

        y_test = np.asarray([int(v[h]) for v in y_test_all], dtype=int)
        y_pred = np.asarray(pred_test_all[:, h], dtype=int)

        res_test = compute_r_binary_eval(
            y_true=y_test,
            y_pred=y_pred,
            freq_tag=freq_tag,
            horizon_id=horizon_id,
            eval_type="test",
        )

        containers = store_r_test_result(containers, horizon_id, res_test)

        print(
            f"[TEST] {model_key}_{window_mode}_h{horizon_id:02d}: "
            f"{get_r_summary_accuracy(res_test):.4f}"
        )

        y_real = np.asarray([int(v[h]) for v in y_real_all], dtype=int)
        y_real_pred = np.asarray(pred_real_all[:, h], dtype=int)

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

        print(
            f"[REAL] {model_key}_{window_mode}_h{horizon_id:02d}: "
            f"{get_r_summary_accuracy(res_real):.4f}"
        )

    return containers


def run_one_frequency_window(
    *,
    freq_tag: str,
    window_mode: str,
    model_id: str,
    device_map: str,
):
    paths = get_paths(freq_tag, window_mode=window_mode)
    mk = str(model_id).strip().lower()
    wtag = window_tag(window_mode)

    print("\n==================================================")
    print(f" CHRONOS EXPERIMENT | freq={freq_tag} | mode={window_mode} | model={model_id}")
    print("==================================================")
    print(f"Labeled : {paths.labeled_rds}")
    print(f"Split   : {paths.split_index_rds}")
    print(f"REAL    : {paths.real_eval_rds}")
    print(f"Out dir : {RESULTS_DIR / mk}")

    results_dir = RESULTS_DIR / mk
    models_dir = MODELS_DIR / mk / f"{freq_tag}_{wtag}"

    results_dir.mkdir(parents=True, exist_ok=True)
    models_dir.mkdir(parents=True, exist_ok=True)

    # 1) Load data
    dataset = get_chronos_data(
        freq_tag=freq_tag,
        model_id=model_id,
        window_mode=window_mode,
    )

    X_test = dataset["X_test"]
    y_test = dataset["y_test"]
    X_real = dataset["X_real"]
    y_real = dataset["y_real"]
    series_name_real = dataset.get("series_name_real", None)

    n_horizons = _get_n_horizons_from_dataset(dataset)

    print(f"[LOAD] X_test={_safe_len(X_test)}, y_test={_safe_len(y_test)}")
    print(f"[REAL] X_real={_safe_len(X_real)}, y_real={_safe_len(y_real)}")
    print(f"[INFO] n_horizons={n_horizons}")

    # 2) Init model
    models = get_sktime_models(
        model_id=model_id,
        random_state=42,
        device_map=device_map,
    )

    name, model = next(iter(models.items()))
    print(f"[MODEL] {name}")

    # 3) Fit/train step
    print("[FIT] Skipped for Chronos zero-shot forecasting.")

    # 4) Predict full horizon once
    print("[PREDICT] Forecasting TEST full horizon...")
    pred_test_all = _forecast_chronos_labels(
        series_list=X_test,
        model=model,
        n_horizons=n_horizons,
    )

    print("[PREDICT] Forecasting REAL full horizon...")
    pred_real_all = _forecast_chronos_labels(
        series_list=X_real,
        model=model,
        n_horizons=n_horizons,
    )

    # 5) Evaluate
    containers = init_r_eval_containers(n_horizons)

    containers = _evaluate_all_horizons(
        freq_tag=freq_tag,
        window_mode=window_mode,
        model_key=mk,
        containers=containers,
        y_test_all=y_test,
        pred_test_all=pred_test_all,
        y_real_all=y_real,
        pred_real_all=pred_real_all,
        series_name_real=series_name_real,
    )

    # 6) Save consolidated result
    eval_obj = build_r_consolidated_eval_object(containers)

    eval_rds_path = str(
        RESULTS_DIR / mk / f"{mk}_eval_{freq_tag}.rds"
    )

    save_r_consolidated_eval_object(
        eval_obj,
        eval_rds_path,
        window_mode=window_mode,
    )

    print(f"[SAVE] {eval_rds_path}")
    print("[DONE] Frequency/window completed.")


def main():
    print("\n===============================================")
    print(" Running Chronos experiments (R -> Python)")
    print("===============================================")
    print(f"PROJECT_ROOT       : {PROJECT_ROOT}")
    print(f"FREQ_TAGS          : {FREQ_TAGS}")
    print(f"MODEL_ID           : {MODEL_ID}")
    print(f"WINDOW_MODES       : {WINDOW_MODES}")
    print(f"Available models   : {get_available_model_ids()}")

    device_map = detect_device()
    print(f"DEVICE_MAP         : {device_map}")

    model_ids = MODEL_ID if isinstance(MODEL_ID, list) else [MODEL_ID]

    for freq_tag in FREQ_TAGS:
        for window_mode in WINDOW_MODES:
            for model_id in model_ids:
                try:
                    run_one_frequency_window(
                        freq_tag=freq_tag,
                        window_mode=window_mode,
                        model_id=model_id,
                        device_map=device_map,
                    )
                except Exception as e:
                    print(
                        f"[SKIP] freq={freq_tag} mode={window_mode} "
                        f"model={model_id} | reason: {e}"
                    )
                    continue

    print("\n===============================================")
    print(" All requested Chronos experiments completed.")
    print("===============================================")


if __name__ == "__main__":
    main()