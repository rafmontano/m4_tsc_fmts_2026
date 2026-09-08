# ==============================================================================
# run_tsc_chronos2.py
#
# Purpose:
#   Run the Chronos-2 forecasting experiments used in the paper.
# Inputs:
#   R pipeline artifacts, split indices, frequencies, and window modes.
# Outputs:
#   Consolidated Chronos-2 evaluation RDS files.
# Run from:
#   Project root using the foundation-model Python environment.
# ==============================================================================

import warnings
from typing import Any, Dict, List

import numpy as np

from src.python.chronos_forecast import chronos_forecast
from src.python.eval_reports import (
    build_r_consolidated_eval_object,
    compute_label_vector,
    compute_r_binary_eval,
    get_r_summary_accuracy,
    init_r_eval_containers,
    save_r_consolidated_eval_object,
    store_r_real_result,
    store_r_test_result,
)
from src.python.r_bridge_sktime import (
    PROJECT_ROOT,
    RESULTS_DIR,
    get_chronos_data,
    get_paths,
)


warnings.filterwarnings("ignore")
warnings.filterwarnings("ignore", category=FutureWarning)


FREQ_TAGS = ["w", "h", "y", "q", "m", "d"]
MODEL_ID = "CHRONOS"
MODEL_NAME = "amazon/chronos-2"
# WINDOW_MODES = ["small", "default", "large", "full"]
WINDOW_MODES = ["default"]

FREQUENCY_BY_TAG = {
    "y": "D",
    "q": "QS",
    "m": "MS",
    "w": "D",
    "d": "D",
    "h": "h",
}


def _safe_len(x: Any) -> int:
    try:
        return len(x)
    except Exception:
        return -1


def _get_n_horizons_from_dataset(
    dataset: Dict[str, Any],
) -> int:
    y_test = dataset["y_test"]
    y_real = dataset["y_real"]

    if len(y_test) > 0:
        return int(len(y_test[0]))

    if len(y_real) > 0:
        return int(len(y_real[0]))

    raise ValueError(
        "Cannot infer horizon length from empty Chronos-2 dataset."
    )


def _forecast_chronos2_labels(
    series_list: List[np.ndarray],
    n_horizons: int,
    freq: str,
) -> np.ndarray:
    all_labels = []

    for index, x_hist in enumerate(series_list):
        x_hist = np.asarray(
            x_hist,
            dtype=float,
        ).reshape(-1)

        if x_hist.size == 0:
            raise ValueError(
                f"Chronos-2 input series is empty at index {index}."
            )

        xx_pred = chronos_forecast(
            x=x_hist,
            h=n_horizons,
            model_name=MODEL_NAME,
            freq=freq,
        )
        labels = compute_label_vector(
            x_hist,
            xx_pred,
        )
        labels = np.asarray(
            labels,
            dtype=np.int32,
        ).reshape(-1)

        if labels.size != n_horizons:
            raise ValueError(
                "Chronos-2 label length mismatch for series "
                f"index {index}. Expected {n_horizons}, "
                f"got {labels.size}."
            )

        all_labels.append(labels)

    return np.asarray(
        all_labels,
        dtype=np.int32,
    )


def _evaluate_all_horizons(
    *,
    freq_tag: str,
    window_mode: str,
    containers,
    y_test_all,
    pred_test_all: np.ndarray,
    y_real_all,
    pred_real_all: np.ndarray,
    series_name_real=None,
):
    n_horizons = pred_test_all.shape[1]

    for horizon_index in range(n_horizons):
        horizon_id = horizon_index + 1

        y_test = np.asarray(
            [
                int(value[horizon_index])
                for value in y_test_all
            ],
            dtype=int,
        )
        y_test_pred = np.asarray(
            pred_test_all[:, horizon_index],
            dtype=int,
        )

        res_test = compute_r_binary_eval(
            y_true=y_test,
            y_pred=y_test_pred,
            freq_tag=freq_tag,
            horizon_id=horizon_id,
            eval_type="test",
        )
        containers = store_r_test_result(
            containers,
            horizon_id,
            res_test,
        )

        print(
            f"[TEST] chronos_{window_mode}_h{horizon_id:02d}: "
            f"{get_r_summary_accuracy(res_test):.4f}"
        )

        y_real = np.asarray(
            [
                int(value[horizon_index])
                for value in y_real_all
            ],
            dtype=int,
        )
        y_real_pred = np.asarray(
            pred_real_all[:, horizon_index],
            dtype=int,
        )

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
            f"[REAL] chronos_{window_mode}_h{horizon_id:02d}: "
            f"{get_r_summary_accuracy(res_real):.4f}"
        )

    return containers


def run_one_frequency_window(
    *,
    freq_tag: str,
    window_mode: str,
):
    paths = get_paths(
        freq_tag,
        window_mode=window_mode,
    )
    model_key = "chronos"
    freq = FREQUENCY_BY_TAG[freq_tag]

    print("\n==================================================")
    print(
        f" CHRONOS-2 EXPERIMENT | freq={freq_tag} "
        f"| mode={window_mode}"
    )
    print("==================================================")
    print(f"Labeled : {paths.labeled_rds}")
    print(f"Split   : {paths.split_index_rds}")
    print(f"REAL    : {paths.real_eval_rds}")
    print(f"Model   : {MODEL_NAME}")
    print(f"Out dir : {RESULTS_DIR / model_key}")

    results_dir = RESULTS_DIR / model_key
    results_dir.mkdir(
        parents=True,
        exist_ok=True,
    )

    dataset = get_chronos_data(
        freq_tag=freq_tag,
        model_id=MODEL_ID,
        window_mode=window_mode,
    )

    X_test = dataset["X_test"]
    y_test = dataset["y_test"]
    X_real = dataset["X_real"]
    y_real = dataset["y_real"]
    series_name_real = dataset.get(
        "series_name_real",
        None,
    )

    n_horizons = _get_n_horizons_from_dataset(dataset)

    print(
        f"[LOAD] X_test={_safe_len(X_test)}, "
        f"y_test={_safe_len(y_test)}"
    )
    print(
        f"[REAL] X_real={_safe_len(X_real)}, "
        f"y_real={_safe_len(y_real)}"
    )
    print(f"[INFO] n_horizons={n_horizons}")
    print("[FIT] Skipped for Chronos-2 zero-shot forecasting.")

    print("[PREDICT] Forecasting TEST full horizon...")
    pred_test_all = _forecast_chronos2_labels(
        series_list=X_test,
        n_horizons=n_horizons,
        freq=freq,
    )

    print("[PREDICT] Forecasting REAL full horizon...")
    pred_real_all = _forecast_chronos2_labels(
        series_list=X_real,
        n_horizons=n_horizons,
        freq=freq,
    )

    containers = init_r_eval_containers(n_horizons)
    containers = _evaluate_all_horizons(
        freq_tag=freq_tag,
        window_mode=window_mode,
        containers=containers,
        y_test_all=y_test,
        pred_test_all=pred_test_all,
        y_real_all=y_real,
        pred_real_all=pred_real_all,
        series_name_real=series_name_real,
    )

    eval_obj = build_r_consolidated_eval_object(containers)
    eval_rds_path = str(
        results_dir / f"{model_key}_eval_{freq_tag}.rds"
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
    print(" Running Chronos-2 experiments (R -> Python)")
    print("===============================================")
    print(f"PROJECT_ROOT : {PROJECT_ROOT}")
    print(f"FREQ_TAGS    : {FREQ_TAGS}")
    print(f"MODEL_NAME   : {MODEL_NAME}")
    print(f"WINDOW_MODES : {WINDOW_MODES}")

    for freq_tag in FREQ_TAGS:
        for window_mode in WINDOW_MODES:
            run_one_frequency_window(
                freq_tag=freq_tag,
                window_mode=window_mode,
            )

    print("\n===============================================")
    print(" All requested Chronos-2 experiments completed.")
    print("===============================================")


if __name__ == "__main__":
    main()
