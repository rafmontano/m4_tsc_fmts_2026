# ==============================================================================
# eval_reports.py
#
# Purpose:
#   Connect Python model predictions to the shared R evaluation functions.
# Inputs:
#   Predicted and actual labels, evaluation metadata, and result containers.
# Outputs:
#   R evaluation objects and consolidated RDS result files.
# Used by:
#   The sktime, Chronos, and Mantis experiment runners.
# ==============================================================================

import numpy as np
import rpy2.robjects as ro

from src.python.r_bridge_sktime import PROJECT_ROOT


_R_LOADED = False


def _tag_to_period(freq_tag: str) -> str:
    mapping = {
        "y": "Yearly",
        "q": "Quarterly",
        "m": "Monthly",
        "w": "Weekly",
        "d": "Daily",
        "h": "Hourly",
    }

    return mapping[str(freq_tag).strip().lower()]


def _ensure_r_loaded() -> None:
    global _R_LOADED

    if _R_LOADED:
        return

    ro.r["source"](str(PROJECT_ROOT / "src" / "r" / "utils.R"))
    ro.r["source"](str(PROJECT_ROOT / "src" / "r" / "util_metrics.R"))

    _R_LOADED = True


def init_r_eval_containers(n_horizons: int):
    _ensure_r_loaded()

    return ro.globalenv["init_eval_containers"](int(n_horizons))


def compute_r_binary_eval(
    y_true,
    y_pred,
    *,
    freq_tag: str,
    horizon_id: int,
    eval_type: str,
):
    _ensure_r_loaded()

    period_i = _tag_to_period(freq_tag)
    y_true = np.asarray(y_true).reshape(-1).astype(int)
    y_pred = np.asarray(y_pred).reshape(-1).astype(int)

    return ro.globalenv["compute_binary_eval"](
        ro.IntVector(y_true.tolist()),
        ro.IntVector(y_pred.tolist()),
        period_i,
        freq_tag,
        int(horizon_id),
        eval_type,
    )


def store_r_test_result(containers, horizon_id: int, res_test):
    _ensure_r_loaded()

    return ro.globalenv["store_test_eval_result"](
        containers,
        int(horizon_id),
        res_test,
    )


def store_r_real_result(
    containers,
    horizon_id: int,
    res_real,
    series_name=None,
):
    _ensure_r_loaded()

    if series_name is None:
        return ro.globalenv["store_real_eval_result"](
            containers,
            int(horizon_id),
            res_real,
            ro.r("NULL"),
        )

    return ro.globalenv["store_real_eval_result"](
        containers,
        int(horizon_id),
        res_real,
        ro.StrVector(list(series_name)),
    )


def build_r_consolidated_eval_object(containers, real_eval_df=None):
    _ensure_r_loaded()

    if real_eval_df is None:
        return ro.globalenv["build_consolidated_eval_object"](
            containers,
            ro.r("NULL"),
        )

    return ro.globalenv["build_consolidated_eval_object"](
        containers,
        real_eval_df,
    )


def save_r_consolidated_eval_object(
    eval_obj,
    eval_rds_path: str,
    window_mode: str = "default",
):
    _ensure_r_loaded()

    return ro.globalenv["save_consolidated_eval_object"](
        eval_obj,
        eval_rds_path,
        window_mode,
    )


def get_r_summary_accuracy(res_eval) -> float:
    summary_df = res_eval.rx2("summary")

    return float(summary_df.rx2("accuracy")[0])


def compute_label_vector(x_hist, xx_pred):
    x_hist = np.asarray(x_hist, dtype=float)
    xx_pred = np.asarray(xx_pred, dtype=float)

    if x_hist.size == 0:
        raise ValueError("x_hist is empty")

    if xx_pred.size == 0:
        raise ValueError("xx_pred is empty")

    x_last = x_hist[-1]

    return (xx_pred > x_last).astype(np.int32)