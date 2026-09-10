# ==============================================================================
# run_tsc_experiment.py
#
# Purpose:
#   Train and evaluate the conventional classifiers used in the paper.
# Inputs:
#   R pipeline artifacts, split indices, model settings, and window modes.
# Outputs:
#   Fitted models and consolidated evaluation RDS files.
# Run from:
#   Project root using the classifier Python environment.
# ==============================================================================

import os
import warnings
from typing import Any

from src.python.eval_reports import (
    build_r_consolidated_eval_object,
    compute_r_binary_eval,
    get_r_summary_accuracy,
    init_r_eval_containers,
    save_r_consolidated_eval_object,
    store_r_real_result,
    store_r_test_result,
)
from src.python.get_sktime_models import (
    get_available_model_ids,
    get_sktime_models,
)
from src.python.model_io import save_models
from src.python.r_bridge_sktime import (
    MODELS_DIR,
    PROJECT_ROOT,
    RESULTS_DIR,
    _normalise_eval_df,
    get_model_data,
    get_paths,
    read_rds,
    window_tag,
)


os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"

warnings.filterwarnings("ignore")
warnings.filterwarnings("ignore", category=FutureWarning)

try:
    from numba.core.errors import NumbaTypeSafetyWarning

    warnings.filterwarnings(
        "ignore",
        category=NumbaTypeSafetyWarning,
    )
except Exception:
    pass

FREQ_TAGS = ["w", "h", "d", "y", "q", "m"]

MODEL_ID = [
    "RotF",
    "ROCKET",
    "InceptionTime",
]

# WINDOW_MODES = [
#     "small",
#     "default",
#     "large",
#     "full",
# ]
WINDOW_MODES = ["default"]

N_JOBS = 8

ROCKET_NUM_KERNELS = 10000
INCEPTION_EPOCHS = 150
INCEPTION_BATCH_SIZE = 32

os.environ["JOBLIB_TEMP_FOLDER"] = str(PROJECT_ROOT / "tmp_joblib")
(PROJECT_ROOT / "tmp_joblib").mkdir(parents=True, exist_ok=True)


def _safe_shape(x: Any):
    return getattr(x, "shape", (len(x),))


def _get_n_horizons(freq_tag: str, window_mode: str) -> int:
    paths = get_paths(freq_tag, window_mode=window_mode)
    df = _normalise_eval_df(read_rds(paths.features_rds).copy())

    return int(len(df["labels"].iloc[0]))


def run_one_frequency(
    freq_tag: str,
    model_id: str,
    horizon_id: int,
    window_mode: str,
    containers,
):
    paths = get_paths(freq_tag, window_mode=window_mode)
    model_key = str(model_id).strip().lower()
    window_key = window_tag(window_mode)

    print("\n==================================================")
    print(
        f" TSC EXPERIMENT | freq={freq_tag} | mode={window_mode} "
        f"| model={model_id} | h={horizon_id}"
    )
    print("==================================================")
    print(f"Features : {paths.features_rds}")
    print(f"Split    : {paths.split_index_rds}")
    print(f"REAL     : {paths.real_eval_rds}")
    print(f"Subset   : {paths.subset_clean_rds}")

    results_dir = RESULTS_DIR / model_key
    models_dir = MODELS_DIR / model_key / f"{freq_tag}_{window_key}"

    results_dir.mkdir(parents=True, exist_ok=True)
    models_dir.mkdir(parents=True, exist_ok=True)

    print(f"Out   : {results_dir}")
    print(f"Models: {models_dir}")

    dataset = get_model_data(
        freq_tag=freq_tag,
        model_id=model_id,
        horizon_id=horizon_id,
        window_mode=window_mode,
    )

    X_train = dataset["X_train"]
    y_train = dataset["y_train"]
    X_test = dataset["X_test"]
    y_test = dataset["y_test"]
    X_real = dataset["X_real"]
    y_real = dataset["y_real"]

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

    models = get_sktime_models(
        model_id=model_id,
        n_jobs=N_JOBS,
        random_state=42,
        rocket_num_kernels=ROCKET_NUM_KERNELS,
        inception_epochs=INCEPTION_EPOCHS,
        inception_batch_size=INCEPTION_BATCH_SIZE,
    )

    name, model = next(iter(models.items()))
    print(f"[MODEL] {name}")

    print("\n-----------------------------------------------")
    print(f"[FIT] {name} | h={horizon_id}")
    print("-----------------------------------------------")

    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)

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
        {model_key: model},
        models_dir,
        freq_tag=freq_tag,
        horizon_id=horizon_id,
    )

    y_real_pred = model.predict(X_real)

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
        None,
    )

    print("\n[DONE] Frequency completed.")

    return containers


def main():
    print("\n===============================================")
    print(" Running conventional classifier experiments")
    print("===============================================")
    print(f"PROJECT_ROOT       : {PROJECT_ROOT}")
    print(f"FREQ_TAGS          : {FREQ_TAGS}")
    print(f"MODEL_ID           : {MODEL_ID}")
    print(f"WINDOW_MODES       : {WINDOW_MODES}")
    print(f"N_JOBS             : {N_JOBS}")
    print(f"ROCKET_NUM_KERNELS : {ROCKET_NUM_KERNELS}")
    print(f"INCEPTION_EPOCHS   : {INCEPTION_EPOCHS}")
    print(f"INCEPTION_BATCH    : {INCEPTION_BATCH_SIZE}")
    print(f"Available models   : {get_available_model_ids()}")

    model_ids = MODEL_ID if isinstance(MODEL_ID, list) else [MODEL_ID]

    for freq_tag in FREQ_TAGS:
        for window_mode in WINDOW_MODES:
            try:
                n_horizons = _get_n_horizons(
                    freq_tag,
                    window_mode,
                )
            except Exception as error:
                print(
                    f"[FAILED] freq={freq_tag} mode={window_mode} "
                    f"| reason: {error}"
                )
                raise

            for model_id in model_ids:
                containers = init_r_eval_containers(n_horizons)

                for horizon_id in range(1, n_horizons + 1):
                    try:
                        containers = run_one_frequency(
                            freq_tag,
                            model_id,
                            horizon_id,
                            window_mode,
                            containers,
                        )
                    except Exception as error:
                        print(
                            f"[FAILED] freq={freq_tag} mode={window_mode} "
                            f"model={model_id} h={horizon_id} "
                            f"| reason: {error}"
                        )
                        raise

                eval_obj = build_r_consolidated_eval_object(containers)
                eval_rds_path = str(
                    RESULTS_DIR
                    / str(model_id).strip().lower()
                    / f"{str(model_id).strip().lower()}_eval_{freq_tag}.rds"
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
