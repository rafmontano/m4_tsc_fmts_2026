# ==============================================================================
# get_sktime_models.py
#
# Purpose:
#   Define and initialise the models available to the Python experiment runners.
# Inputs:
#   Model identifier, execution settings, and model hyperparameters.
# Outputs:
#   A dictionary containing the requested initialised models.
# Used by:
#   The conventional-classifier and Chronos experiment runners.
# ==============================================================================

from __future__ import annotations

from typing import Callable, Dict, List


def get_available_model_ids() -> List[str]:
    return [
        "DTW",
        "EUCLIDEAN",
        "RotF",
        "ROCKET",
        "InceptionTime",
        "HIVECOTEV2",
        "CHRONOS",
    ]


def _build_registry() -> Dict[str, Callable[..., object]]:
    registry: Dict[str, Callable[..., object]] = {}

    def make_dtw(
        *,
        n_jobs: int,
        dtw_window_frac: float,
        knn_k: int = 1,
        **_,
    ):
        from sktime.classification.distance_based import (
            KNeighborsTimeSeriesClassifier,
        )

        return KNeighborsTimeSeriesClassifier(
            n_neighbors=int(knn_k),
            weights="uniform",
            distance="dtw",
            distance_params={"window": float(dtw_window_frac)},
            algorithm="brute_incr",
            n_jobs=int(n_jobs),
        )

    def make_euclidean(
        *,
        n_jobs: int,
        knn_k: int = 1,
        **_,
    ):
        from sktime.classification.distance_based import (
            KNeighborsTimeSeriesClassifier,
        )

        return KNeighborsTimeSeriesClassifier(
            n_neighbors=int(knn_k),
            weights="uniform",
            distance="euclidean",
            algorithm="brute_incr",
            n_jobs=int(n_jobs),
        )

    def make_rotf(
        *,
        n_jobs: int,
        random_state: int,
        rotf_n_estimators: int = 50,
        **_,
    ):
        from sktime.classification.sklearn import RotationForest

        return RotationForest(
            n_estimators=int(rotf_n_estimators),
            n_jobs=int(n_jobs),
            random_state=int(random_state),
        )

    def make_rocket(
        *,
        n_jobs: int,
        random_state: int,
        rocket_num_kernels: int,
        **_,
    ):
        from sktime.classification.kernel_based import RocketClassifier

        return RocketClassifier(
            num_kernels=int(rocket_num_kernels),
            rocket_transform="rocket",
            use_multivariate="auto",
            n_jobs=int(n_jobs),
            random_state=int(random_state),
        )

    def make_inception(
        *,
        random_state: int,
        inception_epochs: int,
        inception_batch_size: int,
        **_,
    ):
        from sktime.classification.deep_learning import InceptionTimeClassifier

        return InceptionTimeClassifier(
            n_epochs=int(inception_epochs),
            batch_size=int(inception_batch_size),
            random_state=int(random_state),
            loss="categorical_crossentropy",
            verbose=False,
        )

    def make_hivecote(
        *,
        n_jobs: int,
        random_state: int,
        hivecote_time_limit_minutes: float = 1.0,
        **_,
    ):
        from sktime.classification.hybrid import HIVECOTEV2

        return HIVECOTEV2(
            time_limit_in_minutes=float(hivecote_time_limit_minutes),
            n_jobs=int(n_jobs),
            random_state=int(random_state),
            verbose=0,
        )

    def make_chronos(
        *,
        random_state: int,
        device_map: str = None,
        **_,
    ):
        import torch
        from sktime.forecasting.chronos import ChronosForecaster

        if device_map is None:
            if torch.cuda.is_available():
                device_map = "cuda"
            elif torch.backends.mps.is_available():
                device_map = "mps"
            else:
                device_map = "cpu"

        dtype = torch.bfloat16 if device_map in ["cuda", "mps"] else torch.float32

        return ChronosForecaster(
            model_path="amazon/chronos-t5-large",
            config={
                "device_map": device_map,
                "dtype": dtype,
            },
            seed=int(random_state),
        )

    registry["DTW"] = make_dtw
    registry["EUCLIDEAN"] = make_euclidean
    registry["RotF"] = make_rotf
    registry["ROCKET"] = make_rocket
    registry["InceptionTime"] = make_inception
    registry["HIVECOTEV2"] = make_hivecote
    registry["CHRONOS"] = make_chronos

    return registry


def get_sktime_models(
    *,
    model_id: str = "ALL",
    n_jobs: int = 1,
    random_state: int = 42,
    knn_k: int = 1,
    dtw_window_frac: float = 0.10,
    rotf_n_estimators: int = 50,
    rocket_num_kernels: int = 10000,
    inception_epochs: int = 10,
    inception_batch_size: int = 64,
    hivecote_time_limit_minutes: float = 1.0,
    device_map: str = "cpu",
) -> Dict[str, object]:
    registry = _build_registry()
    available = get_available_model_ids()

    mid = str(model_id).strip()
    selected = available if mid.upper() == "ALL" else [mid]

    unknown = [model for model in selected if model not in registry]

    if unknown:
        raise ValueError(
            f"Unknown model_id(s)={unknown}. Available={available}"
        )

    models: Dict[str, object] = {}

    for model_id_selected in selected:
        factory = registry[model_id_selected]

        try:
            models[model_id_selected] = factory(
                n_jobs=n_jobs,
                random_state=random_state,
                knn_k=knn_k,
                dtw_window_frac=dtw_window_frac,
                rotf_n_estimators=rotf_n_estimators,
                rocket_num_kernels=rocket_num_kernels,
                inception_epochs=inception_epochs,
                inception_batch_size=inception_batch_size,
                hivecote_time_limit_minutes=hivecote_time_limit_minutes,
                device_map=device_map,
            )
        except Exception as error:
            raise RuntimeError(
                f"Failed to initialize model '{model_id_selected}'. "
                "Optional dependencies may be missing or misconfigured. "
                f"Original error: {error!r}"
            ) from error

    return models