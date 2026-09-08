# File: src/python/mantis_models.py
# Purpose:
#   Mantis feature-extraction pipeline for raw univariate rolling windows.
#   Contract:
#     - input: list[np.ndarray] of raw 1D series
#     - output: embeddings and RandomForest predictions

from __future__ import annotations

from typing import Any, Dict, List, Tuple

import numpy as np
import torch
import torch.nn.functional as F
from sklearn.ensemble import RandomForestClassifier

from mantis.architecture import Mantis8M
from mantis.trainer import MantisTrainer


# --------------------------------------------------
# Constants
# --------------------------------------------------

DEFAULT_MANTIS_CHECKPOINT = "paris-noah/Mantis-8M"
DEFAULT_RESIZE_TO = 512
DEFAULT_RF_TREES = 200
DEFAULT_RANDOM_STATE = 42


# --------------------------------------------------
# Small helpers
# --------------------------------------------------

def _as_1d_float_array(x: Any) -> np.ndarray:
    return np.asarray(x, dtype=float).reshape(-1)


def _stack_univariate_series(X: List[np.ndarray]) -> np.ndarray:
    arrays = [_as_1d_float_array(x) for x in X]

    lengths = [len(x) for x in arrays]
    if len(set(lengths)) != 1:
        raise ValueError(
            "All input series must have the same length before stacking. "
            f"Observed lengths: {sorted(set(lengths))}"
        )

    X_np = np.stack(arrays, axis=0)          # (n_samples, seq_len)
    X_np = X_np[:, np.newaxis, :]            # (n_samples, 1, seq_len)
    return X_np.astype(np.float32)


#def resize_for_mantis(X: List[np.ndarray], size: int = DEFAULT_RESIZE_TO) -> np.ndarray:
#    X_np3d = _stack_univariate_series(X)
#    X_tensor = torch.tensor(X_np3d, dtype=torch.float32)
#    X_scaled = F.interpolate(
#        X_tensor,
#        size=size,
#        mode="linear",
#        align_corners=False,
#    )
#    return X_scaled.cpu().numpy()

def resize_for_mantis(X: List[np.ndarray], size: int = DEFAULT_RESIZE_TO) -> np.ndarray:
    arrays = [_as_1d_float_array(x) for x in X]
    lengths = [len(x) for x in arrays]

    # --------------------------------------------------
    # FAST PATH: all same length (unchanged)
    # --------------------------------------------------
    if len(set(lengths)) == 1:
        X_np3d = _stack_univariate_series(arrays)
        X_tensor = torch.tensor(X_np3d, dtype=torch.float32)

        X_scaled = F.interpolate(
            X_tensor,
            size=size,
            mode="linear",
            align_corners=False,
        )

        return X_scaled.cpu().numpy().astype(np.float32)

    # --------------------------------------------------
    # GROUPED PATH: variable lengths (OPTIMISED)
    # --------------------------------------------------
    # 1. Group indexes by length
    groups = {}
    for idx, (x, l) in enumerate(zip(arrays, lengths)):
        if l not in groups:
            groups[l] = {"idx": [], "data": []}
        groups[l]["idx"].append(idx)
        groups[l]["data"].append(x)

    # 2. Prepare output container
    X_out = [None] * len(arrays)

    # 3. Process each group in batch
    for l, g in groups.items():
        batch = np.stack(g["data"], axis=0)          # (n_group, length)
        batch = batch[:, np.newaxis, :]              # (n_group, 1, length)

        batch_tensor = torch.tensor(batch, dtype=torch.float32)

        batch_scaled = F.interpolate(
            batch_tensor,
            size=size,
            mode="linear",
            align_corners=False,
        ).cpu().numpy()

        # 4. Place results back in original order
        for pos, original_idx in enumerate(g["idx"]):
            X_out[original_idx] = batch_scaled[pos]

    # 5. Stack final result
    X_ready = np.stack(X_out, axis=0)

    return X_ready.astype(np.float32)

def get_mantis_device(preferred: str = "cpu") -> str:
    preferred = str(preferred).strip().lower()

    if preferred == "cuda" and torch.cuda.is_available():
        return "cuda"

    if preferred == "mps" and getattr(torch.backends, "mps", None) is not None:
        if torch.backends.mps.is_available():
            return "mps"

    return "cpu"


# --------------------------------------------------
# Mantis loading / embedding extraction
# --------------------------------------------------

def load_mantis_model(
    device: str = "cpu",
    checkpoint: str = DEFAULT_MANTIS_CHECKPOINT,
):
    device_use = get_mantis_device(device)
    network = Mantis8M(device=device_use)
    network = network.from_pretrained(checkpoint)
    trainer = MantisTrainer(device=device_use, network=network)
    return trainer


def extract_mantis_embeddings(
    X: List[np.ndarray],
    trainer,
    resize_to: int = DEFAULT_RESIZE_TO,
) -> np.ndarray:
    X_ready = resize_for_mantis(X, size=resize_to)
    Z = trainer.transform(X_ready)
    return np.asarray(Z)


# --------------------------------------------------
# RandomForest on top of embeddings
# --------------------------------------------------

def build_rf_classifier(
    n_estimators: int = DEFAULT_RF_TREES,
    random_state: int = DEFAULT_RANDOM_STATE,
    n_jobs: int = -1,
) -> RandomForestClassifier:
    return RandomForestClassifier(
        n_estimators=n_estimators,
        random_state=random_state,
        n_jobs=n_jobs,
    )


def fit_mantis_rf(
    X_train: List[np.ndarray],
    y_train: np.ndarray,
    device: str = "cpu",
    checkpoint: str = DEFAULT_MANTIS_CHECKPOINT,
    resize_to: int = DEFAULT_RESIZE_TO,
    n_estimators: int = DEFAULT_RF_TREES,
    random_state: int = DEFAULT_RANDOM_STATE,
    n_jobs: int = -1,
) -> Dict[str, Any]:
    trainer = load_mantis_model(device=device, checkpoint=checkpoint)

    Z_train = extract_mantis_embeddings(X_train, trainer=trainer, resize_to=resize_to)

    predictor = build_rf_classifier(
        n_estimators=n_estimators,
        random_state=random_state,
        n_jobs=n_jobs,
    )
    predictor.fit(Z_train, y_train)

    return {
        "trainer": trainer,
        "predictor": predictor,
        "Z_train": Z_train,
    }


def predict_mantis_rf(
    X: List[np.ndarray],
    trainer,
    predictor,
    resize_to: int = DEFAULT_RESIZE_TO,
) -> Tuple[np.ndarray, np.ndarray]:
    Z = extract_mantis_embeddings(X, trainer=trainer, resize_to=resize_to)
    y_pred = predictor.predict(Z)
    return y_pred, Z