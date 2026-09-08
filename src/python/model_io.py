# ==============================================================================
# model_io.py
#
# Purpose:
#   Save and load fitted Python models using the project naming convention.
# Inputs:
#   Models, model directory, frequency tag, and forecast horizon.
# Outputs:
#   Serialized joblib model files or loaded model objects.
# Used by:
#   The conventional-classifier and Mantis experiment runners.
# ==============================================================================

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Optional, Union

import joblib


PathLike = Union[str, Path]


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _safe_import_version(pkg_name: str) -> Optional[str]:
    try:
        import importlib.metadata as importlib_metadata

        return importlib_metadata.version(pkg_name)
    except Exception:
        return None


def save_models(
    models: Dict[str, Any],
    models_root: PathLike,
    *,
    freq_tag: str,
    horizon_id: int,
) -> None:
    models_root = Path(models_root)
    models_root.mkdir(
        parents=True,
        exist_ok=True,
    )

    stem = f"model_h{int(horizon_id):02d}"

    for model_id, model in models.items():
        model_key = str(model_id).strip().lower()
        model_path = models_root / f"{stem}.joblib"

        joblib.dump(model, model_path)

        print(f"[model_io] Saved {model_key} → {model_path}")


def load_models(
    models_root: PathLike,
    *,
    horizon_id: int,
) -> Dict[str, Any]:
    models_root = Path(models_root)
    loaded: Dict[str, Any] = {}

    stem = f"model_h{int(horizon_id):02d}"
    path = models_root / f"{stem}.joblib"

    if not path.exists():
        raise FileNotFoundError(
            f"[model_io] Expected model file not found: {path}"
        )

    loaded[stem] = joblib.load(path)

    print(f"[model_io] Loaded {stem} ← {path}")

    return loaded