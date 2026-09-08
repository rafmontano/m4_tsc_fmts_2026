# src/python/chronos_forecast.py

import numpy as np
import pandas as pd
import torch
from chronos import Chronos2Pipeline


import builtins

if not hasattr(builtins, "_CHRONOS2_PIPELINE_CACHE"):
    builtins._CHRONOS2_PIPELINE_CACHE = {}

_PIPELINE_CACHE = builtins._CHRONOS2_PIPELINE_CACHE


def _mps_available():
    return (
        hasattr(torch.backends, "mps")
        and torch.backends.mps.is_available()
        and torch.backends.mps.is_built()
    )


def _get_device(device_map=None):
    if device_map is not None:
        return device_map

    if torch.cuda.is_available():
        return "cuda"

    if _mps_available():
        return "mps"

    return "cpu"


def _load_pipeline(model_name, device_map):
    cache_key = (model_name, device_map)

    if cache_key in _PIPELINE_CACHE:
        return _PIPELINE_CACHE[cache_key]

    pipeline = Chronos2Pipeline.from_pretrained(
        model_name,
        device_map=device_map,
    )

    _PIPELINE_CACHE[cache_key] = pipeline

    return pipeline


def _extract_point_forecast(forecast_df):
    if "mean" in forecast_df.columns:
        return forecast_df["mean"].to_numpy(dtype=float)

    if "0.5" in forecast_df.columns:
        return forecast_df["0.5"].to_numpy(dtype=float)

    if 0.5 in forecast_df.columns:
        return forecast_df[0.5].to_numpy(dtype=float)

    raise ValueError(
        "Could not find point forecast column. Available columns: "
        + ", ".join(map(str, forecast_df.columns))
    )


def chronos_forecast(
    x,
    h,
    model_name="amazon/chronos-2",
    device_map=None,
    freq="D",
):
    x = np.array(x, dtype=float, copy=True).reshape(-1)
    h = int(h)

    if x.size == 0:
        raise ValueError("Input time series is empty.")

    if h <= 0:
        raise ValueError("Forecast horizon h must be positive.")

    if not np.all(np.isfinite(x)):
        raise ValueError("Input time series contains non-finite values.")

    device = _get_device(device_map)

    pipeline = _load_pipeline(
        model_name=model_name,
        device_map=device,
    )

    context_df = pd.DataFrame({
        "id": "series_1",
        "date": pd.date_range(
            start="2000-01-01",
            periods=x.size,
            freq=freq,
        ),
        "target": x,
    })

    forecast_df = pipeline.predict_df(
        context_df,
        prediction_length=h,
        quantile_levels=[0.5],
        id_column="id",
        timestamp_column="date",
    )

    forecast = _extract_point_forecast(forecast_df).reshape(-1)

    if forecast.size != h:
        raise ValueError(
            f"Forecast length mismatch. Expected {h}, got {forecast.size}."
        )

    if not np.all(np.isfinite(forecast)):
        raise ValueError("Chronos forecast contains non-finite values.")

    return forecast.tolist()