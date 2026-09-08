# ==============================================================================
# 01_fct_methods.R
#
# Purpose:
#   Define forecasting methods used by the M4 and sensitivity analyses.
# Inputs:
#   Time-series vectors, forecast horizons, and configured model access.
# Outputs:
#   Forecast-method registries and forecasting helper functions.
# Run from:
#   Project root; sourced by analysis scripts.
# ==============================================================================

fct_forec_methods <- function() {
  list(
    naive2_forec = naive2_forec,
    auto_arima_forec = auto_arima_forec,
    ets_forec = ets_forec
  )
}

# Additional forecast methods ------------------------------------------------

fct_forec_methods_table6_full <- function() {
  list(
    naive2_forec = naive2_forec,
    ses_forec = ses_forec,
    holt_forec = holt_forec,
    damped_forec = damped_forec,
    comb_forec = comb_forec,
    thetaf_forec = thetaf_forec,
    ets_forec = ets_forec,
    auto_arima_forec = auto_arima_forec,
    chronos_forec = chronos_forec
  )
}

# Helper to standardise forecast output --------------------------------------

as_fct_numeric <- function(x, h) {
  out <- as.numeric(x)

  if (length(out) != h) {
    stop("Forecast length mismatch. Expected ", h, ", got ", length(out))
  }

  out
}

# Seasonal naive forecast ----------------------------------------------------

snaive_forec <- function(x, h) {
  frq <- stats::frequency(x)
  out <- utils::tail(x, frq)[((seq_len(h) - 1L) %% frq) + 1L]
  as_fct_numeric(out, h)
}

# Naive forecast -------------------------------------------------------------

naive_forec <- function(x, h) {
  model <- forecast::naive(x, h = length(x))
  out <- forecast::forecast(model, h = h)$mean
  as_fct_numeric(out, h)
}

# Auto ARIMA forecast --------------------------------------------------------

auto_arima_forec <- function(x, h) {
  model <- forecast::auto.arima(
    x,
    stepwise = FALSE,
    approximation = FALSE
  )

  out <- forecast::forecast(model, h = h)$mean
  as_fct_numeric(out, h)
}

# ETS forecast ---------------------------------------------------------------

ets_forec <- function(x, h) {
  model <- forecast::ets(
    x,
    opt.crit = "mae"
  )

  out <- forecast::forecast(model, h = h)$mean
  as_fct_numeric(out, h)
}

# NNETAR forecast ------------------------------------------------------------

nnetar_forec <- function(x, h) {
  model <- forecast::nnetar(x)
  out <- forecast::forecast(model, h = h)$mean
  as_fct_numeric(out, h)
}

# TBATS forecast -------------------------------------------------------------

tbats_forec <- function(x, h) {
  model <- forecast::tbats(
    x,
    use.parallel = FALSE
  )

  out <- forecast::forecast(model, h = h)$mean
  as_fct_numeric(out, h)
}

# STLM with AR forecast ------------------------------------------------------

stlm_ar_forec <- function(x, h) {
  model <- tryCatch(
    {
      forecast::stlm(
        x,
        modelfunction = stats::ar
      )
    },
    error = function(e) {
      forecast::auto.arima(
        x,
        d = 0,
        D = 0
      )
    }
  )

  out <- forecast::forecast(model, h = h)$mean
  as_fct_numeric(out, h)
}

# Random walk with drift forecast --------------------------------------------

rw_drift_forec <- function(x, h) {
  model <- forecast::rwf(
    x,
    drift = TRUE,
    h = length(x)
  )

  out <- forecast::forecast(model, h = h)$mean
  as_fct_numeric(out, h)
}

# Theta forecast -------------------------------------------------------------

thetaf_forec <- function(x, h) {
  out <- forecast::thetaf(x, h = h)$mean
  as_fct_numeric(out, h)
}

# Chronos forecast using Python ----------------------------------------------

chronos_forec <- function(x, h) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Missing required package: reticulate")
  }

  x_num <- as.numeric(x)
  h_int <- as.integer(h)

  model_name <- if (exists("FCT_CHRONOS_MODEL")) {
    FCT_CHRONOS_MODEL
  } else {
    "amazon/chronos-2"
  }

  device_map <- if (exists("FCT_CHRONOS_DEVICE")) {
    FCT_CHRONOS_DEVICE
  } else {
    NULL
  }

  freq <- if (stats::frequency(x) == 12) {
    "MS"
  } else if (stats::frequency(x) == 4) {
    "QS"
  } else if (stats::frequency(x) == 24) {
    "h"
  } else {
    "D"
  }

  if (!reticulate::py_available(initialize = FALSE)) {
    reticulate::use_condaenv("m4_fmts_foundation", required = TRUE)
  }

  if (!exists("chronos_forecast", mode = "function")) {
    reticulate::source_python("src/python/chronos_forecast.py")
  }

  out <- chronos_forecast(
    x = x_num,
    h = h_int,
    model_name = model_name,
    device_map = device_map,
    freq = freq
  )

  as_fct_numeric(out, h_int)
}

# SES forecast ---------------------------------------------------------------

ses_forec <- function(x, h) {
  model <- forecast::ses(x, h = h)
  out <- model$mean
  as_fct_numeric(out, h)
}

# Holt forecast --------------------------------------------------------------

holt_forec <- function(x, h) {
  model <- forecast::holt(x, h = h)
  out <- model$mean
  as_fct_numeric(out, h)
}

# Damped Holt forecast -------------------------------------------------------

damped_forec <- function(x, h) {
  model <- forecast::holt(
    x,
    h = h,
    damped = TRUE
  )

  out <- model$mean
  as_fct_numeric(out, h)
}

# Combination forecast -------------------------------------------------------
# Arithmetic average of SES, Holt, and Damped

comb_forec <- function(x, h) {
  ff_ses <- ses_forec(x, h)
  ff_holt <- holt_forec(x, h)
  ff_damped <- damped_forec(x, h)

  out <- rowMeans(
    cbind(ff_ses, ff_holt, ff_damped),
    na.rm = FALSE
  )

  as_fct_numeric(out, h)
}

# M4 seasonality test used by Naive2 -----------------------------------------

SeasonalityTest <- function(input, ppy) {
  tcrit <- 1.645

  if (length(input) < 3 * ppy) {
    test_seasonal <- FALSE
  } else {
    xacf <- stats::acf(input, plot = FALSE)$acf[-1, 1, 1]

    clim <- tcrit / sqrt(length(input)) *
      sqrt(cumsum(c(1, 2 * xacf^2)))

    test_seasonal <- abs(xacf[ppy]) > clim[ppy]

    if (is.na(test_seasonal)) {
      test_seasonal <- FALSE
    }
  }

  test_seasonal
}

# Naive2 forecast from the M4 competition ------------------------------------

naive2_forec <- function(x, h) {
  input <- x
  fh <- h

  ppy <- stats::frequency(input)
  ST <- FALSE

  if (ppy > 1) {
    ST <- SeasonalityTest(input, ppy)
  }

  if (ST == TRUE) {
    Dec <- stats::decompose(input, type = "multiplicative")
    des_input <- input / Dec$seasonal

    SIout <- utils::head(
      rep(
        Dec$seasonal[(length(Dec$seasonal) - ppy + 1L):length(Dec$seasonal)],
        fh
      ),
      fh
    )
  } else {
    des_input <- input
    SIout <- rep(1, fh)
  }

  out <- forecast::naive(des_input, h = fh)$mean * SIout
  as_fct_numeric(out, h)
}
