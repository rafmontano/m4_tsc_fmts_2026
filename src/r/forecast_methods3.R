# ==============================================================================
# forecast_methods3.R
#
# Purpose: Provide time-series conversion, directional metrics, and ARIMA and
#          ETS forecast wrappers.
# Inputs:  Numeric series, M4 period labels, and forecast horizons.
# Outputs: Directional metric values and numeric forecast vectors.
# Run from: Repository root; sourced by feature and forecasting scripts.
# ==============================================================================

# Dependencies ----------------------------------------------------------------

library(forecast)

# Time-series conversion ------------------------------------------------------

vec_to_ts2 <- function(x_vec, period) {
  stats::ts(
    as.numeric(x_vec),
    frequency = infer_frequency(period)
  )
}

# Directional metrics ---------------------------------------------------------

# Metrics operate directly on actual and forecast values.

MDA2 <- function(
  actual,
  forecast,
  na.rm = TRUE,
  reward = 1,
  penalty = 0
) {
  actual_direction <- sign(diff(actual))
  forecast_direction <- sign(diff(forecast))
  directional_hit <- actual_direction == forecast_direction

  (reward - penalty) *
    mean(directional_hit, na.rm = na.rm) +
    penalty
}

MDV2 <- function(actual, forecast, na.rm = TRUE) {
  actual_change <- diff(actual)
  actual_direction <- sign(actual_change)
  forecast_direction <- sign(diff(forecast))

  directional_value <- ifelse(
    actual_direction == forecast_direction,
    1,
    -1
  )

  mean(
    abs(actual_change) * directional_value,
    na.rm = na.rm
  )
}

MDPV2 <- function(actual, forecast, na.rm = TRUE) {
  actual_change <- diff(actual)
  actual_direction <- sign(actual_change)
  forecast_direction <- sign(diff(forecast))

  directional_value <- ifelse(
    actual_direction == forecast_direction,
    1,
    -1
  )

  mean(
    abs(actual_change / actual[-1]) * directional_value,
    na.rm = na.rm
  ) * 100
}

PT2 <- function(actual, forecast, na.rm = TRUE) {
  a <- sign(diff(actual))
  f <- sign(diff(forecast))

  i <- is.finite(a) &
    is.finite(f) &
    a != 0 &
    f != 0

  a <- as.integer(a[i] > 0)
  f <- as.integer(f[i] > 0)

  if (length(a) == 0L) {
    return(NA_real_)
  }

  p <- mean(a, na.rm = na.rm)
  q <- mean(f, na.rm = na.rm)
  p_hat <- mean(a == f, na.rm = na.rm)
  p_0 <- p * q + (1 - p) * (1 - q)

  denom <- sqrt(
    p * q * (1 - p) * (1 - q) / length(a)
  )

  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }

  (p_hat - p_0) / denom
}

PT_pvalue2 <- function(actual, forecast, na.rm = TRUE) {
  z <- PT2(
    actual = actual,
    forecast = forecast,
    na.rm = na.rm
  )

  if (!is.finite(z)) {
    return(NA_real_)
  }

  2 * (1 - stats::pnorm(abs(z)))
}

# Forecast wrappers -----------------------------------------------------------

auto_arima_forec <- function(x, h) {
  model <- forecast::auto.arima(x)

  as.numeric(
    forecast::forecast(model, h = h)$mean
  )
}

ets_forec <- function(x, h) {
  model <- forecast::ets(x, opt.crit = "mae")

  as.numeric(
    forecast::forecast(model, h = h)$mean
  )
}
