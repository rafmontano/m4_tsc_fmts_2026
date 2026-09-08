# =====================================================================
# File: src/r/forecast_methods3.R
# Purpose:
#   Fit ARIMA and ETS using the forecast package and compute:
#     - MDA
#     - MDV
#     - MDPV
#     - PT_pvalue
#
# Input:
#   train_vec : numeric vector used for training
#   test_vec  : numeric vector of future actual values
#   period    : M4 period label
#   h         : forecast horizon; defaults to length(test_vec)
#
# Output:
#   one-row data.frame with columns:
#     - da_mda_arima
#     - da_mdv_arima
#     - da_mdpv_arima
#     - da_pt_pvalue_arima
#     - da_mda_ets
#     - da_mdv_ets
#     - da_mdpv_ets
#     - da_pt_pvalue_ets
# =====================================================================

library(forecast)

# ---------------------------------------------------------------------
# Convert numeric vector to ts
# ---------------------------------------------------------------------

vec_to_ts2 <- function(x_vec, period) {
  stats::ts(as.numeric(x_vec), frequency = infer_frequency(period))
}

# ---------------------------------------------------------------------
# Directional metrics
# Faithful to the fabletools definitions, but using actual and forecast
# directly instead of reconstructing forecast through residuals
# ---------------------------------------------------------------------

MDA2 <- function(actual, forecast, na.rm = TRUE, reward = 1, penalty = 0) {
  actual_direction <- sign(diff(actual))
  forecast_direction <- sign(diff(forecast))
  directional_hit <- actual_direction == forecast_direction
  
  (reward - penalty) * mean(directional_hit, na.rm = na.rm) + penalty
}

MDV2 <- function(actual, forecast, na.rm = TRUE) {
  actual_change <- diff(actual)
  actual_direction <- sign(actual_change)
  forecast_direction <- sign(diff(forecast))
  directional_value <- ifelse(actual_direction == forecast_direction, 1, -1)
  
  mean(abs(actual_change) * directional_value, na.rm = na.rm)
}

MDPV2 <- function(actual, forecast, na.rm = TRUE) {
  actual_change <- diff(actual)
  actual_direction <- sign(actual_change)
  forecast_direction <- sign(diff(forecast))
  directional_value <- ifelse(actual_direction == forecast_direction, 1, -1)
  
  mean(abs(actual_change / actual[-1]) * directional_value, na.rm = na.rm) * 100
}

PT2 <- function(actual, forecast, na.rm = TRUE) {
  a <- sign(diff(actual))
  f <- sign(diff(forecast))
  
  i <- is.finite(a) & is.finite(f) & a != 0 & f != 0
  a <- as.integer(a[i] > 0)
  f <- as.integer(f[i] > 0)
  
  if (length(a) == 0L) {
    return(NA_real_)
  }
  
  p <- mean(a, na.rm = na.rm)
  q <- mean(f, na.rm = na.rm)
  p_hat <- mean(a == f, na.rm = na.rm)
  p_0 <- p * q + (1 - p) * (1 - q)
  
  denom <- sqrt(p * q * (1 - p) * (1 - q) / length(a))
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  
  (p_hat - p_0) / denom
}

PT_pvalue2 <- function(actual, forecast, na.rm = TRUE) {
  z <- PT2(actual = actual, forecast = forecast, na.rm = na.rm)
  
  if (!is.finite(z)) {
    return(NA_real_)
  }
  
  2 * (1 - stats::pnorm(abs(z)))
}

# ---------------------------------------------------------------------
# Forecast wrappers using the original FFORMA style
# ---------------------------------------------------------------------

auto_arima_forec <- function(x, h) {
 # model <- forecast::auto.arima(x, stepwise = FALSE, approximation = FALSE)
  model <- forecast::auto.arima(x) #, stepwise = FALSE, approximation = FALSE)
  as.numeric(forecast::forecast(model, h = h)$mean)
}

ets_forec <- function(x, h) {
  model <- forecast::ets(x, opt.crit = "mae")
  as.numeric(forecast::forecast(model, h = h)$mean)
}

