# ==============================================================================
# features.R
#
# Purpose: Extract FFORMA-compatible and directional time-series features.
# Inputs:  Series entries containing x and, where required, xx; forecast and
#          directional-metric helpers from src/r/forecast_methods3.R.
# Outputs: Series entries with feature rows and directional-feature data frames.
# Run from: Repository root; sourced by feature-construction scripts.
# ==============================================================================

# Dependencies ----------------------------------------------------------------

library(tsfeatures)
library(forecast)
library(tibble)

# Compatibility helpers -------------------------------------------------------

# FFORMA used a patched implementation of this feature.

heterogeneity_tsfeat_workaround <- function(x) {
  output <- c(
    arch_acf  = 0,
    garch_acf = 0,
    arch_r2   = 0,
    garch_r2  = 0
  )

  try(output <- tsfeatures::heterogeneity(x), silent = TRUE)
  output
}

entropy_tsfeat_workaround <- function(x) {
  output <- c(entropy = 0)

  series_sd <- stats::sd(
    as.numeric(x),
    na.rm = TRUE
  )

  if (length(x) < 2L ||
      !is.finite(series_sd) ||
      series_sd == 0) {
    return(output)
  }

  value <- try(
    tsfeatures::entropy(x),
    silent = TRUE
  )

  if (!inherits(value, "try-error") &&
      length(value) > 0L &&
      is.finite(value[1L])) {
    output[[1L]] <- as.numeric(value[1L])
  }

  output
}

hw_parameters_tsfeat_workaround <- function(x) {
  pars <- c(NA, NA, NA)

  try(
    {
      hw_fit <- forecast::ets(x, model = "AAA")
      p <- hw_fit$par

      len <- min(3L, length(p))

      if (len > 0) {
        pars[1:len] <- as.numeric(p[1:len])
      }
    },
    silent = TRUE
  )

  names(pars) <- c("hw_alpha", "hw_beta", "hw_gamma")
  pars
}

# FFORMA-compatible features --------------------------------------------------

calc_features <- function(seriesentry) {
  series <- seriesentry$x

  featrow <- tsfeatures(
    series,
    features = c(
      "acf_features",
      "arch_stat",
      "crossing_points",
      entropy_tsfeat_workaround,
      "flat_spots",
      heterogeneity_tsfeat_workaround,
      "holt_parameters",
      "hurst",
      "lumpiness",
      "nonlinearity",
      "pacf_features",
      "stl_features",
      "stability",
      hw_parameters_tsfeat_workaround,
      "unitroot_kpss",
      "unitroot_pp"
    )
  )

  series_length <- length(series)

  featrow <- tibble::add_column(
    featrow,
    series_length = series_length
  )

  # Match the FFORMA missing-value treatment.

  featrow[is.na(featrow)] <- 0

  # Preserve the seasonal feature-vector shape for non-seasonal series.

  if (length(featrow) == 37) {
    featrow <- tibble::add_column(
      featrow,
      seas_acf1 = 0,
      .before = 7
    )

    featrow <- tibble::add_column(
      featrow,
      seas_pacf = 0,
      .before = 24
    )

    featrow <- tibble::add_column(
      featrow,
      seasonal_strength = 0,
      peak = 0,
      trough = 0,
      .before = 33
    )
  }

  seriesentry$features <- featrow
  seriesentry
}

# Directional features --------------------------------------------------------

da_features <- function(x, xx, h, period) {
  x_ts <- vec_to_ts2(x, period)

  fc_arima <- auto_arima_forec(x_ts, h)
  fc_ets <- ets_forec(x_ts, h)

  data.frame(
    da_mda_arima = MDA2(xx, fc_arima),
    da_mdv_arima = MDV2(xx, fc_arima),
    da_mdpv_arima = MDPV2(xx, fc_arima),
    da_pt_pvalue_arima = PT_pvalue2(xx, fc_arima),
    da_mda_ets = MDA2(xx, fc_ets),
    da_mdv_ets = MDV2(xx, fc_ets),
    da_mdpv_ets = MDPV2(xx, fc_ets),
    da_pt_pvalue_ets = PT_pvalue2(xx, fc_ets),
    stringsAsFactors = FALSE
  )
}

# Combined features -----------------------------------------------------------

calc_features_with_da <- function(seriesentry, period) {
  x <- seriesentry$x
  xx <- seriesentry$xx
  h <- length(xx)

  featrow <- tsfeatures(
    x,
    features = c(
      "acf_features",
      "arch_stat",
      "crossing_points",
      entropy_tsfeat_workaround,
      "flat_spots",
      heterogeneity_tsfeat_workaround,
      "holt_parameters",
      "hurst",
      "lumpiness",
      "nonlinearity",
      "pacf_features",
      "stl_features",
      "stability",
      hw_parameters_tsfeat_workaround,
      "unitroot_kpss",
      "unitroot_pp"
    )
  )

  featrow$series_length <- length(x)
  featrow[is.na(featrow)] <- 0

  if (ncol(featrow) == 37) {
    featrow <- tibble::add_column(
      featrow,
      seas_acf1 = 0,
      .before = 7
    )

    featrow <- tibble::add_column(
      featrow,
      seas_pacf = 0,
      .before = 24
    )

    featrow <- tibble::add_column(
      featrow,
      seasonal_strength = 0,
      peak = 0,
      trough = 0,
      .before = 33
    )
  }

  featrow <- cbind(
    featrow,
    da_features(x, xx, h, period)
  )

  seriesentry$features <- featrow
  seriesentry
}
