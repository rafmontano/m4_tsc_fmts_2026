# src/r/utils.R
# Common utility functions for the M4-XGB pipeline

# ------------------------------------------------------------
# Frequency helpers
# ------------------------------------------------------------

infer_frequency <- function(period) {
  p <- as.character(period)
  
  freq <- switch(
    p,
    "Yearly"    = 1,
    "Quarterly" = 4,
    "Monthly"   = 12,
    "Weekly"    = 52,
    "Daily"     = 7,
    "Hourly"    = 24,
    NA_real_
  )
  
  if (!is.finite(freq)) {
    stop("Unknown period: ", p)
  }
  
  as.integer(freq)
}

get_m4_horizon <- function(period) {
  p <- as.character(period)
  
  switch(
    p,
    "Yearly"    = 6L,
    "Quarterly" = 8L,
    "Monthly"   = 18L,
    "Weekly"    = 13L,
    "Daily"     = 14L,
    "Hourly"    = 48L,
    stop("Unknown M4 period: ", p)
  )
}

get_window_size_from_h <- function(period, window_mode = WINDOW_MODE) {
  
  grid <- list(
    Yearly    = c(small = 8L,   default = 16L,  large = 32L),
    Quarterly = c(small = 16L,  default = 32L,  large = 64L),
    Monthly   = c(small = 32L,  default = 64L,  large = 128L),
    Weekly    = c(small = 32L,  default = 64L,  large = 128L),
    Daily     = c(small = 32L,  default = 64L,  large = 128L),
    Hourly    = c(small = 128L, default = 256L, large = 512L)
  )
  
  p <- as.character(period)
  
  if (!p %in% names(grid)) {
    stop("No rule defined for period: ", p)
  }
  
  if (window_mode == "full") {
    return(Inf)
  }
  
  if (!window_mode %in% c("small", "default", "large")) {
    stop("Invalid window_mode: ", window_mode)
  }
  
  as.integer(grid[[p]][[window_mode]])
}

freq_tag <- function(period) {
  p <- as.character(period)
  
  switch(
    p,
    "Yearly"    = "y",
    "Quarterly" = "q",
    "Monthly"   = "m",
    "Weekly"    = "w",
    "Daily"     = "d",
    "Hourly"    = "h",
    stop("Unknown period: ", p, call. = FALSE)
  )
}

window_tag <- function(window_mode) {
  switch(
    window_mode,
    "small"   = "s",
    "default" = "d",
    "large"   = "l",
    "full"   = "f",
    stop("Invalid window_mode: ", window_mode, call. = FALSE)
  )
}

period_to_freq <- function(period) {
  p <- tolower(period)
  
  if (p == "yearly")    return(1)
  if (p == "quarterly") return(4)
  if (p == "monthly")   return(12)
  if (p == "weekly")    return(52)
  if (p == "daily")     return(1)
  if (p == "hourly")    return(24)
  
  return(1)
}

# ------------------------------------------------------------
# Scaling helpers
# ------------------------------------------------------------

minmax_vec <- function(v) {
  v <- as.numeric(v)
  r <- range(v, na.rm = TRUE)
  
  if (r[1] == r[2]) {
    return(rep(0, length(v)))
  }
  
  (v - r[1]) / (r[2] - r[1])
}

standardise_vec <- function(v) {
  v <- as.numeric(v)
  m <- mean(v, na.rm = TRUE)
  s <- sd(v, na.rm = TRUE)
  
  if (s == 0 || is.na(s)) {
    return(rep(0, length(v)))
  }
  
  (v - m) / s
}

scale_pair_minmax_std <- function(x, xx = NULL) {
  x  <- as.numeric(x)
  xx <- if (!is.null(xx)) as.numeric(xx) else NULL
  
  min_x   <- min(x, na.rm = TRUE)
  max_x   <- max(x, na.rm = TRUE)
  range_x <- max_x - min_x
  
  if (!is.finite(range_x) || range_x == 0) {
    x_mm  <- rep(0, length(x))
    xx_mm <- if (!is.null(xx)) rep(0, length(xx)) else NULL
  } else {
    x_mm  <- (x - min_x) / range_x
    xx_mm <- if (!is.null(xx)) (xx - min_x) / range_x else NULL
  }
  
  mean_x <- mean(x_mm, na.rm = TRUE)
  sd_x   <- sd(x_mm, na.rm = TRUE)
  
  if (!is.finite(sd_x) || sd_x == 0) {
    x_std  <- rep(0, length(x_mm))
    xx_std <- if (!is.null(xx_mm)) rep(0, length(xx_mm)) else NULL
  } else {
    x_std  <- (x_mm - mean_x) / sd_x
    xx_std <- if (!is.null(xx_mm)) (xx_mm - mean_x) / sd_x else NULL
  }
  
  list(x_std = x_std, xx_std = xx_std)
}

scale_pair_std <- function(x, xx = NULL) {
  x  <- as.numeric(x)
  xx <- if (!is.null(xx)) as.numeric(xx) else NULL
  
  mean_x <- mean(x, na.rm = TRUE)
  sd_x   <- sd(x, na.rm = TRUE)
  
  if (!is.finite(sd_x) || sd_x == 0) {
    x_std  <- rep(0, length(x))
    xx_std <- if (!is.null(xx)) rep(0, length(xx)) else NULL
  } else {
    x_std  <- (x - mean_x) / sd_x
    xx_std <- if (!is.null(xx)) (xx - mean_x) / sd_x else NULL
  }
  
  list(x_std = x_std, xx_std = xx_std)
}

# ------------------------------------------------------------
# Threshold helper
# ------------------------------------------------------------

compute_c <- function(z, q = 0.40) {
  z <- z[is.finite(z)]
  abs_z <- abs(z)
  unname(quantile(abs_z, probs = 1 - q))
}

# ------------------------------------------------------------
# Label helpers
# ------------------------------------------------------------

label_from_z_int <- function(z, c, label_id) {
  
  label_id <- as.integer(label_id)
  
  if (label_id == 5L) {
    return(as.integer(z))
  }
  
  if (label_id == 6L) {
    return(as.integer(z > 0))
  }
  
  ifelse(z >= c, 2L,
         ifelse(z <= -c, 0L, 1L))
}

compute_z <- function(x, xx) {
  w <- as.numeric(x)
  h <- as.numeric(xx)
  
  H <- length(h)
  
  w_last <- w[length(w)]
  w_tail <- tail(w, H)
  
  S_mean   <- mean(h)
  S_median <- median(h)
  
  B_last      <- w_last
  B_mean_tail <- mean(w_tail)
  
  z1 <- S_mean   - B_last
  z2 <- S_median - B_last
  z3 <- S_mean   - B_mean_tail
  z4 <- S_median - B_mean_tail
  z5 <- ifelse(S_mean > B_last, 1L, 0L)
  z6 <- h[H] - w_last
  
  c(z1 = z1, z2 = z2, z3 = z3, z4 = z4, z5 = z5, z6 = z6)
}

compute_z_generic <- function(x, xx, label_id) {
  label_id <- as.integer(label_id)
  
  if (!label_id %in% 1:6) {
    stop("label_id must be in 1..6.")
  }
  
  z <- compute_z(x, xx)
  unname(z[label_id])
}

compute_z_all <- function(all_windows_std) {
  t(
    mapply(
      compute_z,
      x  = all_windows_std$x,
      xx = all_windows_std$xx
    )
  )
}

compute_label_vector <- function(x, xx) {
  x  <- as.numeric(x)
  xx <- as.numeric(xx)
  
  x_last <- x[length(x)]
  as.integer(xx > x_last)
}