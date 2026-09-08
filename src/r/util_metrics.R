# ------------------------------------------------------------
# Get split for a given model
# Falls back to uncap if model-specific split does not exist
# ------------------------------------------------------------

get_model_split <- function(split_index, model_id = "xgboost") {
  if (model_id %in% names(split_index)) {
    return(split_index[[model_id]])
  }
  split_index$uncap
}

# ------------------------------------------------------------
# Build one standardized summary row for missing evaluations
# ------------------------------------------------------------

make_empty_eval_summary_row <- function(period_i, tag_i, eval_type, horizon_i, n_rows, status) {
  tibble::tibble(
    period            = period_i,
    tag               = tag_i,
    eval_type         = eval_type,
    horizon_id        = as.integer(horizon_i),
    n_rows            = as.integer(n_rows),
    accuracy          = NA_real_,
    balanced_accuracy = NA_real_,
    kappa             = NA_real_,
    precision_0       = NA_real_,
    recall_0          = NA_real_,
    f1_0              = NA_real_,
    precision_1       = NA_real_,
    recall_1          = NA_real_,
    f1_1              = NA_real_,
    macro_f1          = NA_real_,
    status            = status
  )
}

# ------------------------------------------------------------
# Compute binary evaluation metrics in one standard format
# ------------------------------------------------------------

compute_binary_eval <- function(y_true, pred_class, period_i, tag_i, horizon_i, eval_type) {
  y_true <- as.integer(y_true)
  pred_class <- as.integer(pred_class)
  
  cm <- caret::confusionMatrix(
    factor(pred_class, levels = c(0L, 1L)),
    factor(y_true, levels = c(0L, 1L))
  )
  
  conf_mat <- as.matrix(cm$table)
  
  precision_class <- function(cls) {
    tp <- sum(pred_class == cls & y_true == cls)
    fp <- sum(pred_class == cls & y_true != cls)
    if ((tp + fp) == 0L) return(NA_real_)
    tp / (tp + fp)
  }
  
  recall_class <- function(cls) {
    tp <- sum(pred_class == cls & y_true == cls)
    fn <- sum(pred_class != cls & y_true == cls)
    if ((tp + fn) == 0L) return(NA_real_)
    tp / (tp + fn)
  }
  
  f1_class <- function(p, r) {
    if (!is.finite(p) || !is.finite(r) || (p + r) == 0) return(NA_real_)
    2 * p * r / (p + r)
  }
  
  p0 <- precision_class(0L)
  r0 <- recall_class(0L)
  f0 <- f1_class(p0, r0)
  
  p1 <- precision_class(1L)
  r1 <- recall_class(1L)
  f1 <- f1_class(p1, r1)
  
  summary_row <- tibble::tibble(
    period            = period_i,
    tag               = tag_i,
    eval_type         = eval_type,
    horizon_id        = as.integer(horizon_i),
    n_rows            = length(y_true),
    accuracy          = as.numeric(cm$overall["Accuracy"]),
    balanced_accuracy = mean(c(r0, r1), na.rm = TRUE),
    kappa             = as.numeric(cm$overall["Kappa"]),
    precision_0       = p0,
    recall_0          = r0,
    f1_0              = f0,
    precision_1       = p1,
    recall_1          = r1,
    f1_1              = f1,
    macro_f1          = mean(c(f0, f1), na.rm = TRUE),
    status            = "ok"
  )
  
  detail_obj <- list(
    confusion_matrix = conf_mat,
    y_true           = y_true,
    pred_class       = pred_class
  )
  
  list(summary = summary_row, detail = detail_obj)
}

# ------------------------------------------------------------
# Get predictor columns from a dataset
# ------------------------------------------------------------

get_predictor_cols_base <- function(df) {
  setdiff(
    names(df),
    c("x", "xx", "labels", "series_id", "series_name", "st")
  )
}

# ------------------------------------------------------------
# Remove constant columns based on TRAIN only
# ------------------------------------------------------------

get_predictor_cols_train_safe <- function(train_df, predictor_cols_base) {
  feature_df_train <- train_df[, predictor_cols_base, drop = FALSE]
  feature_vars <- sapply(feature_df_train, function(col) var(col, na.rm = TRUE))
  constant_cols <- names(feature_vars[feature_vars == 0 | is.na(feature_vars)])
  setdiff(predictor_cols_base, constant_cols)
}

# ------------------------------------------------------------
# Build REAL last-window evaluation dataset for one frequency
# ------------------------------------------------------------

build_real_eval_dataset <- function(M4_clean_all, period_i, feature_eng = "da", window_mode = WINDOW_MODE) {
  M4_period <- Filter(function(s) as.character(s$period) == period_i, M4_clean_all)
  
  window_size_i <- get_window_size_from_h(period_i, window_mode)
  
  rows <- lapply(seq_along(M4_period), function(i) {
    s <- M4_period[[i]]
    
    x_full  <- as.numeric(unlist(s$x,  use.names = FALSE))
    xx_full <- as.numeric(unlist(s$xx, use.names = FALSE))
    
    if (length(x_full) >= window_size_i) {
      x_win <- tail(x_full, window_size_i)
    } else {
      pad_n <- window_size_i - length(x_full)
      x_win <- c(rep(x_full[1], pad_n), x_full)
    }
    
    scaled <- scale_pair_std(x_win, xx_full)
    
    feats <- tryCatch({
      
      featrow <- calc_features(
        list(x = as.numeric(scaled$x_std))
      )$features
      
      if (feature_eng == "da") {
        
        h <- length(scaled$xx_std)
        n <- length(scaled$x_std)
        
        if (n > h) {
          x_hist  <- head(as.numeric(scaled$x_std), n - h)
          xx_hist <- tail(as.numeric(scaled$x_std), h)
          
          featrow <- cbind(
            featrow,
            da_features(
              x = x_hist,
              xx = xx_hist,
              h = h,
              period = period_i
            )
          )
        } else {
          featrow <- cbind(
            featrow,
            data.frame(
              da_mda_arima = 0.5,
              da_mdv_arima = 0,
              da_mdpv_arima = 0,
              da_pt_pvalue_arima = 1,
              da_mda_ets = 0.5,
              da_mdv_ets = 0,
              da_mdpv_ets = 0,
              da_pt_pvalue_ets = 1,
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      featrow
      
    }, error = function(e) NULL)
    
    if (is.null(feats)) return(NULL)
    
    tibble::tibble(
      series_id   = i,
      series_name = if (!is.null(s$st)) as.character(s$st) else NA_character_,
      x           = list(as.numeric(scaled$x_std)),
      xx          = list(as.numeric(scaled$xx_std)),
      labels      = list(compute_label_vector(scaled$x_std, scaled$xx_std))
    ) |>
      dplyr::bind_cols(feats)
  })
  
  dplyr::bind_rows(rows)
}

# ------------------------------------------------------------
# Initialise evaluation containers for one frequency
# ------------------------------------------------------------

init_eval_containers <- function(H_i) {
  list(
    summary_test_rows = vector("list", H_i),
    summary_real_rows = vector("list", H_i),
    detail_test       = vector("list", H_i),
    detail_real       = vector("list", H_i)
  )
}

# ------------------------------------------------------------
# Store one TEST evaluation result at horizon h
# ------------------------------------------------------------

store_test_eval_result <- function(containers, h, res_test) {
  containers$summary_test_rows[[h]] <- res_test$summary
  containers$detail_test[[h]] <- res_test$detail
  containers
}

# ------------------------------------------------------------
# Store one REAL evaluation result at horizon h
# ------------------------------------------------------------

store_real_eval_result <- function(containers, h, res_real, series_name = NULL) {
  containers$summary_real_rows[[h]] <- res_real$summary
  
  if (is.null(series_name)) {
    containers$detail_real[[h]] <- res_real$detail
  } else {
    containers$detail_real[[h]] <- c(
      res_real$detail,
      list(series_name = series_name)
    )
  }
  
  containers
}

# ------------------------------------------------------------
# Build final consolidated evaluation object
# ------------------------------------------------------------

build_consolidated_eval_object <- function(containers, real_eval_df = NULL) {
  out <- list(
    test = list(
      summary = dplyr::bind_rows(containers$summary_test_rows),
      detail  = containers$detail_test
    ),
    real = list(
      summary = dplyr::bind_rows(containers$summary_real_rows),
      detail  = containers$detail_real
    )
  )
  
  if (!is.null(real_eval_df)) {
    out$real$dataset <- real_eval_df
  }
  
  out
}

# ------------------------------------------------------------
# Save consolidated evaluation object
# ------------------------------------------------------------

save_consolidated_eval_object <- function(eval_obj, eval_rds_path, window_mode = "default") {
  if (file.exists(eval_rds_path)) {
    obj <- readRDS(eval_rds_path)
  } else {
    obj <- list()
  }
  
  obj[[window_mode]] <- eval_obj
  
  saveRDS(obj, eval_rds_path)
  invisible(obj)
}