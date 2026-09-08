# =====================================================================
# parallel_util.R
# Shared parallel, chunking, and recovery utilities
# =====================================================================

# ---------------------------------------------------------------------
# Detect the number of workers from the active future plan
# ---------------------------------------------------------------------

autodetect_num_workers <- function() {
#  strat <- future::plan()
#  num_workers <- as.list(args(strat))$workers
#  
#  if (is.null(num_workers)) {
##    return(1L)
#  }
  
#  num_workers <- eval(num_workers) 
  
#  if (is.null(num_workers) || !is.finite(num_workers) || num_workers < 1) {
#    return(1L)
#  }
  
#  as.integer(min(num_workers, 32L))

  n_workers <- max(1L, parallel::detectCores()- 1)
  n_workers <- min(n_workers, 16L)
  as.integer(n_workers)
  }

# ---------------------------------------------------------------------
# Calculate chunk size based on dataset size and number of workers
# ---------------------------------------------------------------------

calculate_chunk_size <- function(dataset, num_workers) {
  N <- length(dataset)
  
  if (N <= 1L) {
    return(1L)
  }
  
  data_size <- as.numeric(utils::object.size(dataset)) / (1024 * 1024)
  
  MAX_MEMORY_PERCHUNK <- 45
  
  if (!is.finite(data_size) || data_size <= 0) {
    return(max(1L, min(N, num_workers)))
  }
  
  max_chunk_size <- MAX_MEMORY_PERCHUNK / (data_size / N)
  chunk_size <- num_workers * floor(max_chunk_size / num_workers)
  
  if (!is.finite(chunk_size) || chunk_size < 1L) {
    chunk_size <- max(1L, min(N, num_workers))
  }
  
  if (chunk_size > N / 2) {
    chunk_size <- floor(N / 2)
  }
  
  chunk_size <- max(1L, chunk_size)
  
  message("Auto calculated chunk size: ", chunk_size)
  chunk_size
}

# ---------------------------------------------------------------------
# Apply a function to a dataset in chunks with optional caching/resume
# ---------------------------------------------------------------------

chunk_xapply <- function(.chunk_dataset, chunk_size, save_foldername, .idcall, .apply_FUN, ...) {
  if (!is.null(save_foldername)) {
    message("using cache in: ", save_foldername, " for saving/resuming computations")
  }
  
  temp_dataset <- NULL
  data_length <- length(.chunk_dataset)
  
  chunk_index <- seq(1, data_length, chunk_size)
  chunk_index <- c(chunk_index, data_length + 1L)
  
  start_time <- proc.time()
  
  for (i in seq_len(length(chunk_index) - 1L)) {
    start_ind <- chunk_index[i]
    end_ind <- chunk_index[i + 1L] - 1L
    
    if (!is.null(save_foldername)) {
      whether_memoize <- memoise::memoise
      cache_obj <- cachem::cache_disk(save_foldername)
    } else {
      whether_memoize <- function(x, cache) x
      cache_obj <- NULL
    }
    
    memofun <- whether_memoize(
      function(call_id, chunk_id, my_apply_fun, ...) {
        my_apply_fun(.chunk_dataset[start_ind:end_ind], ...)
      },
      cache = cache_obj
    )
    
    chunk_result <- memofun(.idcall, start_ind, .apply_FUN, ...)
    temp_dataset <- c(temp_dataset, chunk_result)
    
    rm(chunk_result)
    gc()
    
    endchunk_time <- proc.time()
    
    message(
      paste(
        "From", start_ind, "to", end_ind,
        ",", round(100 * end_ind / data_length, 2),
        "% of the dataset processed, remaining time:",
        round((endchunk_time - start_time)[3] * (data_length / end_ind - 1), 2),
        "seconds"
      )
    )
  }
  
  temp_dataset
}

# ---------------------------------------------------------------------
# Run a generic processing step in parallel over a dataset
# ---------------------------------------------------------------------

run_step_parallel <- function(dataset,
                              step_fun,
                              ...,
                              chunk_size = NULL,
                              save_foldername = NULL,
                              step_name = "step_call") {
  num_workers <- autodetect_num_workers()
  
  if (is.null(chunk_size)) {
    chunk_size <- calculate_chunk_size(dataset, num_workers)
  }
  
  chunk_xapply(
    .chunk_dataset = dataset,
    chunk_size = chunk_size,
    save_foldername = save_foldername,
    .idcall = step_name,
    .apply_FUN = future.apply::future_lapply,
    FUN = step_fun,
    ...,
    future.seed = TRUE
  )
}

# ---------------------------------------------------------------------
# Set future plan
# ---------------------------------------------------------------------

set_parallel_plan <- function(run_parallel) {
  if (run_parallel) {
    n_workers <- max(1L, parallel::detectCores()- 1)
    n_workers <- min(n_workers, 16L)
  #  n_workers <- autodetect_num_workers()
    future::plan(future::multisession, workers = n_workers)
  } else {
    future::plan(future::sequential)
  }
}



