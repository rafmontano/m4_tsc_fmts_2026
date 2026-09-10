# ==============================================================================
# parallel_util.R
#
# Purpose: Provide shared parallel execution, chunking, caching, and recovery
#          utilities.
# Inputs:  Datasets, processing functions, cache locations, and run settings.
# Outputs: Reusable helper functions and configured future execution plans.
# Run from: Repository root; sourced by pipeline scripts.
# ==============================================================================

# Worker detection ------------------------------------------------------------


autodetect_num_workers <- function() {
  available <- parallel::detectCores()

  if (length(available) != 1L ||
      !is.numeric(available) ||
      !is.finite(available) ||
      available < 1L) {
    available <- 1L
  }

  max_workers <- get0(
    "MAX_WORKERS",
    ifnotfound = 16L,
    inherits = TRUE
  )

  if (length(max_workers) != 1L ||
      !is.numeric(max_workers) ||
      !is.finite(max_workers) ||
      max_workers < 1L) {
    max_workers <- 16L
  }

  as.integer(max(1L, min(available - 1L, max_workers)))
}


# Chunk sizing ----------------------------------------------------------------

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

# Chunked execution -----------------------------------------------------------

chunk_xapply <- function(
  .chunk_dataset,
  chunk_size,
  save_foldername,
  .idcall,
  .apply_FUN,
  ...
) {
  if (!is.null(save_foldername)) {
    message(
      "using cache in: ",
      save_foldername,
      " for saving/resuming computations"
    )
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
      function(call_id, chunk_start, chunk_end,
               chunk_data, my_apply_fun, ...) {
        my_apply_fun(chunk_data, ...)
      },
      cache = cache_obj
    )
    
    chunk_result <- memofun(
      call_id = .idcall,
      chunk_start = start_ind,
      chunk_end = end_ind,
      chunk_data = .chunk_dataset[start_ind:end_ind],
      my_apply_fun = .apply_FUN,
      ...
    )
    temp_dataset <- c(temp_dataset, chunk_result)

    rm(chunk_result)
    gc()

    endchunk_time <- proc.time()

    message(
      paste(
        "From", start_ind, "to", end_ind,
        ",", round(100 * end_ind / data_length, 2),
        "% of the dataset processed, remaining time:",
        round(
          (endchunk_time - start_time)[3] *
            (data_length / end_ind - 1),
          2
        ),
        "seconds"
      )
    )
  }

  temp_dataset
}

# Parallel processing ---------------------------------------------------------

run_step_parallel <- function(
  dataset,
  step_fun,
  ...,
  chunk_size = NULL,
  save_foldername = NULL,
  step_name = "step_call"
) {
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

# Future plan -----------------------------------------------------------------

set_parallel_plan <- function(run_parallel) {
  if (run_parallel) {
    future::plan(
      future::multisession,
      workers = autodetect_num_workers()
    )
    message("Parallel workers: ", future::nbrOfWorkers())
  } else {
    future::plan(future::sequential)
  }
}
