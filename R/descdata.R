#' Fast descriptive statistics
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to describe.
#' @param stats Statistics to compute.
#' @param first Name for the first column.
#' @param cores Number of CPU cores.
#' @param verbose Logical.
#' @return A data frame of descriptive statistics.
#' @export
descdata <- function(data, cols = NULL, stats = 1:9,
                     first = "variables", cores = NULL, verbose = FALSE) {
  t0 <- Sys.time()

  if (is.vector(data) && !is.list(data)) {
    mat <- matrix(data, ncol = 1)
    # FIX: deparse(substitute(data)) becomes "." or the whole
    # expression inside pipelines. Use a stable name.
    colnames(mat) <- "value"
    cols <- 1
  } else {
    idx <- resolve_numeric_cols(data, cols)
    check_numeric_cols(data, idx)
    mat <- to_numeric_matrix(data, idx)
  }

  stat_names <- c("n", "na", "mean", "sd", "median",
                  "trimmed", "min", "max", "IQR")
  if (is.character(stats)) {
    stats_idx <- match(stats, stat_names)
    if (any(is.na(stats_idx))) stop("Invalid statistic name(s)")
  } else {
    stats_idx <- as.integer(stats)
  }

  n_threads <- if (is.null(cores)) 0L else as.integer(cores)

  res_mat <- desc_stats_cpp(mat, stats_idx, n_threads)
  result <- as.data.frame(res_mat)
  colnames(result) <- stat_names[stats_idx]

  if (is.vector(data) && !is.list(data)) {
    first_col <- "value"
  } else {
    col_names <- colnames(mat)
    first_col <- if (all(!grepl("\\D", gsub("[.]", "", col_names)))) {
      as.numeric(col_names)
    } else {
      col_names
    }
  }
  result <- cbind(first_col, result)
  names(result)[1] <- first
  rownames(result) <- NULL

  if (verbose) {
    cat("Time used by descdata:",
        format(Sys.time() - t0, digits = 3), "\n")
  }
  result
}