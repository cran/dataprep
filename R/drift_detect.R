#' Rolling drift detection
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param window Window size.
#' @param threshold Threshold in standard deviation units.
#' @param method "mean" or "sd".
#' @param group Optional grouping column.
#' @param date_col Time column.
#' @param verbose Logical.
#' @return A logical matrix.
#' @export
drift_detect <- function(data, cols = NULL, window = 30, threshold = 3,
                         method = c("mean", "sd"), group = NULL,
                         date_col = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method)

  if (is.vector(data) && !is.list(data)) {
    x <- as.numeric(data)
    rolling <- roll_stats_cpp(x, window, method)
    drift <- abs(rolling - mean(rolling, na.rm = TRUE)) / sd(rolling, na.rm = TRUE)
    drift <- drift > threshold
    if (verbose) cat("Drift detection completed.\n")
    return(drift)
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  mask_mat <- matrix(FALSE, nrow = nrow(data), ncol = length(idx))
  colnames(mask_mat) <- names(data)[idx]

  if (is.null(group)) {
    for (jj in seq_along(idx)) {
      x <- data[[idx[jj]]]
      rolling <- roll_stats_cpp(x, window, method)
      drift <- abs(rolling - mean(rolling, na.rm = TRUE)) / sd(rolling, na.rm = TRUE)
      mask_mat[, jj] <- drift > threshold
    }
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    ug <- unique(data[[group_col]])
    for (g in ug) {
      rows <- which(data[[group_col]] == g)
      for (jj in seq_along(idx)) {
        x <- data[rows, idx[jj]]
        rolling <- roll_stats_cpp(x, window, method)
        drift <- abs(rolling - mean(rolling, na.rm = TRUE)) / sd(rolling, na.rm = TRUE)
        mask_mat[rows, jj] <- drift > threshold
      }
    }
  }

  if (verbose) cat("Drift detection completed.\n")
  mask_mat
}