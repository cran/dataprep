#' Logarithmic returns
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param group Optional grouping column.
#' @param verbose Logical.
#' @return A data frame or vector of log returns.
#' @export
log_returns <- function(data, cols = NULL, group = NULL, verbose = FALSE) {
  t0 <- Sys.time()

  if (is.vector(data) && !is.list(data)) {
    x <- as.numeric(data)
    res <- log_returns_cpp(x)
    names(res) <- names(data)
    if (verbose) cat("Log returns computed.\n")
    return(res)
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  if (is.null(group)) {
    for (j in idx) data[[j]] <- log_returns_cpp(data[[j]])
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    ug <- unique(data[[group_col]])
    for (g in ug) {
      rows <- which(data[[group_col]] == g)
      for (j in idx) data[rows, j] <- log_returns_cpp(data[rows, j])
    }
  }

  if (verbose) cat("Log returns computed.\n")
  data
}