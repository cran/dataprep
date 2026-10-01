#' Diagnose missing-value patterns
#' @param data A data frame or matrix.
#' @param cols Columns to diagnose. If \code{NULL}, all numeric columns are used.
#' @param date_col Reserved for future use; currently ignored.
#' @param verbose Logical.
#' @return A data frame of NA statistics.
#' @export
na_diagnose <- function(data, cols = NULL, date_col = NULL, verbose = FALSE) {
  if (is.vector(data) && !is.list(data)) {
    data <- data.frame(value = data, stringsAsFactors = FALSE)
    cols <- 1
  } else if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }
  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)
  mat <- to_numeric_matrix(data, idx)

  result_list <- lapply(seq_along(idx), function(j) {
    x <- mat[, j]
    runs <- na_runs_cpp(x)
    data.frame(
      variable = colnames(mat)[j],
      n = nrow(mat),
      na = runs$n_na,
      na_frac = round(runs$n_na / nrow(mat), 4),
      na_runs = runs$n_runs,
      max_run = runs$max_run,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, result_list)
  rownames(result) <- NULL

  if (verbose) cat("Missing value diagnosis completed.\n")
  result
}