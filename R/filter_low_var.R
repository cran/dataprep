#' Remove low-variance variables
#' @param data A data frame.
#' @param cols Columns to check. If \code{NULL}, all numeric columns are used.
#' @param cutoff Variance or SD threshold.
#' @param method "var" or "sd".
#' @param verbose Logical.
#' @return A data frame with low-variance variables removed.
#' @export
filter_low_var <- function(data, cols = NULL, cutoff = 0.01,
                           method = "var", verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method, c("var", "sd"))

  if (is.vector(data) && !is.list(data)) {
    stop("filter_low_var requires a data frame or matrix with multiple columns")
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  mat <- to_numeric_matrix(data, idx)
  p <- ncol(mat)
  if (p < 2) {
    if (verbose) cat("Only one column selected, nothing to filter.\n")
    return(data)
  }

  keep_logical <- filter_low_var_cpp(mat, cutoff, method == "sd")
  keep_cols <- idx[keep_logical]

  if (verbose) {
    cat(length(idx) - length(keep_cols),
        "columns removed due to low", method,
        "(cutoff =", cutoff, ")\n")
  }
  result <- data[, keep_cols, drop = FALSE]

  if (verbose)
    cat("Time used by filter_low_var:",
        format(Sys.time() - t0, digits = 3), "\n")
  result
}