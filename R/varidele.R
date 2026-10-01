#' Delete variables with excessive missing values
#' @param data A data frame or matrix.
#' @param cols Columns to consider. If \code{NULL}, all numeric columns are used.
#' @param fraction Missing fraction threshold.
#' @param verbose Logical.
#' @return A data frame with low-quality variables removed.
#' @export
varidele <- function(data, cols = NULL, fraction = .25, verbose = FALSE) {
  t0 <- Sys.time()

  if (is.vector(data) && !is.list(data)) {
    if (mean(is.na(data)) >= fraction) {
      if (verbose) cat("Missing fraction too high, vector deleted\n")
      return(NULL)
    } else {
      if (verbose) cat("Vector retained\n")
      return(data)
    }
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")

  mat <- to_numeric_matrix(data, idx)

  keep_rows <- rowSums(is.na(mat)) != ncol(mat)
  if (!any(keep_rows)) {
    if (verbose) cat("All rows have NA in all selected columns\n")
    return(NULL)
  }

  mat_sub <- mat[keep_rows, , drop = FALSE]
  frac <- colMeans(is.na(mat_sub))

  high_na <- which(frac >= fraction)
  high_na_cols <- idx[high_na]

  if (length(high_na_cols) == 0) {
    if (verbose) cat("No variables deleted\n")
    result <- data
  } else {
    keep_cols <- setdiff(seq_len(ncol(data)), high_na_cols)
    result <- data[, keep_cols, drop = FALSE]
    deleted <- colnames(data)[high_na_cols]
    if (verbose) {
      cat(length(deleted), "variables are deleted:",
          paste(deleted, collapse = ", "), "\n")
    }
  }

  if (verbose)
    cat("Time used by varidele:",
        format(Sys.time() - t0, digits = 3), "\n")
  result
}