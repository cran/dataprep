#' Impute missing values
#' @param data A data frame or matrix.
#' @param cols Columns to impute. If \code{NULL}, all numeric columns are used.
#' @param method "linear", "locf", "nocb", "mean", or "median".
#' @param group Optional grouping column.
#' @param date_col Time column.
#' @param max_gap Not implemented; ignored with a warning.
#' @param verbose Logical.
#' @return A data frame with imputed values.
#' @examples
#' impute_missing(data[1:100, c(1, 4, 17:19)], cols = 3:5, method = "locf")
#'
#' @references
#' 1. Example data is from \url{https://smear.avaa.csc.fi/download}.
#'    It includes particle number concentrations in SMEAR I Varrio forest.
#' @export
impute_missing <- function(data, cols = NULL, method = "linear",
                           group = NULL, date_col = NULL, max_gap = NULL,
                           verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method, c("linear", "locf", "nocb", "mean", "median"))

  if (!is.null(max_gap)) {
    warning("max_gap is not implemented in dataprep 0.1.8 and will be ignored.")
  }

  if (is.vector(data) && !is.list(data)) {
    return(impute_cpp(data, method))
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)
  mat <- to_numeric_matrix(data, idx)

  if (is.null(group)) {
    imputed_mat <- impute_matrix_cpp(mat, method)
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    ug <- unique(data[[group_col]])
    imputed_mat <- mat
    for (g in ug) {
      rows <- which(data[[group_col]] == g)
      if (length(rows) > 0) {
        imputed_mat[rows, ] <- impute_matrix_cpp(mat[rows, , drop = FALSE], method)
      }
    }
  }
  data[, idx] <- imputed_mat
  if (verbose) cat("Missing value imputation completed.\n")
  data
}