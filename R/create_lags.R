#' Create lagged variables
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to lag. If \code{NULL}, all numeric columns are used.
#' @param lags Vector of lag orders.
#' @param prefix Prefix for new columns.
#' @param group Optional grouping column.
#' @param date_col Time column.
#' @param verbose Logical.
#' @return A data frame or matrix with lag columns.
#' @export
create_lags <- function(data, cols = NULL, lags = 1, prefix = "lag_",
                        group = NULL, date_col = NULL, verbose = FALSE) {
  t0 <- Sys.time()

  if (is.vector(data) && !is.list(data)) {
    x <- as.numeric(data)
    n <- length(x)
    mat <- matrix(NA, nrow = n, ncol = length(lags))
    colnames(mat) <- paste0(prefix, lags)
    for (k in seq_along(lags)) {
      lg <- lags[k]
      if (lg > 0) {
        if (lg >= n) next           # all-NA column
        mat[(lg + 1):n, k] <- x[1:(n - lg)]
      } else if (lg < 0) {
        if (-lg >= n) next          # all-NA column
        mat[1:(n + lg), k] <- x[(1 - lg):n]
      } else {
        mat[, k] <- x
      }
    }
    if (verbose) cat("Created", length(lags), "lag columns.\n")
    return(mat)
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  if (!is.null(group)) {
    group_col <- if (is.character(group)) group else names(data)[group]
    group_vec <- as.integer(factor(data[[group_col]]))
  } else {
    group_vec <- rep(0L, nrow(data))
  }

  mat <- to_numeric_matrix(data, idx)
  lags_int <- as.integer(lags)
  out_mat <- create_lags_cpp(mat, group_vec, lags_int)

  new_names <- character()
  for (j in seq_along(idx)) {
    for (k in seq_along(lags)) {
      new_names <- c(new_names,
                     paste0(prefix, names(data)[idx[j]], "_", lags[k]))
    }
  }
  colnames(out_mat) <- new_names

  result <- cbind(data, as.data.frame(out_mat))

  if (verbose) {
    cat("Created", length(lags) * length(idx), "lag columns.\n")
    if (!is.null(group)) cat("Grouped by:", group_col, "\n")
  }
  result
}