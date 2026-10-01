#' Physical range filtering
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to filter.
#' @param min_val Lower bound(s).
#' @param max_val Upper bound(s).
#' @param group Not used.
#' @param date_col Not used.
#' @param verbose Logical.
#' @return A data frame or vector with out-of-range values set to NA.
#' @export
phys_filter <- function(data, cols = NULL, min_val = NULL, max_val = NULL,
                        group = NULL, date_col = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  if (is.null(min_val) && is.null(max_val)) {
    stop("At least one of min_val or max_val must be provided")
  }

  if (is.vector(data) && !is.list(data)) {
    x <- as.numeric(data)
    if (!is.null(min_val)) x[x < min_val] <- NA_real_
    if (!is.null(max_val)) x[x > max_val] <- NA_real_
    if (verbose) cat("Physical filter applied.\n")
    return(x)
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  check_numeric_cols(data, idx)

  if (length(min_val) == 1) min_val <- rep(min_val, length(idx))
  if (length(max_val) == 1) max_val <- rep(max_val, length(idx))

  for (k in seq_along(idx)) {
    j <- idx[k]
    x <- data[[j]]
    if (!is.null(min_val)) {
      x[x < min_val[k]] <- NA_real_
    }
    if (!is.null(max_val)) {
      x[x > max_val[k]] <- NA_real_
    }
    data[[j]] <- x
  }

  if (verbose) cat("Physical filter applied.\n")
  data
}
