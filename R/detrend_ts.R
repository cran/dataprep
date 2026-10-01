#' Remove linear trend from time series
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to detrend. If \code{NULL}, all numeric columns are used.
#' @param date_col Time column.
#' @param verbose Logical.
#' @return A data frame or vector with trend removed.
#' @export
detrend_ts <- function(data, cols = NULL, date_col = NULL, verbose = FALSE) {
  t0 <- Sys.time()

  detrend_one <- function(x) {
    n <- length(x)
    valid <- !is.na(x)
    nv <- sum(valid)
    if (nv < 2) return(x)
    t_v    <- which(valid)
    sum_t  <- sum(t_v)
    sum_t2 <- sum(t_v^2)
    sum_y  <- sum(x[valid])
    sum_ty <- sum(t_v * x[valid])
    denom  <- nv * sum_t2 - sum_t^2
    if (denom == 0) return(x)
    slope     <- (nv * sum_ty - sum_t * sum_y) / denom
    intercept <- (sum_y - slope * sum_t) / nv
    x[valid]  <- x[valid] - (intercept + slope * t_v)
    x
  }

  if (is.vector(data) && !is.list(data)) {
    res <- detrend_one(as.numeric(data))
    names(res) <- names(data)
    if (verbose) cat("Linear trend removed.\n")
    return(res)
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  for (j in idx) data[[j]] <- detrend_one(data[[j]])

  if (verbose) cat("Linear trend removed.\n")
  data
}