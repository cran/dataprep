#' Detect outliers using IQR, MAD, or percentile
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param method "iqr", "mad", or "percentile".
#' @param top,bottom Percentile thresholds.
#' @param coef Coefficient for IQR or MAD.
#' @param group Optional grouping column.
#' @param mask_only Return logical mask if TRUE.
#' @param verbose Logical.
#' @return A logical matrix or data frame with outliers set to NA.
#' @export
detect_outliers <- function(data, cols = NULL, method = "iqr",
                            top = 0.995, bottom = 0.0025, coef = 1.5,
                            group = NULL, mask_only = TRUE, verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method, c("iqr", "mad", "percentile"))

  if (is.vector(data) && !is.list(data)) {
    mask <- detect_outliers_cpp(data, method, top, bottom, coef)
    if (mask_only) return(mask)
    data[mask] <- NA_real_
    return(data)
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
      mask_mat[, jj] <- detect_outliers_cpp(data[[idx[jj]]],
                                            method, top, bottom, coef)
    }
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    ug <- unique(data[[group_col]])
    for (g in ug) {
      rows <- which(data[[group_col]] == g)
      for (jj in seq_along(idx)) {
        mask_mat[rows, jj] <- detect_outliers_cpp(data[rows, idx[jj]],
                                                  method, top, bottom, coef)
      }
    }
  }

  if (mask_only) return(mask_mat)

  for (jj in seq_along(idx)) {
    data[[idx[jj]]][mask_mat[, jj]] <- NA_real_
  }

  if (verbose) cat("Outlier detection completed.\n")
  data
}