#' Winsorize extreme values
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to winsorize. If \code{NULL}, all numeric columns are used.
#' @param top,bottom Quantile thresholds.
#' @param group Optional grouping column.
#' @param verbose Logical.
#' @return A data frame or vector with capped values.
#' @export
winsorize <- function(data, cols = NULL, top = 0.995, bottom = 0.0025,
                      group = NULL, verbose = FALSE) {
  t0 <- Sys.time()

  if (is.vector(data) && !is.list(data)) {
    return(winsorize_cpp(data, top, bottom))
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")
  check_numeric_cols(data, idx)

  if (is.null(group)) {
    for (j in idx) {
      data[[j]] <- winsorize_cpp(data[[j]], top, bottom)
    }
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    ug <- unique(data[[group_col]])
    for (g in ug) {
      rows <- which(data[[group_col]] == g)
      for (j in idx) {
        data[rows, j] <- winsorize_cpp(data[rows, j], top, bottom)
      }
    }
  }

  if (verbose) cat("Winsorization completed.\n")
  data
}