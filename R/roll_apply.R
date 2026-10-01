#' Rolling window statistics
#' @param data A data frame, matrix, or numeric vector.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param window Window size.
#' @param method "mean", "sd", "var", "median", "sum", "min", "max".
#' @param align "right", "left", or "center".
#' @param group Optional grouping column.
#' @param date_col Time column.
#' @param verbose Logical.
#' @return A data frame or vector.
#' @export
roll_apply <- function(data, cols = NULL, window = 3, method = "mean",
                       align = "right", group = NULL, date_col = NULL,
                       verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method,
                      c("mean", "sd", "var", "median", "sum", "min", "max"))
  align <- match.arg(align, c("right", "left", "center"))

  apply_align <- function(x) {
    if (align == "left") {
      if (window <= 1) return(x)
      c(x[-(1:(window - 1))], rep(NA, window - 1))
    } else if (align == "center") {
      half <- floor(window / 2)
      if (half <= 0) return(x)
      c(x[(half + 1):length(x)], rep(NA, half))
    } else {
      x
    }
  }

  if (is.vector(data) && !is.list(data)) {
    x <- as.numeric(data)
    res <- roll_stats_cpp(x, window, method)
    res <- apply_align(res)
    names(res) <- names(data)
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
    for (j in idx) {
      data[[j]] <- apply_align(roll_stats_cpp(data[[j]], window, method))
    }
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    ug <- unique(data[[group_col]])
    for (g in ug) {
      rows <- which(data[[group_col]] == g)
      for (j in idx) {
        data[rows, j] <- apply_align(
          roll_stats_cpp(data[rows, j], window, method))
      }
    }
  }

  if (verbose) cat("Rolling statistics applied.\n")
  data
}