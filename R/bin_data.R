#' Discretize continuous variables into bins
#' @param data A data frame or numeric vector.
#' @param cols Columns to bin.
#' @param method Binning method: \code{"equal_width"}, \code{"equal_freq"}, \code{"custom"}.
#' @param bins Number of bins.
#' @param breaks Custom breakpoints (for \code{method = "custom"}).
#' @param include_lowest Include lowest break value.
#' @param labels Optional factor labels.
#' @param verbose Logical.
#' @return A data frame or factor.
#' @export
bin_data <- function(data, cols = NULL, method = "equal_width",
                     bins = 10, breaks = NULL, include_lowest = TRUE,
                     labels = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method, c("equal_width", "equal_freq", "custom"))

  if (is.vector(data) && !is.list(data)) {
    x <- as.numeric(data)
    if (method == "custom") {
      if (is.null(breaks)) stop("breaks must be provided for custom method")
      b <- unique(sort(breaks))
    } else {
      x_clean <- x[!is.na(x)]
      if (length(x_clean) == 0) stop("No non-NA values")
      if (method == "equal_width") {
        b <- seq(min(x_clean), max(x_clean), length.out = bins + 1)
      } else {
        probs <- seq(0, 1, length.out = bins + 1)
        b <- quantile(x_clean, probs, na.rm = TRUE, names = FALSE)
        b <- unique(b)
      }
    }
    if (length(b) < 2) stop("Not enough distinct break points to form bins")
    out <- bin_data_cpp(x, b, include_lowest)
    if (!is.null(labels)) {
      if (length(labels) != length(b) - 1)
        stop("labels length must equal number of bins")
      out <- factor(out, levels = 1:(length(b) - 1), labels = labels)
    } else {
      out <- factor(out, levels = 1:(length(b) - 1))
    }
    if (verbose) cat("Time used by bin_data:", format(Sys.time() - t0, digits = 3), "\n")
    return(out)
  }

  idx <- resolve_numeric_cols(data, cols)
  check_numeric_cols(data, idx)

  result <- data
  for (j in idx) {
    x <- data[[j]]
    if (method == "custom") {
      b <- unique(sort(breaks))
    } else {
      x_clean <- x[!is.na(x)]
      if (length(x_clean) == 0) next
      if (method == "equal_width") {
        b <- seq(min(x_clean), max(x_clean), length.out = bins + 1)
      } else {
        probs <- seq(0, 1, length.out = bins + 1)
        b <- quantile(x_clean, probs, na.rm = TRUE, names = FALSE)
        b <- unique(b)
      }
    }
    if (length(b) < 2) next
    out <- bin_data_cpp(x, b, include_lowest)
    if (!is.null(labels)) {
      if (length(labels) != length(b) - 1)
        stop("labels length must equal number of bins")
      result[[j]] <- factor(out, levels = 1:(length(b) - 1), labels = labels)
    } else {
      result[[j]] <- factor(out, levels = 1:(length(b) - 1))
    }
  }

  if (verbose) cat("Time used by bin_data:", format(Sys.time() - t0, digits = 3), "\n")
  result
}
