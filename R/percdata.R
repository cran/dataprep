#' Percentile summaries
#' @param data A data frame.
#' @param cols Columns to summarize.
#' @param group Optional grouping column.
#' @param diff Difference between quantile probabilities.
#' @param part \code{"both"}, \code{"bottom"}, or \code{"top"}.
#' @param na.rm Remove NA values.
#' @param verbose Logical.
#' @return A data frame with percentile values.
#' @export
percdata <- function(data, cols = NULL, group = NULL, diff = 0.1,
                     part = "both", na.rm = TRUE, verbose = FALSE) {
  t0 <- Sys.time()

  idx <- resolve_numeric_cols(data, cols)

  group_idx <- NULL
  if (!is.null(group)) {
    if (is.character(group)) {
      group_idx <- match(group, names(data))
    } else {
      group_idx <- as.integer(group)
    }
    if (is.na(group_idx) || group_idx < 1 || group_idx > ncol(data))
      stop("Invalid group column")
    if (group_idx %in% idx) stop("group column should not be within cols")
    group_col <- names(data)[group_idx]
  }

  if (part %in% c("both", 2)) {
    seq_vals <- c(seq(0, diff*5, diff), seq(100-diff*5, 100, diff))
  } else if (part %in% c("bottom", 0)) {
    seq_vals <- seq(0, diff*5, diff)
  } else if (part %in% c("top", 1)) {
    seq_vals <- seq(100-diff*5, 100, diff)
  } else {
    stop("part must be 'both', 'bottom', or 'top'")
  }
  probs <- seq_vals / 100

  if (is.null(group_idx)) {
    qmat <- sapply(data[, idx, drop = FALSE],
                   function(x) quantile(x, probs, na.rm = na.rm))
    result <- as.data.frame(qmat)
    result$percentile <- paste0(seq_vals, "th")
    result <- result[, c("percentile", names(data)[idx])]
  } else {
    result_list <- list()
    groups <- unique(data[[group_col]])
    for (g in groups) {
      sub <- data[data[[group_col]] == g, idx, drop = FALSE]
      qmat <- sapply(sub, function(x) quantile(x, probs, na.rm = na.rm))
      temp <- as.data.frame(qmat)
      temp[[group_col]] <- g
      temp$percentile <- paste0(seq_vals, "th")
      temp <- temp[, c(group_col, "percentile", names(data)[idx])]
      result_list[[as.character(g)]] <- temp
    }
    result <- do.call(rbind, result_list)
  }

  result$percentile <- factor(result$percentile, levels = paste0(seq_vals, "th"))
  rownames(result) <- NULL

  if (verbose) cat("Time used by percdata:", format(Sys.time() - t0, digits = 3), "\n")
  result
}
