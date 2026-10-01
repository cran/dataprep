#' Remove duplicate observations
#' @param data A data frame.
#' @param cols Columns to check.
#' @param method \code{"exact"} or \code{"fuzzy"}.
#' @param key_cols Deprecated.
#' @param tol Tolerance for fuzzy matching.
#' @param max_dist Not used.
#' @param verbose Logical.
#' @return A data frame with duplicates removed.
#' @export
deduplicate <- function(data, cols = NULL, method = "exact",
                        key_cols = NULL, tol = 1e-8, max_dist = 1,
                        verbose = FALSE) {
  t0 <- Sys.time()
  method <- match.arg(method, c("exact", "fuzzy"))

  if (is.vector(data) && !is.list(data)) {
    data <- data.frame(value = data, stringsAsFactors = FALSE)
    cols <- 1
    return_vec <- TRUE
  } else {
    return_vec <- FALSE
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (is.null(cols)) cols <- seq_len(ncol(data))
  }

  n_orig <- nrow(data)

  if (method == "exact") {
    if (is.null(cols)) {
      dup_flags <- duplicated(data)
    } else {
      idx <- resolve_cols(data, cols)
      dup_flags <- duplicated(data[, idx, drop = FALSE])
    }
  } else {
    if (is.null(cols)) {
      idx <- which(sapply(data, is.numeric))
      if (length(idx) == 0) stop("No numeric columns for fuzzy matching")
    } else {
      idx <- resolve_cols(data, cols)
      if (!all(sapply(data[idx], is.numeric))) stop("Fuzzy matching requires numeric columns")
    }
    decimals <- max(0, ceiling(-log10(tol)))
    tmp <- data
    for (j in idx) {
      tmp[[j]] <- round(tmp[[j]], decimals)
    }
    dup_flags <- duplicated(tmp[, idx, drop = FALSE])
  }

  result <- data[!dup_flags, , drop = FALSE]
  rownames(result) <- NULL

  if (return_vec) {
    result <- result$value
  }

  if (verbose) {
    cat(n_orig - nrow(result), "observations removed as duplicates.\n")
  }
  result
}
