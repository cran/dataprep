#' Random sampling with optional stratification
#' @param data A data frame.
#' @param group Grouping column.
#' @param size Sample size per group (see Details).
#' @param frac Sampling fraction per group (see Details).
#' @param replace Sample with replacement.
#' @param seed Random seed. If \code{NULL}, the current RNG state is
#'   used; if supplied, \code{set.seed(seed)} is called before sampling.
#' @param verbose Logical.
#' @return A sampled data frame.
#'
#' @details
#' If \code{group} is \code{NULL}, either \code{size} (absolute number
#' of rows) or \code{frac} (fraction of \code{nrow(data)}) must be
#' supplied.
#'
#' If \code{group} is provided, sampling is performed separately within
#' each group. With \code{size}, every group contributes the same
#' absolute number of rows (capped at the group's own size unless
#' \code{replace = TRUE}). With \code{frac}, every group contributes
#' its own \code{round(frac * group_size)} rows (at least 1), so groups
#' of different sizes are sampled proportionally. If \code{group}
#' is provided but both \code{size} and \code{frac} are
#' \code{NULL}, each group contributes exactly one row.
#'
#' @examples
#' sample_data(mtcars, frac = 0.5)
#' sample_data(mtcars, size = 3, group = "cyl", seed = 123)
#'
#' # frac is per-group: large groups contribute more rows
#' n_by_cyl <- function(d) table(d$cyl)
#' n_by_cyl(sample_data(mtcars, frac = 0.5, group = "cyl", seed = 1))
#' @export
sample_data <- function(data, group = NULL, size = NULL, frac = NULL,
                        replace = FALSE, seed = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  if (is.vector(data) && !is.list(data)) {
    stop("sample_data requires a data frame")
  }
  if (is.null(group) && is.null(size) && is.null(frac)) {
    stop("At least one of group, size, or frac must be provided")
  }
  if (!is.null(size) && !is.null(frac)) {
    stop("Provide either size or frac, not both")
  }

  if (!is.null(seed)) set.seed(as.integer(seed)[1])

  if (is.null(group)) {
    n <- nrow(data)
    if (!is.null(size)) {
      k <- as.integer(size)[1]
      if (!replace) k <- min(k, n)
    } else {
      k <- max(1L, as.integer(round(frac * n)))
      if (!replace) k <- min(k, n)
    }
    idx <- sample(n, k, replace = replace)
    result <- data[idx, , drop = FALSE]
  } else {
    group_col <- if (is.character(group)) group else names(data)[group]
    gvec <- data[[group_col]]
    ug   <- unique(gvec)

    all_idx <- integer(0)
    for (g in ug) {
      idx_g <- which(gvec == g)
      m <- length(idx_g)
      if (m == 0) next
      if (!is.null(size)) {
        k <- as.integer(size)[1]
        if (!replace) k <- min(k, m)
      } else {
        k <- max(1L, as.integer(round(frac * m)))
        if (!replace) k <- min(k, m)
      }
      all_idx <- c(all_idx, sample(idx_g, k, replace = replace))
    }
    result <- data[all_idx, , drop = FALSE]
  }

  rownames(result) <- NULL
  if (verbose) {
    cat("Time used by sample_data:",
        format(Sys.time() - t0, digits = 3), "\n")
  }
  result
}