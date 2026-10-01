#' Melt a data.frame (wide to long)
#'
#' Fast reshape using a SIMD + OpenMP C++ backend (\code{melt_cpp}).
#'
#' @param data               data.frame
#' @param id                 id columns (character names, integer indices,
#'                           logical mask, or NULL).
#' @param measure.vars       measure columns.
#' @param variable.name      name of the output "variable" column.
#' @param value.name         name of the output "value" column.
#' @param na.rm              drop rows where value is NA. Handled inside
#'                           the C++ backend via a two-pass count + prefix
#'                           offsets, so no intermediate full table is
#'                           constructed.
#' @param cores              number of OpenMP threads; NULL = auto.
#' @param major              "col" (reshape2-style, default) or
#'                           "row" (tidyr-style). NULL is equivalent to "col".
#' @param as.factor          Whether the \code{variable} column is a factor.
#'                           \code{NULL} (default) uses \code{TRUE} for
#'                           \code{major = "col"} and \code{FALSE} for
#'                           \code{major = "row"}. Explicit \code{TRUE} /
#'                           \code{FALSE} overrides that default.
#' @param verbose            print timing/messages.
#' @param parallel_threshold minimum output size before auto parallelism.
#' @param id.vars            alias of \code{id}.
#'
#' @return data.frame in long format.
#' @export
melt <- function(data,
                 id                  = NULL,
                 measure.vars        = NULL,
                 variable.name       = "variable",
                 value.name          = "value",
                 na.rm               = FALSE,
                 cores               = NULL,
                 major               = NULL,
                 as.factor           = NULL,
                 verbose             = FALSE,
                 parallel_threshold  = 5e6,
                 id.vars             = NULL) {

  if (!is.data.frame(data)) stop("melt is only supported for data frames")
  if (ncol(data) < 1L)      stop("data has no columns")
  if (nrow(data) == 0L)     stop("data has no rows")

  if (is.null(major)) {
    major <- "col"          # default: reshape2-compatible column-major
  } else {
    major <- match.arg(major, c("col", "row"))
  }

  # `variable` column type.  NULL => factor for "col", character for "row".
  # Explicit TRUE / FALSE overrides that default.
  if (is.null(as.factor)) {
    as.factor <- (major == "col")
  } else {
    as.factor <- isTRUE(as.factor)
  }

  if (!is.null(id) && !is.null(id.vars))
    stop("Specify only one of `id` or `id.vars` (they are aliases).")
  if (is.null(id)) id <- id.vars
  if (!is.null(id) && !is.null(measure.vars))
    stop("Please specify only one of id (id.vars) or measure.vars")

  ncols  <- ncol(data)
  cnames <- names(data)

  id_arg         <- NULL
  n_measure_cols <- NULL

  if (!is.null(id)) {
    if (is.character(id)) {
      bad <- !(id %in% cnames)
      if (any(bad)) stop(sprintf("id column(s) not found: %s",
                                 paste(id[bad], collapse = ", ")))
      id_arg <- match(id, cnames)
    } else if (is.numeric(id)) {
      id_arg <- as.integer(id)
      if (any(id_arg < 1L | id_arg > ncols))
        stop("id contains invalid column indices")
    } else if (is.logical(id)) {
      if (length(id) != ncols)
        stop("logical id must have length ncol(data)")
      id_arg <- which(id)
    } else stop("id must be character, numeric, or logical")

    n_measure_cols <- ncols - length(unique(id_arg))

  } else if (!is.null(measure.vars)) {
    if (is.character(measure.vars)) {
      bad <- !(measure.vars %in% cnames)
      if (any(bad)) stop(sprintf("measure.vars not found: %s",
                                 paste(measure.vars[bad], collapse = ", ")))
      midx <- match(measure.vars, cnames)
    } else if (is.numeric(measure.vars)) {
      midx <- as.integer(measure.vars)
    } else if (is.logical(measure.vars)) {
      if (length(measure.vars) != ncols)
        stop("logical measure.vars must have length ncol(data)")
      midx <- which(measure.vars)
    } else stop("measure.vars must be character, numeric, or logical")

    if (anyNA(midx) || any(midx < 1L | midx > ncols))
      stop("measure.vars contains invalid column indices")

    id_arg         <- setdiff(seq_len(ncols), unique(midx))
    n_measure_cols <- length(unique(midx))

  } else {
    id_arg         <- NULL
    n_measure_cols <- max(1L, ncols - 1L)
  }

  total_elements <- as.double(nrow(data)) * as.double(n_measure_cols)

  MAX_THREADS_CAP <- 128L

  if (is.null(cores)) {
    opt <- getOption("dataprep.cores", NULL)
    if (!is.null(opt)) {
      cores <- as.integer(opt)
    } else if (total_elements < parallel_threshold) {
      cores <- 1L
    } else {
      cores <- as.integer(floor(total_elements / 2e6))
    }
  }
  cores <- as.integer(cores)
  if (is.na(cores) || cores < 1L) cores <- 1L
  if (cores > MAX_THREADS_CAP)    cores <- MAX_THREADS_CAP

  if (verbose) {
    cat(sprintf(
      "[melt] rows=%d  cols=%d  measure_cols=%d  major=%s  threads=%d  na.rm=%s\n",
      nrow(data), ncols, n_measure_cols,
      major,
      cores,
      if (isTRUE(na.rm)) "TRUE" else "FALSE"))
  }

  major_arg <- major

  melt_cpp(
    df            = data,
    id            = id_arg,
    variable_name = variable.name,
    value_name    = value.name,
    major         = major_arg,
    as_factor     = as.factor,
    n_threads     = cores,
    na_rm         = isTRUE(na.rm)
  )
}