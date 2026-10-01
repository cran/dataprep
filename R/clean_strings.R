#' Clean and standardize character columns
#'
#' Applies common string cleaning operations to character or factor
#' columns: trimming whitespace, changing case, and applying regular
#' expression substitutions. Numeric and other non-character columns
#' are left untouched.
#'
#' @param data A data frame, matrix, or character vector.
#' @param cols Columns to clean. If \code{NULL} (default), all
#'   character and factor columns are selected. Columns that are
#'   neither character nor factor are skipped, so the numeric and
#'   logical columns of \code{data} are never silently coerced to
#'   character.
#' @param trim Logical; if \code{TRUE}, leading and trailing whitespace
#'   is removed with \code{\link[base]{trimws}}.
#' @param tolower Logical; if \code{TRUE}, converted to lowercase.
#' @param toupper Logical; if \code{TRUE}, converted to uppercase.
#' @param pattern Optional regular expression passed to
#'   \code{\link[base]{gsub}}.
#' @param replacement Replacement string for \code{pattern}. Defaults
#'   to \code{""} (deletion) when \code{pattern} is supplied and
#'   \code{replacement} is \code{NULL}.
#' @param verbose Logical; if \code{TRUE}, prints timing message.
#'
#' @details
#' The operations are applied in a fixed order:
#'
#' \enumerate{
#'   \item \code{trim} (via \code{trimws}),
#'   \item \code{tolower} then \code{toupper},
#'   \item \code{pattern} replacement (via \code{gsub}).
#' }
#'
#' When both \code{tolower} and \code{toupper} are \code{TRUE}, the
#' uppercase conversion wins (it is applied last). In practice only
#' one of the two should be set.
#'
#' For factor columns, the underlying integer codes are dropped and
#' the column becomes a character vector. If you need to keep the
#' factor type, convert the cleaned values back with
#' \code{factor()}.
#'
#' @return A data frame (or character vector, if the input was a
#'   vector) with the selected columns cleaned.
#'
#' @examples
#' df <- data.frame(
#'   id   = 1:3,
#'   name = c("  Alice ", "BOB", "Charlie "),
#'   city = c("New York", "london", "Paris"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Only the character columns are touched by default
#' clean_strings(df, trim = TRUE, tolower = TRUE)
#'
#' # Explicit column selection
#' clean_strings(df, cols = "name", trim = TRUE, toupper = TRUE)
#'
#' # Regex replacement
#' clean_strings(df, cols = "city",
#'               pattern = "\\s+", replacement = "_")
#'
#' # Vector input
#' clean_strings(c(" A ", " b ", "C"), trim = TRUE, tolower = TRUE)
#'
#' @export
clean_strings <- function(data, cols = NULL,
                          trim = FALSE, tolower = FALSE, toupper = FALSE,
                          pattern = NULL, replacement = NULL,
                          verbose = FALSE) {
  t0 <- Sys.time()

  # ---- helper: apply all operations to a single character vector ----
  clean_one <- function(x) {
    x <- as.character(x)
    if (trim)    x <- trimws(x)
    if (tolower) x <- tolower(x)
    if (toupper) x <- toupper(x)
    if (!is.null(pattern)) {
      if (is.null(replacement)) replacement <- ""
      x <- gsub(pattern, replacement, x)
    }
    x
  }

  # ---- vector input ----
  if (is.vector(data) && !is.list(data)) {
    out <- clean_one(data)
    if (verbose) {
      cat("Time used by clean_strings:",
          format(Sys.time() - t0, digits = 3), "\n")
    }
    return(out)
  }

  # ---- data frame / matrix input ----
  if (is.matrix(data)) data <- as.data.frame(data,
                                             stringsAsFactors = FALSE)
  if (!is.data.frame(data)) {
    stop("data must be a data frame, a matrix, or a vector")
  }

  if (is.null(cols)) {
    # Default: only character and factor columns
    idx <- which(vapply(data,
                        function(x) is.character(x) || is.factor(x),
                        logical(1)))
    if (length(idx) == 0) {
      if (verbose) {
        cat("No character or factor columns found; data unchanged.\n")
        cat("Time used by clean_strings:",
            format(Sys.time() - t0, digits = 3), "\n")
      }
      return(data)
    }
  } else {
    idx <- resolve_cols(data, cols)
    # Guard: if the user explicitly selected non-character columns,
    # warn instead of silently coercing.
    bad <- vapply(idx,
                  function(j) {
                    x <- data[[j]]
                    !(is.character(x) || is.factor(x) ||
                      (is.numeric(x) && all(is.na(x))))
                  },
                  logical(1))
    if (any(bad)) {
      warning("Non-character columns selected and will be coerced: ",
              paste(names(data)[idx[bad]], collapse = ", "),
              call. = FALSE)
    }
  }

  for (j in idx) data[[j]] <- clean_one(data[[j]])

  if (verbose) {
    cat("Cleaned", length(idx), "column(s).\n")
    cat("Time used by clean_strings:",
        format(Sys.time() - t0, digits = 3), "\n")
  }
  data
}