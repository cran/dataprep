\name{deduplicate}
\alias{deduplicate}
\title{
Remove duplicate observations
}
\description{
Identifies and removes duplicate rows. Supports exact duplication on all or selected columns, and fuzzy duplication for numeric columns using a rounding tolerance.
}
\usage{
deduplicate(data, cols = NULL, method = "exact",
            key_cols = NULL, tol = 1e-8, max_dist = 1,
            verbose = FALSE)
}
\arguments{
  \item{data}{A data frame.}
  \item{cols}{The column indices or names used for duplication detection. If NULL and \code{method = "exact"}, all columns are used. For \code{method = "fuzzy"}, must be numeric columns.}
  \item{method}{Duplicate detection method. One of \code{"exact"} or \code{"fuzzy"}.}
  \item{key_cols}{Deprecated. Use \code{cols} instead.}
  \item{tol}{Tolerance for fuzzy matching. Values are rounded to the number of decimal places indicated by \code{tol} before duplicate detection.}
  \item{max_dist}{Not used in current implementation.}
  \item{verbose}{Logical; if \code{TRUE}, prints number of duplicates removed.}
}
\details{
Fuzzy matching works by rounding numeric columns to a sufficient number of decimal places (derived from \code{tol}) and then applying exact duplicate detection.
}
\value{
A data frame with duplicates removed.
}
\examples{
# Exact duplicates on all columns
deduplicate(data[1:200, c(1, 4, 17:19)])
# Exact duplicates on selected columns
deduplicate(data[1:200, c(1, 4, 17:19)], cols = 3:5)
# Fuzzy duplicates with tolerance 0.01
deduplicate(data[1:200, c(1, 4, 17:19)], cols = 3:5, method = "fuzzy", tol = 0.01)
}