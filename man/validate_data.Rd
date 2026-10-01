\name{validate_data}
\alias{validate_data}
\title{
Validate data against a set of rules
}
\description{
Checks whether data conforms to user-specified rules such as column type, value range, uniqueness, and presence of missing values. Returns a report indicating which rules passed and which failed.
}
\usage{
validate_data(data, rules, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame to validate.}
  \item{rules}{A list of rules. Each rule is a list with components: \code{column} (column name), \code{type} (one of "numeric", "integer", "character", "factor"), \code{min}, \code{max}, \code{unique} (logical), and \code{na_allowed} (logical).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame with columns: \code{rule}, \code{column}, \code{passed} (logical), and \code{message}. If a rule passes, \code{message} is "OK"; otherwise it contains a description of the failure.
}
\examples{
rules <- list(
  list(column = "3.98", type = "numeric", min = 0, na_allowed = TRUE),
  list(column = "monthyear", type = "character", unique = FALSE)
)
validate_data(data[1:50, c(1, 4, 17:19)], rules)
}