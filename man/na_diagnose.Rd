\name{na_diagnose}
\alias{na_diagnose}
\title{
Diagnose missing value patterns in data
}
\description{
Provides a summary of missing values for selected columns, including
the number and fraction of missing values, the number of consecutive
missing runs, and the longest consecutive missing run length (in
number of observations).
}
\usage{
na_diagnose(data, cols = NULL, date_col = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame or matrix containing variables to diagnose.}
  \item{cols}{The column indices or names of selected variables. If
    \code{NULL}, all numeric columns are used.}
  \item{date_col}{Reserved for future use; currently ignored.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress messages.}
}
\value{
A data frame with columns: \code{variable}, \code{n} (total
observations), \code{na} (number of missing values), \code{na_frac}
(missing fraction), \code{na_runs} (number of consecutive missing
runs), \code{max_run} (longest consecutive missing run length in
number of observations).
}
\examples{
na_diagnose(data[1:500, c(1, 4, 17:19)], cols = 3:5)
}