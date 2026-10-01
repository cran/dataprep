\name{descdata}
\alias{descdata}
\title{
Fast descriptive statistics
}
\description{
Computes descriptive statistics for selected numeric columns using
efficient C++ routines.
}
\usage{
descdata(data, cols = NULL, stats = 1:9, first = "variables",
         cores = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector. If a vector is
    supplied, it is treated as a single variable and the output's
    first column is named \code{"value"}.}
  \item{cols}{Column indices or names of numeric variables to describe.
    If \code{NULL}, all numeric columns are used.}
  \item{stats}{Statistic selection from the nine available:
    \code{n}, \code{na}, \code{mean}, \code{sd}, \code{median},
    \code{trimmed}, \code{min}, \code{max}, \code{IQR}. Can be a
    numeric vector or a character vector of statistic names.}
  \item{first}{Name for the first column of the output, which contains
    the variable identifiers.}
  \item{cores}{Number of CPU cores. \code{NULL} (default) means single-threaded execution; pass an integer > 0 to enable OpenMP when \code{nrow(x) * ncol(x) > 1e5}.}
  \item{verbose}{Logical; if \code{TRUE}, prints timing message.}
}
\details{
The function accepts vectors, matrices, and data frames. Numeric
columns are required. The calculation is performed entirely in C++
via Rcpp for speed.
}
\value{
A data frame with descriptive statistics for each variable.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}.
   It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
descdata(data, cols = 5:65)
descdata(data, cols = 5:65, stats = c(2, 7:9))
descdata(data, cols = 5:65, stats = c("na", "min", "max", "IQR"))
}
