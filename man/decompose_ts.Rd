\name{decompose_ts}
\alias{decompose_ts}
\title{
Simple time series decomposition
}
\description{
Decomposes a time series into trend, seasonal, and residual components using simple moving averages and group means. The function returns the residual component (or the original series with trend removed) to simplify analysis.
}
\usage{
decompose_ts(data, cols = NULL, date_col = NULL,
             period = "month", method = "additive", verbose = FALSE)
}
\arguments{
  \item{data}{A data frame with a time column.}
  \item{cols}{Column indices or names to decompose.}
  \item{date_col}{Time column index or name.}
  \item{period}{Seasonal period: \code{"month"}, \code{"day"}, or \code{"hour"}.}
  \item{method}{Decomposition method: \code{"additive"} or \code{"multiplicative"}.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame with the residual component.
}
\examples{
decompose_ts(data[1:500, c(1, 4, 17:19)], cols = 3:5, date_col = 1, period = "month")
}