\name{detrend_ts}
\alias{detrend_ts}
\title{
Remove linear trend from time series
}
\description{
Fits a linear trend (intercept + slope) to each selected numeric column (or vector) and returns the residuals. This is useful for removing long-term trends before correlation analysis.
}
\usage{
detrend_ts(data, cols = NULL, date_col = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector.}
  \item{cols}{Column indices or names to detrend. If NULL, all numeric columns are used.}
  \item{date_col}{Optional time column (not strictly required for linear detrending).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame or vector with the linear trend removed.
}
\examples{
detrend_ts(data[1:100, c(1, 4, 17:19)], cols = 3:5)
}