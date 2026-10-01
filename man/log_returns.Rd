\name{log_returns}
\alias{log_returns}
\title{
Logarithmic returns for financial time series
}
\description{
Computes the logarithmic return \eqn{\log(x_t / x_{t-1})} for selected numeric columns. Used extensively in financial time series analysis. Group-wise computation is supported.
}
\usage{
log_returns(data, cols = NULL, group = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector.}
  \item{cols}{Column indices or names of numeric variables. If \code{NULL}, all numeric columns are used.}
  \item{group}{Optional grouping column for group-specific returns.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\details{
The function only computes a return when both the current and previous values are positive. Otherwise, the result is \code{NA}.
}
\value{
A vector or data frame with logarithmic returns. The first observation of each group (or the vector) is \code{NA}.
}
\examples{
log_returns(data[1:200, c(1, 4, 17:19)], cols = 3:5)
}