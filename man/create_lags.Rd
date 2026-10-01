\name{create_lags}
\alias{create_lags}
\title{
Create lagged variables
}
\description{
Generates lagged (or lead) versions of selected numeric columns. Positive lags shift values backward in time (past values), negative lags shift forward. Useful for time series modeling.
}
\usage{
create_lags(data, cols = NULL, lags = 1, prefix = "lag_",
            group = NULL, date_col = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector.}
  \item{cols}{Column indices or names to create lags for.}
  \item{lags}{Vector of lag orders (positive for past, negative for future).}
  \item{prefix}{Prefix for new column names.}
  \item{group}{Optional grouping column for group-specific lags.}
  \item{date_col}{Currently ignored. Data are assumed to be in the correct time order.}
  \item{verbose}{Logical; if \code{TRUE}, prints number of lag columns created.}
}
\value{
A data frame or matrix with additional lag columns.
}
\examples{
create_lags(data[1:100, c(1, 4, 17:19)], cols = 3:5, lags = c(1, 2))
}
