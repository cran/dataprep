\name{drift_detect}
\alias{drift_detect}
\title{
Sensor drift detection
}
\description{
Detects potential sensor drift by comparing rolling window statistics (mean or standard deviation) to their long-term baseline. Returns a logical matrix indicating time points where the rolling statistic deviates beyond a threshold.
}
\usage{
drift_detect(data, cols = NULL, window = 30, threshold = 3,
             method = c("mean", "sd"), group = NULL,
             date_col = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector.}
  \item{cols}{Column indices or names of numeric variables. If \code{NULL}, all numeric columns are used.}
  \item{window}{Window size for rolling statistics.}
  \item{threshold}{Threshold in units of standard deviation. Values with absolute z-score above this are flagged as drift.}
  \item{method}{Rolling statistic to use: \code{"mean"} or \code{"sd"}.}
  \item{group}{Optional grouping column for group-wise detection.}
  \item{date_col}{Optional time column (not required for calculation).}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A logical matrix with rows corresponding to observations and columns to the selected variables. \code{TRUE} indicates suspected drift.
}
\examples{
drift_detect(data[1:500, c(1, 4, 17:19)], cols = 3:5, window = 50, threshold = 3)
}