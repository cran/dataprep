\name{roll_apply}
\alias{roll_apply}
\title{
Apply rolling window statistics
}
\description{
Computes rolling statistics (mean, sd, median, sum, min, max, var) over a moving window of fixed length. Supports group-wise computation and various alignments.
}
\usage{
roll_apply(data, cols = NULL, window = 3, method = "mean",
           align = "right", group = NULL, date_col = NULL,
           verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector.}
  \item{cols}{Column indices or names to process.}
  \item{window}{Window size (number of observations).}
  \item{method}{Statistic to compute: \code{"mean"}, \code{"sd"}, \code{"median"}, \code{"sum"}, \code{"min"}, \code{"max"}, or \code{"var"}.}
  \item{align}{Alignment of window: \code{"right"}, \code{"left"}, or \code{"center"}.}
  \item{group}{Optional grouping column for group-wise rolling computation.}
  \item{date_col}{Not used. Present for interface consistency.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame or vector with rolling statistics.
}
\examples{
roll_apply(data[1:100, c(1, 4, 17:19)], cols = 3:5, window = 5, method = "mean")
}