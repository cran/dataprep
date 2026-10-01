\name{bin_data}
\alias{bin_data}
\title{
  Discretize continuous variables into bins
}
\description{
  Converts numeric columns into categorical factors using equal-width, equal-frequency,
  or custom breakpoints. This is a common preprocessing step for transforming
  continuous variables for modeling or visualization.
}
\usage{
bin_data(data, cols = NULL, method = "equal_width", 
         bins = 10, breaks = NULL, include_lowest = TRUE,
         labels = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame containing numeric columns to bin.}
  \item{cols}{Column indices or names to bin. If \code{NULL}, all numeric columns are used.}
  \item{method}{Binning method. One of \code{"equal_width"}, \code{"equal_freq"}, or \code{"custom"}.}
  \item{bins}{Number of bins for equal-width and equal-frequency methods.}
  \item{breaks}{Numeric vector of breakpoints for custom binning. Required when \code{method = "custom"}.}
  \item{labels}{Optional character vector of labels for the bins.}
  \item{include_lowest}{Logical; if \code{TRUE}, the lowest break value is included in the first bin.}
  \item{verbose}{Logical; if \code{TRUE}, prints a timing message.}
}
\details{
  For \code{"equal_width"}, bins are created by dividing the range of the data into
  equal-width intervals. For \code{"equal_freq"}, bins are created by quantiles so
  that each bin contains approximately the same number of observations. The \code{"custom"}
  method uses user-supplied breakpoints.

  Missing values are preserved as \code{NA} in the output.
}
\value{
  If \code{data} is a data frame, a data frame with the selected columns replaced by factors. If \code{data} is a numeric vector, a factor vector.
}
\examples{
data <- data.frame(x = rnorm(100), y = runif(100))
# Equal-width binning into 5 bins
bin_data(data, cols = 1:2, bins = 5)

# Equal-frequency binning
bin_data(data, cols = "x", method = "equal_freq", bins = 4)

# Custom breaks
bin_data(data, cols = "x", method = "custom", breaks = c(-Inf, 0, Inf), labels = c("neg", "pos"))
}
