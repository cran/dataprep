\name{filter_low_var}
\alias{filter_low_var}
\title{
Remove low-variance (near-constant) variables
}
\description{
Removes numeric columns whose variance (or standard deviation) falls
below a specified threshold.
}
\usage{
filter_low_var(data, cols = NULL, cutoff = 0.01,
               method = "var", verbose = FALSE)
}
\arguments{
  \item{data}{A data frame.}
  \item{cols}{Column indices or names of numeric variables to check.
    If \code{NULL}, all numeric columns are used.}
  \item{cutoff}{Variance (or standard deviation) threshold. Columns
    with statistic \code{<= cutoff} are removed.}
  \item{method}{Statistic to use: \code{"var"} for variance,
    \code{"sd"} for standard deviation.}
  \item{verbose}{Logical; if \code{TRUE}, prints removed column names
    and their variance.}
}
\details{
Variance is computed with NA values removed. For \code{method = "sd"},
the standard deviation is compared \strong{directly} against
\code{cutoff}; no internal squaring is performed.
}
\value{
A data frame with low-variance columns removed.
}
\examples{
data <- data.frame(
  id = 1:100,
  const = rep(5, 100),
  noise = rnorm(100, sd = 0.005)
)
filter_low_var(data, cutoff = 0.001)
}