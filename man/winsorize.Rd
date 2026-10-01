\name{winsorize}
\alias{winsorize}
\title{
Winsorize outliers by capping extreme values
}
\description{
Replaces extreme values in selected columns with the specified top and bottom quantile thresholds, rather than deleting them. This is useful for reducing the influence of outliers while preserving sample size.
}
\usage{
winsorize(data, cols = NULL, top = 0.995, bottom = 0.0025,
          group = NULL, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame, matrix, or numeric vector.}
  \item{cols}{The column indices or names of selected variables. If NULL, all columns are used.}
  \item{top}{The top quantile threshold. Values above this are capped at the threshold.}
  \item{bottom}{The bottom quantile threshold. Values below this are capped at the threshold.}
  \item{group}{Optional grouping column for group-wise winsorization.}
  \item{verbose}{Logical; if \code{TRUE}, prints progress message.}
}
\value{
A data frame with extreme values capped.
}
\examples{
winsorize(data[1:100, c(1, 4, 17:19)], cols = 3:5)
}