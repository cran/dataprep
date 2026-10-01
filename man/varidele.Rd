\name{varidele}
\alias{varidele}
\title{
Delete variables containing too many missing values
}
\description{
Removes any numeric column whose missing value proportion exceeds a
given threshold.
}
\usage{
varidele(data, cols = NULL, fraction = 0.25, verbose = FALSE)
}
\arguments{
  \item{data}{A data frame or matrix containing numeric columns.}
  \item{cols}{Column indices or names of the numeric columns to
    consider. If \code{NULL}, all columns are used.}
  \item{fraction}{Missing proportion threshold. Columns with a missing
    fraction \code{>= fraction} are deleted. Default 0.25.}
  \item{verbose}{Logical; if \code{TRUE}, prints timing and deleted
    variables.}
}
\details{
The missing fraction is computed after excluding rows where all
selected variables are NA. Any column whose missing fraction reaches
the threshold is removed, regardless of its position in the selected
block.
}
\value{
A data frame with the retained columns.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}.
   It includes particle number concentrations in SMEAR I Varrio forest.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
varidele(data, cols = 5:65)
}