\name{zerona}
\alias{zerona}
\title{
Turn zeros to missing values
}
\description{
Converts zero values to \code{NA}. Useful when zero represents an invalid measurement or when a logarithmic scale is desired.
}
\usage{
zerona(x)
}
\arguments{
  \item{x}{
  A data frame, matrix, or vector containing zeros.
}
}
\value{
An object of the same class with zeros replaced by \code{NA}.
}
\author{
Chun-Sheng Liang <chun-shengliang@qq.com>
}
\examples{
zerona(0:5)
zerona(cbind(a = 0:5, b = c(6:10, 0)))
}
