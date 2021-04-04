\name{zerona}
\alias{zerona}
\title{
Turn zeros to missing values
}
\description{
Zeros are suitable in logarithmic scale and should be removed for plots.
}
\usage{
zerona(x)
}
\arguments{
  \item{x}{
A dataframe, matrix, or vector containing zeros.
}
}
\value{
A dataframe, matrix, or vector with zeros being turned into missing values.
}
\author{
Chun-Sheng Liang <liangchunsheng@lzu.edu.cn>
}
\examples{
zerona(0:5)
zerona(cbind(a=0:5,b=c(6:10,0)))
}
