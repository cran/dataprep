\name{varidele}
\alias{varidele}
\title{
Delete variables containing too many missing values (NA)
}
\description{
Missing values often exist in real data. However, excessive missing values would lead to information distortion and inappropriate handling of them may end up coming to inaccurate conclusions about the data. Therefore, to control and improve the quality of original data that have already been produced by instruments in the first beginning, the data preprocessing method in this package introduces the deletion of variables to filter out and delete the variables with too many missing values.
}
\usage{
varidele(data, start = NULL, end = NULL, fraction = 0.25)
}
\arguments{
  \item{data}{
  A data frame containing variables with excessive missing values. Its columns from \code{start} to \code{end} will be checked.
}
  \item{start}{
  The column number of the first selected variable.
}
  \item{end}{
  The column number of the last selected variable.
}
  \item{fraction}{
  The proportion of missing values of variables. Default is 0.25.
}
}
\details{
It operates only at the beginning and the end variables, so as to ensure that the remaining variables after deleting are continuous without breaks. The deletion of variables with excessive missing values is mainly based on the proportion of missing values of variables, excluding blank observations. The default proportion is 0.25, which can be adjusted according to practical needs.
}
\value{
A data frame after deleting variables with too many missing values.
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download
}

\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\examples{
# Show the first 5 and last 5 rows and columns besides the date column.
varidele(data,5,65)[c(1:5,(nrow(data)-4):nrow(data)),c(1,5:9,35:39)]
# The first 22 variables and last 4 variables are deleted with the NA proportion of 0.25.


# Increasing the NA proportion can keep more variables.
# Proportions 0.4 and 0.5 have the same result for this example data.
# Namely 2 more variables in the beginning and 1 more variable in the end are kept.
varidele(data,5,65,.5)[c(1:5,(nrow(data)-4):nrow(data)),c(1,5:9,38:42)]

# Setting proportion as 0.6, then 3 more variables in the beginning are kept
# than that of proportions 0.4 and 0.5.
varidele(data,5,65,.6)[c(1:5,(nrow(data)-4):nrow(data)),c(1,5:9,41:45)]
}


