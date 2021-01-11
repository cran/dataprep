\name{percdata}
\alias{percdata}
\title{
Calculate the top and bottom percentiles of each selected variable
}
\description{
Outliers can be preliminarily checked by the calculated top and bottom percentiles. Basic R functions in packages from system library are used to get these percentiles of selected variables in data frames, instead of calling other packages. It saves time.
}
\usage{
percdata(data, start = NULL, end = NULL, group = NULL, diff = 0.1, part = 'both')
}
\arguments{
  \item{data}{
  A data frame to calculate percentiles, from the column \code{start} to the column \code{end}.
}
  \item{start}{
  The column number of the first variable to calculate percentiles for.
}
  \item{end}{
  The column number of the last variable to calculate percentiles for.
}
  \item{group}{
  The column number of the grouping variable. It can be selected according to whether the data needs to be processed in groups. If grouping is not required, leave it default (NULL); if grouping is required, set \code{group} as the column number (position) where the grouping variable is located. If there are more than one grouping variable, it can be turned into a longer group through combination and transformation in advance.
}
  \item{diff}{
  The common difference between \code{quantile}'s \code{probs}. Default is 0.1.
}
  \item{part}{
  The option of calculating bottom and/or top percentiles (parts). Default is 'both', or 2 for both bottom and top parts. Setting it as 'bottom' or 0 for bottom part and 'top' or 1 for top part.
}
}
\details{
The data to be processed ranges from the column \code{start} to the last column \code{end}. The column numbers of these two columns are needed for the arguments. This requires that the variables of the data to be processed are arranged continuously in the database or table. Or else, it is necessary to move the columns in advance to make a continuous arrangement.
}
\value{
  Top (highest or greatest) and bottom (lowest or smallest) percentiles are calculated. According to the default \code{diff} (=0.1), the calculated values are as follows.
  \item{0th }{Quantile with \code{probs = 0}}
  \item{0.1th }{Quantile with \code{probs = 0.001}}
  \item{0.2th }{Quantile with \code{probs = 0.002}}
  \item{0.3th }{Quantile with \code{probs = 0.003}}
  \item{0.4th }{Quantile with \code{probs = 0.004}}
  \item{0.5th }{Quantile with \code{probs = 0.005}}
  \item{99.5th }{Quantile with \code{probs = 0.995}}
  \item{99.6th }{Quantile with \code{probs = 0.996}}
  \item{99.7th }{Quantile with \code{probs = 0.997}}
  \item{99.8th }{Quantile with \code{probs = 0.998}}
  \item{99.9th }{Quantile with \code{probs = 0.999}}
  \item{100th }{Quantile with \code{probs = 1}}
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download
}

\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\seealso{
\code{dataprep::percplot}
}
\examples{
# Select the grouping variable and remaining variables after deletion by varidele.
# Column 4 ('monthyear') is the group and the fraction for varidele is 0.25.
# After extracting according to the result by varidele, the group is in the first column.
percdata(data[,c(4,27:61)],2,36,1)
}


