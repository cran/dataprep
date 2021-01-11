\name{percoutl}
\alias{percoutl}
\title{
Traditional percentile-based outlier removal
}
\description{
The percentile-based outlier removal methods usually take a quantile as a threshold and values above it will be deleted. Here, two quantiles are used for both the top and the bottom. For the bottom, accordingly, values below the quantile threshold will be removed.
}
\usage{
percoutl(data, start = NULL, end = NULL, group = NULL,
top = 0.995, bottom = 0.0025, by = "min", half = 30, cores = NULL)
}
\arguments{
  \item{data}{
  A data frame containing outliers (and missing values). Its columns from \code{start} to \code{end} will be checked.
}
  \item{start}{
  The column number of the first selected variable.
}
  \item{end}{
  The column number of the last selected variable.
}
  \item{group}{
    The column number of the grouping variable. It can be selected according to whether the data needs to be processed in groups. If grouping is not required, leave it default (NULL); if grouping is required, set \code{group} as the column number (position) where the grouping variable is located. If there are more than one grouping variable, it can be turned into a longer group through combination and transformation in advance.
}
  \item{top}{
  The top percentile is 0.995 by default.
}
  \item{bottom}{
  The bottom percentile is 0.0025 by default.
}
  \item{by}{
  The time extension unit by is a minute ("min") by default. The user can specify other time units. For example, "5 min" means that the time extension unit is 5 minutes.
}
  \item{half}{
  Half window size of hourly moving average. It is 30 (minutes) by default, which is determined by the time expansion unit minute ("min").
}
  \item{cores}{
  The number of CPU cores.
}
}
\details{
Unlike \code{condextr}, a point-by-point considered outlier removal method, the traditional percentile-based \code{percoutl} is a "one size fits all" outlier deletion method. It may delete too many or too few values that are non-outliers or outliers respectively.
}
\value{
A data frame after deleting outliers.
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download

2. Wickham, H., Francois, R., Henry, L. & Muller, K. 2017. dplyr: A Grammar of Data Manipulation. 0.7.4 ed. http://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr.

3. Wickham, H., Francois, R., Henry, L. & Muller, K. 2019. dplyr: A Grammar of Data Manipulation. R package version 0.8.3. https://CRAN.R-project.org/package=dplyr.
}
\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\seealso{
\code{dataprep::condextr}
}

\examples{
percoutl(obsedele(data[c(1:200,3255:3454),c(1:4,27:61)],5,39,4,cores=2),5,39,4,cores=2)
# Result
}

