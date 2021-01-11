\name{obsedele}
\alias{obsedele}
\title{
Delete observations with variable(s) containing too many consecutive missing values (NA) in time series
}
\description{
The description of varidele mentions that missing values are common in real data but excessive missing values would led to inaccurate information and conclusions about the data. To control and improve the quality of original data, besides deleting the variables with too many missing values, the observations with one or more variables containing excessive consecutive missing values in time series should further be deleted.
}
\usage{
obsedele(data, start = NULL, end = NULL, group = NULL, by = "min",
half = 30, cores = NULL)
}
\arguments{
  \item{data}{
  A data frame containing variables with too many consecutive missing values (NA) in time series. Its columns from \code{start} to \code{end} will be checked.
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
  \item{by}{
  The time extension unit \code{by} is a minute ("min") by default. The user can specify other time units. For example, "5 min" means that the time extension unit is 5 minutes.
}
  \item{half}{
  Half window size of hourly moving average. It is 30 (minutes) by default, which is determined by the time expansion unit minute ("min"). Users can set its value as required.
}
  \item{cores}{
  The number of CPU cores.
}
}
\details{
How to delete observations based on consecutive missing value? The idea here is to remove the observations with incomplete half-hour averages, that is, the observations with at least one variable missing more than half an hour. Besides the design of flexible constraints, the fast C + + optimized algorithm is also used, which saves much more time. Using \code{tidyr::complete} (and \code{dplyr::group_by}, without loop) or basic functions such \code{merge} and \code{seq} (in loop) temporarily to extend the full time period and using the C + + optimized \code{RcppRoll::roll_mean} (much faster than ordinary functions) to calculate the hourly moving average are very important to quickly detect consecutive missing values. For the loop, parallel computing can be conducted using packages \code{parallel}, \code{doParallel}, and \code{foreach}. Further, this method will also be used to delete outliers. In this way, it ensures that the observations with excessive consecutive missing values are deleted completely and the interpolation in time series is reasonable.
}
\value{
A data frame after deleting observations with too many consecutive missing values in time series.
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download

2. Wickham, H., Francois, R., Henry, L. & Muller, K. 2017. dplyr: A Grammar of Data Manipulation. 0.7.4 ed. http://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr.

3. Wickham, H., Francois, R., Henry, L. & Muller, K. 2019. dplyr: A Grammar of Data Manipulation. R package version 0.8.3. https://CRAN.R-project.org/package=dplyr.

4. Wickham, H. & Henry, L. 2019. tidyr: Tidy Messy Data. R package version 1.0.0. https://CRAN.R-project.org/package=tidyr.

5. Ushey, K. 2018. RcppRoll: Efficient Rolling / Windowed Operations. R package version 0.3.0. https://CRAN.R-project.org/package=RcppRoll.

6. Wallig, M., Microsoft & Weston, S. 2020. foreach: Provides Foreach Looping Construct. R package version 1.5.0. https://CRAN.R-project.org/package=foreach.

7. Ooi, H., Corporation, M. & Weston, S. 2019. doParallel: Foreach Parallel Adaptor for the 'parallel' Package. R package version 1.0.15. https://CRAN.R-project.org/package=doParallel.
}
\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\examples{
# Select start as 27 and end as 61 according to varidele
# This selection ignores the first 22 and the last 4 variables
# Not show the first 22 variables dropped by varidele
# A total of 39 variables left (65 - 22 - 4)
# Here, only 2 cores are used for saving time.
\donttest{
obsedele(data[c(1:200,3255:3454),c(1:4,27:61)],5,39,4,cores=2)
}
}
