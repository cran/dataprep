\name{condextr}
\alias{condextr}
\title{
Remove outliers using point-by-point weighed outlier removal by conditional extremum
}
\description{
Care is needed when dealing with outliers that are common real-life phenomena besides missing values in data. Unfortunately, many non-outliers may be removed by a one-for-all threshold method, which will be largely avoided if a one-by-one considered way is developed and applied. The \code{condextr} proposed here considers every value (point) that will potentially be removed, combining constraint conditions and extremum (maximum and minimum). Therefore, it is a function of point-by-point weighed outlier removal by conditional extremum. Observation deletion is combined in the process of outlier removal since large gaps consisted of excessive missing values may be formed in time series after removing certain outliers.
}
\usage{
condextr(data, start = NULL, end = NULL, group = NULL, top = 0.995,
top.error = 0.1, top.magnitude = 0.2, bottom = 0.0025, bottom.error = 0.2,
bottom.magnitude = 0.4, interval = 10, by = "min", half = 30,
times = 10, cores = NULL)
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
  \item{top.error}{
  The top allowable error coefficient is 0.1 by default.
}
  \item{top.magnitude}{
  The order of magnitude coefficient of the top error is 0.2 by default.
}
  \item{bottom}{
  The bottom percentile is 0.0025 by default.
}
  \item{bottom.error}{
  The bottom allowable error coefficient is 0.2 by default.
}
  \item{bottom.magnitude}{
  The order of magnitude coefficient of the bottom error is 0.4 by default.
}
  \item{interval}{
  The interval of observation deletion, i.e. the number of outlier deletions before each observation deletion, is 10 by default.
}
  \item{by}{
  The time extension unit by is a minute ("min") by default. The user can specify other time units. For example, "5 min" means that the time extension unit is 5 minutes.
}
  \item{half}{
  Half window size of hourly moving average. It is 30 (minutes) by default, which is determined by the time expansion unit minute ("min").
}
  \item{times}{
  The number of observation deletions in outlier removal is 10 by default.
}
  \item{cores}{
  The number of CPU cores.
}
}
\details{
A point-by-point constraint (consideration) outlier removal method based on conditional extremum is proposed, which is more advantageous than the traditional "one size fits all" percentile deletion method in deleting outliers. Moreover, it emphasizes that the outlier removal should be grouped if there are groups such as month because of the value difference among different groups.
}
\value{
A data frame after removing outliers.
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download

2. Wickham, H., Francois, R., Henry, L. & Muller, K. 2017. dplyr: A Grammar of Data Manipulation. 0.7.4 ed. http://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr.

3. Wickham, H., Francois, R., Henry, L. & Muller, K. 2019. dplyr: A Grammar of Data Manipulation. R package version 0.8.3. https://CRAN.R-project.org/package=dplyr.

4. Wickham, H. & Henry, L. 2019. tidyr: Tidy Messy Data. R package version 1.0.0. https://CRAN.R-project.org/package=tidyr.

5. Ushey, K. 2018. RcppRoll: Efficient Rolling / Windowed Operations. R package version 0.3.0. https://CRAN.R-project.org/package=RcppRoll.
}
\author{
Chun-Sheng Liang <lcs14@mails.tsinghua.edu.cn; liangchunsheng@lzu.edu.cn>
}

\examples{
# Remove outliers by condextr after deleting observations by obsedele
# 530 observations will be deleted in obsedele(data[,c(1:4,27:61)],5,39,4).
# Further, 438 observations will be deleted in condextr by obsedele
# Here, for executing time reason, a smaller example is used to show.
# Besides, only 2 cores are used for submission test.
condextr(obsedele(data[1:500,c(1,4,17:19)],3,5,2,cores=2),3,5,2,cores=2)
}

