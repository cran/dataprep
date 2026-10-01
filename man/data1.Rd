\name{data1}
\alias{data1}
\docType{data}
\title{
Example data (aggregated particle number concentrations, SMEAR I
Varrio forest)
}
\description{
An already-aggregated seven-column version of \code{\link{data}}.
The 61 size bins of the raw size distribution have been collapsed
into three log-normal modes (nucleation, Aitken, accumulation) plus
two total concentrations (\code{tconc}, \code{TPNC}). The dataset
covers the same two-month window as \code{data} (January and July
2020) and is intended for read-only demos.
}
\usage{data1}
\format{
A data frame with 7,640 observations on the following 7 variables.
\describe{
  \item{\code{date}}{a POSIXct vector, sampling time (10-minute resolution).}
  \item{\code{monthyear}}{a character vector, e.g. \code{"January 2020"}.}
  \item{\code{Nucleation}}{a numeric vector, number concentration in the
        nucleation mode (nominally 3--25 nm).}
  \item{\code{Aitken}}{a numeric vector, number concentration in the
        Aitken mode (nominally 25--100 nm).}
  \item{\code{Accumulation}}{a numeric vector, number concentration in
        the accumulation mode (nominally 100--1000 nm).}
  \item{\code{tconc}}{a numeric vector, total number concentration.}
  \item{\code{TPNC}}{a numeric vector, total particle number
        concentration.}
}
}
\details{
Unlike \code{\link{data}}, which is the raw size-resolved table,
\code{data1} contains only mode-integrated values. The three
integrated values can remain non-zero even when the smallest raw size
bins (\code{1} through \code{2}) are entirely \code{NA} in the
two-month subset. It has:

\itemize{
  \item very few missing values,
  \item no long consecutive missing runs,
  \item no obvious outliers.
}

Because of this, \code{data1} is \strong{not} a useful input for the
cleaning pipeline (\code{\link{varidele}},
\code{\link{obsedele}}, \code{\link{condextr}},
\code{\link{shorvalu}}, \code{\link{dataprep}}): those steps have
nothing to remove on an already-clean table. It is intended for
demos that only read the data:

\itemize{
  \item \code{\link{descdata}} / \code{\link{descplot}} ---
        descriptive statistics and plots,
  \item \code{\link{percdata}} / \code{\link{percplot}} ---
        percentile summaries,
  \item \code{\link{na_diagnose}} --- missing-value diagnostics,
  \item \code{\link{data_report}} --- compact quality report.
}

Use \code{\link{data}} for anything that modifies values.
}
\source{
\url{https://smear.avaa.csc.fi/download}
}
\references{
Liang, C.-S., Wu, H., Li, H.-Y., Zhang, Q., Li, Z. & He, K.-B.
(2020). Efficient data preprocessing, episode classification, and
source apportionment of particle number concentrations.
\emph{Science of the Total Environment}, 741, 140923.
\doi{10.1016/j.scitotenv.2020.140923}
}
\examples{
data1

dim(data1)
names(data1)

# The three log-normal modes and the two total concentrations
summary(data1[, 3:7])

# A quick look at how the modes relate to the totals
cor(data1[, 3:7], use = "complete.obs")

# data1 has very few missing values compared with the raw size bins
colMeans(is.na(data1))

# For a read-only descriptive plot
if (interactive()) {
  descplot(data1, cols = 3:7)
}
}
\keyword{datasets}
