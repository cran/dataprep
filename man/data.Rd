\name{data}
\alias{data}
\docType{data}
\title{
Example data (particle number concentrations in SMEAR I Varrio forest)
}
\description{
Particle number size distribution (PNSD) and auxiliary variables
measured at the SMEAR I Varrio forest station in Finland. The raw
data were downloaded from \url{https://smear.avaa.csc.fi/download}
and subset to two months (January and July 2020) for package size
reasons.
}
\usage{data}
\format{
A data frame with 7,640 observations on the following 65 variables.
\describe{
  \item{\code{date}}{a POSIXct vector, sampling time (10-minute resolution).}
  \item{\code{tconc}}{a numeric vector, total number concentration.}
  \item{\code{TPNC}}{a numeric vector, total particle number concentration.}
  \item{\code{monthyear}}{a character vector, e.g. \code{"January 2020"}.}
  \item{\code{1}}{a numeric vector, particle diameter in nm (1 nm).}
  \item{\code{1.12}}{a numeric vector, particle diameter in nm.}
  \item{\code{1.26}}{a numeric vector, particle diameter in nm.}
  \item{\code{1.41}}{a numeric vector, particle diameter in nm.}
  \item{\code{1.58}}{a numeric vector, particle diameter in nm.}
  \item{\code{1.78}}{a numeric vector, particle diameter in nm.}
  \item{\code{2}}{a numeric vector, particle diameter in nm.}
  \item{\code{2.24}}{a numeric vector, particle diameter in nm.}
  \item{\code{2.51}}{a numeric vector, particle diameter in nm.}
  \item{\code{2.82}}{a numeric vector, particle diameter in nm.}
  \item{\code{3.16}}{a numeric vector, particle diameter in nm.}
  \item{\code{3.55}}{a numeric vector, particle diameter in nm.}
  \item{\code{3.98}}{a numeric vector, particle diameter in nm.}
  \item{\code{4.47}}{a numeric vector, particle diameter in nm.}
  \item{\code{5.01}}{a numeric vector, particle diameter in nm.}
  \item{\code{5.62}}{a numeric vector, particle diameter in nm.}
  \item{\code{6.31}}{a numeric vector, particle diameter in nm.}
  \item{\code{7.08}}{a numeric vector, particle diameter in nm.}
  \item{\code{7.94}}{a numeric vector, particle diameter in nm.}
  \item{\code{8.91}}{a numeric vector, particle diameter in nm.}
  \item{\code{10}}{a numeric vector, particle diameter in nm.}
  \item{\code{11.2}}{a numeric vector, particle diameter in nm.}
  \item{\code{12.6}}{a numeric vector, particle diameter in nm.}
  \item{\code{14.1}}{a numeric vector, particle diameter in nm.}
  \item{\code{15.8}}{a numeric vector, particle diameter in nm.}
  \item{\code{17.8}}{a numeric vector, particle diameter in nm.}
  \item{\code{20}}{a numeric vector, particle diameter in nm.}
  \item{\code{22.4}}{a numeric vector, particle diameter in nm.}
  \item{\code{25.1}}{a numeric vector, particle diameter in nm.}
  \item{\code{28.2}}{a numeric vector, particle diameter in nm.}
  \item{\code{31.6}}{a numeric vector, particle diameter in nm.}
  \item{\code{35.5}}{a numeric vector, particle diameter in nm.}
  \item{\code{39.8}}{a numeric vector, particle diameter in nm.}
  \item{\code{44.7}}{a numeric vector, particle diameter in nm.}
  \item{\code{50.1}}{a numeric vector, particle diameter in nm.}
  \item{\code{56.2}}{a numeric vector, particle diameter in nm.}
  \item{\code{63.1}}{a numeric vector, particle diameter in nm.}
  \item{\code{70.8}}{a numeric vector, particle diameter in nm.}
  \item{\code{79.4}}{a numeric vector, particle diameter in nm.}
  \item{\code{89.1}}{a numeric vector, particle diameter in nm.}
  \item{\code{100}}{a numeric vector, particle diameter in nm.}
  \item{\code{112}}{a numeric vector, particle diameter in nm.}
  \item{\code{126}}{a numeric vector, particle diameter in nm.}
  \item{\code{141}}{a numeric vector, particle diameter in nm.}
  \item{\code{158}}{a numeric vector, particle diameter in nm.}
  \item{\code{178}}{a numeric vector, particle diameter in nm.}
  \item{\code{200}}{a numeric vector, particle diameter in nm.}
  \item{\code{224}}{a numeric vector, particle diameter in nm.}
  \item{\code{251}}{a numeric vector, particle diameter in nm.}
  \item{\code{282}}{a numeric vector, particle diameter in nm.}
  \item{\code{316}}{a numeric vector, particle diameter in nm.}
  \item{\code{355}}{a numeric vector, particle diameter in nm.}
  \item{\code{398}}{a numeric vector, particle diameter in nm.}
  \item{\code{447}}{a numeric vector, particle diameter in nm.}
  \item{\code{501}}{a numeric vector, particle diameter in nm.}
  \item{\code{562}}{a numeric vector, particle diameter in nm.}
  \item{\code{631}}{a numeric vector, particle diameter in nm.}
  \item{\code{708}}{a numeric vector, particle diameter in nm.}
  \item{\code{794}}{a numeric vector, particle diameter in nm.}
  \item{\code{891}}{a numeric vector, particle diameter in nm.}
  \item{\code{1000}}{a numeric vector, particle diameter in nm.}
}
}
\details{
The 61 numeric channels between \code{1} and \code{1000} are size
bins of the differential particle number size distribution,
approximately logarithmically spaced. Column names are
the geometric mean diameter in nanometres, written in the shortest
form that R accepts as a name (so \code{1} rather than \code{1.00},
and \code{10} rather than \code{10.0}). Values are number
concentrations in cm\eqn{^{-3}}. \code{tconc} and \code{TPNC} are
integrals of the size distribution; \code{monthyear} is the month
label used for group-wise cleaning.

The smallest seven size bins (\code{1} through \code{2}) and the
largest three (\code{794}, \code{891}, \code{1000}) are entirely
\code{NA} in this two-month subset, so \code{varidele()} will drop
them even at a permissive \code{fraction}.

The data are the raw input for the cleaning pipeline described in
\code{vignette("dataprep-cleaning")}. They contain:

\itemize{
  \item size bins with substantial missing fractions
        (especially the smallest and largest bins),
  \item observations with long consecutive missing runs,
  \item extreme values that are not always genuine outliers.
}

After running \code{\link{dataprep}} on this table, a smaller,
cleaner table is returned. The \code{\link{data1}} dataset is the
already-aggregated seven-column version; it has no long gaps and
no obvious outliers, and is not a useful input for the cleaning
pipeline.
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
data

dim(data)
names(data)[1:6]

# Per-column missing fraction, ordered by severity.
# The smallest and largest size bins have the most missing data.
na_frac <- sort(colMeans(is.na(data)), decreasing = TRUE)
head(na_frac, 10)

# The 61 size bins run from column 5 to column 65.
size_bins <- 5:65
summary(data[, size_bins[1:3]])
}
\keyword{datasets}
