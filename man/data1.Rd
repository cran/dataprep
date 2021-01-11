\name{data1}
\alias{data1}
\docType{data}
\title{
Example data (data1, particle number concentrations in SMEAR I Varrio forest)
}
\description{
Calculated from the raw data that is downloaded from https://avaa.tdata.fi/web/smart/smear/download.
}
\usage{data1}
\format{
  A data frame with 7640 observations on the following 7 variables.
  \describe{
    \item{\code{date}}{a POSIXct}
    \item{\code{monthyear}}{a character vector}
    \item{\code{Nucleation}}{a numeric vector}
    \item{\code{Aitken}}{a numeric vector}
    \item{\code{Accumulation}}{a numeric vector}
    \item{\code{tconc}}{a numeric vector}
    \item{\code{TPNC}}{a numeric vector}
  }
}

\source{
https://avaa.tdata.fi/web/smart/smear/download
}
\references{
1. Example data (particle number concentrations in SMEAR I Varrio forest) is from https://avaa.tdata.fi/web/smart/smear/download
}
\examples{
data1
## maybe str(data1)
}
\keyword{datasets}
