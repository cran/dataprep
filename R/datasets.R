#' Example data (particle number concentrations in SMEAR I Varrio forest)
#'
#' The raw data is downloaded from \url{https://smear.avaa.csc.fi/download}.
#'
#' @name data
#' @docType data
#' @format A data frame with 7640 observations on the following 65 variables:
#' \describe{
#'   \item{date}{a POSIXct vector}
#'   \item{tconc}{a numeric vector}
#'   \item{TPNC}{a numeric vector}
#'   \item{monthyear}{a character vector}
#'   \item{...}{61 numeric columns named by particle diameter in nm}
#' }
#' @source \url{https://smear.avaa.csc.fi/download}
#' @keywords datasets
NULL

#' Example data (data1)
#'
#' Derived from \code{data}. Contains three aggregated particle modes and
#' two total concentrations.
#'
#' @name data1
#' @docType data
#' @format A data frame with 7640 observations on the following 7 variables:
#' \describe{
#'   \item{date}{a POSIXct vector}
#'   \item{monthyear}{a character vector}
#'   \item{Nucleation}{a numeric vector}
#'   \item{Aitken}{a numeric vector}
#'   \item{Accumulation}{a numeric vector}
#'   \item{tconc}{a numeric vector}
#'   \item{TPNC}{a numeric vector}
#' }
#' @source \url{https://smear.avaa.csc.fi/download}
#' @keywords datasets
NULL