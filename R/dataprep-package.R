#' @keywords package
#' @details
#' The package is organised around a four-step cleaning pipeline:
#'
#' \enumerate{
#'   \item \strong{Variable deletion} ([varidele()]) -- drop columns
#'         whose missing fraction exceeds a threshold.
#'   \item \strong{Observation deletion} ([obsedele()]) -- drop rows
#'         with a consecutive missing run longer than `half` minutes
#'         on both sides.
#'   \item \strong{Outlier removal} ([condextr()]) --
#'         point-by-point weighted conditional extremum detection.
#'   \item \strong{Short-period interpolation} ([shorvalu()]) --
#'         fill remaining short gaps from nearby valid anchors.
#' }
#'
#' Steps 1-4 are wrapped by [dataprep()] for one-call use. The design
#' reasoning behind the ordering is documented in
#' `vignette("dataprep-philosophy")`.
#'
#' The package also provides fast wide-to-long and long-to-wide
#' reshaping ([melt()], [dcast()]), a full set of descriptive and
#' diagnostic helpers ([descdata()], [descplot()], [percdata()],
#' [percplot()], [na_diagnose()], [data_report()]), and
#' fit/transform-style preprocessing plans that prevent data leakage
#' ([prep_fit()], [prep_transform()]).
#'
#' This work was supported by the National Natural Science Foundation
#' of China (No. 12301674).
#'
#' @seealso
#' Core pipeline: [dataprep()], [varidele()], [obsedele()],
#'   [condextr()], [shorvalu()].
#'
#' Reshaping: [melt()], [dcast()].
#'
#' Leakage-free workflow: [prep_fit()], [prep_transform()].
#'
#' Diagnostics and reporting: [descdata()], [descplot()],
#'   [percdata()], [percplot()], [na_diagnose()], [data_report()].
#'
#' @importFrom Rcpp evalCpp
#' @importFrom stats IQR aggregate mad median
#' @importFrom stats quantile sd as.formula
#' @importFrom parallel detectCores makeCluster stopCluster
#' @useDynLib dataprep, .registration = TRUE
"_PACKAGE"

utils::globalVariables(c(
  ".data", "value", "variable", "percentile", "label",
  "snr", "sdr", "orr", "index"
))
