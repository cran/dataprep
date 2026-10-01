#' Turn zeros to missing values
#' @param x A vector, matrix, or data frame.
#' @return An object of the same class with zeros replaced by \code{NA}.
#' @export
zerona <- function(x) {
  x[x == 0] <- NA
  x
}
