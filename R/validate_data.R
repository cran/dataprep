#' Validate data against rules
#' @param data A data frame.
#' @param rules List of rules.
#' @param verbose Logical.
#' @return A data frame with rule results.
#' @export
validate_data <- function(data, rules, verbose = FALSE) {
  t0 <- Sys.time()
  if (!is.data.frame(data)) stop("data must be a data frame")
  results <- list()
  for (i in seq_along(rules)) {
    rule <- rules[[i]]
    col_name <- rule$column
    if (!col_name %in% names(data)) {
      results[[i]] <- data.frame(
        rule = i, column = col_name, passed = FALSE,
        message = paste0("Column does not exist: ", col_name), stringsAsFactors = FALSE
      )
      next
    }
    x <- data[[col_name]]
    msgs <- character()
    if (!is.null(rule$type)) {
      if (rule$type == "numeric" && !is.numeric(x)) msgs <- c(msgs, "Type is not numeric")
      if (rule$type == "integer" && !is.integer(x)) msgs <- c(msgs, "Type is not integer")
      if (rule$type == "character" && !is.character(x)) msgs <- c(msgs, "Type is not character")
      if (rule$type == "factor" && !is.factor(x)) msgs <- c(msgs, "Type is not factor")
    }
    if (!is.null(rule$min) && is.numeric(x)) {
      if (any(x < rule$min, na.rm = TRUE)) msgs <- c(msgs, paste0("Values below ", rule$min))
    }
    if (!is.null(rule$max) && is.numeric(x)) {
      if (any(x > rule$max, na.rm = TRUE)) msgs <- c(msgs, paste0("Values above ", rule$max))
    }
    if (!is.null(rule$unique) && rule$unique) {
      if (anyDuplicated(x[!is.na(x)]) > 0) msgs <- c(msgs, "Duplicate values present")
    }
    if (!is.null(rule$na_allowed) && !rule$na_allowed) {
      if (anyNA(x)) msgs <- c(msgs, "Missing values present")
    }
    passed <- length(msgs) == 0
    results[[i]] <- data.frame(
      rule = i, column = col_name, passed = passed,
      message = if (passed) "OK" else paste(msgs, collapse = "; "),
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, results)
  if (verbose) cat("Validation completed.\n")
  result
}
