#' Plot descriptive statistics
#' @param data A data frame.
#' @param cols Columns to describe.
#' @param stats Statistics to plot.
#' @param first Name for the first column.
#' @param ncol Number of columns in facet layout.
#' @param num_xaxis Handling of numeric column names.
#' @param verbose Logical.
#' @return A \code{ggplot} object.
#' @export
descplot <- function(data, cols = NULL, stats = 1:9, first = "variables",
                     ncol = NULL, num_xaxis = "log", verbose = FALSE) {
  t0 <- Sys.time()

  df_desc <- descdata(data, cols = cols, stats = stats, first = first, verbose = FALSE)
  df_long <- melt(df_desc, id.vars = first, verbose = FALSE)

  orig_names <- if (is.null(cols)) names(data) else names(data)[cols]
  is_numeric_names <- all(grepl("^[0-9.]+$", orig_names))

  if (is_numeric_names) {
    if (num_xaxis == "log") {
      df_long[[first]] <- as.numeric(as.character(df_long[[first]]))
      p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data[[first]], y = value)) +
        ggplot2::geom_line() + ggplot2::scale_x_log10()
    } else if (num_xaxis == "numeric") {
      df_long[[first]] <- as.numeric(as.character(df_long[[first]]))
      p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data[[first]], y = value)) +
        ggplot2::geom_line()
    } else {
      df_long[[first]] <- factor(df_long[[first]], levels = orig_names)
      p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data[[first]], y = value)) +
        ggplot2::geom_col()
    }
  } else {
    df_long[[first]] <- factor(df_long[[first]], levels = orig_names)
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data[[first]], y = value)) +
      ggplot2::geom_col()
  }

  if (!is.null(ncol)) {
    p <- p + ggplot2::facet_wrap(~variable, ncol = ncol, scales = "free_y")
  } else {
    p <- p + ggplot2::facet_wrap(~variable, scales = "free_y")
  }

  if (verbose) cat("Time used by descplot:", format(Sys.time() - t0, digits = 3), "\n")
  p
}
