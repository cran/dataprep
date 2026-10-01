#' Plot top and bottom percentiles
#' @param data A data frame.
#' @param cols Columns to plot.
#' @param group Grouping column.
#' @param diff Difference between quantile probabilities.
#' @param part \code{"both"}, \code{"bottom"}, or \code{"top"}.
#' @param ncol Facet columns.
#' @param num_xaxis Handling of numeric column names.
#' @param verbose Logical.
#' @return A \code{ggplot} object.
#' @export
percplot <- function(data, cols = NULL, group = NULL, diff = 0.1,
                     part = "both", ncol = NULL, num_xaxis = "auto",
                     verbose = FALSE) {
  t0 <- Sys.time()

  idx <- resolve_numeric_cols(data, cols)
  group_idx <- NULL
  if (!is.null(group)) {
    if (is.character(group)) {
      group_idx <- match(group, names(data))
    } else {
      group_idx <- as.integer(group)
    }
    if (is.na(group_idx) || group_idx < 1 || group_idx > ncol(data))
      stop("Invalid group column")
    group_col <- names(data)[group_idx]
  }

  df <- percdata(data, cols = idx, group = group_idx, diff = diff, part = part)

  percentile_levels <- levels(df$percentile)
  df$percentile <- as.character(df$percentile)

  group_levels <- NULL
  if (!is.null(group_idx)) {
    if (is.factor(df[[group_col]])) {
      group_levels <- levels(df[[group_col]])
    } else {
      group_levels <- unique(df[[group_col]])
    }
    df[[group_col]] <- as.character(df[[group_col]])
  }

  if (!is.null(group_idx)) {
    id_vars <- c(group_col, "percentile")
  } else {
    id_vars <- "percentile"
  }

  df_long <- melt(df, id.vars = id_vars, verbose = FALSE)

  df_long$percentile <- factor(df_long$percentile, levels = percentile_levels)
  if (!is.null(group_idx)) {
    df_long[[group_col]] <- factor(df_long[[group_col]], levels = group_levels)
  }

  meas_names <- names(data)[idx]
  is_numeric_names <- all(grepl("^[0-9.]+$", meas_names))

  x_log <- FALSE
  if (is_numeric_names) {
    num_names <- suppressWarnings(as.numeric(meas_names))
    if (num_xaxis == "auto" && all(is.finite(num_names)) && all(num_names > 0)) {
      d <- diff(log(num_names))
      cond1 <- length(unique(signif(d, 2))) < length(num_names) / 10
      data_range <- range(data[, idx, drop = FALSE], na.rm = TRUE)
      cond2 <- all(is.finite(data_range)) &&
               data_range[1] > 0 &&
               data_range[2] / data_range[1] >= 1000
      x_log <- cond1 && cond2
    } else if (identical(num_xaxis, "log")) {
      x_log <- TRUE
    }
  }

  y_log <- FALSE
  if (is_numeric_names && part %in% c("both", 2)) {
    y_log <- TRUE
  }

  pal <- c("grey", "red", "green", "pink", "gold", "forestgreen",
           "orange", "blue", "purple", "cyan", "brown", "black")
  n_levels <- nlevels(df_long$percentile)
  if (n_levels > length(pal)) {
    pal <- rep(pal, length.out = n_levels)
  } else {
    pal <- pal[1:n_levels]
  }

  # When the data are non-numeric AND part == "both" AND grouping is used,
  # build the "part" column here so it can be shared by the facet_grid call.
  use_part_facet <- (!is_numeric_names && part %in% c("both", 2) &&
                     !is.null(group_idx))
  if (use_part_facet) {
    n_perc <- nlevels(df_long$percentile)
    half <- floor(n_perc / 2)
    bottom_levels <- levels(df_long$percentile)[seq_len(half)]
    top_levels    <- levels(df_long$percentile)[setdiff(seq_len(n_perc),
                                                        seq_len(half))]
    df_long$part <- ifelse(df_long$percentile %in% bottom_levels,
                           "bottom", "top")
    df_long$part <- factor(df_long$part, levels = c("top", "bottom"))
  }

  # Per-group sample size and missing count are shown on the facet strip
  # as "group: n = .., na = ..". They are not drawn inside the panel.
  if (!is.null(group_idx)) {
    groups <- unique(data[[group_col]])
    n_vec  <- integer(length(groups))
    na_vec <- integer(length(groups))
    for (i in seq_along(groups)) {
      rows <- which(data[[group_col]] == groups[i])
      n_vec[i]  <- length(rows)
      na_vec[i] <- sum(is.na(data[rows, idx, drop = FALSE]))
    }
    grp_labels <- paste0(groups, ": n = ", n_vec, ", na = ", na_vec)
    names(grp_labels) <- as.character(groups)
    grp_labeller <- ggplot2::as_labeller(grp_labels)
  }

  if (is_numeric_names) {
    df_long$variable <- as.numeric(as.character(df_long$variable))
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = variable, y = value,
                                               color = percentile, group = percentile)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::labs(color = paste0("n:", nrow(data),
                                   "\nna:", sum(is.na(data[, idx, drop = FALSE])),
                                   "\n\nPercentiles")) +
      ggplot2::xlab("Variable")

    if (x_log) {
      p <- p + ggplot2::scale_x_log10(
        sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL))
    } else {
      p <- p + ggplot2::scale_x_continuous(
        sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL))
    }

    if (y_log) {
      p <- p + ggplot2::scale_y_log10(
        sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL))
    } else {
      p <- p + ggplot2::scale_y_continuous(
        sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL))
    }

    if (!is.null(group_idx)) {
      p <- p + ggplot2::facet_wrap(as.formula(paste("~", group_col)),
                                   ncol = ncol, scales = "free_y",
                                   labeller = grp_labeller)
    }

  } else {
    df_long$variable <- factor(df_long$variable, levels = meas_names)
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = variable, y = value,
                                               fill = percentile)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::scale_fill_manual(values = pal) +
      ggplot2::labs(fill = paste0("n:", nrow(data),
                                  "\nna:", sum(is.na(data[, idx, drop = FALSE])),
                                  "\n\nPercentiles")) +
      ggplot2::xlab("Variable")

    p <- p + ggplot2::scale_y_continuous(
      sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL))

    if (use_part_facet) {
      # Only override the column (group) labels; keep default top / bottom
      # labels for the "part" dimension.
      p <- p + ggplot2::facet_grid(
        as.formula(paste("part ~", group_col)),
        scales = "free_y",
        labeller = ggplot2::labeller(.cols = grp_labeller)
      )
    } else if (!is.null(group_idx)) {
      p <- p + ggplot2::facet_wrap(as.formula(paste("~", group_col)),
                                   ncol = ncol, labeller = grp_labeller)
    }
  }

  # Extra top margin so the legend's "n:... / na:..." block fits
  # inside the canvas at small fig.height values.
  p <- p + ggplot2::theme(
    plot.margin = ggplot2::margin(t = 15, r = 8, b = 8, l = 8, unit = "pt")
  )

  if (verbose) cat("Time used by percplot:", format(Sys.time() - t0, digits = 3), "\n")
  p
}
