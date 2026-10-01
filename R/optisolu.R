#' Find optimal interval and times for condextr
#' @param data A data frame.
#' @param cols Columns to process. If \code{NULL}, all numeric columns are used.
#' @param group Grouping column.
#' @param interval,times Maximum interval and times to test.
#' @param top,top.error,top.magnitude,bottom,bottom.error,bottom.magnitude
#'   Outlier thresholds.
#' @param by,half Time parameters.
#' @param date_col Time column.
#' @param cores Number of CPU cores.
#' @param verbose Logical.
#' @return A data frame with search results.
#' @export
optisolu <- function(data, cols = NULL, group = NULL,
                     interval = 35, times = 10,
                     top = .995, top.error = .1, top.magnitude = .2,
                     bottom = .0025, bottom.error = .2,
                     bottom.magnitude = .4,
                     by = "min", half = 30, date_col = NULL,
                     cores = NULL, verbose = FALSE) {
  t0 <- Sys.time()
  idx <- resolve_numeric_cols(data, cols)
  if (length(idx) < 1) stop("No numeric columns selected")

  all_na <- vapply(idx, function(j) all(is.na(data[[j]])), logical(1))
  if (any(all_na)) {
    warning("The following selected columns are entirely NA and will be ",
            "excluded: ", paste(names(data)[idx][all_na], collapse = ", "),
            ". Consider using varidele to remove them beforehand.")
    idx <- idx[!all_na]
    if (length(idx) == 0)
      stop("No valid columns remain after removing all-NA columns.")
  }

  date_info <- resolve_date_col(data, date_col)
  date_name <- date_info$name

  cases   <- expand.grid(interval = 1:interval, times = 1:times)
  n_cases <- nrow(cases)

  hw <- parallel::detectCores()
  if (is.na(hw) || hw < 1) hw <- 1L
  outer_cores <- min(n_cases, hw)
  if (!is.null(cores)) outer_cores <- min(outer_cores, as.integer(cores))
  if (outer_cores < 1L) outer_cores <- 1L
  inner_cores <- 1L

  run_case <- function(i) {
    inte <- cases$interval[i]
    tim  <- cases$times[i]
    a <- condextr(data, cols = idx, group = group,
                  interval = inte, times = tim,
                  top = top, top.error = top.error, top.magnitude = top.magnitude,
                  bottom = bottom, bottom.error = bottom.error,
                  bottom.magnitude = bottom.magnitude,
                  by = by, half = half, date_col = date_col,
                  cores = inner_cores, verbose = FALSE)
    sdr <- 1 - nrow(a) / nrow(data)
    orr <- (sum(is.na(a[idx])) -
              sum(is.na(data[data[[date_name]] %in% a[[date_name]], idx]))) /
           (nrow(data) * length(idx))
    snr <- mean(vapply(idx, function(j)
      mean(a[[j]], na.rm = TRUE) / sd(a[[j]], na.rm = TRUE), numeric(1)))
    data.frame(case = i, interval = inte, times = tim,
               sdr = sdr, orr = orr, snr = snr)
  }

  if (.Platform$OS.type == "unix") {
    dflist <- parallel::mclapply(seq_len(n_cases), run_case,
                                 mc.cores = outer_cores,
                                 mc.preschedule = TRUE)
  } else {
    cl <- parallel::makeCluster(outer_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Load the package on each worker so that every internal helper
    # (resolve_numeric_cols, to_numeric_matrix, check_numeric_cols,
    # condextr, obsedele_cpp, ...) is available without an explicit
    # export list. This replaces a fragile manual clusterExport that
    # was missing check_numeric_cols in 0.1.5.
    parallel::clusterEvalQ(cl, library(dataprep))
    parallel::clusterExport(
      cl,
      c("data", "idx", "cases", "run_case", "inner_cores",
        "group", "top", "top.error", "top.magnitude",
        "bottom", "bottom.error", "bottom.magnitude",
        "by", "half", "date_col", "date_name"),
      envir = environment()
    )
    dflist <- parallel::parLapply(cl, seq_len(n_cases), run_case)
  }

  c <- do.call(rbind, dflist)
  c <- transform(c, index = (exp(snr) - 2) * 10 / exp(sdr + orr))
  c <- transform(c, relaindex = index / log(interval * times + 1))

  d <- percoutl(data, cols = idx, group = group,
                top = top, bottom = bottom,
                by = by, half = half, date_col = date_col,
                verbose = FALSE)
  e <- data.frame(
    sdr = 1 - nrow(d) / nrow(data),
    orr = (sum(is.na(d[idx])) -
             sum(is.na(data[data[[date_name]] %in% d[[date_name]], idx]))) /
          (nrow(data) * length(idx)),
    snr = mean(vapply(idx, function(j)
      mean(d[[j]], na.rm = TRUE) / sd(d[[j]], na.rm = TRUE), numeric(1)))
  )
  c <- transform(c, optimal =
                   ifelse(sdr < e$sdr & orr < e$orr & snr > e$snr, TRUE, FALSE))

  if (verbose)
    cat("Time used by optisolu:",
        format(Sys.time() - t0, digits = 3), "\n")
  c
}