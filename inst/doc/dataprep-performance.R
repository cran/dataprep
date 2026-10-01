## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 5.5,
  out.width = "75%",
  fig.retina = 2
)

## -----------------------------------------------------------------------------
library(dataprep)

## ----mean-vs-median, fig.width = 7, fig.height = 4.2, fig.retina = 2----------
suppressPackageStartupMessages(library(ggplot2))

read_bench <- function(fname, op) {
  p <- system.file("extdata", fname, package = "dataprep")
  d <- read.csv(p, stringsAsFactors = FALSE)
  d <- d[!d$skipped, c("tool", "mean", "median")]
  d$op <- op
  d
}

bench <- rbind(
  read_bench("bench_melt_ubuntu.csv",  "melt (Ubuntu)"),
  read_bench("bench_dcast_ubuntu.csv", "dcast (Ubuntu)"),
  read_bench("bench_melt_win.csv",     "melt (Windows)"),
  read_bench("bench_dcast_win.csv",    "dcast (Windows)")
)

ggplot(bench, aes(median, mean)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.45, size = 1.4) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ tool, ncol = 4) +
  labs(x = "median (ms, log scale)",
       y = "mean (ms, log scale)") +
  theme_bw(base_size = 10)

## ----eval = FALSE-------------------------------------------------------------
# Sys.setenv(DATAPREP_RUN_BENCHMARK = "1")
# source(system.file("benchmark_melt_dcast.R", package = "dataprep"))

## -----------------------------------------------------------------------------
sessionInfo()

