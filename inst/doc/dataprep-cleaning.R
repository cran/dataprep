## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 5.5,
  out.width = "95%",
  fig.retina = 2
)

## -----------------------------------------------------------------------------
library(dataprep)
set.seed(1)

# The size-bin columns are the ones whose names are numeric
# (1.00, 1.12, ..., 1000). This helper returns their integer
# positions, excluding the four non-size columns (`date`,
# `tconc`, `TPNC`, `monthyear`).
size_bin_cols <- function(x) {
  grep("^[-+]?[0-9]*\\.?[0-9]+$", names(x))
}

## ----echo = FALSE, out.width = "70%"------------------------------------------
knitr::include_graphics("figures/fig1_pipeline.png")

## -----------------------------------------------------------------------------
cleaned <- dataprep(data,
                    cols       = size_bin_cols(data),
                    group      = 4,
                    interval   = 10,
                    times      = 10,
                    intervals  = 30,
                    cores      = 1L)
dim(cleaned)

## ----fig.height = 4.5, fig.width = 7------------------------------------------
percplot(
  rbind(
    transform(data[names(cleaned)],      g = "original"),
    transform(cleaned,                   g = "preprocessed")
  ),
  cols  = size_bin_cols(cleaned),
  group = ncol(cleaned) + 1
)

## -----------------------------------------------------------------------------
df <- data.frame(
  date  = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:19 * 600,
  group = rep(1L, 20),
  x     = c(1, NA, NA, NA, 5, NA, NA, NA, NA, NA,
            1, NA, NA, NA, 5, NA, NA, NA, NA, NA),
  y     = c(NA, 1, NA, NA, 2, NA, NA, NA, NA, NA,
            NA, 1, NA, NA, 2, NA, NA, NA, NA, NA)
)
nrow(df)
nrow(obsedele(df, cols = c("x", "y"), group = "group", half = 2, cores = 1L))

## -----------------------------------------------------------------------------
df_boundary <- data.frame(
  date = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:4 * 600,
  x    = c(1, NA, NA, NA, 5)   # anchors at 0 and 40 minutes
)
nrow(obsedele(df_boundary, cols = "x", half = 30, cores = 1L))

## ----echo = FALSE, out.width = "70%"------------------------------------------
knitr::include_graphics("figures/Outlier_Comparison.png")

## -----------------------------------------------------------------------------
data_slice <- data[3000:4000, ]

# Select the size-bin columns by name pattern: the ones whose
# names are numeric (1.00, 1.12, ... 1000). This excludes the
# four non-size columns (`date`, `tconc`, `TPNC`, `monthyear`),
# including `tconc` and `TPNC` which are numeric but not size
# bins.
num_cols_raw <- size_bin_cols(data_slice)

# Some size bins are entirely NA in this slice and must be dropped
# before any further step.
na_frac <- sapply(data_slice[, num_cols_raw], function(x) mean(is.na(x)))
table(na_frac == 1)

## -----------------------------------------------------------------------------
step0 <- varidele(data_slice,
                  cols     = num_cols_raw,
                  fraction = 0.5)
num_cols <- size_bin_cols(step0)
length(num_cols)          # number of bins that survived

## -----------------------------------------------------------------------------
step1 <- obsedele(step0, cols = num_cols, group = 4, cores = 1L)
nrow(step1)

## ----fig.height = 4, fig.width = 7.5------------------------------------------
percplot(step0, cols = num_cols, group = 4)

## -----------------------------------------------------------------------------
step2 <- condextr(step1, cols = num_cols, group = 4,
                  interval = 10, times = 10, cores = 1L)
nrow(step2)

## ----fig.height = 4, fig.width = 7.5------------------------------------------
percplot(step2, cols = num_cols, group = 4)

## -----------------------------------------------------------------------------
step3 <- shorvalu(step2, cols = num_cols, cores = 1L)
sum(is.na(step2[, num_cols])) - sum(is.na(step3[, num_cols]))

## -----------------------------------------------------------------------------
demo <- data[1:1000, ]
res  <- dataprep(
  demo,
  cols     = size_bin_cols(demo),
  group    = 4,
  interval = 5,
  times    = 3,
  half     = 30,
  cores    = 1L
)
dim(res)

## -----------------------------------------------------------------------------
report <- dry_run(
  data1,
  cols       = c("Nucleation", "Aitken", "Accumulation"),
  steps      = c("varidele", "obsedele", "outlier"),
  fraction   = 0.5
)
str(report, max.level = 2)

## -----------------------------------------------------------------------------
na_diagnose(data1, cols = 3:7)

## -----------------------------------------------------------------------------
demo <- data[1:500, ]
head(winsorize(demo, cols = "7.94")[["7.94"]])

## -----------------------------------------------------------------------------
df <- data.frame(
  id    = 1:100,
  const = rep(5, 100),
  noise = rnorm(100, sd = 0.005)
)
names(filter_low_var(df, cutoff = 0.001))

## ----echo = FALSE, out.width = "60%"------------------------------------------
knitr::include_graphics("figures/Time_Series_Interpolation_Final.png")

## -----------------------------------------------------------------------------
sessionInfo()

