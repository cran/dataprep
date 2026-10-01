test_that("varidele removes columns above fraction threshold", {
  df <- data.frame(
    a = c(1, 2, NA, NA, NA),   # 60% NA
    b = c(1, 2, 3, 4, 5)       # 0% NA
  )
  res <- varidele(df, cols = 1:2, fraction = 0.5, verbose = FALSE)
  expect_equal(ncol(res), 1L)
  expect_equal(names(res), "b")
})

test_that("varidele keeps all columns below threshold", {
  df <- data.frame(
    a = c(1, 2, 3, NA, 5),     # 20% NA
    b = c(1, 2, 3, 4, 5)       # 0% NA
  )
  res <- varidele(df, cols = 1:2, fraction = 0.5, verbose = FALSE)
  expect_equal(ncol(res), 2L)
})

test_that("obsedele boundary at exactly half is inclusive", {
  # Five observations 10 minutes apart, valid values only at both ends.
  # The middle row is exactly 30 minutes from both anchors. With
  # half = 30 the comparison `dl <= half` is inclusive, so the row is
  # retained and all five rows survive.
  df <- data.frame(
    date = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:4 * 600,
    x    = c(1, NA, NA, NA, 5)
  )
  res <- obsedele(df, cols = "x", half = 30, verbose = FALSE)
  expect_equal(nrow(res), 5L)
})

test_that("obsedele deletes rows beyond half on both sides", {
  # 60-minute spacing, half = 30 minutes.
  #
  # Without a `group`, obsedele splits the timeline into "periods"
  # at any gap larger than `threshold_sec = half * 60 = 1800s`. With
  # a 3600s gap between every pair of adjacent observations, each
  # observation becomes its own period, and no NA is ever inside a
  # period with an anchor.
  #
  # Providing a `group` forces the whole subset to be treated as a
  # single sequence. The eight interior NA rows are then each more
  # than 30 minutes from both anchors and are deleted.
  df <- data.frame(
    date  = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:9 * 3600,
    group = rep(1L, 10),
    x     = c(1, NA, NA, NA, NA, NA, NA, NA, NA, 5)
  )
  res <- obsedele(df, cols = "x", group = "group", half = 30,
                  verbose = FALSE)
  expect_true(nrow(res) < 10L)
  expect_equal(nrow(res), 2L)
})

test_that("obsedele scans each column independently", {
  # x has anchors at the two ends only; y has a single anchor in
  # the middle. A row is deleted when *any* column has a missing run
  # longer than half = 30 minutes on both sides.
  #
  # Deleted by x: index 5 (dl=40, dr=50) and index 6 (dl=50, dr=40).
  # Deleted by y: index 1 (Inf, 40), index 9 (40, Inf), index 10
  #               (50, Inf).
  # Union of deletions: {1, 5, 6, 9, 10} -> 5 rows retained.
  df <- data.frame(
    date = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:9 * 600,
    x    = c(1, NA, NA, NA, NA, NA, NA, NA, NA, 5),
    y    = c(NA, NA, NA, NA, 2, NA, NA, NA, NA, NA)
  )
  res <- obsedele(df, cols = c("x", "y"), half = 30, verbose = FALSE)
  expect_true(nrow(res) < 10L)
  expect_equal(nrow(res), 5L)
})

test_that("condextr runs without error and preserves schema", {
  set.seed(42)
  df <- data.frame(
    date  = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:99 * 600,
    group = rep(1L, 100),
    x     = c(rnorm(99), 100)   # one clear outlier
  )
  res <- condextr(df, cols = "x", group = "group",
                  interval = 2, times = 2, verbose = FALSE)
  expect_s3_class(res, "data.frame")
  expect_true("x" %in% names(res))
  expect_true(nrow(res) <= 100L)
})

test_that("shorvalu interpolates short gaps", {
  df <- data.frame(
    date = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:9 * 600,
    x    = c(1, NA, 3, NA, 5, 6, 7, 8, 9, 10)
  )
  res <- shorvalu(df, cols = "x", intervals = 30, verbose = FALSE)
  expect_false(anyNA(res$x))
})

test_that("shorvalu leaves long gaps as NA", {
  # `shorvalu` splits the series into segments at points where the
  # time gap between two *adjacent observations* exceeds `intervals`.
  # Here every adjacent observation is 60 minutes apart (> 30), so
  # each point forms its own segment and the interior NAs cannot be
  # interpolated.
  df <- data.frame(
    date = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:14 * 3600,
    x    = c(1, rep(NA, 7), 9, 10, 11, 12, 13, 14, 15)
  )
  res <- shorvalu(df, cols = "x", intervals = 30, verbose = FALSE)
  expect_true(anyNA(res$x))
})

test_that("dataprep one-call pipeline runs on small data", {
  set.seed(42)
  df <- data.frame(
    date  = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:499 * 600,
    group = rep(1L, 500),
    x     = rnorm(500),
    y     = rnorm(500)
  )
  res <- dataprep(df, cols = c("x", "y"), group = "group",
                  interval = 2, times = 2, verbose = FALSE)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("x", "y") %in% names(res)))
})