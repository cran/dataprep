test_that("melt basic functionality works", {
  df <- data.frame(
    id = 1:3,
    a  = c(1, 2, 3),
    b  = c(4, 5, 6)
  )
  long <- melt(df, id.vars = "id", verbose = FALSE)

  expect_s3_class(long, "data.frame")
  expect_equal(nrow(long), 6L)
  expect_equal(ncol(long), 3L)
  expect_true(all(c("id", "variable", "value") %in% names(long)))
  expect_setequal(unique(long$variable), c("a", "b"))
})

test_that("melt na.rm drops NA rows", {
  df <- data.frame(
    id = 1:3,
    x  = c(1, NA, 3),
    y  = c(4, 5, NA)
  )
  long_keep <- melt(df, id.vars = "id", na.rm = FALSE, verbose = FALSE)
  long_rm   <- melt(df, id.vars = "id", na.rm = TRUE,  verbose = FALSE)

  expect_equal(nrow(long_keep), 6L)
  expect_equal(nrow(long_rm),   4L)
  expect_false(anyNA(long_rm$value))
})

test_that("melt major = 'col' matches major = 'row' in content", {
  set.seed(1)
  df <- data.frame(id = 1:20, a = rnorm(20), b = rnorm(20), c = rnorm(20))

  m_row <- melt(df, id.vars = "id", major = "row", verbose = FALSE)
  m_col <- melt(df, id.vars = "id", major = "col", verbose = FALSE)

  # Default `as.factor = NULL`: factor for "col", character for "row".
  expect_s3_class(m_col$variable, "factor")
  expect_type   (m_row$variable, "character")

  m_row <- m_row[order(m_row$id, m_row$variable), ]
  m_col <- m_col[order(m_col$id, m_col$variable), ]
  rownames(m_row) <- NULL
  rownames(m_col) <- NULL

  expect_equal(m_row$id, m_col$id)
  expect_equal(as.character(m_row$variable),
               as.character(m_col$variable))
  expect_equal(m_row$value, m_col$value, tolerance = 1e-12)
})

test_that("dcast basic functionality works", {
  long <- data.frame(
    id       = rep(1:3, each = 2),
    variable = rep(c("x", "y"), 3),
    value    = c(1, 2, 3, 4, 5, 6)
  )
  wide <- dcast(long, id = "id", variable = "variable", value = "value",
                verbose = FALSE)

  expect_s3_class(wide, "data.frame")
  expect_equal(nrow(wide), 3L)
  expect_true(all(c("id", "x", "y") %in% names(wide)))
})

test_that("dcast formula interface works", {
  long <- data.frame(
    id       = rep(1:3, each = 2),
    variable = rep(c("x", "y"), 3),
    value    = c(1, 2, 3, 4, 5, 6)
  )
  wide <- dcast(long, formula = id ~ variable, value.var = "value",
                verbose = FALSE)
  expect_true(all(c("id", "x", "y") %in% names(wide)))
})

test_that("melt-dcast round trip preserves data", {
  set.seed(42)
  wide_in <- data.frame(
    id = 1:5,
    a  = rnorm(5),
    b  = rnorm(5),
    c  = rnorm(5)
  )
  long     <- melt(wide_in, id.vars = "id", verbose = FALSE)
  wide_out <- dcast(long, id = "id",
                    variable = "variable", value = "value",
                    verbose = FALSE)

  wide_out <- wide_out[order(wide_out$id), ]
  rownames(wide_out) <- NULL
  wide_in_sorted <- wide_in[order(wide_in$id), c("id", "a", "b", "c")]
  rownames(wide_in_sorted) <- NULL

  expect_equal(wide_out$id, wide_in_sorted$id)
  expect_equal(wide_out$a,  wide_in_sorted$a, tolerance = 1e-12)
  expect_equal(wide_out$b,  wide_in_sorted$b, tolerance = 1e-12)
  expect_equal(wide_out$c,  wide_in_sorted$c, tolerance = 1e-12)
})

test_that("dcast fill argument replaces missing cells", {
  # `fill` only fills *missing* (id, variable) combinations -- i.e.
  # combinations that do not appear in the input at all. Cells that
  # appear in the input with value NA are left as NA.
  long <- data.frame(
    id       = c(1, 1, 2),          # (2, "y") is missing
    variable = c("x", "y", "x"),
    value    = c(1, 2, 3)
  )
  wide <- dcast(long, id = "id",
                variable = "variable", value = "value",
                fill = 0, verbose = FALSE)

  expect_equal(wide$y[wide$id == 2], 0)   # missing combination -> fill
  expect_equal(wide$y[wide$id == 1], 2)   # present value -> kept
})

test_that("dcast fun.aggregate reduces duplicate pairs", {
  long_dup <- data.frame(
    id       = c(1, 1, 2),
    variable = "x",
    value    = c(1, 2, 3)
  )
  wide <- dcast(long_dup, id = "id",
                variable = "variable", value = "value",
                fun.aggregate = mean, verbose = FALSE)

  # id=1 has values 1 and 2 -> mean = 1.5
  expect_equal(wide$x[wide$id == 1], 1.5)
})

test_that("dcast na.rm skips NA cells", {
  long_na <- data.frame(
    id       = c(1, 1, 2, 2),
    variable = c("x", "y", "x", "y"),
    value    = c(1, NA, 3, 4)
  )
  wide <- dcast(long_na, id = "id",
                variable = "variable", value = "value",
                na.rm = TRUE, verbose = FALSE)

  # (1, "y") exists with value NA; na.rm skips the scatter so the cell
  # keeps its default fill (NA).
  expect_true(is.na(wide$y[wide$id == 1]))
})

test_that("melt matches reshape2 when available", {
  skip_if_not_installed("reshape2")

  set.seed(1)
  df <- data.frame(id = 1:10, a = rnorm(10), b = rnorm(10))

  m1 <- melt(df, id.vars = "id", verbose = FALSE)
  m2 <- reshape2::melt(df, id.vars = "id")

  m1 <- m1[order(m1$id, m1$variable), ]
  m2 <- m2[order(m2$id, m2$variable), ]
  rownames(m1) <- NULL
  rownames(m2) <- NULL

  expect_equal(m1$value, m2$value, tolerance = 1e-12)
})

test_that("dcast matches reshape2 when available", {
  skip_if_not_installed("reshape2")

  set.seed(1)
  wide_in <- data.frame(id = 1:10, a = rnorm(10), b = rnorm(10))
  long    <- reshape2::melt(wide_in, id.vars = "id")

  w1 <- dcast(long, id = "id", variable = "variable", value = "value",
              verbose = FALSE)
  w2 <- reshape2::dcast(long, id ~ variable, value.var = "value")

  w1 <- w1[order(w1$id), ]
  w2 <- w2[order(w2$id), ]
  rownames(w1) <- NULL
  rownames(w2) <- NULL

  expect_equal(w1$a, w2$a, tolerance = 1e-12)
  expect_equal(w1$b, w2$b, tolerance = 1e-12)
})
# ---- regression: dcast na.rm + explicit non-NA fill (block-path) ----
test_that("dcast na.rm keeps fill value on block path", {
  long5 <- data.frame(
    id       = c(1, 1, 2, 2),
    variable = c("x", "y", "x", "y"),
    value    = c(1, NA, 3, 4)
  )
  r <- dcast(long5, id = "id", variable = "variable", value = "value",
             na.rm = TRUE, fill = -1)
  expect_equal(r[r$id == 1, "y"], -1)
  expect_equal(r[r$id == 1, "x"],  1)
  expect_equal(r[r$id == 2, "x"],  3)
  expect_equal(r[r$id == 2, "y"],  4)

  r0 <- dcast(long5, id = "id", variable = "variable", value = "value",
              na.rm = FALSE, fill = -1)
  expect_true(is.na(r0[r0$id == 1, "y"]))

  rN <- dcast(long5, id = "id", variable = "variable", value = "value",
              na.rm = TRUE)                      # default fill = NA_real_
  expect_true(is.na(rN[rN$id == 1, "y"]))
})

test_that("dcast duplicate (id, variable) is last-wins on both paths", {
  # block-path candidate: period-aligned, id constant, var unique in period
  dup <- data.frame(
    id       = c(1, 1, 2, 2,  1, 1, 2, 2),
    variable = c("x", "y", "x", "y",
                 "x", "y", "x", "y"),
    value    = c(10, 20, 30, 40, 99, 88, 77, 66)
  )
  r <- dcast(dup, id = "id", variable = "variable", value = "value")
  expect_equal(r[r$id == 1, "x"], 99)
  expect_equal(r[r$id == 1, "y"], 88)
  expect_equal(r[r$id == 2, "x"], 77)
  expect_equal(r[r$id == 2, "y"], 66)

  # general path: var non-periodic
  gen <- data.frame(
    id = c(1, 2, 1, 2), variable = "x", value = c(11, 22, 33, 44)
  )
  rg <- dcast(gen, id = "id", variable = "variable", value = "value")
  expect_equal(rg[rg$id == 1, "x"], 33)
  expect_equal(rg[rg$id == 2, "x"], 44)
})
