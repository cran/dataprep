test_that("prep_fit and prep_transform apply training statistics to test", {
  set.seed(1)
  train <- data.frame(
    x = rnorm(100, mean = 10, sd = 2),
    y = rnorm(100, mean = 5,  sd = 1)
  )
  test <- data.frame(
    x = rnorm(20, mean = 10, sd = 2),
    y = rnorm(20, mean = 5,  sd = 1)
  )

  plan <- prep_fit(train, cols = 1:2, steps = c("scale"),
                   scale_method = "zscore", verbose = FALSE)
  test_t <- prep_transform(plan, test, verbose = FALSE)

  # The test values must be standardised with the training mean / sd,
  # not with the test mean / sd.
  expected_x <- (test$x - mean(train$x)) / sd(train$x)
  expect_equal(test_t$x, expected_x, tolerance = 1e-12)
})

test_that("prep_fit stores 1 for constant-column scale", {
  train <- data.frame(
    x = rep(5, 100),
    y = rnorm(100)
  )
  plan <- prep_fit(train, cols = 1:2, steps = c("scale"),
                   scale_method = "zscore", verbose = FALSE)

  # A constant column has sd = 0. Storing 0 would make prep_transform
  # divide by zero; the plan must store 1 instead.
  expect_equal(plan$params$scale_scale[["x"]], 1)

  test <- data.frame(x = c(5, 6, 7), y = c(1, 2, 3))
  test_t <- prep_transform(plan, test, verbose = FALSE)

  # x - 5, then / 1 -> c(0, 1, 2)
  expect_equal(test_t$x, c(0, 1, 2), tolerance = 1e-12)
})

test_that("prep_transform rejects data missing required columns", {
  train <- data.frame(x = rnorm(50), y = rnorm(50))
  plan <- prep_fit(train, cols = 1:2, steps = c("scale"),
                   verbose = FALSE)

  test_bad <- data.frame(x = rnorm(10))
  expect_error(
    prep_transform(plan, test_bad, verbose = FALSE),
    "missing required columns"
  )
})

test_that("prep_fit / prep_transform handle a full pipeline", {
  set.seed(7)
  train <- data.frame(
    x = c(rnorm(98), NA, 100),   # has NA and one outlier
    y = c(rnorm(99), NA)
  )
  test <- data.frame(
    x = c(rnorm(19), NA),
    y = rnorm(20)
  )

  plan <- prep_fit(
    train, cols = 1:2,
    steps = c("varidele", "outlier", "impute", "scale"),
    fraction = 0.5,
    method_outlier = "iqr",
    method_impute  = "linear",
    scale_method   = "zscore",
    verbose = FALSE
  )

  test_clean <- prep_transform(plan, test, verbose = FALSE)

  expect_s3_class(test_clean, "data.frame")
  expect_true(all(c("x", "y") %in% names(test_clean)))
  expect_false(anyNA(test_clean$x))
  expect_false(anyNA(test_clean$y))
})