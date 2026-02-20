test_that("ensure_nonempty_data returns data frame with group column", {
  result <- ensure_nonempty_data(NULL)
  expect_s3_class(result, "data.frame")
  expect_true("group" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("ensure_length_two works correctly", {
  expect_equal(ensure_length_two(5), c(5, 5))
  expect_equal(ensure_length_two(c(1, 2)), c(1, 2))
  expect_error(ensure_length_two(c(1, 2, 3)))
})

test_that("check_pdf_normalization passes for valid PDF", {
  expect_silent(check_pdf_normalization(dnorm, -Inf, Inf, tol = 1e-2))
})

test_that("check_pdf_normalization warns for invalid PDF", {
  bad_fun <- function(x) x^2
  expect_message(check_pdf_normalization(bad_fun, 0, 1, tol = 1e-3))
})

test_that("check_pmf_normalization passes for valid PMF", {
  pmf <- function(x) dbinom(x, size = 10, prob = 0.5)
  expect_silent(check_pmf_normalization(pmf, support = 0:10, tol = 1e-2))
})

test_that("build_step_polygon creates correct step vertices", {
  result <- build_step_polygon(c(0, 1, 2), c(0.2, 0.5, 1.0))
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 3)
  # First y should match first input y

  expect_equal(result$y[1], 0.2)
})

test_that("vectorize works on simple function", {
  f <- function(v) sum(v^2)
  vf <- vectorize(f)
  result <- vf(c(1, 2))
  expect_equal(result, 5)
})
