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

test_that("vectorize works on matrix input", {
  f <- function(v) v^2  # returns length-2 vector
  vf <- vectorize(f)
  mat <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  result <- vf(mat)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
})

test_that("empty detects NULL", {
  expect_true(empty(NULL))
})

test_that("empty detects zero-row data frame", {
  expect_true(empty(data.frame()))
})

test_that("empty detects waiver", {
  expect_true(empty(ggplot2::waiver()))
})

test_that("empty returns FALSE for valid data frame", {
  expect_false(empty(data.frame(x = 1)))
})

test_that("%||% returns a when non-NULL", {
  expect_equal(1 %||% 2, 1)
})

test_that("%||% returns b when a is NULL", {
  expect_equal(NULL %||% 2, 2)
})

test_that("check_pmf_normalization warns for invalid PMF", {
  bad_pmf <- function(x) rep(0.5, length(x))
  expect_message(check_pmf_normalization(bad_pmf, support = 0:5, tol = 1e-3))
})

test_that("check_pmf_normalization errors when function fails", {
  bad_fun <- function(x) stop("broken")
  expect_error(check_pmf_normalization(bad_fun, support = 0:5))
})

test_that("check_pdf_normalization errors when integration fails", {
  bad_fun <- function(x) stop("broken")
  expect_error(check_pdf_normalization(bad_fun, 0, 1))
})

test_that("build_step_polygon handles n=1", {
  result <- build_step_polygon(1, 0.5)
  expect_equal(nrow(result), 1)
  expect_equal(result$x, 1)
  expect_equal(result$y, 0.5)
})

test_that("times is multiplication", {
  expect_equal(times(3, 4), 12)
})

test_that("tibble0 creates a data frame", {
  result <- tibble0(x = 1, y = 2, .size = 1)
  expect_s3_class(result, "data.frame")
  expect_true("x" %in% names(result))
})
