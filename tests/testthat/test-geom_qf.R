test_that("StatQF computes correct quantile values", {
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = qnorm,
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  # Median should be near 0 for standard normal
  mid_idx <- ceiling(101 / 2)
  expect_true(abs(result$q[mid_idx]) < 0.1)
})

test_that("geom_qf builds a ggplot without error", {
  p <- ggplot() + geom_qf(fun = qnorm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf with args builds without error", {
  p <- ggplot() + geom_qf(fun = qnorm, args = list(mean = 5, sd = 2))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf with qbeta builds without error", {
  p <- ggplot() + geom_qf(fun = qbeta, args = list(shape1 = 3, shape2 = 4))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatQF uses Chebyshev nodes on (0,1)", {
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = qnorm,
    n = 11,
    args = list()
  )
  expect_equal(nrow(result), 11)
  # p values should be in (0, 1), never exactly 0 or 1
  expect_true(all(result$p > 0 & result$p < 1))
})

test_that("geom_qf with custom mapping builds without error", {
  p <- ggplot() + geom_qf(fun = qnorm, mapping = aes(linetype = "solid"))
  expect_s3_class(p, "gg")
})

# --- Alternate inputs ---

test_that("StatQF computes QF from cdf_fun (pnorm)", {
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    cdf_fun = pnorm,
    n = 51,
    args = list()
  )
  expect_equal(nrow(result), 51)
  expected <- qnorm(result$p)
  expect_equal(result$q, expected, tolerance = 1e-3)
})

test_that("StatQF computes QF from pdf_fun (dnorm)", {
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dnorm,
    n = 51,
    args = list()
  )
  expect_equal(nrow(result), 51)
  expected <- qnorm(result$p)
  expect_equal(result$q, expected, tolerance = 1e-2)
})

test_that("geom_qf with cdf_fun builds without error", {
  p <- ggplot() + geom_qf(cdf_fun = pnorm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf with pdf_fun builds without error", {
  p <- ggplot() + geom_qf(pdf_fun = dnorm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatQF errors when multiple inputs provided", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = qnorm,
      cdf_fun = pnorm,
      n = 51,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*survival_fun"
  )
})

test_that("StatQF errors when no input provided", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      n = 51,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*survival_fun"
  )
})

# --- Alternate input: survival_fun ---

test_that("StatQF computes QF from survival_fun", {
  s_norm <- function(x) 1 - pnorm(x)
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    survival_fun = s_norm,
    n = 51,
    args = list()
  )
  expect_equal(nrow(result), 51)
  expected <- qnorm(result$p)
  expect_equal(result$q, expected, tolerance = 1e-3)
})

test_that("geom_qf with survival_fun builds without error", {
  s_norm <- function(x) 1 - pnorm(x)
  p <- ggplot() + geom_qf(survival_fun = s_norm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatQF errors when multiple inputs including survival_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = qnorm,
      survival_fun = function(x) 1 - pnorm(x),
      n = 51,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*survival_fun"
  )
})
