test_that("StatCDF computes correct CDF values", {
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = pnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expect_true(result$y[1] < 0.01)
  expect_true(result$y[101] > 0.99)
  # CDF should be monotonically non-decreasing
  expect_true(all(diff(result$y) >= 0))
})

test_that("geom_cdf builds a ggplot without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf with p shading builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf with p_lower/p_upper builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p_lower = 0.025, p_upper = 0.975)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf with lower.tail=FALSE builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf with args builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-5, 15), args = list(mean = 5, sd = 2))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDF warns for invalid CDF", {
  bad_cdf <- function(x) x / 10  # not a real CDF
  scales <- list(x = NULL)
  expect_message(
    StatCDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = bad_cdf,
      xlim = c(0, 5),
      n = 101,
      args = list()
    )
  )
})

# --- Alternate input: pdf_fun ---

test_that("StatCDF computes CDF from pdf_fun (dnorm)", {
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_cdf with pdf_fun builds without error", {
  p <- ggplot() + geom_cdf(pdf_fun = dnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDF errors when both fun and pdf_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = pnorm,
      pdf_fun = dnorm,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*pdf_fun.*survival_fun.*qf_fun.*hf_fun"
  )
})

test_that("StatCDF errors when neither fun nor pdf_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*pdf_fun.*survival_fun.*qf_fun.*hf_fun"
  )
})

# --- Alternate input: survival_fun ---

test_that("StatCDF computes CDF from survival_fun", {
  s_norm <- function(x) 1 - pnorm(x)
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    survival_fun = s_norm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-10)
})

test_that("geom_cdf with survival_fun builds without error", {
  s_norm <- function(x) 1 - pnorm(x)
  p <- ggplot() + geom_cdf(survival_fun = s_norm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

# --- Alternate input: qf_fun ---

test_that("StatCDF computes CDF from qf_fun", {
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    qf_fun = qnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_cdf with qf_fun builds without error", {
  p <- ggplot() + geom_cdf(qf_fun = qnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

# --- Alternate input: hf_fun ---

test_that("StatCDF computes CDF from hf_fun (exponential hazard)", {
  h_exp <- function(x) ifelse(x >= 0, 1, 0)  # rate = 1
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    hf_fun = h_exp,
    xlim = c(0, 5),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pexp(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_cdf with hf_fun builds without error", {
  h_exp <- function(x) ifelse(x >= 0, 1, 0)
  p <- ggplot() + geom_cdf(hf_fun = h_exp, xlim = c(0, 5))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDF errors when multiple sources provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = pnorm,
      survival_fun = function(x) 1 - pnorm(x),
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*pdf_fun.*survival_fun.*qf_fun.*hf_fun"
  )
})
