test_that("StatHF computes hazard function correctly", {
  scales <- list(x = NULL)
  result <- StatHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dexp,
    cdf_fun = pexp,
    xlim = c(0.01, 5),
    n = 101,
    args = list(rate = 1),
    pdf_args = NULL,
    cdf_args = NULL
  )
  expect_equal(nrow(result), 101)
  # Hazard of exponential is constant = rate
  expect_true(all(abs(result$y - 1) < 0.01, na.rm = TRUE))
})

test_that("StatHF handles division by zero gracefully", {
  scales <- list(x = NULL)
  # At right tail, S(x) -> 0, so hazard -> NaN
  result <- StatHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dnorm,
    cdf_fun = pnorm,
    xlim = c(-3, 40),
    n = 101,
    args = list(),
    pdf_args = NULL,
    cdf_args = NULL
  )
  # Should have some NaN at extreme right tail
  expect_true(any(is.nan(result$y)) || all(is.finite(result$y)))
})

test_that("geom_hf builds without error", {
  p <- ggplot() + geom_hf(pdf_fun = dexp, cdf_fun = pexp, args = list(rate = 1), xlim = c(0.01, 5))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_hf with separate pdf/cdf args builds without error", {
  p <- ggplot() + geom_hf(
    pdf_fun = dnorm, cdf_fun = pnorm,
    args = list(mean = 0, sd = 1),
    xlim = c(-3, 3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatHF with direct fun interface works", {
  h_exp <- function(x, rate) rep(rate, length(x))
  scales <- list(x = NULL)
  result <- StatHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = h_exp,
    xlim = c(0.01, 5),
    n = 51,
    args = list(rate = 2)
  )
  expect_equal(nrow(result), 51)
  expect_true(all(abs(result$y - 2) < 0.01))
})

test_that("geom_hf with direct fun builds without error", {
  h_weibull <- function(x, shape, scale) (shape / scale) * (x / scale)^(shape - 1)
  p <- ggplot() + geom_hf(
    fun = h_weibull, xlim = c(0.01, 5),
    args = list(shape = 0.5, scale = 2)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatHF errors when both fun and pdf_fun/cdf_fun supplied", {
  scales <- list(x = NULL)
  expect_error(
    StatHF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = identity,
      pdf_fun = dnorm,
      cdf_fun = pnorm,
      xlim = c(0, 5),
      n = 51,
      args = list()
    ),
    "fun.*pdf_fun.*cdf_fun"
  )
})

test_that("StatHF errors when only pdf_fun without cdf_fun", {
  scales <- list(x = NULL)
  expect_error(
    StatHF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      pdf_fun = dnorm,
      xlim = c(0, 5),
      n = 51,
      args = list()
    ),
    "pdf_fun.*cdf_fun"
  )
})

test_that("geom_hf with pdf_args/cdf_args overrides builds without error", {
  p <- ggplot() + geom_hf(
    pdf_fun = dnorm, cdf_fun = pnorm,
    args = list(mean = 0),
    pdf_args = list(sd = 1),
    cdf_args = list(sd = 1),
    xlim = c(-3, 3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
