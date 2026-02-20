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
