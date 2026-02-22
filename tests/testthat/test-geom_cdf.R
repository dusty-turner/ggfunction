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
