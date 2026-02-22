test_that("StatCDFDiscrete computes correct cumulative values", {
  scales <- list(x = NULL)
  result <- StatCDFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  # CDF at last value should be near 1
  expect_true(abs(result$y[11] - 1) < 0.01)
  # Should be monotonically non-decreasing
  expect_true(all(diff(result$y) >= 0))
})

test_that("geom_cdf_discrete builds without error", {
  p <- ggplot() + geom_cdf_discrete(
    fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf_discrete with custom open_fill builds without error", {
  p <- ggplot() + geom_cdf_discrete(
    fun = dpois, args = list(lambda = 5), xlim = c(0, 15), open_fill = "blue"
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf_discrete with show_points=FALSE show_vert=FALSE builds without error", {
  p <- ggplot() + geom_cdf_discrete(
    fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10),
    show_points = FALSE, show_vert = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf_discrete with support parameter builds without error", {
  f_mean <- function(x, prob) dbinom(round(x * 10), size = 10, prob = prob)
  p <- ggplot() + geom_cdf_discrete(
    fun = f_mean, support = seq(0, 1, by = 0.1), args = list(prob = 0.3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDFDiscrete uses default xlim when NULL", {
  scales <- list(x = NULL)
  result <- StatCDFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = NULL,
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)  # 0:10
})

test_that("StatCDFDiscrete uses support when provided", {
  scales <- list(x = NULL)
  result <- StatCDFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    support = c(0, 5, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 3)
})
