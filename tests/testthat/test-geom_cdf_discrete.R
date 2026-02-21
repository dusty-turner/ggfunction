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
