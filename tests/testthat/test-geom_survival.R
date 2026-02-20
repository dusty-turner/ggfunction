test_that("StatSurvival computes 1 - CDF correctly", {
  scales <- list(x = NULL)
  result <- StatSurvival$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = pnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  # S(-3) should be near 1
  expect_true(result$y[1] > 0.99)
  # S(3) should be near 0
  expect_true(result$y[101] < 0.01)
  # Should be monotonically non-increasing
  expect_true(all(diff(result$y) <= 1e-10))
})

test_that("geom_survival builds without error", {
  p <- ggplot() + geom_survival(fun = pnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival with exponential builds without error", {
  p <- ggplot() + geom_survival(fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
