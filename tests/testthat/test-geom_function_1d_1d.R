test_that("StatFunction1d computes correct values", {
  scales <- list(x = NULL)
  result <- StatFunction1d$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = sin,
    xlim = c(0, 2 * pi),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expect_equal(result$x[1], 0)
  expect_equal(result$x[101], 2 * pi)
  # sin(0) = 0
  expect_equal(result$y[1], 0, tolerance = 1e-10)
})

test_that("geom_function_1d_1d builds without error", {
  p <- ggplot() + geom_function_1d_1d(fun = sin, xlim = c(0, 2 * pi))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_1d with shade_from/shade_to builds without error", {
  p <- ggplot() + geom_function_1d_1d(
    fun = dnorm, xlim = c(-3, 3),
    shade_from = -1, shade_to = 1
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_1d with args builds without error", {
  p <- ggplot() + geom_function_1d_1d(
    fun = dnorm, xlim = c(-5, 15),
    args = list(mean = 5, sd = 2)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
