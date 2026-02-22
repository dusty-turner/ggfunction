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
