test_that("StatQFDiscrete computes correct quantile values", {
  scales <- list(x = NULL)
  result <- StatQFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pmf_fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  # x values should be CDF values: monotonically increasing, ending near 1
  expect_true(all(diff(result$x) >= 0))
  expect_true(abs(result$x[11] - 1) < 0.01)
  # y values should be the support: 0 through 10
  expect_equal(result$y, 0:10)
})

test_that("geom_qf_discrete builds without error", {
  p <- ggplot() + geom_qf_discrete(
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf_discrete with Poisson builds without error", {
  p <- ggplot() + geom_qf_discrete(
    pmf_fun = dpois, args = list(lambda = 5), xlim = c(0, 15)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf_discrete with custom open_fill builds without error", {
  p <- ggplot() + geom_qf_discrete(
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10),
    open_fill = "blue"
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf_discrete with show_points=FALSE show_vert=FALSE builds without error", {
  p <- ggplot() + geom_qf_discrete(
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10),
    show_points = FALSE, show_vert = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf_discrete with support parameter builds without error", {
  f_mean <- function(x, prob) dbinom(round(x * 10), size = 10, prob = prob)
  p <- ggplot() + geom_qf_discrete(
    pmf_fun = f_mean, support = seq(0, 1, by = 0.1), args = list(prob = 0.3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatQFDiscrete uses default xlim when NULL", {
  scales <- list(x = NULL)
  result <- StatQFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pmf_fun = dbinom,
    xlim = NULL,
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)  # 0:10
})
