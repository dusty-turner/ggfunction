test_that("StatSurvivalDiscrete computes correct survival values", {
  scales <- list(x = NULL)
  result <- StatSurvivalDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  # S at first value should be < 1 (some mass removed)
  expect_true(result$y[1] < 1)
  # S at last value should be near 0
  expect_true(abs(result$y[11]) < 0.01)
  # Should be monotonically non-increasing
  expect_true(all(diff(result$y) <= 0))
})

test_that("geom_survival_discrete builds without error", {
  p <- ggplot() + geom_survival_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete with custom open_fill builds without error", {
  p <- ggplot() + geom_survival_discrete(
    fun = dpois, xlim = c(0, 15), args = list(lambda = 5), open_fill = "blue"
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete show_points = FALSE builds without error", {
  p <- ggplot() + geom_survival_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_points = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete show_vert = FALSE builds without error", {
  p <- ggplot() + geom_survival_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_vert = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete with both show_points=FALSE show_vert=FALSE", {
  p <- ggplot() + geom_survival_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_points = FALSE, show_vert = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete with support parameter builds without error", {
  f_mean <- function(x, prob) dbinom(round(x * 10), size = 10, prob = prob)
  p <- ggplot() + geom_survival_discrete(
    fun = f_mean, support = seq(0, 1, by = 0.1), args = list(prob = 0.3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
