test_that("StatCDFDiscrete computes correct cumulative values", {
  scales <- list(x = NULL)
  result <- StatCDFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pmf_fun = dbinom,
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
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf_discrete with custom open_fill builds without error", {
  p <- ggplot() + geom_cdf_discrete(
    pmf_fun = dpois, args = list(lambda = 5), xlim = c(0, 15), open_fill = "blue"
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf_discrete with show_points=FALSE show_vert=FALSE builds without error", {
  p <- ggplot() + geom_cdf_discrete(
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10),
    show_points = FALSE, show_vert = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf_discrete with support parameter builds without error", {
  f_mean <- function(x, prob) dbinom(round(x * 10), size = 10, prob = prob)
  p <- ggplot() + geom_cdf_discrete(
    pmf_fun = f_mean, support = seq(0, 1, by = 0.1), args = list(prob = 0.3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDFDiscrete uses default xlim when NULL", {
  scales <- list(x = NULL)
  result <- StatCDFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pmf_fun = dbinom,
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
    pmf_fun = dbinom,
    support = c(0, 5, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 3)
})

# --- Alternate input: survival_fun ---

test_that("StatCDFDiscrete computes CDF from survival_fun", {
  s_binom <- function(x) 1 - pbinom(x, size = 10, prob = 0.5)
  scales <- list(x = NULL)
  result <- StatCDFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    survival_fun = s_binom,
    xlim = c(0, 10),
    args = list()
  )
  expect_equal(nrow(result), 11)
  expected <- pbinom(0:10, size = 10, prob = 0.5)
  expect_equal(result$y, expected, tolerance = 1e-10)
})

test_that("geom_cdf_discrete with survival_fun builds without error", {
  s_binom <- function(x, size, prob) 1 - pbinom(x, size = size, prob = prob)
  p <- ggplot() + geom_cdf_discrete(
    survival_fun = s_binom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDFDiscrete errors when multiple sources provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDFDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = pbinom,
      survival_fun = function(x) 1 - pbinom(x, size = 10, prob = 0.5),
      xlim = c(0, 10),
      args = list(size = 10, prob = 0.5)
    ),
    "fun.*pmf_fun.*survival_fun"
  )
})

test_that("StatCDFDiscrete errors when no source provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDFDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      xlim = c(0, 10),
      args = list()
    ),
    "fun.*pmf_fun.*survival_fun"
  )
})
