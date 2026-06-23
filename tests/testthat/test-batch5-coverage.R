# Batch 5: value-level coverage for stats whose legacy tests only smoke-checked
# that a plot object builds. These assert the actual computed transform.

test_that("Stat_1d_2d returns the exact parametric trajectory", {
  traj <- Stat_1d_2d$compute_group(
    data.frame(group = 1), scales = list(),
    fun = function(t) c(sin(t), cos(t)),
    tlim = c(0, pi), dt = 0.1, args = list()
  )
  expect_equal(traj$x, sin(traj$t))
  expect_equal(traj$y, cos(traj$t))
  # args are injected into the parametric function
  traj2 <- Stat_1d_2d$compute_group(
    data.frame(group = 1), scales = list(),
    fun = function(t, a = 1) c(a * t, t), tlim = c(0, 1), dt = 0.25,
    args = list(a = 3)
  )
  expect_equal(traj2$x, 3 * traj2$t)
})

test_that("StatFunction2d evaluates the scalar field z = f(x, y) per grid point", {
  r <- StatFunction2d$compute_group(
    data.frame(group = 1), scales = list(),
    fun = function(v) v[1] + 2 * v[2],
    xlim = c(0, 2), ylim = c(0, 2), n = 3, args = list()
  )
  expect_equal(nrow(r), 9)                       # 3 x 3 grid
  expect_equal(r$z, r$x + 2 * r$y)               # z matches f at every point
})

test_that("discrete CDF agrees across fun and pmf input paths", {
  via_fun <- StatCDFDiscrete$compute_group(
    data.frame(group = 1), scales = list(),
    fun = function(x) pbinom(x, 10, 0.4), xlim = c(0, 10), args = list()
  )
  via_pmf <- StatCDFDiscrete$compute_group(
    data.frame(group = 1), scales = list(),
    pmf_fun = function(x) dbinom(x, 10, 0.4), xlim = c(0, 10), args = list()
  )
  expect_equal(via_fun$x, via_pmf$x)
  expect_equal(via_fun$y, via_pmf$y, tolerance = 1e-12)
  expect_equal(via_fun$y[length(via_fun$y)], 1, tolerance = 1e-6)  # reaches 1
})

test_that("discrete survival is the complement of the discrete CDF", {
  surv <- StatSurvivalDiscrete$compute_group(
    data.frame(group = 1), scales = list(),
    cdf_fun = function(x) pbinom(x, 10, 0.4), xlim = c(0, 10), args = list()
  )
  expect_equal(surv$y, 1 - pbinom(surv$x, 10, 0.4), tolerance = 1e-12)
  # survival is monotonically non-increasing
  expect_true(all(diff(surv$y) <= 0))
})
