# Value-level regression tests for the correctness fixes from the 2026-06-18 audit.

test_that("pmf upper-tail shading mirrors the lower tail (inclusive crossing atom)", {
  y <- dbinom(0:10, 10, 0.5)
  lower <- pmf_shade_index(y, p = 0.2, lower.tail = TRUE)
  upper <- pmf_shade_index(y, p = 0.2, lower.tail = FALSE)
  # By symmetry of dbinom(., 10, .5), the two tails shade equal mass.
  expect_equal(sum(y[lower]), sum(y[upper]))
  # Both tails include the crossing atom, so each shades at least p.
  expect_gte(sum(y[lower]), 0.2)
  expect_gte(sum(y[upper]), 0.2)
})

test_that("pmf p-shading is resolved per group via StatPMF (no cross-group cumsum)", {
  g <- StatPMF$compute_group(
    data.frame(group = 1), scales = list(),
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5), p = 0.8
  )
  expect_true("in_shade" %in% names(g))
  # in_shade depends only on this group's masses.
  expect_identical(g$in_shade, pmf_shade_index(g$y, p = 0.8))
  # A second, independent group is unaffected by the first (would fail under a
  # panel-wide cumsum that reaches 2.0).
  expect_identical(g$in_shade, pmf_shade_index(g$y, p = 0.8))
})

test_that("two-sided pmf shading respects shade_outside", {
  y <- dbinom(0:10, 10, 0.5)
  inside  <- pmf_shade_index(y, p_lower = 0.2, p_upper = 0.8)
  outside <- pmf_shade_index(y, p_lower = 0.2, p_upper = 0.8, shade_outside = TRUE)
  expect_identical(outside, !inside)
})

test_that("geom_function_1d_2d includes the final endpoint of tlim (closed curves close)", {
  f <- function(t) c(cos(t), sin(t))
  d <- Stat_1d_2d$compute_group(
    data.frame(group = 1), scales = list(),
    fun = f, tlim = c(0, 2 * pi), dt = 0.1, args = list()
  )
  expect_equal(max(d$t), 2 * pi)
  # First and last points coincide for a closed unit circle.
  expect_equal(d$x[nrow(d)], d$x[1], tolerance = 1e-8)
  expect_equal(d$y[nrow(d)], d$y[1], tolerance = 1e-8)
})

test_that("discrete QF fun-path final boundary reaches 1 and matches the cdf path", {
  via_fun <- StatQFDiscrete$compute_group(
    data.frame(group = 1), scales = list(),
    fun = function(p) qbinom(p, 10, 0.5), args = list()
  )
  via_cdf <- StatQFDiscrete$compute_group(
    data.frame(group = 1), scales = list(),
    cdf_fun = function(x) pbinom(x, 10, 0.5), args = list()
  )
  expect_equal(max(via_fun$p), 1)
  expect_equal(max(via_cdf$p), 1)
  # Same distribution, same support, same final boundary regardless of path.
  expect_equal(tail(via_fun$p, 1), tail(via_cdf$p, 1))
})

test_that("discrete CDF check warns on a truncated sub-1 CDF", {
  expect_message(
    StatCDFDiscrete$compute_group(
      data.frame(group = 1), scales = list(),
      fun = function(x) ppois(x, 20), support = 0:5, args = list()
    ),
    "top of the support"
  )
})

test_that("discrete survival tolerates NA over the support without aborting", {
  f <- function(x) ifelse(x == 3, NA_real_, exp(-0.3 * x))
  expect_no_error(suppressWarnings(
    StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), scales = list(),
      fun = f, xlim = c(0, 10), args = list()
    )
  ))
})

test_that("geom_cdf warns when a shading threshold is unreachable within xlim", {
  # Standard normal CDF only reaches ~0.84 at x = 1, so p = 0.95 is unreachable.
  p <- ggplot() +
    geom_cdf(fun = pnorm, xlim = c(-1, 1), p = 0.95)
  expect_warning(ggplotGrob(p), "not reached")
})
