test_that("StatSurvivalDiscrete computes correct survival values", {
  scales <- list(x = NULL)
  result <- StatSurvivalDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pmf_fun = dbinom,
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
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete with custom open_fill builds without error", {
  p <- ggplot() + geom_survival_discrete(
    pmf_fun = dpois, xlim = c(0, 15), args = list(lambda = 5),
    support = 0:50, open_fill = "blue"
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete show_points = FALSE builds without error", {
  p <- ggplot() + geom_survival_discrete(
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_points = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete show_vert = FALSE builds without error", {
  p <- ggplot() + geom_survival_discrete(
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_vert = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete with both show_points=FALSE show_vert=FALSE", {
  p <- ggplot() + geom_survival_discrete(
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_points = FALSE, show_vert = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival_discrete with support parameter builds without error", {
  f_mean <- function(x, prob) dbinom(round(x * 10), size = 10, prob = prob)
  p <- ggplot() + geom_survival_discrete(
    pmf_fun = f_mean, support = seq(0, 1, by = 0.1), args = list(prob = 0.3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatSurvivalDiscrete computes over full support before xlim display filtering", {
  scales <- list(x = NULL)
  result <- StatSurvivalDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pmf_fun = dbinom,
    support = 0:10,
    xlim = c(3, 7),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(result$x, 3:7)
  expect_equal(result$y, 1 - pbinom(3:7, size = 10, prob = 0.5), tolerance = 1e-10)
})

test_that("StatSurvivalDiscrete aborts for truncated PMF support", {
  scales <- list(x = NULL)
  expect_error(
    StatSurvivalDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      pmf_fun = dbinom,
      xlim = c(3, 7),
      args = list(size = 10, prob = 0.5)
    ),
    "full computational support"
  )
})

# --- Alternate inputs ---

test_that("StatSurvivalDiscrete computes S(x) from cdf_fun", {
  scales <- list(x = NULL)
  result <- StatSurvivalDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    cdf_fun = pbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  expected <- 1 - pbinom(0:10, size = 10, prob = 0.5)
  expect_equal(result$y, expected, tolerance = 1e-6)
})

test_that("StatSurvivalDiscrete computes S(x) from direct fun", {
  s_binom <- function(x, size, prob) 1 - pbinom(x, size = size, prob = prob)
  scales <- list(x = NULL)
  result <- StatSurvivalDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = s_binom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  expected <- 1 - pbinom(0:10, size = 10, prob = 0.5)
  expect_equal(result$y, expected, tolerance = 1e-6)
})

test_that("geom_survival_discrete with cdf_fun builds without error", {
  p <- ggplot() + geom_survival_discrete(
    cdf_fun = pbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatSurvivalDiscrete errors when multiple inputs provided", {
  scales <- list(x = NULL)
  expect_error(
    StatSurvivalDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      cdf_fun = pbinom,
      pmf_fun = dbinom,
      xlim = c(0, 10),
      args = list(size = 10, prob = 0.5)
    ),
    "fun.*cdf_fun.*pmf_fun"
  )
})

test_that("StatSurvivalDiscrete errors when no input provided", {
  scales <- list(x = NULL)
  expect_error(
    StatSurvivalDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      xlim = c(0, 10),
      args = list(size = 10, prob = 0.5)
    ),
    "fun.*cdf_fun.*pmf_fun"
  )
})

# --- shading ---

test_that("geom_survival_discrete shades the upper tail with lower.tail = FALSE", {
  built <- ggplot_build(ggplot() +
    geom_survival_discrete(pmf_fun = dbinom, xlim = c(0, 10),
                           args = list(size = 10, prob = 0.5),
                           p = 0.25, lower.tail = FALSE))
  in_shade <- built$data[[1]]$in_shade
  expect_true(any(in_shade))
  expect_true(any(!in_shade))
  # Upper tail: shaded atoms form a suffix
  expect_true(all(diff(in_shade) >= 0))
})

test_that("geom_survival_discrete shading matches geom_cdf_discrete membership", {
  args <- list(size = 10, prob = 0.5)
  b_srv <- ggplot_build(ggplot() +
    geom_survival_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = args, p = 0.5))
  b_cdf <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = args, p = 0.5))
  expect_identical(b_srv$data[[1]]$in_shade, b_cdf$data[[1]]$in_shade)
})

test_that("geom_survival_discrete without shading args marks all atoms in_shade", {
  built <- ggplot_build(ggplot() +
    geom_survival_discrete(pmf_fun = dbinom, xlim = c(0, 10),
                           args = list(size = 10, prob = 0.5)))
  expect_true(all(built$data[[1]]$in_shade))
})

test_that("geom_survival_discrete draws with shading", {
  p <- ggplot() +
    geom_survival_discrete(pmf_fun = dbinom, xlim = c(0, 10),
                           args = list(size = 10, prob = 0.5),
                           p = 0.25, lower.tail = FALSE)
  expect_silent(ggplotGrob(p))
})


test_that("non-finite survival values over the support are rejected", {
  # Structural validity: a survival function that returns NA on its declared
  # support is invalid and must abort rather than be drawn silently.
  f <- function(x) ifelse(x == 3, NA_real_, exp(-0.3 * x))
  expect_error(
    StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), scales = list(x = NULL, y = NULL),
      fun = f, xlim = c(0, 10), args = list()
    ),
    "finite"
  )
})

test_that("discrete survival is the complement of the discrete CDF", {
  surv <- StatSurvivalDiscrete$compute_group(
    data.frame(group = 1), scales = list(),
    cdf_fun = function(x) pbinom(x, 10, 0.4), xlim = c(0, 10), args = list()
  )
  expect_equal(surv$survival, 1 - pbinom(surv$x_eval, 10, 0.4), tolerance = 1e-12)
  expect_true(all(diff(surv$survival) <= 0))
})
