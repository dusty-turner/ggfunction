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

test_that("geom_cdf_discrete trains y scale to include zero and one", {
  p <- ggplot() +
    geom_cdf_discrete(
      pmf_fun = dbinom,
      support = 0:10,
      xlim = c(3, 7),
      args = list(size = 10, prob = 0.5)
    )
  yrng <- plot_y_range(p)
  expect_lte(yrng[1], 0)
  expect_gte(yrng[2], 1)
})

test_that("geom_cdf_discrete uses x and p as default axis labels", {
  p <- ggplot() + geom_cdf_discrete(
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10)
  )
  expect_equal(plot_axis_titles(p), c(x = "x", y = "p"))
})

test_that("geom_cdf_discrete with custom open_fill builds without error", {
  p <- ggplot() + geom_cdf_discrete(
    pmf_fun = dpois, args = list(lambda = 5), xlim = c(0, 15),
    support = 0:50, open_fill = "blue"
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

test_that("StatCDFDiscrete computes over full support before xlim display filtering", {
  scales <- list(x = NULL)
  result <- StatCDFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pmf_fun = dbinom,
    support = 0:10,
    xlim = c(3, 7),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(result$x, 3:7)
  expect_equal(result$y, pbinom(3:7, size = 10, prob = 0.5), tolerance = 1e-10)
})

test_that("StatCDFDiscrete aborts for truncated PMF support", {
  scales <- list(x = NULL)
  expect_error(
    StatCDFDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      pmf_fun = dbinom,
      xlim = c(3, 7),
      args = list(size = 10, prob = 0.5)
    ),
    "full computational support"
  )
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

# --- shading ---

test_that("geom_cdf_discrete shades the lower tail with p", {
  built <- ggplot_build(
    ggplot() +
      geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10),
                        args = list(size = 10, prob = 0.5), p = 0.5)
  )
  in_shade <- built$data[[1]]$in_shade
  expect_true(any(in_shade))
  expect_true(any(!in_shade))
  # Lower tail: shaded atoms form a prefix
  expect_true(all(diff(in_shade) <= 0))
})

test_that("geom_cdf_discrete shading membership matches across sources", {
  args <- list(size = 10, prob = 0.5)
  b_pmf <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = args, p = 0.5))
  b_fun <- ggplot_build(ggplot() +
    geom_cdf_discrete(fun = pbinom, xlim = c(0, 10), args = args, p = 0.5))
  b_srv <- ggplot_build(ggplot() +
    geom_cdf_discrete(survival_fun = function(x, size, prob) 1 - pbinom(x, size, prob),
                      xlim = c(0, 10), args = args, p = 0.5))
  expect_identical(b_pmf$data[[1]]$in_shade, b_fun$data[[1]]$in_shade)
  expect_identical(b_pmf$data[[1]]$in_shade, b_srv$data[[1]]$in_shade)
})

test_that("geom_cdf_discrete shading matches geom_pmf on the same distribution", {
  args <- list(size = 10, prob = 0.5)
  b_cdf <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = args, p = 0.8))
  b_pmf <- ggplot_build(ggplot() +
    geom_pmf(fun = dbinom, xlim = c(0, 10), args = args, p = 0.8))
  expect_identical(b_cdf$data[[1]]$in_shade, b_pmf$data[[1]]$in_shade)
})

test_that("geom_cdf_discrete supports two-sided and outside shading", {
  args <- list(size = 10, prob = 0.5)
  b_in <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = args,
                      p_lower = 0.1, p_upper = 0.9))
  b_out <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = args,
                      p_lower = 0.1, p_upper = 0.9, shade_outside = TRUE))
  expect_identical(b_in$data[[1]]$in_shade, !b_out$data[[1]]$in_shade)
})

test_that("geom_cdf_discrete shading is computed on the full support before xlim", {
  args <- list(size = 10, prob = 0.5)
  b_full <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, support = 0:10, args = args, p = 0.5))
  b_clip <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, support = 0:10, xlim = c(3, 7),
                      args = args, p = 0.5))
  full <- b_full$data[[1]]
  clip <- b_clip$data[[1]]
  expect_identical(
    clip$in_shade,
    full$in_shade[full$x >= 3 & full$x <= 7]
  )
})

test_that("geom_cdf_discrete without shading args marks all atoms in_shade", {
  built <- ggplot_build(ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10),
                      args = list(size = 10, prob = 0.5)))
  expect_true(all(built$data[[1]]$in_shade))
})

test_that("geom_cdf_discrete draws with shading", {
  p <- ggplot() +
    geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10),
                      args = list(size = 10, prob = 0.5), p = 0.5)
  expect_silent(ggplotGrob(p))
})


test_that("a truncated sub-1 CDF triggers the soft endpoint diagnostic", {
  expect_message(
    StatCDFDiscrete$compute_group(
      data.frame(group = 1), scales = list(),
      fun = function(x) ppois(x, 20), support = 0:5, args = list()
    ),
    "top of the support"
  )
})

test_that("the discrete CDF agrees across fun and pmf input paths", {
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
  expect_equal(via_fun$y[length(via_fun$y)], 1, tolerance = 1e-6)
})
