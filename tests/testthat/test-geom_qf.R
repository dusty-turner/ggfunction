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

test_that("geom_qf uses p and x as default axis labels", {
  p <- ggplot() + geom_qf(fun = qnorm)
  expect_equal(plot_axis_titles(p), c(x = "p", y = "x"))
})

test_that("geom_qf x-axis range includes 0 and 1", {
  p <- ggplot() + geom_qf(fun = qnorm)
  xrng <- plot_x_range(p)
  expect_lte(xrng[1], 0)
  expect_gte(xrng[2], 1)
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

# --- Alternate inputs ---

test_that("StatQF computes QF from cdf_fun (pnorm)", {
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    cdf_fun = pnorm,
    n = 51,
    args = list()
  )
  expect_equal(nrow(result), 51)
  expected <- qnorm(result$p)
  expect_equal(result$q, expected, tolerance = 1e-3)
})

test_that("StatQF computes QF from pdf_fun (dnorm)", {
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dnorm,
    n = 51,
    args = list()
  )
  expect_equal(nrow(result), 51)
  expected <- qnorm(result$p)
  expect_equal(result$q, expected, tolerance = 1e-2)
})

test_that("geom_qf with cdf_fun builds without error", {
  p <- ggplot() + geom_qf(cdf_fun = pnorm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_qf with pdf_fun builds without error", {
  p <- ggplot() + geom_qf(pdf_fun = dnorm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatQF errors when multiple inputs provided", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = qnorm,
      cdf_fun = pnorm,
      n = 51,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*survival_fun"
  )
})

test_that("StatQF errors when no input provided", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      n = 51,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*survival_fun"
  )
})

# --- Alternate input: survival_fun ---

test_that("StatQF computes QF from survival_fun", {
  s_norm <- function(x) 1 - pnorm(x)
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    survival_fun = s_norm,
    n = 51,
    args = list()
  )
  expect_equal(nrow(result), 51)
  expected <- qnorm(result$p)
  expect_equal(result$q, expected, tolerance = 1e-3)
})

test_that("geom_qf with survival_fun builds without error", {
  s_norm <- function(x) 1 - pnorm(x)
  p <- ggplot() + geom_qf(survival_fun = s_norm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatQF errors when multiple inputs including survival_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = qnorm,
      survival_fun = function(x) 1 - pnorm(x),
      n = 51,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*survival_fun"
  )
})

# --- Alternate input: hf_fun ---

test_that("StatQF computes quantile function from a Weibull hazard", {
  h_weibull <- function(t) 2 * t  # shape 2, scale 1: Q(p) = sqrt(-log(1 - p))
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    hf_fun = h_weibull,
    support = c(0, Inf),
    n = 51,
    args = list()
  )
  expect_equal(result$x, sqrt(-log(1 - result$p)), tolerance = 1e-2)
})

test_that("geom_qf with hf_fun builds without error", {
  p <- ggplot() +
    geom_qf(hf_fun = function(t) 2 * t, support = c(0, Inf))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatQF errors when hf_fun combined with another source", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = qnorm,
      hf_fun = function(t) 2 * t,
      n = 51,
      args = list()
    ),
    "hf_fun"
  )
})

# --- xlim (probability range) ---

test_that("geom_qf xlim restricts the probability range", {
  built <- ggplot_build(ggplot() + geom_qf(fun = qnorm, xlim = c(0.2, 0.8)))
  p_vals <- built$data[[1]]$x
  expect_true(all(p_vals >= 0.2 & p_vals <= 0.8))
})

test_that("geom_qf xlim = NULL matches previous default grid", {
  scales <- list(x = NULL)
  ref <- StatQF$compute_group(
    data = data.frame(group = 1), scales = scales,
    fun = qnorm, n = 101, args = list()
  )
  k <- seq_len(101)
  expect_equal(ref$p, (1 - cos((2 * k - 1) * pi / (2 * 101))) / 2)
})

test_that("geom_qf errors on invalid xlim", {
  scales <- list(x = NULL)
  expect_error(
    StatQF$compute_group(
      data = data.frame(group = 1), scales = scales,
      fun = qnorm, xlim = c(0.8, 0.2), n = 51, args = list()
    ),
    "xlim"
  )
})

# --- check ---

test_that("StatQF check alerts on a non-monotone quantile function", {
  scales <- list(x = NULL)
  expect_message(
    StatQF$compute_group(
      data = data.frame(group = 1), scales = scales,
      fun = function(p) sin(20 * p), n = 51, args = list()
    ),
    "monotonically"
  )
})

test_that("StatQF check = FALSE suppresses the diagnostic", {
  scales <- list(x = NULL)
  expect_silent(
    StatQF$compute_group(
      data = data.frame(group = 1), scales = scales,
      fun = function(p) sin(20 * p), n = 51, args = list(),
      check = FALSE
    )
  )
})
