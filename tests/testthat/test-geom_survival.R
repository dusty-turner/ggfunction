test_that("StatSurvival computes 1 - CDF correctly via cdf_fun", {
  scales <- list(x = NULL)
  result <- StatSurvival$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    cdf_fun = pnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  # S(-3) should be near 1
  expect_true(result$y[1] > 0.99)
  # S(3) should be near 0
  expect_true(result$y[101] < 0.01)
  # Should be monotonically non-increasing
  expect_true(all(diff(result$y) <= 1e-10))
})

test_that("geom_survival builds without error", {
  p <- ggplot() + geom_survival(cdf_fun = pnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival with exponential builds without error", {
  p <- ggplot() + geom_survival(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival with custom mapping builds without error", {
  p <- ggplot() + geom_survival(
    cdf_fun = pexp, xlim = c(0, 10), args = list(rate = 0.5),
    mapping = aes(linetype = "solid")
  )
  expect_s3_class(p, "gg")
})

# --- Alternate inputs ---

test_that("StatSurvival computes S(x) from cdf_fun", {
  scales <- list(x = NULL)
  result <- StatSurvival$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    cdf_fun = pnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- 1 - pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-6)
})

test_that("StatSurvival computes S(x) from pdf_fun", {
  scales <- list(x = NULL)
  result <- StatSurvival$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- 1 - pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("StatSurvival computes S(x) from direct fun", {
  s_norm <- function(x) 1 - pnorm(x)
  scales <- list(x = NULL)
  result <- StatSurvival$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = s_norm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expected <- 1 - pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-6)
})

test_that("geom_survival with cdf_fun builds without error", {
  p <- ggplot() + geom_survival(cdf_fun = pnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_survival with pdf_fun builds without error", {
  p <- ggplot() + geom_survival(pdf_fun = dnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatSurvival errors when multiple inputs provided", {
  scales <- list(x = NULL)
  expect_error(
    StatSurvival$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = function(x) 1 - pnorm(x),
      cdf_fun = pnorm,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*qf_fun"
  )
})

test_that("StatSurvival errors when no input provided", {
  scales <- list(x = NULL)
  expect_error(
    StatSurvival$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*qf_fun"
  )
})

# --- Alternate input: qf_fun ---

test_that("StatSurvival computes S(x) from qf_fun", {
  scales <- list(x = NULL)
  result <- StatSurvival$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    qf_fun = qnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- 1 - pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_survival with qf_fun builds without error", {
  p <- ggplot() + geom_survival(qf_fun = qnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatSurvival errors when multiple inputs including qf_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatSurvival$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      cdf_fun = pnorm,
      qf_fun = qnorm,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*cdf_fun.*pdf_fun.*qf_fun"
  )
})

# --- Alternate input: hf_fun ---

test_that("StatSurvival computes survival from a Weibull hazard", {
  h_weibull <- function(t) 2 * t  # shape 2, scale 1: S(t) = exp(-t^2)
  scales <- list(x = NULL)
  result <- StatSurvival$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    hf_fun = h_weibull,
    support = c(0, Inf),
    xlim = c(0, 3),
    n = 101,
    args = list()
  )
  expect_equal(result$y, exp(-result$x^2), tolerance = 1e-3)
})

test_that("geom_survival with hf_fun builds without error", {
  p <- ggplot() +
    geom_survival(hf_fun = function(t) 2 * t, support = c(0, Inf), xlim = c(0, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatSurvival errors when hf_fun combined with another source", {
  scales <- list(x = NULL)
  expect_error(
    StatSurvival$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      cdf_fun = pnorm,
      hf_fun = function(t) 2 * t,
      xlim = c(0, 3),
      n = 101,
      args = list()
    ),
    "hf_fun"
  )
})

# --- shading ---

test_that("geom_survival shades the lower tail with p", {
  p <- ggplot() +
    geom_survival(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10), p = 0.5)
  expect_s3_class(p, "gg")
  expect_silent(ggplotGrob(p))
})

test_that("geom_survival shades the upper tail with lower.tail = FALSE", {
  p <- ggplot() +
    geom_survival(cdf_fun = pnorm, xlim = c(-3, 3), p = 0.1, lower.tail = FALSE)
  expect_silent(ggplotGrob(p))
})

test_that("geom_survival shades two-sided regions", {
  p <- ggplot() +
    geom_survival(cdf_fun = pnorm, xlim = c(-3, 3), p_lower = 0.25, p_upper = 0.75)
  expect_silent(ggplotGrob(p))
})

test_that("geom_survival keeps exact out-of-window shade boundaries without clamping", {
  # The p = 0.99 quantile (~2.33) lies outside xlim = c(-3, 0). The exact raw
  # boundary is retained as metadata and the visible shading is clipped by
  # the window; no false boundary is created at the window edge.
  p <- ggplot() +
    geom_survival(cdf_fun = pnorm, xlim = c(-3, 0), p = 0.99)
  expect_no_warning(ggplotGrob(p))
  d <- ggplot_build(p)$data[[1]]
  expect_equal(
    unique(stats::na.omit(d$shade_x_upper_raw)),
    qnorm(0.99),
    tolerance = 1e-6
  )
  expect_lte(max(d$x_eval), 0)
})

# --- check ---

test_that("StatSurvival check alerts on an invalid survival function", {
  scales <- list(x = NULL)
  msgs <- capture_messages(
    StatSurvival$compute_group(
      data = data.frame(group = 1), scales = scales,
      fun = pnorm,  # increasing: a CDF, not a survival function
      xlim = c(-3, 3), n = 101, args = list()
    )
  )
  expect_true(any(grepl("valid survival function", msgs)))
  expect_true(any(grepl("non-increasing", msgs)))
})

test_that("StatSurvival check = FALSE suppresses the diagnostic", {
  scales <- list(x = NULL)
  expect_silent(
    StatSurvival$compute_group(
      data = data.frame(group = 1), scales = scales,
      fun = pnorm,
      xlim = c(-3, 3), n = 101, args = list(),
      check = FALSE
    )
  )
})

test_that("user-supplied colour is respected (no default color override)", {
  b <- ggplot_build(
    ggplot() +
      geom_survival(cdf_fun = pexp, xlim = c(0, 6), args = list(rate = 1),
                    colour = "steelblue")
  )
  expect_identical(unique(b$data[[1]]$colour), "steelblue")
  b2 <- ggplot_build(
    ggplot() + geom_hf(fun = function(x) rep(1, length(x)), xlim = c(0, 3),
                       colour = "firebrick")
  )
  expect_identical(unique(b2$data[[1]]$colour), "firebrick")
  b3 <- ggplot_build(
    ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), colour = "tomato")
  )
  expect_identical(unique(b3$data[[1]]$colour), "tomato")
  b4 <- ggplot_build(
    ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), colour = "tomato")
  )
  expect_identical(unique(b4$data[[1]]$colour), "tomato")
})
