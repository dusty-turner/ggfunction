test_that("StatPDF computes correct density values", {
  scales <- list(x = NULL)
  result <- StatPDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expect_true(all(result$y >= 0))
  expect_equal(result$x[1], -3)
  expect_equal(result$x[101], 3)
  # Peak should be near x = 0
  peak_idx <- which.max(result$y)
  expect_true(abs(result$x[peak_idx]) < 0.1)
})

test_that("geom_pdf builds a ggplot without error", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf exposes linewidth as an explicit argument", {
  expect_true("linewidth" %in% names(formals(geom_pdf)))
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), linewidth = 3)
  expect_equal(p$layers[[1]]$aes_params$linewidth, 3)
})

test_that("geom_pdf defaults alpha to 0.35", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3))
  expect_equal(p$layers[[1]]$aes_params$alpha, 0.35)
})

test_that("geom_pdf with p shading builds without error", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), p = 0.975)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf with p_lower/p_upper builds without error", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), p_lower = 0.025, p_upper = 0.975)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf with args builds without error", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-5, 15), args = list(mean = 5, sd = 2))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf with shade_outside builds without error", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3),
    p_lower = 0.025, p_upper = 0.975, shade_outside = TRUE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf with shade_hdr builds without error (unimodal)", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), shade_hdr = 0.95)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf with shade_hdr builds without error (bimodal, disconnected HDR)", {
  f_bim <- function(x) 0.5 * dnorm(x, -2, 0.5) + 0.5 * dnorm(x, 2, 0.5)
  p <- ggplot() + geom_pdf(fun = f_bim, xlim = c(-4, 4), shade_hdr = 0.9)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf with lower.tail=FALSE builds without error", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), p = 0.975, lower.tail = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pdf with custom mapping builds without error", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), mapping = aes(linetype = "solid"))
  expect_s3_class(p, "gg")
})

# --- Alternate input: cdf_fun ---

test_that("StatPDF computes PDF from cdf_fun (pnorm)", {
  scales <- list(x = NULL)
  result <- StatPDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    cdf_fun = pnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- dnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_pdf with cdf_fun builds without error", {
  p <- ggplot() + geom_pdf(cdf_fun = pnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatPDF errors when both fun and cdf_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatPDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = dnorm,
      cdf_fun = pnorm,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*cdf_fun.*survival_fun.*qf_fun"
  )
})

test_that("StatPDF errors when neither fun nor cdf_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatPDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*cdf_fun.*survival_fun.*qf_fun"
  )
})

# --- Alternate input: survival_fun ---

test_that("StatPDF computes PDF from survival_fun", {
  s_norm <- function(x) 1 - pnorm(x)
  scales <- list(x = NULL)
  result <- StatPDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    survival_fun = s_norm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- dnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_pdf with survival_fun builds without error", {
  s_norm <- function(x) 1 - pnorm(x)
  p <- ggplot() + geom_pdf(survival_fun = s_norm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

# --- Alternate input: qf_fun ---

test_that("StatPDF computes PDF from qf_fun", {
  scales <- list(x = NULL)
  result <- StatPDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    qf_fun = qnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- dnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-2)
})

test_that("geom_pdf with qf_fun builds without error", {
  p <- ggplot() + geom_pdf(qf_fun = qnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatPDF errors when multiple sources provided", {
  scales <- list(x = NULL)
  expect_error(
    StatPDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = dnorm,
      survival_fun = function(x) 1 - pnorm(x),
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*cdf_fun.*survival_fun.*qf_fun"
  )
})
