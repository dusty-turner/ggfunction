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
