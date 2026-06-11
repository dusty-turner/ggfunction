test_that("StatPMF computes correct PMF values", {
  scales <- list(x = NULL)
  result <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  expect_true(all(result$y >= 0))
  expect_true(abs(sum(result$y) - 1) < 0.01)
})

test_that("geom_pmf builds a ggplot without error", {
  p <- ggplot() + geom_pmf(fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with p shading builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5), p = 0.8
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with lower.tail=FALSE builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    p = 0.8, lower.tail = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with p_lower/p_upper builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    p_lower = 0.1, p_upper = 0.9
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with shade_outside builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    p_lower = 0.1, p_upper = 0.9, shade_outside = TRUE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with shade_hdr builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3),
    shade_hdr = 0.7
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatPMF computes HDR levels for shade_hdr", {
  result <- suppressMessages(StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.3),
    shade_hdr = c(0.5, 0.8)
  ))
  expect_s3_class(result$probs, "ordered")
  expect_equal(levels(result$probs), c(">80%", "80%", "50%"))
  expect_gte(sum(result$y[result$probs == "50%"]), 0.5)
  expect_gte(sum(result$y[result$probs %in% c("50%", "80%")]), 0.8)

  plain <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.3)
  )
  expect_false("probs" %in% names(plain))
})

test_that("geom_pmf maps probs to alpha when shade_hdr is supplied", {
  l_hdr <- geom_pmf(fun = dbinom, xlim = c(0, 10),
    args = list(size = 10, prob = 0.5), shade_hdr = 0.8)
  l_plain <- geom_pmf(fun = dbinom, xlim = c(0, 10),
    args = list(size = 10, prob = 0.5))
  expect_equal(rlang::as_label(l_hdr$mapping$alpha), "after_stat(probs)")
  expect_null(l_plain$mapping$alpha)
})

test_that("geom_pmf with support parameter builds without error", {
  f_mean <- function(x, prob) dbinom(round(x * 10), size = 10, prob = prob)
  p <- ggplot() + geom_pmf(
    fun = f_mean, support = seq(0, 1, by = 0.1), args = list(prob = 0.3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatPMF uses default xlim when not provided", {
  scales <- list(x = NULL)
  result <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = NULL,
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)  # 0:10
})

test_that("StatPMF uses support when provided", {
  scales <- list(x = NULL)
  result <- suppressMessages(StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    support = c(0, 5, 10),
    args = list(size = 10, prob = 0.5)
  ))
  expect_equal(nrow(result), 3)
})
