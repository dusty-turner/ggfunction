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
