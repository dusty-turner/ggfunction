test_that("StatCHF computes H(x) from cdf_fun", {
  scales <- list(x = NULL)
  result <- StatCHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    cdf_fun = pexp,
    xlim = c(0, 5),
    n = 101,
    args = list(rate = 1)
  )
  expect_equal(nrow(result), 101)
  # For exponential(rate=1): H(x) = x
  expected <- -log(1 - pexp(result$x, rate = 1))
  expect_equal(result$y, expected, tolerance = 1e-6)
})

test_that("StatCHF computes H(x) from survival_fun", {
  s_exp <- function(x, rate = 1) exp(-rate * x)
  scales <- list(x = NULL)
  result <- StatCHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    survival_fun = s_exp,
    xlim = c(0, 5),
    n = 101,
    args = list(rate = 0.5)
  )
  expect_equal(nrow(result), 101)
  # H(x) = -log(S(x)) = rate * x = 0.5 * x
  expected <- 0.5 * result$x
  expect_equal(result$y, expected, tolerance = 1e-6)
})

test_that("StatCHF computes H(x) from hf_fun (exponential)", {
  h_exp <- function(x) ifelse(x >= 0, 1, 0)  # rate = 1
  scales <- list(x = NULL)
  result <- StatCHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    hf_fun = h_exp,
    xlim = c(0, 5),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  # H(x) = x for exponential(rate=1)
  expect_equal(result$y, result$x, tolerance = 1e-3)
})

test_that("StatCHF computes H(x) from direct fun", {
  H_exp <- function(x, rate = 1) rate * x
  scales <- list(x = NULL)
  result <- StatCHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = H_exp,
    xlim = c(0, 5),
    n = 101,
    args = list(rate = 2)
  )
  expected <- 2 * result$x
  expect_equal(result$y, expected, tolerance = 1e-10)
})

test_that("StatCHF computes H(x) from pdf_fun", {
  scales <- list(x = NULL)
  result <- StatCHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dexp,
    xlim = c(0, 5),
    n = 101,
    args = list(rate = 1)
  )
  expect_equal(nrow(result), 101)
  expected <- -log(1 - pexp(result$x, rate = 1))
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("StatCHF computes H(x) from qf_fun", {
  scales <- list(x = NULL)
  result <- StatCHF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    qf_fun = qexp,
    xlim = c(0, 5),
    n = 101,
    args = list(rate = 1)
  )
  expect_equal(nrow(result), 101)
  expected <- -log(1 - pexp(result$x, rate = 1))
  expect_equal(result$y, expected, tolerance = 1e-2)
})

# --- Geom-level smoke tests ---

test_that("geom_chf with cdf_fun builds without error", {
  p <- ggplot() + geom_chf(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_chf with hf_fun builds without error", {
  h_exp <- function(x) ifelse(x >= 0, 0.5, 0)
  p <- ggplot() + geom_chf(hf_fun = h_exp, xlim = c(0, 10))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_chf with survival_fun builds without error", {
  p <- ggplot() + geom_chf(survival_fun = function(x) 1 - pnorm(x), xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_chf with pdf_fun builds without error", {
  p <- ggplot() + geom_chf(pdf_fun = dexp, args = list(rate = 1), xlim = c(0, 5))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_chf with qf_fun builds without error", {
  p <- ggplot() + geom_chf(qf_fun = qexp, xlim = c(0, 5))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

# --- Validation tests ---

test_that("StatCHF errors when no input provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCHF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      xlim = c(0, 5),
      n = 101,
      args = list()
    ),
    "fun.*hf_fun.*cdf_fun.*pdf_fun.*survival_fun.*qf_fun"
  )
})

test_that("StatCHF errors when multiple inputs provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCHF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      cdf_fun = pexp,
      survival_fun = function(x) exp(-x),
      xlim = c(0, 5),
      n = 101,
      args = list()
    ),
    "fun.*hf_fun.*cdf_fun.*pdf_fun.*survival_fun.*qf_fun"
  )
})
