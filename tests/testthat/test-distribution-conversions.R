test_that("continuous conversion helpers expose route metadata", {
  f <- as_cdf_1d(pdf_fun = dnorm)
  expect_equal(attr(f, "ggfunction_route"), "pdf->cdf")
  expect_true(isTRUE(attr(f, "ggfunction_approximate")))

  g <- as_pdf_1d(fun = dnorm)
  expect_equal(attr(g, "ggfunction_route"), "pdf")
  expect_false(isTRUE(attr(g, "ggfunction_approximate")))
})

test_that("bounded support is honored for beta PDF/CDF/quantile routes", {
  beta_args <- list(shape1 = 2, shape2 = 5)
  cdf_from_pdf <- as_cdf_1d(pdf_fun = dbeta, args = beta_args, support = c(0, 1))
  pdf_from_cdf <- as_pdf_1d(cdf_fun = pbeta, args = beta_args, support = c(0, 1))
  qf_from_pdf <- as_qf_1d(pdf_fun = dbeta, args = beta_args, support = c(0, 1))

  x <- seq(0.02, 0.98, length.out = 25)
  p <- c(0.05, 0.25, 0.5, 0.9)

  expect_equal(cdf_from_pdf(x), pbeta(x, 2, 5), tolerance = 1e-4)
  expect_equal(pdf_from_cdf(x), dbeta(x, 2, 5), tolerance = 1e-3)
  expect_equal(qf_from_pdf(p), qbeta(p, 2, 5), tolerance = 1e-4)
})

test_that("heavy-tailed CDF inversion adaptively brackets Cauchy quantiles", {
  qf_from_cdf <- as_qf_1d(cdf_fun = pcauchy)
  p <- c(0.001, 0.01, 0.5, 0.99, 0.999)
  expect_equal(qf_from_cdf(p), qcauchy(p), tolerance = 1e-6)
})

test_that("hazard conversions handle increasing and decreasing Weibull hazards", {
  h_weibull <- function(x, shape, scale) {
    ifelse(x >= 0, (shape / scale) * (x / scale)^(shape - 1), 0)
  }
  x <- seq(0.05, 4, length.out = 40)

  pdf_inc <- as_pdf_1d(
    hf_fun = h_weibull,
    args = list(shape = 2, scale = 1.5),
    support = c(0, Inf)
  )
  pdf_dec <- as_pdf_1d(
    hf_fun = h_weibull,
    args = list(shape = 0.7, scale = 1.5),
    support = c(0, Inf)
  )

  expect_equal(pdf_inc(x), dweibull(x, shape = 2, scale = 1.5), tolerance = 1e-4)
  expect_equal(pdf_dec(x), dweibull(x, shape = 0.7, scale = 1.5), tolerance = 1e-4)
})

test_that("function argument lists must be named", {
  expect_error(
    as_pdf_1d(fun = dnorm, args = list(0, 1)),
    "named list"
  )
})

# --- B-03: tail-stable hazard conversion ---

test_that("survival-route hazards are exact deep in the tail (B-03)", {
  h <- as_hf_1d(survival_fun = function(x) exp(-x))
  expect_equal(h(c(1, 20, 36, 40)), rep(1, 4), tolerance = 1e-4)
  expect_true(all(is.finite(h(c(1, 20, 36, 40)))))
})

test_that("pdf and pdf+cdf hazard routes are tail-stable (B-03)", {
  h_pdf <- as_hf_1d(pdf_fun = dexp, support = c(0, Inf))
  h_pair <- as_hf_1d(pdf_fun = dexp, cdf_fun = pexp, support = c(0, Inf))
  expect_equal(h_pdf(c(20, 40)), c(1, 1), tolerance = 1e-4)
  expect_equal(h_pair(c(20, 40)), c(1, 1), tolerance = 1e-4)
})

test_that("normal upper-tail hazard matches the Mills-ratio value (B-03)", {
  x <- c(6, 8)
  h <- as_hf_1d(pdf_fun = dnorm)
  expect_equal(
    h(x),
    dnorm(x) / pnorm(x, lower.tail = FALSE),
    tolerance = 1e-4
  )
})

test_that("CDF-only hazard warns on saturated tails instead of returning zero (B-03)", {
  h_cdf_only <- as_hf_1d(cdf_fun = pexp)
  expect_warning(
    value <- h_cdf_only(40),
    "rounded|saturated|tail"
  )
  expect_true(is.na(value) || !is.finite(value))
})

# --- B-04: exact hazard-derived support endpoints ---

test_that("hazard-derived functions are exact at infinite endpoints (B-04)", {
  h <- function(x) rep(1, length(x))
  H <- as_chf_1d(hf_fun = h, support = c(0, Inf))
  F <- as_cdf_1d(hf_fun = h, support = c(0, Inf))
  S <- as_survival_1d(hf_fun = h, support = c(0, Inf))

  expect_identical(H(Inf), Inf)
  expect_identical(F(Inf), 1)
  expect_identical(S(Inf), 0)
})

test_that("hf_lower sets the exact integration origin (B-04)", {
  h <- function(x) rep(1, length(x))
  H <- as_chf_1d(hf_fun = h, hf_lower = 1, support = c(0, Inf))
  F <- as_cdf_1d(hf_fun = h, hf_lower = 1, support = c(0, Inf))
  S <- as_survival_1d(hf_fun = h, hf_lower = 1, support = c(0, Inf))
  f <- as_pdf_1d(hf_fun = h, hf_lower = 1, support = c(0, Inf))

  expect_equal(H(c(0, 0.5, 1, 2)), c(0, 0, 0, 1), tolerance = 1e-8)
  expect_equal(F(c(0.5, 1, 2)), c(0, 0, 1 - exp(-1)), tolerance = 1e-8)
  expect_equal(S(c(0.5, 1, 2)), c(1, 1, exp(-1)), tolerance = 1e-8)
  expect_equal(f(0.5), 0)
})

test_that("finite-support divergent hazards reach exactly 1 without warnings (B-04)", {
  h <- function(x) 1 / (1 - x)
  F <- as_cdf_1d(hf_fun = h, support = c(0, 1))
  expect_no_warning(vals <- F(c(0.5, 0.99, 1, 1.1)))
  expect_equal(vals, c(0.5, 0.99, 1, 1), tolerance = 1e-6)
})

test_that("an origin at or above the upper support endpoint aborts (B-04)", {
  expect_error(
    as_chf_1d(hf_fun = function(x) rep(1, length(x)), hf_lower = 2, support = c(0, 2)),
    "origin"
  )
})
