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
