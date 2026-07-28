# A-01: transformed-scale contract for generated-position stats.
# Public xlim/support are data coordinates; positions are transformed exactly
# once; canonical raw columns retain evaluation-space values.

test_that("geom_pdf respects data-space xlim under log10 x and y scales (A-01)", {
  b <- ggplot_build(
    ggplot() +
      geom_pdf(fun = dexp, xlim = c(0.1, 10), n = 3) +
      scale_x_log10() +
      scale_y_log10()
  )$data[[1]]

  expect_equal(b$x, c(-1, 0, 1), tolerance = 1e-12)
  expect_equal(b$x_eval, c(0.1, 1, 10), tolerance = 1e-12)
  expect_equal(b$y, log10(dexp(b$x_eval)), tolerance = 1e-12)
  expect_equal(b$density, dexp(b$x_eval), tolerance = 1e-12)
})

test_that("geom_pdf reverse x scale evaluates in data coordinates (A-01)", {
  b <- ggplot_build(
    ggplot() +
      geom_pdf(fun = dnorm, xlim = c(-1, 1), n = 3) +
      scale_x_reverse()
  )$data[[1]]

  expect_equal(b$x, c(1, 0, -1))
  expect_equal(b$x_eval, c(-1, 0, 1))
  expect_equal(b$density, dnorm(b$x_eval))
})

test_that("geom_pdf identity scales remain numerically unchanged (A-01)", {
  b <- ggplot_build(
    ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), n = 5)
  )$data[[1]]

  expect_identical(b$x, b$x_eval)
  expect_equal(b$y, b$density)
  expect_equal(b$density, dnorm(b$x_eval))
})

test_that("geom_function_1d_1d evaluates in data coordinates under log10 x (A-01)", {
  b <- ggplot_build(
    ggplot() +
      geom_function_1d_1d(fun = identity, xlim = c(1, 100), n = 3) +
      scale_x_log10()
  )$data[[1]]

  expect_equal(b$x, c(0, 1, 2))
  expect_equal(b$x_eval, c(1, 10, 100))
  expect_equal(b$y_raw, c(1, 10, 100))
})

test_that("malformed xlim aborts instead of falling through (A-01/B-01)", {
  expect_error(geom_pdf(fun = dnorm, xlim = c(3, -3)), "increasing")
  expect_error(geom_pdf(fun = dnorm, xlim = c(0, Inf)), "increasing")
  expect_error(geom_pdf(fun = dnorm, xlim = 3), "increasing")
  expect_error(
    geom_function_1d_1d(fun = sin, xlim = c(1, -1)),
    "increasing"
  )
})

test_that("xlim outside the x transform domain fails loudly, not silently (A-01)", {
  # The scale transformation is only known at build time; ggplot2 downgrades
  # Stat errors to a loud computation warning with zero rows drawn.
  expect_warning(
    b <- ggplot_build(
      ggplot() + geom_pdf(fun = dnorm, xlim = c(-1, 1)) + scale_x_log10()
    ),
    "domain"
  )
  expect_equal(nrow(b$data[[1]]), 0)
})

test_that("pdf shading boundaries are exact quantiles, independent of grid and y scale (B-02)", {
  expected <- qnorm(0.3)
  for (n in c(3, 4, 101)) {
    d <- ggplot_build(
      ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3), n = n, p = 0.3)
    )$data[[1]]
    expect_equal(unique(stats::na.omit(d$shade_upper)), expected, tolerance = 1e-8)
    expect_true(any(abs(d$x_eval - expected) < 1e-8))
  }

  d_log <- ggplot_build(
    ggplot() +
      geom_pdf(fun = dnorm, xlim = c(-3, 3), p = 0.3) +
      scale_y_log10()
  )$data[[1]]
  expect_equal(unique(stats::na.omit(d_log$shade_upper)), expected, tolerance = 1e-8)
})

test_that("geom_pdf under log-y renders finite grobs and one baseline warning (A-01 render)", {
  p <- ggplot() +
    geom_pdf(fun = dnorm, xlim = c(-3, 3)) +
    scale_y_log10()
  w <- testthat::capture_warnings(g <- ggplotGrob(p))
  expect_length(w, 1)
  expect_match(w, "baseline", ignore.case = TRUE)

  polys <- find_grobs(g, c("polygon", "area", "ribbon"))
  expect_gt(length(polys), 0)
  for (poly in polys) {
    expect_true(all(is.finite(as.numeric(poly$x))))
    expect_true(all(is.finite(as.numeric(poly$y))))
  }
})

test_that("geom_pdf on identity scales renders without baseline warnings (A-01)", {
  p <- ggplot() + geom_pdf(fun = dnorm, xlim = c(-3, 3))
  expect_no_warning(ggplotGrob(p))
})

test_that("geom_survival transforms positions once and keeps raw survival (A-01)", {
  b <- ggplot_build(
    ggplot() +
      geom_survival(fun = function(x) exp(-x), xlim = c(0.1, 10), n = 3,
                    support = c(0, Inf)) +
      scale_x_log10() +
      scale_y_log10()
  )$data[[1]]

  expect_equal(b$x, c(-1, 0, 1), tolerance = 1e-12)
  expect_equal(b$x_eval, c(0.1, 1, 10), tolerance = 1e-12)
  expect_equal(b$survival, exp(-b$x_eval), tolerance = 1e-12)
  expect_equal(b$y, log10(exp(-b$x_eval)), tolerance = 1e-12)
})

test_that("geom_hf and geom_chf transform positions once with raw aliases (A-01)", {
  b_hf <- ggplot_build(
    ggplot() +
      geom_hf(fun = function(x) rep(2, length(x)), xlim = c(0.1, 10), n = 3) +
      scale_x_log10() +
      scale_y_log10()
  )$data[[1]]
  expect_equal(b_hf$x, c(-1, 0, 1), tolerance = 1e-12)
  expect_equal(b_hf$hazard, rep(2, 3))
  expect_equal(b_hf$y, rep(log10(2), 3), tolerance = 1e-12)

  b_chf <- ggplot_build(
    ggplot() +
      geom_chf(fun = identity, xlim = c(0.1, 10), n = 3) +
      scale_x_log10()
  )$data[[1]]
  expect_equal(b_chf$x, c(-1, 0, 1), tolerance = 1e-12)
  expect_equal(b_chf$cumhazard, c(0.1, 1, 10), tolerance = 1e-12)
})

test_that("geom_qf places raw quantiles on transformed output scales once (A-01)", {
  b <- ggplot_build(
    ggplot() +
      geom_qf(fun = qlnorm, n = 5) +
      scale_y_log10()
  )$data[[1]]
  expect_equal(b$y, log10(qlnorm(b$p)), tolerance = 1e-10)
  expect_equal(b$q, qlnorm(b$p), tolerance = 1e-10)

  b_id <- ggplot_build(ggplot() + geom_qf(fun = qnorm, n = 5))$data[[1]]
  expect_equal(b_id$y, qnorm(b_id$p), tolerance = 1e-10)
  expect_equal(b_id$x, b_id$p)
})

test_that("survival shading boundaries are exact quantiles on any y scale (B-02)", {
  for (scale_layer in list(NULL, scale_y_log10())) {
    p <- ggplot() + geom_survival(cdf_fun = pnorm, xlim = c(-3, 3), p = 0.3)
    if (!is.null(scale_layer)) p <- p + scale_layer
    d <- ggplot_build(p)$data[[1]]
    expect_equal(
      unique(stats::na.omit(d$shade_x_upper_raw)),
      qnorm(0.3),
      tolerance = 1e-6
    )
    expect_true(any(abs(d$x_eval - qnorm(0.3)) < 1e-6))
  }
})

test_that("survival probability axis trains on 0 and 1; CHF trains on zero (C-05)", {
  rng <- plot_y_range(
    ggplot() +
      geom_survival(fun = function(x) exp(-x), xlim = c(0, 1), support = c(0, Inf))
  )
  expect_lte(rng[1], 0)
  expect_gte(rng[2], 1)

  rng_chf <- plot_y_range(
    ggplot() + geom_chf(fun = function(x) 5 + x, xlim = c(0, 1))
  )
  expect_lte(rng_chf[1], 0)
})
