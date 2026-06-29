test_that("order_stat_sample sorts data and computes plotting positions", {
  result <- ggfunction:::order_stat_sample(c(3, 1, 2), a = 1 / 2)

  expect_equal(result$sample, c(1, 2, 3))
  expect_equal(result$p, stats::ppoints(3, a = 1 / 2))
  expect_equal(result$n, c(3L, 3L, 3L))
})

test_that("order_stat_sample handles non-finite values", {
  expect_warning(
    result <- ggfunction:::order_stat_sample(c(1, NA, Inf, 2), na.rm = FALSE),
    "non-finite"
  )
  expect_equal(result$sample, c(1, 2))

  expect_silent(
    result_silent <- ggfunction:::order_stat_sample(c(1, NA, Inf, 2), na.rm = TRUE)
  )
  expect_equal(result_silent$sample, c(1, 2))
})

test_that("StatPPPlot computes theoretical and observed probabilities", {
  result <- StatPPPlot$compute_group(
    data = data.frame(x = c(1, -1, 0)),
    scales = list(),
    fun = pnorm,
    a = 1 / 2,
    args = list()
  )

  expected_p <- stats::ppoints(3, a = 1 / 2)
  expected_sample <- c(-1, 0, 1)

  expect_equal(result$x, expected_p)
  expect_equal(result$p, expected_p)
  expect_equal(result$sample, expected_sample)
  expect_equal(result$y, pnorm(expected_sample))
  expect_equal(result$observed, pnorm(expected_sample))
})

test_that("StatSPPlot computes stabilized theoretical and observed probabilities", {
  result <- StatSPPlot$compute_group(
    data = data.frame(x = c(1, -1, 0)),
    scales = list(),
    fun = pnorm,
    a = 1 / 2,
    args = list()
  )

  expected_p <- stats::ppoints(3, a = 1 / 2)
  expected_sample <- c(-1, 0, 1)

  expect_equal(result$x, ggfunction:::sp_transform(expected_p))
  expect_equal(result$p, expected_p)
  expect_equal(result$sample, expected_sample)
  expect_equal(result$y, ggfunction:::sp_transform(pnorm(expected_sample)))
  expect_equal(result$observed, ggfunction:::sp_transform(pnorm(expected_sample)))
})

test_that("StatQQPlot computes theoretical and observed quantiles", {
  result <- StatQQPlot$compute_group(
    data = data.frame(x = c(1, -1, 0)),
    scales = list(),
    fun = qnorm,
    a = 1 / 2,
    args = list()
  )

  expected_p <- stats::ppoints(3, a = 1 / 2)
  expected_sample <- c(-1, 0, 1)

  expect_equal(result$x, qnorm(expected_p))
  expect_equal(result$theoretical, qnorm(expected_p))
  expect_equal(result$y, expected_sample)
  expect_equal(result$observed, expected_sample)
  expect_equal(result$p, expected_p)
})

test_that("PP DKW band computes default and custom levels", {
  result <- StatPPPlotBand$compute_group(
    data = data.frame(x = 1:5),
    scales = list(),
    level = 0.95,
    band_n = 5,
    a = 1 / 2
  )
  eps <- sqrt(log(2 / (1 - 0.95)) / (2 * 5))

  expect_equal(result$x, seq(0, 1, length.out = 5))
  expect_equal(result$ymin, pmax(0, result$x - eps))
  expect_equal(result$ymax, pmin(1, result$x + eps))

  custom <- StatPPPlotBand$compute_group(
    data = data.frame(x = 1:5),
    scales = list(),
    level = 0.8,
    band_n = 3,
    a = 1 / 2
  )
  eps_custom <- sqrt(log(2 / (1 - 0.8)) / (2 * 5))
  expect_equal(custom$ymax, pmin(1, custom$x + eps_custom))
})

test_that("SP DKW band transforms the probability-scale limits", {
  result <- StatSPPlotBand$compute_group(
    data = data.frame(x = 1:5),
    scales = list(),
    level = 0.95,
    band_n = 5,
    a = 1 / 2
  )
  eps <- sqrt(log(2 / (1 - 0.95)) / (2 * 5))
  p_grid <- seq(0, 1, length.out = 5)

  expect_equal(result$x, ggfunction:::sp_transform(p_grid))
  expect_equal(result$ymin, ggfunction:::sp_transform(pmax(0, p_grid - eps)))
  expect_equal(result$ymax, ggfunction:::sp_transform(pmin(1, p_grid + eps)))
})

test_that("QQ DKW band computes on a finite probability grid", {
  result <- StatQQPlotBand$compute_group(
    data = data.frame(x = 1:5),
    scales = list(),
    fun = qnorm,
    level = 0.95,
    band_n = 5,
    a = 1 / 2,
    args = list()
  )
  observed_p <- stats::ppoints(5, a = 1 / 2)
  p_grid <- seq(
    min(observed_p) / 2,
    1 - (1 - max(observed_p)) / 2,
    length.out = 5
  )
  eps <- sqrt(log(2 / (1 - 0.95)) / (2 * 5))

  expect_equal(result$p, p_grid)
  expect_equal(result$x, qnorm(p_grid))
  expect_equal(result$ymin, qnorm(pmax(0, p_grid - eps)))
  expect_equal(result$ymax, qnorm(pmin(1, p_grid + eps)))
})

test_that("geom_qqplot ribbon extends past points without training scales", {
  set.seed(1)
  df <- data.frame(x = rnorm(100))

  with_band <- ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_qqplot(fun = qnorm, identity_line = FALSE)
  )
  without_band <- ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_qqplot(fun = qnorm, conf_int = FALSE, identity_line = FALSE)
  )

  band <- with_band$data[[1]]
  points <- with_band$data[[2]]

  expect_equal(
    with_band$layout$panel_params[[1]]$y.range,
    without_band$layout$panel_params[[1]]$y.range
  )
  expect_equal(
    with_band$layout$panel_params[[1]]$x.range,
    without_band$layout$panel_params[[1]]$x.range
  )
  expect_lt(min(band$qq_x), min(points$x))
  expect_gt(max(band$qq_x), max(points$x))
})

test_that("geom_ppplot and geom_qqplot build without error", {
  df <- data.frame(x = rnorm(30))

  p_pp <- ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm)
  p_sp <- ggplot(df, aes(x = x)) + geom_spplot(fun = pnorm)
  p_qq <- ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm)

  expect_s3_class(p_pp, "gg")
  expect_s3_class(p_sp, "gg")
  expect_s3_class(p_qq, "gg")
  expect_silent(ggplot_build(p_pp))
  expect_silent(ggplot_build(p_sp))
  expect_silent(ggplot_build(p_qq))
})

test_that("geom_ppplot and geom_qqplot build without confidence bands", {
  df <- data.frame(x = rnorm(30))

  p_pp <- ggplot(df, aes(x = x)) +
    geom_ppplot(fun = pnorm, conf_int = FALSE)
  p_sp <- ggplot(df, aes(x = x)) +
    geom_spplot(fun = pnorm, conf_int = FALSE)
  p_qq <- ggplot(df, aes(x = x)) +
    geom_qqplot(fun = qnorm, conf_int = FALSE)

  expect_silent(ggplot_build(p_pp))
  expect_silent(ggplot_build(p_sp))
  expect_silent(ggplot_build(p_qq))
})

test_that("diagnostic ribbons use neutral geom_smooth-like defaults", {
  df <- data.frame(x = c(-1.4, -0.6, -0.2, 0.3, 0.8, 1.6))
  plots <- list(
    ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm),
    ggplot(df, aes(x = x)) + geom_spplot(fun = pnorm),
    ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm)
  )

  for (p in plots) {
    band <- ggplot_build(p)$data[[1]]
    expect_equal(unique(band$fill), "grey60")
    expect_equal(unique(band$alpha), 0.4)
  }
})

test_that("geom_ppplot and geom_qqplot use ggplot2 default colour scale", {
  df <- data.frame(x = rnorm(30))

  p_pp <- ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm)
  p_sp <- ggplot(df, aes(x = x)) + geom_spplot(fun = pnorm)
  p_qq <- ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm)
  built_pp <- ggplot_build(p_pp)
  built_sp <- ggplot_build(p_sp)
  built_qq <- ggplot_build(p_qq)

  expect_s3_class(built_pp$plot$scales$get_scales("colour"), "ScaleContinuous")
  expect_s3_class(built_sp$plot$scales$get_scales("colour"), "ScaleContinuous")
  expect_s3_class(built_qq$plot$scales$get_scales("colour"), "ScaleContinuous")
  expect_true(all(built_pp$data[[3]]$shape == 19))
  expect_true(all(built_sp$data[[3]]$shape == 19))
  expect_true(all(built_qq$data[[3]]$shape == 19))
})

test_that("geom_ppplot and geom_qqplot support fixed black colour and explicit spectral scales", {
  df <- data.frame(x = rnorm(30))

  fixed_pp <- ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm, colour = "black")
  )
  fixed_sp <- ggplot_build(
    ggplot(df, aes(x = x)) + geom_spplot(fun = pnorm, colour = "black")
  )
  fixed_qq <- ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm, colour = "black")
  )
  expect_null(fixed_pp$plot$scales$get_scales("colour"))
  expect_null(fixed_sp$plot$scales$get_scales("colour"))
  expect_null(fixed_qq$plot$scales$get_scales("colour"))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_ppplot(fun = pnorm) +
      scale_colour_gradientn(colors = grDevices::rainbow(10))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_spplot(fun = pnorm) +
      scale_colour_gradientn(colors = grDevices::rainbow(10))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_qqplot(fun = qnorm) +
      scale_colour_gradientn(colors = grDevices::rainbow(10))
  ))
})

test_that("geom_ppplot supports alternate CDF inputs", {
  df <- data.frame(x = rnorm(20))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(pdf_fun = dnorm)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(survival_fun = function(x) 1 - pnorm(x))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(qf_fun = qnorm)
  ))
})

test_that("geom_spplot supports alternate CDF inputs", {
  df <- data.frame(x = rnorm(20))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_spplot(pdf_fun = dnorm)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_spplot(survival_fun = function(x) 1 - pnorm(x))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_spplot(qf_fun = qnorm)
  ))
})

test_that("geom_qqplot supports alternate quantile inputs", {
  df <- data.frame(x = rnorm(20))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(cdf_fun = pnorm)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(pdf_fun = dnorm)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(survival_fun = function(x) 1 - pnorm(x))
  ))
})

test_that("geom_ppplot and geom_qqplot support hazard inputs", {
  df <- data.frame(x = rexp(10))
  exp_hazard <- function(x) rep(1, length(x))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(hf_fun = exp_hazard, hf_lower = 0)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_spplot(hf_fun = exp_hazard, hf_lower = 0)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(hf_fun = exp_hazard, hf_lower = 0)
  ))
})

test_that("geom_ppplot and geom_qqplot pass args to distribution functions", {
  df <- data.frame(x = rnorm(20, mean = 2, sd = 3))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_ppplot(fun = pnorm, args = list(mean = 2, sd = 3))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_spplot(fun = pnorm, args = list(mean = 2, sd = 3))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_qqplot(fun = qnorm, args = list(mean = 2, sd = 3))
  ))
})

test_that("geom_ppplot and geom_qqplot build with grouped data and ties", {
  df <- data.frame(
    x = c(1, 1, 2, 3, 1, 2, 2, 4),
    group = rep(c("A", "B"), each = 4)
  )

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x, group = group)) + geom_ppplot(fun = pnorm)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x, group = group)) + geom_spplot(fun = pnorm)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x, group = group)) + geom_qqplot(fun = qnorm)
  ))
})

test_that("geom_ppplot and geom_qqplot respect colour-based grouping", {
  df <- data.frame(
    x = c(rnorm(5), rnorm(5, mean = 2)),
    group = rep(c("A", "B"), each = 5)
  )

  pp_built <- ggplot_build(
    ggplot(df, aes(x = x, colour = group)) +
      geom_ppplot(
        fun = pnorm,
        conf_int = FALSE,
        identity_line = FALSE
      )
  )
  sp_built <- ggplot_build(
    ggplot(df, aes(x = x, colour = group)) +
      geom_spplot(
        fun = pnorm,
        conf_int = FALSE,
        identity_line = FALSE
      )
  )
  qq_built <- ggplot_build(
    ggplot(df, aes(x = x, colour = group)) +
      geom_qqplot(
        fun = qnorm,
        conf_int = FALSE,
        identity_line = FALSE
      )
  )

  expect_equal(length(unique(pp_built$data[[1]]$group)), 2)
  expect_equal(length(unique(sp_built$data[[1]]$group)), 2)
  expect_equal(length(unique(qq_built$data[[1]]$group)), 2)
})

test_that("geom_ppplot and geom_qqplot support custom mappings", {
  df <- data.frame(x = rnorm(20))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_ppplot(fun = pnorm, mapping = aes(alpha = after_stat(p)))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_spplot(fun = pnorm, mapping = aes(alpha = after_stat(p)))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_qqplot(fun = qnorm, mapping = aes(alpha = after_stat(p)))
  ))
})

test_that("geom_ppplot and geom_qqplot handle missing values with na.rm", {
  df <- data.frame(x = c(rnorm(10), NA))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm, na.rm = TRUE)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_spplot(fun = pnorm, na.rm = TRUE)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm, na.rm = TRUE)
  ))
})

test_that("geom_ppplot and geom_qqplot use default axis labels", {
  df <- data.frame(x = rnorm(20))

  p_pp <- ggplot(df, aes(x = x)) +
    geom_ppplot(fun = pnorm, conf_int = FALSE)
  p_sp <- ggplot(df, aes(x = x)) +
    geom_spplot(fun = pnorm, conf_int = FALSE)
  p_qq <- ggplot(df, aes(x = x)) +
    geom_qqplot(fun = qnorm, conf_int = FALSE)

  expect_equal(plot_axis_titles(p_pp), c(
    x = "Theoretical probabilities",
    y = "Observed probabilities"
  ))
  expect_equal(plot_axis_titles(p_sp), c(
    x = "Stabilized theoretical probabilities",
    y = "Stabilized observed probabilities"
  ))
  expect_equal(plot_axis_titles(p_qq), c(
    x = "Theoretical quantiles",
    y = "Observed quantiles"
  ))
})

test_that("geom_ppplot and geom_qqplot do not override explicit labels", {
  df <- data.frame(x = rnorm(20))

  p_pp <- ggplot(df, aes(x = x)) +
    labs(x = "prob", y = "cdf", colour = "value") +
    geom_ppplot(fun = pnorm, conf_int = FALSE)
  p_sp <- ggplot(df, aes(x = x)) +
    labs(x = "transformed", y = "cdf", colour = "value") +
    geom_spplot(fun = pnorm, conf_int = FALSE)
  p_qq <- ggplot(df, aes(x = x)) +
    labs(x = "theory", y = "sample", colour = "probability") +
    geom_qqplot(fun = qnorm, conf_int = FALSE)

  expect_equal(plot_axis_titles(p_pp), c(x = "prob", y = "cdf"))
  expect_equal(plot_axis_titles(p_sp), c(x = "transformed", y = "cdf"))
  expect_equal(plot_axis_titles(p_qq), c(x = "theory", y = "sample"))
  expect_equal(ggplot_build(p_pp)$plot$labels$colour, "value")
  expect_equal(ggplot_build(p_sp)$plot$labels$colour, "value")
  expect_equal(ggplot_build(p_qq)$plot$labels$colour, "probability")
})
