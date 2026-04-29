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
  p_grid <- seq(min(stats::ppoints(5, a = 1 / 2)),
                max(stats::ppoints(5, a = 1 / 2)),
                length.out = 5)
  eps <- sqrt(log(2 / (1 - 0.95)) / (2 * 5))

  expect_equal(result$p, p_grid)
  expect_equal(result$x, qnorm(p_grid))
  expect_equal(result$ymin, qnorm(pmax(0, p_grid - eps)))
  expect_equal(result$ymax, qnorm(pmin(1, p_grid + eps)))
})

test_that("geom_ppplot and geom_qqplot build without error", {
  df <- data.frame(x = rnorm(30))

  p_pp <- ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm)
  p_qq <- ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm)

  expect_s3_class(p_pp, "gg")
  expect_s3_class(p_qq, "gg")
  expect_silent(ggplot_build(p_pp))
  expect_silent(ggplot_build(p_qq))
})

test_that("geom_ppplot and geom_qqplot build without confidence bands", {
  df <- data.frame(x = rnorm(30))

  p_pp <- ggplot(df, aes(x = x)) +
    geom_ppplot(fun = pnorm, conf_int = FALSE)
  p_qq <- ggplot(df, aes(x = x)) +
    geom_qqplot(fun = qnorm, conf_int = FALSE)

  expect_silent(ggplot_build(p_pp))
  expect_silent(ggplot_build(p_qq))
})

test_that("geom_ppplot and geom_qqplot use ggplot2 default fill scale", {
  df <- data.frame(x = rnorm(30))

  p_pp <- ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm)
  p_qq <- ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm)

  expect_s3_class(ggplot_build(p_pp)$plot$scales$get_scales("fill"), "ScaleContinuous")
  expect_s3_class(ggplot_build(p_qq)$plot$scales$get_scales("fill"), "ScaleContinuous")
})

test_that("geom_ppplot and geom_qqplot support fixed black fill and explicit spectral scales", {
  df <- data.frame(x = rnorm(30))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm, fill = "black")
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm, fill = "black")
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_ppplot(fun = pnorm) +
      scale_fill_gradientn(colors = grDevices::rainbow(10))
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) +
      geom_qqplot(fun = qnorm) +
      scale_fill_gradientn(colors = grDevices::rainbow(10))
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
  qq_built <- ggplot_build(
    ggplot(df, aes(x = x, colour = group)) +
      geom_qqplot(
        fun = qnorm,
        conf_int = FALSE,
        identity_line = FALSE
      )
  )

  expect_equal(length(unique(pp_built$data[[1]]$group)), 2)
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
      geom_qqplot(fun = qnorm, mapping = aes(alpha = after_stat(p)))
  ))
})

test_that("geom_ppplot and geom_qqplot handle missing values with na.rm", {
  df <- data.frame(x = c(rnorm(10), NA))

  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_ppplot(fun = pnorm, na.rm = TRUE)
  ))
  expect_silent(ggplot_build(
    ggplot(df, aes(x = x)) + geom_qqplot(fun = qnorm, na.rm = TRUE)
  ))
})

test_that("geom_ppplot and geom_qqplot use default axis labels", {
  df <- data.frame(x = rnorm(20))

  p_pp <- ggplot(df, aes(x = x)) +
    geom_ppplot(fun = pnorm, conf_int = FALSE)
  p_qq <- ggplot(df, aes(x = x)) +
    geom_qqplot(fun = qnorm, conf_int = FALSE)

  expect_equal(plot_axis_titles(p_pp), c(
    x = "Theoretical probabilities",
    y = "Observed probabilities"
  ))
  expect_equal(plot_axis_titles(p_qq), c(
    x = "Theoretical quantiles",
    y = "Observed quantiles"
  ))
})

test_that("geom_ppplot and geom_qqplot do not override explicit labels", {
  df <- data.frame(x = rnorm(20))

  p_pp <- ggplot(df, aes(x = x)) +
    labs(x = "prob", y = "cdf", fill = "value") +
    geom_ppplot(fun = pnorm, conf_int = FALSE)
  p_qq <- ggplot(df, aes(x = x)) +
    labs(x = "theory", y = "sample", fill = "probability") +
    geom_qqplot(fun = qnorm, conf_int = FALSE)

  expect_equal(plot_axis_titles(p_pp), c(x = "prob", y = "cdf"))
  expect_equal(plot_axis_titles(p_qq), c(x = "theory", y = "sample"))
  expect_equal(ggplot_build(p_pp)$plot$labels$fill, "value")
  expect_equal(ggplot_build(p_qq)$plot$labels$fill, "probability")
})
