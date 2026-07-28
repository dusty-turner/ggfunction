test_that("StatCDF computes correct CDF values", {
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = pnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expect_true(result$y[1] < 0.01)
  expect_true(result$y[101] > 0.99)
  # CDF should be monotonically non-decreasing
  expect_true(all(diff(result$y) >= 0))
})

test_that("geom_cdf builds a ggplot without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf uses x and p as default axis labels", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3))
  expect_equal(plot_axis_titles(p), c(x = "x", y = "p"))
})

test_that("geom_cdf with p shading builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf with p_lower/p_upper builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p_lower = 0.025, p_upper = 0.975)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf with lower.tail=FALSE builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf with args builds without error", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-5, 15), args = list(mean = 5, sd = 2))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_cdf warns for invalid CDF", {
  bad_cdf <- function(x) x / 10  # not a real CDF
  p <- ggplot() + geom_cdf(fun = bad_cdf, xlim = c(0, 5))
  expect_message(ggplotGrob(p), "valid CDF")
})

test_that("geom_cdf can suppress endpoint diagnostics", {
  bad_cdf <- function(x) x / 10
  p <- ggplot() + geom_cdf(fun = bad_cdf, xlim = c(0, 5), check = FALSE)
  expect_silent(ggplotGrob(p))
})

test_that("geom_cdf aligns inherited range with ECDF panel range", {
  set.seed(1234)
  df <- data.frame(x = rnorm(50))
  p <- ggplot(df, aes(x)) +
    geom_rug() +
    geom_ecdf(conf_int = FALSE) +
    geom_cdf(fun = pnorm, color = "purple")

  expect_silent(ggplotGrob(p))

  build <- ggplot_build(p)
  f <- make_cdf_function(fun = pnorm)
  panel_data <- cdf_panel_data(build$data[[3]], build$layout$panel_params[[1]], f, 101)
  expect_equal(range(panel_data$x), build$layout$panel_params[[1]]$x.range, tolerance = 1e-12)
})

# --- Alternate input: pdf_fun ---

test_that("StatCDF computes CDF from pdf_fun (dnorm)", {
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    pdf_fun = dnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_cdf with pdf_fun builds without error", {
  p <- ggplot() + geom_cdf(pdf_fun = dnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDF errors when both fun and pdf_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = pnorm,
      pdf_fun = dnorm,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*pdf_fun.*survival_fun.*qf_fun.*hf_fun"
  )
})

test_that("StatCDF errors when neither fun nor pdf_fun provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*pdf_fun.*survival_fun.*qf_fun.*hf_fun"
  )
})

# --- Alternate input: survival_fun ---

test_that("StatCDF computes CDF from survival_fun", {
  s_norm <- function(x) 1 - pnorm(x)
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    survival_fun = s_norm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-10)
})

test_that("geom_cdf with survival_fun builds without error", {
  s_norm <- function(x) 1 - pnorm(x)
  p <- ggplot() + geom_cdf(survival_fun = s_norm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

# --- Alternate input: qf_fun ---

test_that("StatCDF computes CDF from qf_fun", {
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    qf_fun = qnorm,
    xlim = c(-3, 3),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pnorm(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("geom_cdf with qf_fun builds without error", {
  p <- ggplot() + geom_cdf(qf_fun = qnorm, xlim = c(-3, 3))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

# --- Alternate input: hf_fun ---

test_that("StatCDF computes CDF from hf_fun (exponential hazard)", {
  h_exp <- function(x) ifelse(x >= 0, 1, 0)  # rate = 1
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    hf_fun = h_exp,
    xlim = c(0, 5),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expected <- pexp(result$x)
  expect_equal(result$y, expected, tolerance = 1e-3)
})

test_that("StatCDF computes CDF from finite-support Weibull hazard", {
  h_weibull <- function(x, shape, scale) (shape / scale) * (x / scale)^(shape - 1)
  scales <- list(x = NULL)
  result <- StatCDF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    hf_fun = h_weibull,
    hf_lower = 0,
    xlim = c(0.01, 5),
    n = 101,
    args = list(shape = 2, scale = 1)
  )
  expect_equal(result$y, pweibull(result$x, shape = 2, scale = 1), tolerance = 1e-3)
})

test_that("geom_cdf with hf_fun builds without error", {
  h_exp <- function(x) ifelse(x >= 0, 1, 0)
  p <- ggplot() + geom_cdf(hf_fun = h_exp, xlim = c(0, 5))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatCDF errors when multiple sources provided", {
  scales <- list(x = NULL)
  expect_error(
    StatCDF$compute_group(
      data = data.frame(group = 1),
      scales = scales,
      fun = pnorm,
      survival_fun = function(x) 1 - pnorm(x),
      xlim = c(-3, 3),
      n = 101,
      args = list()
    ),
    "fun.*pdf_fun.*survival_fun.*qf_fun.*hf_fun"
  )
})

# --- B-01: finite default ranges with documented precedence ---

test_that("function-only CDF layers get a finite default range (B-01)", {
  expect_no_warning(
    b <- ggplot_build(ggplot() + geom_cdf(fun = pnorm))
  )
  d <- b$data[[1]]
  expect_equal(nrow(d), 101)
  expect_true(all(is.finite(d$x)))
  expect_equal(range(d$x_eval), c(0, 1))
  expect_equal(d$cdf, pnorm(d$x_eval))
  expect_equal(d$p, d$cdf)
})

test_that("finite support supplies the default range for untrained scales (B-01)", {
  d <- ggplot_build(
    ggplot() + geom_cdf(fun = pnorm, support = c(-3, 3), n = 3)
  )$data[[1]]
  expect_equal(range(d$x_eval), c(-3, 3))
})

test_that("a trained scale window outranks a wider declared support (B-01)", {
  d <- ggplot_build(
    ggplot(data.frame(x = c(-1, 1)), aes(x = x)) +
      geom_point(y = 0.5) +
      geom_cdf(fun = pnorm, support = c(-10, 10), n = 3)
  )$data[[2]]
  expect_lte(max(abs(range(d$x_eval))), 1.5)
})

test_that("log-x fallback evaluates over raw c(1, 10), reverse over data space (B-01)", {
  log_data <- ggplot_build(
    ggplot() + geom_cdf(fun = pnorm, n = 2) + scale_x_log10()
  )$data[[1]]
  expect_equal(log_data$x, c(0, 1))
  expect_equal(log_data$x_eval, c(1, 10))

  reverse_data <- ggplot_build(
    ggplot() + geom_cdf(fun = pnorm, xlim = c(-1, 1), n = 3) + scale_x_reverse()
  )$data[[1]]
  expect_equal(reverse_data$x, c(1, 0, -1))
  expect_equal(reverse_data$x_eval, c(-1, 0, 1))
})

test_that("malformed xlim aborts in the constructor (B-01)", {
  expect_error(geom_cdf(fun = pnorm, xlim = c(1, -1)), "increasing")
  expect_error(geom_cdf(fun = pnorm, xlim = c(0, NA)), "increasing")
})

# --- B-02: shading from raw probabilities in the Stat ---

test_that("CDF shading boundaries are exact quantiles, independent of grid (B-02)", {
  expected_boundary <- qnorm(0.3)
  for (n in c(3, 4, 101)) {
    d <- ggplot_build(
      ggplot() +
        geom_cdf(fun = pnorm, xlim = c(-3, 3), n = n, p = 0.3)
    )$data[[1]]
    expect_equal(
      unique(stats::na.omit(d$shade_x_upper_raw)),
      expected_boundary,
      tolerance = 1e-8
    )
    expect_true(any(abs(d$x_eval - expected_boundary) < 1e-8))
  }
})

test_that("two-sided CDF shading boundaries are exact (B-02)", {
  d_pair <- ggplot_build(
    ggplot() +
      geom_cdf(fun = pnorm, xlim = c(-3, 3), p_lower = 0.025, p_upper = 0.975)
  )$data[[1]]
  expect_equal(
    unique(stats::na.omit(d_pair$shade_x_lower_raw)),
    qnorm(0.025),
    tolerance = 1e-8
  )
  expect_equal(
    unique(stats::na.omit(d_pair$shade_x_upper_raw)),
    qnorm(0.975),
    tolerance = 1e-8
  )
})

test_that("CDF shade boundaries are independent of the y scale (B-02)", {
  d_id <- ggplot_build(
    ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.3)
  )$data[[1]]
  d_log <- ggplot_build(
    ggplot() + geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.3) + scale_y_log10()
  )$data[[1]]
  expect_equal(
    unique(stats::na.omit(d_log$shade_x_upper_raw)),
    unique(stats::na.omit(d_id$shade_x_upper_raw))
  )

  p_log <- ggplot() +
    geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.3) +
    scale_y_log10()
  w <- testthat::capture_warnings(g <- ggplotGrob(p_log))
  expect_length(w, 1)
  expect_match(w, "baseline", ignore.case = TRUE)
  polys <- find_grobs(g, "polygon")
  expect_gt(length(polys), 0)
  for (poly in polys) {
    expect_true(all(is.finite(as.numeric(poly$x))))
    expect_true(all(is.finite(as.numeric(poly$y))))
  }
})

test_that("out-of-window shading boundary is retained but not drawn (B-02)", {
  built <- ggplot_build(
    ggplot() +
      geom_cdf(fun = pnorm, xlim = c(-1, 1), n = 11, p = 0.9999) +
      scale_x_continuous(expand = expansion(mult = 0))
  )
  d <- built$data[[1]]

  expect_equal(
    unique(stats::na.omit(d$shade_x_upper_raw)),
    qnorm(0.9999),
    tolerance = 1e-8
  )
  expect_equal(max(d$x_eval), 1)
  expect_lte(max(d$x), 1)
  expect_equal(
    built$layout$panel_params[[1]]$x.range,
    c(-1, 1),
    tolerance = 1e-12
  )

  # The shading polygon must stay inside the panel viewport.
  p <- ggplot() +
    geom_cdf(fun = pnorm, xlim = c(-1, 1), n = 11, p = 0.9999) +
    scale_x_continuous(expand = expansion(mult = 0))
  polys <- layer_grobs(p, 1, "polygon")
  expect_gt(length(polys), 0)
  for (poly in polys) {
    xs <- as.numeric(poly$x)
    expect_true(all(xs >= -1e-9 & xs <= 1 + 1e-9))
  }
})

test_that("probability axis trains on the mathematical endpoints (C-05)", {
  p <- ggplot() + geom_cdf(fun = pnorm, xlim = c(-1, 1))
  rng <- plot_y_range(p)
  expect_lte(rng[1], 0)
  expect_gte(rng[2], 1)
})

test_that("invalid probability arguments abort at construction (B-05)", {
  expect_error(geom_cdf(fun = pnorm, p = 1.1), "between 0 and 1")
  expect_error(geom_cdf(fun = pnorm, p_lower = 0.5), "together")
  expect_error(
    geom_cdf(fun = pnorm, p_lower = 0.9, p_upper = 0.1),
    "less than"
  )
})
