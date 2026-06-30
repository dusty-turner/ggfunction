# ── .tabulate_km ─────────────────────────────────────────────────────────────

test_that(".tabulate_km returns correct structure for uncensored data", {
  # 5 observations, all events (no censoring)
  tab <- ggfunction:::.tabulate_km(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 1, 1, 1),
    na.rm  = FALSE
  )
  expect_equal(tab$time, c(1, 2, 3, 4, 5))
  expect_equal(tab$n_risk, c(5, 4, 3, 2, 1))
  expect_equal(tab$n_event, c(1, 1, 1, 1, 1))
  expect_equal(tab$surv, cumprod(1 - 1 / 5:1))
  expect_equal(tab$chf, cumsum(1 / 5:1))
  expect_equal(tab$n, rep(5L, 5))
})

test_that(".tabulate_km handles censoring correctly", {
  # times: 1(event), 2(censored), 3(event), 4(event)
  tab <- ggfunction:::.tabulate_km(
    time   = c(1, 2, 3, 4),
    status = c(1, 0, 1, 1),
    na.rm  = FALSE
  )
  # Only event times returned: 1, 3, 4
  expect_equal(tab$time, c(1, 3, 4))
  # At t=1: 4 at risk, 1 event
  expect_equal(tab$n_risk[1], 4)
  expect_equal(tab$n_event[1], 1)
  # At t=3: 4 - 1(event at 1) - 1(censored at 2) = 2 at risk
  expect_equal(tab$n_risk[2], 2)
  # At t=4: 2 - 1(event at 3) = 1 at risk
  expect_equal(tab$n_risk[3], 1)
})

test_that(".tabulate_km handles tied event times", {
  tab <- ggfunction:::.tabulate_km(
    time   = c(1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1),
    na.rm  = FALSE
  )
  expect_equal(tab$time, c(1, 2, 3))
  expect_equal(tab$n_risk, c(5, 3, 1))
  expect_equal(tab$n_event, c(2, 1, 1))
  # S(1) = (1 - 2/5) = 0.6
  expect_equal(tab$surv[1], 3/5)
})

test_that(".tabulate_km returns empty for all-censored data", {
  tab <- ggfunction:::.tabulate_km(
    time   = c(1, 2, 3),
    status = c(0, 0, 0),
    na.rm  = FALSE
  )
  expect_equal(nrow(tab), 0)
})

test_that(".tabulate_km handles single observation (event)", {
  tab <- ggfunction:::.tabulate_km(
    time   = 5,
    status = 1,
    na.rm  = FALSE
  )
  expect_equal(tab$time, 5)
  expect_equal(tab$surv, 0)
  expect_equal(tab$chf, 1)
  expect_equal(tab$n_risk, 1)
})

test_that(".tabulate_km handles single observation (censored)", {
  tab <- ggfunction:::.tabulate_km(
    time   = 5,
    status = 0,
    na.rm  = FALSE
  )
  expect_equal(nrow(tab), 0)
})

test_that(".tabulate_km removes NAs with na.rm = TRUE", {
  tab <- ggfunction:::.tabulate_km(
    time   = c(1, NA, 3),
    status = c(1, 1, 1),
    na.rm  = TRUE
  )
  expect_equal(tab$time, c(1, 3))
  expect_equal(tab$n, rep(2L, 2))
})

test_that(".tabulate_km warns and drops missing values with na.rm = FALSE", {
  expect_warning(
    tab <- ggfunction:::.tabulate_km(
      time   = c(1, NA, 3),
      status = c(1, 1, NA),
      na.rm  = FALSE
    ),
    "missing or non-finite"
  )
  expect_equal(tab$time, 1)
  expect_equal(tab$n, 1L)
})

test_that(".tabulate_km validates status values", {
  expect_error(
    ggfunction:::.tabulate_km(
      time   = c(1, 2, 3),
      status = c(1, 2, 0),
      na.rm  = FALSE
    ),
    "0/1"
  )
})

test_that(".tabulate_km returns empty for empty input", {
  tab <- ggfunction:::.tabulate_km(
    time   = numeric(0),
    status = integer(0),
    na.rm  = FALSE
  )
  expect_equal(nrow(tab), 0)
})

test_that(".tabulate_km Greenwood variance is zero-guarded when n_i == d_i", {
  # Last observation: n=1, d=1 => n*(n-d) = 0
  tab <- ggfunction:::.tabulate_km(
    time   = c(1, 2),
    status = c(1, 1),
    na.rm  = FALSE
  )
  expect_true(all(is.finite(tab$var_surv)))
})


# ── .censoring_times ─────────────────────────────────────────────────────────

test_that(".censoring_times returns correct values", {
  ct <- ggfunction:::.censoring_times(
    time   = c(3, 1, 4, 2),
    status = c(1, 0, 0, 1),
    na.rm  = FALSE
  )
  expect_equal(ct, c(1, 4))
})

test_that(".censoring_times returns empty when all events", {
  ct <- ggfunction:::.censoring_times(
    time   = c(1, 2, 3),
    status = c(1, 1, 1),
    na.rm  = FALSE
  )
  expect_equal(length(ct), 0)
})


# ── Validation against survival::survfit ─────────────────────────────────────

test_that(".tabulate_km matches survival::survfit", {
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 100
  true_time <- rexp(n, rate = 0.5)
  cens_time <- rexp(n, rate = 0.2)
  obs_time  <- pmin(true_time, cens_time)
  status    <- as.integer(true_time <= cens_time)

  tab <- ggfunction:::.tabulate_km(obs_time, status, na.rm = FALSE)
  sf  <- survival::survfit(survival::Surv(obs_time, status) ~ 1)

  # survfit includes all unique times; .tabulate_km only event times
  evt <- sf$n.event > 0
  expect_equal(tab$time, sf$time[evt], tolerance = 1e-10)
  expect_equal(tab$surv, sf$surv[evt], tolerance = 1e-10)
  expect_equal(sqrt(tab$var_surv) / tab$surv, sf$std.err[evt], tolerance = 1e-10)
})


# ── StatECDFKM ───────────────────────────────────────────────────────────────

test_that("StatECDFKM computes correct values", {
  result <- StatECDFKM$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5), status = c(1, 1, 0, 1, 1)),
    scales = list(),
    na.rm = FALSE
  )
  # Events at 1, 2, 4, 5 (3 is censored)
  expect_true(nrow(result) > 0)
  expect_true(all(result$y >= 0 & result$y <= 1))
  expect_true(all(diff(result$y) <= 0))  # decreasing
})

test_that("StatECDFKM returns empty for no-event data", {
  result <- StatECDFKM$compute_group(
    data = data.frame(x = c(1, 2, 3), status = c(0, 0, 0)),
    scales = list(),
    na.rm = FALSE
  )
  expect_equal(nrow(result), 0)
})


# ── StatECHFNA ───────────────────────────────────────────────────────────────

test_that("StatECHFNA computes correct values", {
  result <- StatECHFNA$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5), status = c(1, 1, 0, 1, 1)),
    scales = list(),
    na.rm = FALSE
  )
  expect_true(nrow(result) > 0)
  expect_true(all(result$y >= 0))
  expect_true(all(diff(result$y) >= 0))  # increasing
})

test_that("StatECHFNA returns empty for no-event data", {
  result <- StatECHFNA$compute_group(
    data = data.frame(x = c(1, 2, 3), status = c(0, 0, 0)),
    scales = list(),
    na.rm = FALSE
  )
  expect_equal(nrow(result), 0)
})


# ── StatECDFKMBand ───────────────────────────────────────────────────────────

test_that("StatECDFKMBand computes valid band", {
  result <- StatECDFKMBand$compute_group(
    data = data.frame(
      x = c(1, 2, 3, 4, 5, 6, 7, 8),
      status = c(1, 1, 0, 1, 1, 0, 1, 1)
    ),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_true(nrow(result) > 0)
  expect_true(all(result$ymin >= 0))
  expect_true(all(result$ymax <= 1))
  expect_true(all(result$ymin <= result$ymax))
})

test_that("KM EP critical value uses Nair endpoints", {
  expect_equal(
    ggfunction:::.ep_critical_value(a_L = 0.1, a_U = 0.8, alpha = 0.05),
    2.986739,
    tolerance = 1e-6
  )
})

test_that("StatECDFKMBand returns empty for empty data", {
  result <- StatECDFKMBand$compute_group(
    data = data.frame(x = numeric(0), status = integer(0)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_equal(nrow(result), 0)
})


# ── StatECHFNABand ───────────────────────────────────────────────────────────

test_that("StatECHFNABand computes valid band", {
  result <- StatECHFNABand$compute_group(
    data = data.frame(
      x = c(1, 2, 3, 4, 5, 6, 7, 8),
      status = c(1, 1, 0, 1, 1, 0, 1, 1)
    ),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_true(nrow(result) > 0)
  expect_true(all(result$ymin >= 0))
  expect_true(all(result$ymin <= result$ymax))
})

test_that("StatECHFNABand uses pointwise normal critical values", {
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    status = c(1, 1, 0, 1, 1, 0, 1, 1)
  )
  result <- StatECHFNABand$compute_group(
    data = data,
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  tab <- ggfunction:::.tabulate_km(data$x, data$status, na.rm = FALSE)
  z <- qnorm(0.975)
  expected <- data.frame(
    x = tab$time,
    ymin = pmax(0, tab$chf - z * sqrt(tab$var_chf)),
    ymax = tab$chf + z * sqrt(tab$var_chf)
  )
  expected <- ggfunction:::.expand_step_ribbon(expected)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("StatECHFNABand returns empty for empty data", {
  result <- StatECHFNABand$compute_group(
    data = data.frame(x = numeric(0), status = integer(0)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_equal(nrow(result), 0)
})


# ── StatCensorMarks ──────────────────────────────────────────────────────────

test_that("StatCensorMarks returns correct positions", {
  result <- StatCensorMarks$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5), status = c(1, 0, 1, 0, 1)),
    scales = list(),
    na.rm = FALSE
  )
  # Censor times at 2 and 4

  expect_equal(result$x, c(2, 4))
  # y values should be S(t) at those times
  expect_true(all(result$y >= 0 & result$y <= 1))
  # S(2): the KM curve drops at t=1 (from 1 to 4/5), t=2 is censored so
  # S(2) should be the value after t=1
  expect_equal(result$y[1], 4/5)
})

test_that("StatCensorMarks returns empty when no censorings", {
  result <- StatCensorMarks$compute_group(
    data = data.frame(x = c(1, 2, 3), status = c(1, 1, 1)),
    scales = list(),
    na.rm = FALSE
  )
  expect_equal(nrow(result), 0)
})


# ── geom_ecdf_km smoke tests ────────────────────────────────────────────────

test_that("geom_ecdf_km builds without error", {
  set.seed(1)
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5, 6, 7, 8),
    status = c(1, 1, 0, 1, 0, 1, 1, 0)
  )
  p <- ggplot(df, aes(x = time, status = status)) + geom_ecdf_km()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf_km builds without conf_int", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_ecdf_km(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf_km builds without censor_marks", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_ecdf_km(censor_marks = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf_km builds with neither conf_int nor censor_marks", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf_km builds with grouped data", {
  set.seed(2)
  df <- data.frame(
    time   = c(rexp(20, 0.5), rexp(20, 1)),
    status = sample(0:1, 40, replace = TRUE, prob = c(0.3, 0.7)),
    group  = rep(c("A", "B"), each = 20)
  )
  p <- ggplot(df, aes(x = time, status = status, colour = group)) +
    geom_ecdf_km()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf_km handles all-censored data", {
  df <- data.frame(time = c(1, 2, 3), status = c(0, 0, 0))
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_ecdf_km()
  expect_s3_class(p, "gg")
  # May produce warnings about empty data but should not error
  expect_no_error(ggplot_build(p))
})

test_that("geom_ecdf_km handles single observation", {
  df <- data.frame(time = 5, status = 1)
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})


# ── geom_echf_na smoke tests ────────────────────────────────────────────────

test_that("geom_echf_na builds without error", {
  set.seed(1)
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5, 6, 7, 8),
    status = c(1, 1, 0, 1, 0, 1, 1, 0)
  )
  p <- ggplot(df, aes(x = time, status = status)) + geom_echf_na()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_echf_na draws interval bars by default", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5, 6, 7, 8),
    status = c(1, 1, 0, 1, 0, 1, 1, 0)
  )
  p <- ggplot(df, aes(x = time, status = status)) + geom_echf_na()

  expect_true(inherits(p$layers[[1]]$geom, "GeomErrorbar"))
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomCDFDiscrete")

  built <- ggplot_build(p)
  interval_data <- built$data[[1]]
  tab <- ggfunction:::.tabulate_km(df$time, df$status, na.rm = FALSE)

  expect_equal(interval_data$x, tab$time)
  expect_true(all(is.finite(interval_data$x)))
  expect_true(all(is.finite(interval_data$ymin)))
  expect_true(all(is.finite(interval_data$ymax)))
  expect_true(all(interval_data$ymin >= 0))
  expect_true(all(interval_data$ymin <= interval_data$ymax))
  expect_equal(unique(interval_data$colour), "grey60")
  expect_equal(unique(interval_data$alpha), 0.4)
  expect_equal(unique(interval_data$linewidth), 0.4)
  expect_equal(unique(interval_data$width), 0.02 * diff(range(tab$time)))
})

test_that("geom_echf_na interval bar style is configurable", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_echf_na(
      conf_colour = "grey50",
      conf_linewidth = 0.6,
      conf_alpha = 0.4,
      conf_width = 0.1
    )
  interval_data <- ggplot_build(p)$data[[1]]

  expect_equal(unique(interval_data$colour), "grey50")
  expect_equal(unique(interval_data$linewidth), 0.6)
  expect_equal(unique(interval_data$alpha), 0.4)
  expect_equal(unique(interval_data$width), 0.1)
})

test_that("geom_echf_na allows explicit capless interval bars", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_echf_na(conf_width = 0)
  interval_data <- ggplot_build(p)$data[[1]]

  expect_equal(unique(interval_data$width), 0)
})

test_that("geom_echf_na ribbon display remains available", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_echf_na(conf_geom = "ribbon")

  expect_equal(unname(vapply(p$layers, function(layer) class(layer$geom)[1], character(1))),
               c("GeomRibbon", "GeomCDFDiscrete"))

  built_band <- ggplot_build(p)$data[[1]][, c("x", "ymin", "ymax")]
  expected_band <- StatECHFNABand$compute_group(
    data = data.frame(x = df$time, status = df$status),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  row.names(built_band) <- NULL
  row.names(expected_band) <- NULL

  expect_equal(built_band, expected_band, tolerance = 1e-10)
})

test_that("geom_echf_na can suppress confidence display via conf_geom", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_echf_na(conf_geom = "none")

  expect_length(p$layers, 1L)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomCDFDiscrete")
})

test_that("geom_echf_na builds without conf_int", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_echf_na(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("other empirical confidence displays keep ribbon geoms", {
  km_df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  echf_df <- data.frame(x = c(0.3, 1.1, 1.7, 2.2, 3.4))
  diag_df <- data.frame(x = c(-1.2, -0.4, 0.2, 0.8, 1.6))

  km_plot <- ggplot(km_df, aes(x = time, status = status)) + geom_ecdf_km()
  echf_plot <- ggplot(echf_df, aes(x = x)) + geom_echf()
  pp_plot <- ggplot(diag_df, aes(x = x)) + geom_ppplot(fun = pnorm)

  expect_equal(class(km_plot$layers[[1]]$geom)[1], "GeomRibbon")
  expect_equal(class(echf_plot$layers[[1]]$geom)[1], "GeomRibbon")
  expect_true(any(vapply(pp_plot$layers, function(layer) class(layer$geom)[1],
                         character(1)) == "GeomRibbon"))
})

test_that("geom_ecdf_km ribbon uses neutral geom_smooth-like defaults", {
  df <- data.frame(
    time   = c(1, 2, 3, 4, 5),
    status = c(1, 1, 0, 1, 1)
  )
  band <- ggplot_build(
    ggplot(df, aes(x = time, status = status)) + geom_ecdf_km()
  )$data[[1]]

  expect_equal(unique(band$fill), "grey60")
  expect_equal(unique(band$alpha), 0.4)
})

test_that("geom_echf_na builds with grouped data", {
  set.seed(3)
  df <- data.frame(
    time   = c(rexp(20, 0.5), rexp(20, 1)),
    status = sample(0:1, 40, replace = TRUE, prob = c(0.3, 0.7)),
    group  = rep(c("A", "B"), each = 20)
  )
  p <- ggplot(df, aes(x = time, status = status, colour = group)) +
    geom_echf_na()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_echf_na handles all-censored data", {
  df <- data.frame(time = c(1, 2, 3), status = c(0, 0, 0))
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_echf_na()
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))
})

test_that("geom_echf_na handles single observation", {
  df <- data.frame(time = 5, status = 1)
  p <- ggplot(df, aes(x = time, status = status)) +
    geom_echf_na(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
