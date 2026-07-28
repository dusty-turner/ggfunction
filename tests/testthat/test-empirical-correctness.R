# Empirical/KM/Nelson-Aalen and diagnostic correctness: confidence-band
# domains, observation-domain anchors, band caps, and raw diagnostic inputs.

test_that("the equal-precision KM band has no false terminal interval", {
  d <- data.frame(
    x = 1:5,
    status = rep(1L, 5),
    group = 1
  )

  band <- StatECDFKMBand$compute_group(
    d,
    scales = list(x = NULL, y = NULL),
    level = 0.95,
    ep_range = c(0.2, 0.8)
  )

  expect_gt(nrow(band), 0)
  reference <- band[
    !band$domain_anchor &
      !duplicated(band$x, fromLast = TRUE),
    c("x", "ymin", "ymax")
  ]
  expect_equal(reference$x, 1:4)
  expect_false(any(band$x == 5 & band$ymin == 0 & band$ymax == 0))
  expect_true(all(band$ymax >= band$ymin))
  expect_equal(
    reference$ymin,
    c(0.2807085, 0, 0, 0),
    tolerance = 1e-6
  )
  expect_equal(
    reference$ymax,
    c(1, 1, 1, 0.7192915),
    tolerance = 1e-6
  )

  anchor <- band[band$domain_anchor, , drop = FALSE]
  expect_equal(unique(anchor$x), 5)
  expect_true(all(!anchor$jump))
  expect_equal(unique(anchor$ymin), 0, tolerance = 1e-6)
  expect_equal(unique(anchor$ymax), 0.7192915, tolerance = 1e-6)
})

test_that("the default plug-in band excludes the singular terminal row", {
  d <- data.frame(x = 1:5, status = rep(1L, 5), group = 1)
  band <- StatECDFKMBand$compute_group(
    d, scales = list(x = NULL, y = NULL), level = 0.95
  )
  expect_false(any(band$x == 5 & band$ymin == 0 & band$ymax == 0))
  expect_true(all(band$ymax >= band$ymin))
  anchor <- band[band$domain_anchor, , drop = FALSE]
  expect_equal(unique(anchor$x), 5)
})

test_that("the EP critical value matches its reference and rejects bad domains", {
  expect_equal(.ep_critical_value(0.1, 0.8, 0.05), 2.986739, tolerance = 1e-5)
  expect_equal(.ep_critical_value(0.2, 0.8, 0.05), 2.902927, tolerance = 1e-5)
  expect_error(.ep_critical_value(0.8, 0.2, 0.05), "0 < a_L < a_U < 1")
  expect_error(.ep_critical_value(0.5, 0.5, 0.05), "0 < a_L < a_U < 1")
  expect_error(validate_ep_range(c(0.8, 0.2)), "ep_range")
  expect_error(validate_ep_range(c(0, 0.5)), "ep_range")
  expect_error(geom_ecdf_km(ep_range = c(0.9, 0.1)), "ep_range")
})

test_that("degenerate data omit the band with a warning, never qnorm", {
  d <- data.frame(x = c(2, 2), status = c(1L, 1L), group = 1)
  expect_warning(
    band <- StatECDFKMBand$compute_group(
      d, scales = list(x = NULL, y = NULL), level = 0.95
    ),
    "No valid domain"
  )
  expect_equal(nrow(band), 0)
})

test_that("nonterminal Greenwood errors match survival::survfit", {
  skip_if_not_installed("survival")
  set.seed(7)
  time <- round(rexp(30, 0.4), 2)
  status <- rbinom(30, 1, 0.7)
  tab <- .tabulate_km(time, status, na.rm = FALSE)
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1, conf.type = "plain"
  )
  fit_events <- fit$n.event > 0
  nonterminal <- is.finite(tab$var_surv)
  expect_equal(tab$surv[nonterminal], fit$surv[fit_events][nonterminal],
               tolerance = 1e-10)
  expect_equal(
    sqrt(tab$var_surv[nonterminal]),
    fit$std.err[fit_events][nonterminal] * fit$surv[fit_events][nonterminal],
    tolerance = 1e-8
  )
})

test_that("curves extend through trailing follow-up without a jump", {
  d <- data.frame(time = c(1, 10), status = c(1L, 0L))

  b <- ggplot_build(
    ggplot(d, aes(time, status = status)) +
      geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  )$data[[1]]
  expect_equal(max(b$x), 10)
  expect_equal(tail(b$y, 1), 0.5)
  expect_false(tail(b$jump, 1))

  b_na <- ggplot_build(
    ggplot(d, aes(time, status = status)) +
      geom_echf_na(conf_int = FALSE)
  )$data[[1]]
  expect_equal(max(b_na$x), 10)
  expect_equal(tail(b_na$y, 1), 0.5)
  expect_false(tail(b_na$jump, 1))
})

test_that("all-censored data keep the observation domain", {
  d <- data.frame(time = 1:3, status = 0L)

  km <- ggplot_build(
    ggplot(d, aes(time, status = status)) +
      geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  )$data[[1]]
  expect_gt(nrow(km), 0)
  expect_true(all(km$y == 1))
  expect_equal(range(km$x), c(1, 3))
  expect_true(all(!km$jump))

  na <- ggplot_build(
    ggplot(d, aes(time, status = status)) +
      geom_echf_na(conf_int = FALSE)
  )$data[[1]]
  expect_true(all(na$y == 0))
  expect_true(all(!na$jump))
})

test_that("anchors draw no vertical segments or event points", {
  d <- data.frame(time = c(1, 10), status = c(1L, 0L))
  p <- ggplot(d, aes(time, status = status)) +
    geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  segs <- layer_grobs(p, 1, "segments")
  # Vertical jump segments: x == xend. Only the single event at t = 1 jumps.
  verticals <- 0L
  for (g in segs) {
    verticals <- verticals + sum(abs(as.numeric(g$x0) - as.numeric(g$x1)) < 1e-9)
  }
  expect_equal(verticals, 1L)

  # All-censored: no jumps at all.
  d2 <- data.frame(time = 1:3, status = 0L)
  p2 <- ggplot(d2, aes(time, status = status)) +
    geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  segs2 <- layer_grobs(p2, 1, "segments")
  verticals2 <- 0L
  for (g in segs2) {
    verticals2 <- verticals2 + sum(abs(as.numeric(g$x0) - as.numeric(g$x1)) < 1e-9)
  }
  expect_equal(verticals2, 0L)
})

test_that("bands extend through late censoring without new inference", {
  d <- data.frame(
    time = c(1, 2, 3, 4, 10),
    status = c(1L, 1L, 1L, 1L, 0L)
  )
  b <- ggplot_build(
    ggplot(d, aes(time, status = status)) +
      geom_ecdf_km(censor_marks = FALSE, ep_range = c(0.2, 0.8))
  )
  band <- b$data[[1]]
  expect_equal(max(band$x), 10)
  last_two <- tail(band, 2)
  expect_equal(last_two$ymin[1], last_two$ymin[2])
  expect_equal(last_two$ymax[1], last_two$ymax[2])
})

test_that("each grouped curve stops at its own follow-up", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 3),
    time = c(1, 1.5, 2, 1, 5, 10),
    status = c(1L, 1L, 0L, 1L, 1L, 0L)
  )
  b <- ggplot_build(
    ggplot(d, aes(time, status = status, colour = g)) +
      geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  )$data[[1]]
  expect_equal(max(b$x[b$group == 1]), 2)
  expect_equal(max(b$x[b$group == 2]), 10)
})

test_that("censor marks do not change the trained x domain", {
  d <- data.frame(time = c(1, 2, 3, 10), status = c(1L, 1L, 1L, 0L))
  p_with <- ggplot(d, aes(time, status = status)) +
    geom_ecdf_km(conf_int = FALSE, censor_marks = TRUE)
  p_without <- ggplot(d, aes(time, status = status)) +
    geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  expect_equal(plot_x_range(p_with), plot_x_range(p_without))
})

# --- band_max semantics ---

test_that("band_max = Inf keeps genuinely unbounded upper ECHF bounds", {
  d <- data.frame(x = 1:10)

  unbounded <- StatECHFBand$compute_group(
    d, scales = list(x = NULL, y = NULL), band_max = Inf
  )
  expect_true(any(is.infinite(unbounded$ymax)))

  default <- suppressMessages(StatECHFBand$compute_group(
    d, scales = list(x = NULL, y = NULL), band_max = NULL
  ))
  expect_true(all(is.finite(default$ymax)))
  expect_equal(max(default$ymax), log(20), tolerance = 1e-12)

  finite <- StatECHFBand$compute_group(
    d, scales = list(x = NULL, y = NULL), band_max = 0.5
  )
  expect_lte(max(finite$ymax), 0.5)
  expect_true(all(finite$ymin <= finite$ymax))
  expect_true(all(finite$ymin <= 0.5))
})

test_that("band_max is validated", {
  d <- data.frame(x = 1:10)
  for (bad in list(-1, NA_real_, NaN, c(1, 2), "a")) {
    expect_error(
      StatECHFBand$compute_group(
        d, scales = list(x = NULL, y = NULL), band_max = bad
      ),
      "band_max"
    )
  }
})

test_that("infinite upper bounds render at the visible panel edge", {
  d <- data.frame(x = 1:10)
  p <- ggplot(d, aes(x = x)) + geom_echf(band_max = Inf)
  g <- ggplotGrob(p)
  polys <- find_grobs(g, c("polygon", "ribbon"))
  expect_gt(length(polys), 0)
  ys <- unlist(lapply(polys, function(gr) as.numeric(gr$y)))
  expect_true(all(is.finite(ys)))
  expect_gte(max(ys), 0.99)
})

# --- raw diagnostic samples under transformed output scales ---

test_that("QQ diagnostics keep raw samples under log output scales", {
  d <- data.frame(value = c(1, 10, 100))
  b <- ggplot_build(
    ggplot(d, aes(sample = value)) +
      geom_qqplot(
        fun = qlnorm,
        a = 0.5,
        conf_int = FALSE,
        identity_line = FALSE
      ) +
      scale_x_log10() +
      scale_y_log10()
  )$data[[1]]

  expect_equal(b$sample, c(1, 10, 100))
  expect_equal(b$p, c(1 / 6, 1 / 2, 5 / 6))
  expect_equal(b$y, c(0, 1, 2))
  expect_equal(
    b$x,
    qnorm(ppoints(3, a = 0.5)) / log(10),
    tolerance = 1e-12
  )
})

test_that("PP and SP raw computed columns are identical on identity and log scales", {
  d <- data.frame(value = qlnorm(seq(0.1, 0.9, length.out = 9)))
  raw_cols <- c("p", "theoretical", "observed", "sample")

  for (ctor in list(
    function(...) geom_ppplot(..., fun = plnorm, conf_int = FALSE, identity_line = FALSE),
    function(...) geom_spplot(..., fun = plnorm, conf_int = FALSE, identity_line = FALSE)
  )) {
    b_id <- ggplot_build(ggplot(d, aes(sample = value)) + ctor())
    b_log <- ggplot_build(
      ggplot(d, aes(sample = value)) + ctor() + scale_y_log10()
    )
    i <- which(vapply(b_id$data, function(df) "sample" %in% names(df), logical(1)))[1]
    expect_equal(b_id$data[[i]][raw_cols], b_log$data[[i]][raw_cols])
  }
})

test_that("legacy aes(x=) input warns on identity scales and aborts on transforms", {
  d <- data.frame(x = rlnorm(12))
  expect_warning(
    ggplot_build(
      ggplot(d, aes(x = x)) + geom_ppplot(fun = plnorm, conf_int = FALSE)
    ),
    "deprecated"
  )
  expect_error(
    ggplot_build(
      ggplot(d, aes(x = x)) +
        geom_ppplot(fun = plnorm, conf_int = FALSE) +
        scale_x_log10()
    ),
    "sample"
  )
})

test_that("discrete-null point diagnostics work without null_type", {
  d <- data.frame(x = rep(0:1, each = 50))
  expect_no_error(
    ggplot_build(
      ggplot(d, aes(sample = x)) +
        geom_ppplot(
          fun = function(x) pbinom(x, 1, 0.5),
          null_type = "discrete",
          conf_int = FALSE
        )
    )
  )
  expect_error(
    geom_ppplot(
      fun = function(x) pbinom(x, 1, 0.5),
      null_type = "discrete",
      conf_int = TRUE
    ),
    "continuous"
  )
  expect_error(geom_ppplot(fun = pnorm, conf_int = TRUE), "null_type")
  expect_error(geom_spplot(fun = pnorm, conf_int = TRUE), "null_type")
  expect_error(
    geom_spplot(fun = pnorm, null_type = "discrete", conf_int = TRUE),
    "continuous"
  )
})
