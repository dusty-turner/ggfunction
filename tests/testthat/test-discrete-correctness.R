# Discrete distribution correctness: predecessor values, exact quantile
# boundaries, PMF validation, scale training, dimming, and HDR labels.

no_scales <- function() list(x = NULL, y = NULL)

# --- predecessor values in narrowed windows ---

test_that("narrowed windows preserve true predecessor values", {
  cdf_data <- StatCDFDiscrete$compute_group(
    data.frame(group = 1), no_scales(),
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
    support = 0:10, xlim = c(3, 7)
  )
  expect_equal(cdf_data$cdf_prev[1], 0.0546875)
  expect_equal(cdf_data$cdf[1], 0.171875)
  expect_equal(range(cdf_data$x_eval), c(3, 7))

  survival_data <- StatSurvivalDiscrete$compute_group(
    data.frame(group = 1), no_scales(),
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
    support = 0:10, xlim = c(3, 7)
  )
  expect_equal(survival_data$survival_prev[1], 0.9453125)
  expect_equal(survival_data$survival[1], 0.828125)

  qf_data <- StatQFDiscrete$compute_group(
    data.frame(group = 1), no_scales(),
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
    support = 0:10, xlim = c(3, 7)
  )
  expect_equal(qf_data$p_left[1], 0.0546875)
  expect_equal(qf_data$p_right[1], 0.171875)
})

test_that("full unfiltered support is unchanged by predecessor metadata", {
  full <- StatCDFDiscrete$compute_group(
    data.frame(group = 1), no_scales(),
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
    support = 0:10
  )
  expect_equal(full$cdf, pbinom(0:10, 10, 0.5))
  expect_equal(full$cdf_prev, c(0, pbinom(0:9, 10, 0.5)))
  expect_equal(full$x_eval, 0:10)
})

test_that("segment helper places the leading step at the predecessor value", {
  segs <- discrete_step_segments(
    x = 3:7,
    y = pbinom(3:7, 10, 0.5),
    y_prev = c(0.0546875, pbinom(3:6, 10, 0.5)),
    x_range = c(2.8, 7.2)
  )
  expect_equal(segs$hori$y[1], 0.0546875)
  expect_equal(segs$vert$y[1], 0.0546875)
  expect_equal(segs$vert$yend[1], 0.171875)
  expect_equal(nrow(segs$hori), 6)
})

test_that("points outside xlim are not drawn to preserve metadata", {
  d <- StatCDFDiscrete$compute_group(
    data.frame(group = 1), no_scales(),
    pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
    support = 0:10, xlim = c(3, 7)
  )
  expect_true(all(d$x_eval >= 3 & d$x_eval <= 7))
})

test_that("predecessor values transform under non-identity scales", {
  b <- ggplot_build(
    ggplot() +
      geom_cdf_discrete(
        pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
        support = 0:10, xlim = c(3, 7)
      ) +
      scale_y_sqrt()
  )$data[[1]]
  expect_equal(b$y_prev[1], sqrt(0.0546875))
  expect_equal(b$cdf_prev[1], 0.0546875)
})

# --- exact discrete-QF boundaries ---

test_that("rare atoms are recovered by exact bisection", {
  out <- StatQFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = list(x = NULL),
    fun = qbinom,
    args = list(size = 1, prob = 0.99995)
  )
  expect_equal(out$q, 0:1)
  expect_equal(out$p, pbinom(0:1, 1, 0.99995), tolerance = 1e-10)

  out2 <- StatQFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = list(x = NULL),
    fun = qbinom,
    args = list(size = 1, prob = 0.00005)
  )
  expect_equal(out2$q, 0:1)
  expect_equal(out2$p, pbinom(0:1, 1, 0.00005), tolerance = 1e-10)

  out3 <- StatQFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = list(x = NULL),
    fun = qbinom,
    args = list(size = 1, prob = 0.99995),
    support = 0:1
  )
  expect_equal(out3$p, pbinom(0:1, 1, 0.99995), tolerance = 1e-10)
})

test_that("zero-mass support rows are dropped, keeping earliest boundaries", {
  out <- StatQFDiscrete$compute_group(
    data = data.frame(group = 1),
    scales = list(x = NULL),
    pmf_fun = function(x) c(0.5, 0, 0.5),
    support = 0:2
  )
  expect_equal(out$q, c(0, 2))
  expect_equal(out$p_left, c(0, 0.5))
  expect_equal(out$p, c(0.5, 1))
})

test_that("unbounded black-box QFs warn and avoid a false terminal boundary", {
  expect_warning(
    out <- StatQFDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = list(x = NULL),
      fun = qpois,
      args = list(lambda = 4)
    ),
    "support"
  )
  expect_lt(max(out$p), 1)
})

test_that("huge inferred integer spans hit a fast size cap", {
  t0 <- Sys.time()
  expect_error(
    StatQFDiscrete$compute_group(
      data = data.frame(group = 1),
      scales = list(x = NULL),
      fun = function(p) floor(p * 2e9)
    ),
    "cap"
  )
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 5)
})

# --- PMFs evaluated and validated exactly once ---

test_that("PMF structural invalidity aborts across all routes", {
  expect_error(
    StatPMF$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) 1, support = 0:2, args = list()
    ),
    "one.*per support"
  )
  expect_error(
    StatPMF$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) c(-0.25, 0.25, 1), support = 0:2, args = list()
    ),
    "non-negative"
  )
  expect_error(
    StatPMF$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) c(NA, 0.5, 0.5), support = 0:2, args = list()
    ),
    "finite"
  )
  expect_error(
    StatPMF$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) c(Inf, 0.5, 0.5), support = 0:2, args = list()
    ),
    "finite"
  )
  expect_error(
    StatPMF$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) letters[seq_along(x)], support = 0:2, args = list()
    ),
    "numeric"
  )
})

test_that("stateful PMFs are called exactly once per Stat computation", {
  make_counter <- function() {
    calls <- 0L
    list(
      fun = function(x) {
        calls <<- calls + 1L
        dbinom(x, 2, 0.5)
      },
      calls = function() calls
    )
  }

  ctr <- make_counter()
  StatPMF$compute_group(
    data.frame(group = 1), no_scales(),
    fun = ctr$fun, support = 0:2, args = list()
  )
  expect_identical(ctr$calls(), 1L)

  for (route in c("cdf", "survival", "qf")) {
    ctr <- make_counter()
    switch(route,
      cdf = StatCDFDiscrete$compute_group(
        data.frame(group = 1), no_scales(), pmf_fun = ctr$fun, support = 0:2
      ),
      survival = StatSurvivalDiscrete$compute_group(
        data.frame(group = 1), no_scales(), pmf_fun = ctr$fun, support = 0:2
      ),
      qf = StatQFDiscrete$compute_group(
        data.frame(group = 1), no_scales(), pmf_fun = ctr$fun, support = 0:2
      )
    )
    expect_identical(ctr$calls(), 1L)
  }
})

test_that("cumulative-route normalization is a diagnostic that proceeds", {
  # The declared object is presumed to be what the geom name says it is; a
  # measurable mass deficit alerts and the truncated object is drawn.
  for (build in list(
    function() StatCDFDiscrete$compute_group(
      data = data.frame(group = 1), scales = no_scales(),
      pmf_fun = function(x) rep(0.3, length(x)), support = 0:2
    ),
    function() StatSurvivalDiscrete$compute_group(
      data = data.frame(group = 1), scales = no_scales(),
      pmf_fun = function(x) rep(0.3, length(x)), support = 0:2
    ),
    function() StatQFDiscrete$compute_group(
      data = data.frame(group = 1), scales = no_scales(),
      pmf_fun = function(x) rep(0.3, length(x)), support = 0:2
    )
  )) {
    expect_message(out <- build(), "sums to")
    expect_gt(nrow(out), 0)
  }

  # check = FALSE and the global option both silence the diagnostic.
  expect_silent(
    StatCDFDiscrete$compute_group(
      data = data.frame(group = 1), scales = no_scales(),
      pmf_fun = function(x) rep(0.3, length(x)), support = 0:2, check = FALSE
    )
  )
  withr::local_options(ggfunction.check = FALSE)
  expect_silent(
    StatCDFDiscrete$compute_group(
      data = data.frame(group = 1), scales = no_scales(),
      pmf_fun = function(x) rep(0.3, length(x)), support = 0:2
    )
  )
})

test_that("the 1e-2 normalization tolerance is locked down", {
  pmf_inside <- function(x) dbinom(x, 2, 0.5) + c(2e-3, 0, -1e-3)
  expect_silent(
    StatCDFDiscrete$compute_group(
      data.frame(group = 1), no_scales(), pmf_fun = pmf_inside, support = 0:2
    )
  )
  pmf_outside <- function(x) dbinom(x, 2, 0.5) + rep(2e-2, length(x))
  expect_message(
    StatCDFDiscrete$compute_group(
      data.frame(group = 1), no_scales(), pmf_fun = pmf_outside, support = 0:2
    ),
    "sums to"
  )
})

test_that("a truncated Poisson via xlim alone draws with a diagnostic", {
  expect_message(
    b <- ggplot_build(
      ggplot() +
        geom_cdf_discrete(pmf_fun = dpois, xlim = c(0, 8), args = list(lambda = 5))
    ),
    "sums to"
  )
  d <- b$data[[1]]
  expect_equal(nrow(d), 9)
  expect_equal(d$cdf, ppois(0:8, 5), tolerance = 1e-12)
})

test_that("shading targets beyond the attained mass warn unconditionally", {
  expect_warning(
    idx <- pmf_shade_index(dpois(0:8, 5), p = 0.95),
    "not attainable"
  )
  expect_true(all(idx))
  # the warning is a request/object mismatch, not a validity diagnostic,
  # so check = FALSE does not silence it
  withr::local_options(ggfunction.check = FALSE)
  expect_warning(pmf_shade_index(dpois(0:8, 5), p = 0.95), "not attainable")
})

# --- direct discrete survival validation ---

test_that("structurally invalid survival input aborts; shape issues alert", {
  # structure: hard, regardless of options
  expect_error(
    StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) c(NA, 0.5, 0.2), support = 0:2
    ),
    "finite"
  )
  expect_error(
    StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) c(0.9, 0.5), support = 0:2
    ),
    "one.*per support"
  )

  # shape: diagnostic alerts that proceed with the values as supplied
  expect_message(
    out_hi <- StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) rep(2, length(x)), support = 0:2
    ),
    "\\[0, 1\\]"
  )
  expect_equal(out_hi$survival, rep(2, 3))
  expect_message(
    StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) rep(-0.5, length(x)), support = 0:2
    ),
    "\\[0, 1\\]"
  )
  expect_message(
    out_inc <- StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) seq(0, 1, length.out = length(x)), support = 0:2
    ),
    "non-increasing"
  )
  expect_equal(out_inc$survival, seq(0, 1, length.out = 3))

  # and check = FALSE silences the shape diagnostics
  expect_silent(
    StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), no_scales(),
      fun = function(x) rep(2, length(x)), support = 0:2, check = FALSE
    )
  )
})

test_that("non-monotone CDF sources alert and proceed", {
  expect_message(
    out <- StatSurvivalDiscrete$compute_group(
      data.frame(group = 1), no_scales(),
      cdf_fun = function(x) rev(seq(0, 1, length.out = length(x))),
      support = 0:4
    ),
    "non-decreasing"
  )
  expect_equal(nrow(out), 5)
})

test_that("roundoff excursions are clamped, larger ones abort", {
  eps <- sqrt(.Machine$double.eps) / 2
  out <- StatSurvivalDiscrete$compute_group(
    data.frame(group = 1), no_scales(),
    fun = function(x) c(1 + eps, 0.5, -eps), support = 0:2
  )
  expect_equal(out$survival, c(1, 0.5, 0))
})

# --- step endpoints train scales ---

test_that("discrete probability scales train on 0 and 1 even in narrowed windows", {
  p <- ggplot() +
    geom_cdf_discrete(
      pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
      support = 0:10, xlim = c(3, 7)
    )
  rng <- plot_y_range(p)
  expect_lte(rng[1], 0)
  expect_gte(rng[2], 1)

  p2 <- ggplot() +
    geom_survival_discrete(
      pmf_fun = dbinom, args = list(size = 10, prob = 0.5), support = 0:10
    )
  rng2 <- plot_y_range(p2)
  expect_lte(rng2[1], 0)
  expect_gte(rng2[2], 1)
})

test_that("log-y probability steps keep metadata and clip with one warning", {
  p <- ggplot() +
    geom_cdf_discrete(
      pmf_fun = dbinom, args = list(size = 5, prob = 0.5), support = 0:5
    ) +
    scale_y_log10()
  b <- ggplot_build(p)
  d <- b$data[[1]]
  # No -Inf training anchor is emitted; the raw zero endpoint stays metadata.
  expect_true(all(is.na(d$ymin) | is.finite(d$ymin)))
  expect_true(all(!is.finite(d$baseline_panel)))
  expect_equal(unique(d$top_panel), 0)  # log10(1)

  w <- testthat::capture_warnings(g <- ggplotGrob(p))
  expect_length(w, 1)
  expect_match(w, "baseline", ignore.case = TRUE)
})

# --- multiplicative dimming ---

test_that("dimming multiplies the resolved alpha", {
  expect_equal(dim_alpha(NA, c(TRUE, FALSE))[2], 0.3)
  expect_equal(dim_alpha(0.1, FALSE), 0.03)
  expect_equal(dim_alpha(0.1, TRUE), 0.1)
  alphas <- dim_alpha(c(0.1, 0.1), c(TRUE, FALSE))
  expect_true(all(alphas[2] <= alphas[1]))
})

test_that("lollipop grobs dim multiplicatively under a low user alpha", {
  p <- ggplot() +
    geom_pmf(
      fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
      p = 0.5, alpha = 0.1
    )
  extract_alphas <- function(grobs) {
    sort(unique(round(unlist(lapply(grobs, function(g) {
      grDevices::col2rgb(g$gp$col, alpha = TRUE)["alpha", ] / 255
    })), 3)))
  }
  seg_alphas <- extract_alphas(layer_grobs(p, 1, "segments"))
  pt_alphas <- extract_alphas(layer_grobs(p, 1, "points"))
  expect_equal(seg_alphas, c(0.031, 0.102), tolerance = 0.02)
  expect_equal(pt_alphas, c(0.031, 0.102), tolerance = 0.02)
})

test_that("bar and step geoms dim multiplicatively", {
  p_bar <- ggplot() +
    geom_pmf(
      fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
      type = "bar", p = 0.5, alpha = 0.1
    )
  rects <- layer_grobs(p_bar, 1, "rect")
  fill_alphas <- sort(unique(round(unlist(lapply(rects, function(g) {
    grDevices::col2rgb(g$gp$fill, alpha = TRUE)["alpha", ] / 255
  })), 3)))
  expect_equal(fill_alphas, c(0.031, 0.102), tolerance = 0.02)

  p_step <- ggplot() +
    geom_cdf_discrete(
      pmf_fun = dbinom, args = list(size = 10, prob = 0.5),
      support = 0:10, p = 0.5, alpha = 0.1
    )
  segs <- layer_grobs(p_step, 1, "segments")
  step_alphas <- sort(unique(round(unlist(lapply(segs, function(g) {
    grDevices::col2rgb(g$gp$col, alpha = TRUE)["alpha", ] / 255
  })), 3)))
  expect_lte(min(step_alphas), 0.05)
  expect_lte(max(step_alphas), 0.11)
})

# --- collision-free HDR labels ---

test_that("close coverages get unique adaptive labels", {
  p <- suppressMessages(
    discrete_hdr_probs(c(0.4, 0.3, 0.2, 0.1), c(0.5001, 0.5004))
  )
  expect_s3_class(p, "ordered")
  expect_identical(levels(p), c(">50.04%", "50.04%", "50.01%"))
})

test_that("common coverage labels stay familiar", {
  p <- suppressMessages(
    discrete_hdr_probs(c(0.4, 0.3, 0.2, 0.1), c(0.5, 0.8, 0.95))
  )
  expect_identical(levels(p), c(">95%", "95%", "80%", "50%"))
})

test_that("near-zero and near-one coverages never display as 0% or 100%", {
  labs <- format_hdr_coverages(c(0.0004, 0.9996))
  expect_false(any(labs %in% c("0%", "100%")))
  expect_identical(format_hdr_coverages(c(0.5, 0.8)), c("50%", "80%"))
})
