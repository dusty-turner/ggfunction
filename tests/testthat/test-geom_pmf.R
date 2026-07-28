test_that("StatPMF computes correct PMF values", {
  scales <- list(x = NULL)
  result <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  expect_true(all(result$y >= 0))
  expect_true(abs(sum(result$y) - 1) < 0.01)
})

test_that("geom_pmf builds a ggplot without error", {
  p <- ggplot() + geom_pmf(fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf lollipop display trains y scale to include zero", {
  p <- ggplot() +
    geom_pmf(fun = function(x) rep(0.25, length(x)), support = 1:4)
  yrng <- plot_y_range(p)
  expect_lte(yrng[1], 0)
  expect_gte(yrng[2], 0)
})

test_that("geom_pmf bar display builds and trains y scale to include zero", {
  l_bar <- geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    type = "bar"
  )
  expect_s3_class(l_bar$geom, "GeomPMFBar")

  p <- ggplot() +
    geom_pmf(fun = function(x) rep(0.25, length(x)), support = 1:4, type = "bar")
  yrng <- plot_y_range(p)
  expect_lte(yrng[1], 0)
  expect_gte(yrng[2], 0)
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf validates display type", {
  expect_error(
    geom_pmf(
      fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
      type = "sticks"
    ),
    "'arg' should be one of"
  )
})

test_that("geom_pmf with p shading builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5), p = 0.8
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with lower.tail=FALSE builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    p = 0.8, lower.tail = FALSE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with p_lower/p_upper builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    p_lower = 0.1, p_upper = 0.9
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf with shade_outside builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    p_lower = 0.1, p_upper = 0.9, shade_outside = TRUE
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf bar display preserves p-based shading", {
  p <- ggplot() +
    geom_pmf(
      fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
      p = 0.5, type = "bar"
    )
  built <- ggplot_build(p)
  expect_true(any(built$data[[1]]$in_shade))
  expect_true(any(!built$data[[1]]$in_shade))
})

test_that("geom_pmf with shade_hdr builds without error", {
  p <- ggplot() + geom_pmf(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3),
    shade_hdr = 0.7
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatPMF computes HDR levels for shade_hdr", {
  result <- suppressMessages(StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.3),
    shade_hdr = c(0.5, 0.8)
  ))
  expect_s3_class(result$probs, "ordered")
  expect_equal(levels(result$probs), c(">80%", "80%", "50%"))
  expect_gte(sum(result$y[result$probs == "50%"]), 0.5)
  expect_gte(sum(result$y[result$probs %in% c("50%", "80%")]), 0.8)

  plain <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.3)
  )
  expect_false("probs" %in% names(plain))
})

test_that("geom_pmf maps probs to alpha when shade_hdr is supplied", {
  l_hdr <- geom_pmf(fun = dbinom, xlim = c(0, 10),
    args = list(size = 10, prob = 0.5), shade_hdr = 0.8)
  l_plain <- geom_pmf(fun = dbinom, xlim = c(0, 10),
    args = list(size = 10, prob = 0.5))
  expect_equal(rlang::as_label(l_hdr$mapping$alpha), "after_stat(probs)")
  expect_null(l_plain$mapping$alpha)
})

test_that("geom_pmf allows shade_hdr probs to map to colour", {
  p <- ggplot() +
    geom_pmf(
      fun = dbinom,
      xlim = c(0, 10),
      args = list(size = 10, prob = 0.3),
      shade_hdr = c(0.5, 0.8, 0.95),
      mapping = aes(colour = after_stat(probs)),
      alpha = 1
    ) +
    scale_colour_viridis_d()

  built <- suppressMessages(ggplot_build(p))

  expect_equal(unique(built$data[[1]]$alpha), 1)
  expect_gt(length(unique(built$data[[1]]$colour)), 1)
  expect_false(all(unique(built$data[[1]]$colour) == "black"))
})

test_that("geom_pmf still accepts fixed color", {
  p <- ggplot() +
    geom_pmf(
      fun = dbinom,
      xlim = c(0, 10),
      args = list(size = 10, prob = 0.3),
      color = "red"
    )

  built <- ggplot_build(p)

  expect_equal(unique(built$data[[1]]$colour), "red")
})

test_that("geom_pmf renders black lollipops by default", {
  p <- ggplot() +
    geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3))
  built <- ggplot_build(p)
  expect_equal(unique(built$data[[1]]$colour), "black")
})

test_that("geom_pmf points use a fillable shape with fill following colour", {
  expect_equal(rlang::eval_tidy(GeomPMF$default_aes$shape), 21)

  p <- ggplot() +
    geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3))
  pt_grob <- ggplot2::layer_grob(p, 1)[[1]]$children[[2]]
  expect_true(all(pt_grob$gp$fill %in% c("black", "#000000", "#000000FF")))

  p_fill <- ggplot() +
    geom_pmf(
      fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3),
      shade_hdr = c(0.5, 0.8, 0.95),
      mapping = aes(fill = after_stat(probs)), alpha = 1
    ) +
    scale_fill_viridis_d()
  built <- suppressMessages(ggplot_build(p_fill))
  expect_equal(unique(built$data[[1]]$shape), 21)
  expect_gt(length(unique(built$data[[1]]$fill)), 1)
  expect_equal(unique(built$data[[1]]$colour), "black")
  expect_equal(unique(built$data[[1]]$alpha), 1)
})

test_that("geom_pmf with support parameter builds without error", {
  f_mean <- function(x, prob) dbinom(round(x * 10), size = 10, prob = prob)
  p <- ggplot() + geom_pmf(
    fun = f_mean, support = seq(0, 1, by = 0.1), args = list(prob = 0.3)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatPMF uses default xlim when not provided", {
  scales <- list(x = NULL)
  result <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = NULL,
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)  # 0:10
})

test_that("StatPMF uses support when provided", {
  scales <- list(x = NULL)
  result <- suppressMessages(StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    support = c(0, 5, 10),
    args = list(size = 10, prob = 0.5)
  ))
  expect_equal(nrow(result), 3)
})

test_that("StatPMF evaluates nothing for integer-free xlim ranges", {
  calls <- 0L
  f_count <- function(x) {
    calls <<- calls + 1L
    rep(0.5, length(x))
  }

  result <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = f_count,
    xlim = c(0.2, 0.8)
  )
  expect_identical(calls, 0L)
  expect_equal(nrow(result), 0)

  with_hdr <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = f_count,
    xlim = c(0.2, 0.8),
    shade_hdr = 0.8
  )
  expect_identical(calls, 0L)
  expect_equal(nrow(with_hdr), 0)
  expect_s3_class(with_hdr$probs, "ordered")

  b <- ggplot_build(
    ggplot() + geom_pmf(fun = f_count, xlim = c(0.2, 0.8))
  )
  expect_identical(calls, 0L)
  expect_equal(nrow(b$data[[1]]), 0)
})

test_that("shade_hdr includes support points tied at the cutoff", {
  f_unif <- function(x) rep(0.25, length(x))
  result <- suppressMessages(StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = f_unif,
    support = 1:4,
    shade_hdr = 0.5
  ))
  expect_true(all(result$probs == "50%"))
})


test_that("pmf p-shading is resolved per group (no cross-group cumsum)", {
  # Two genuinely independent groups in one built layer: each group's shading
  # must be computed from only that group's masses. A panel-wide cumsum
  # (reaching total mass 2.0) would mis-shade the second group.
  d <- data.frame(g = c("a", "b"))
  b <- ggplot_build(
    ggplot(d, aes(colour = g, group = g)) +
      geom_pmf(
        data = d,
        fun = dbinom, xlim = c(0, 10),
        args = list(size = 10, prob = 0.5), p = 0.8
      )
  )$data[[1]]

  expect_equal(length(unique(b$group)), 2)
  for (gid in unique(b$group)) {
    rows <- b[b$group == gid, ]
    expect_true("in_shade" %in% names(rows))
    expect_identical(rows$in_shade, pmf_shade_index(rows$mass, p = 0.8))
    expect_true(any(rows$in_shade) && any(!rows$in_shade))
  }
})
