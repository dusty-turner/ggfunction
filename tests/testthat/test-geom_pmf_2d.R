dbinom2 <- function(v, sizes = c(10, 10), probs = c(0.5, 0.5)) {
  dbinom(v[1], sizes[1], probs[1]) * dbinom(v[2], sizes[2], probs[2])
}

dtrinom <- function(v, size = 8, prob = c(0.3, 0.3, 0.4)) {
  if (sum(v) > size) return(0)
  dmultinom(c(v, size - sum(v)), prob = prob)
}

test_that("StatPMF2d computes correct PMF values on the lattice", {
  result <- StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom2,
    xlim = c(0, 10),
    ylim = c(0, 10)
  )
  expect_equal(nrow(result), 121)
  expect_true(all(result$prob >= 0))
  expect_lt(abs(sum(result$prob) - 1), 0.01)
  expect_false("probs" %in% names(result))
  expect_equal(
    result$prob[result$x == 5 & result$y == 5],
    dbinom(5, 10, 0.5)^2
  )
})

test_that("geom_pmf_2d builds tile mode without error", {
  p <- ggplot() +
    geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
      type = "tile")
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf_2d builds point mode without error", {
  p <- ggplot() +
    geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
      type = "point") +
    scale_size_area()
  expect_silent(ggplot_build(p))
})

test_that("geom_pmf_2d defaults to point mode and switches geoms", {
  l_default <- geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10))
  l_tile <- geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
    type = "tile")
  expect_s3_class(l_default$geom, "GeomPMF2dPoint")
  expect_s3_class(l_tile$geom, "GeomPMF2dTile")
  expect_s3_class(l_default$stat, "StatPMF2d")
})

test_that("invalid type errors via match.arg", {
  expect_error(
    geom_pmf_2d(fun = dbinom2, type = "raster"),
    "should be one of"
  )
})

test_that("args are passed to fun", {
  dbinom2_fixed <- function(v) dbinom2(v, probs = c(0.3, 0.7))
  b_args <- ggplot_build(
    ggplot() +
      geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
        args = list(probs = c(0.3, 0.7)))
  )
  b_fixed <- ggplot_build(
    ggplot() +
      geom_pmf_2d(fun = dbinom2_fixed, xlim = c(0, 10), ylim = c(0, 10))
  )
  expect_equal(b_args$data[[1]], b_fixed$data[[1]])
})

test_that("StatPMF2d uses the default 0:10 lattice", {
  result <- StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom2
  )
  expect_equal(nrow(result), 121)
  expect_equal(range(result$x), c(0, 10))
  expect_equal(range(result$y), c(0, 10))
})

test_that("StatPMF2d respects non-integer support_x/support_y", {
  f_mean <- function(v, probs = c(0.5, 0.5)) {
    dbinom(round(v[1] * 10), 10, probs[1]) *
      dbinom(round(v[2] * 10), 10, probs[2])
  }
  result <- StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = f_mean,
    support_x = seq(0, 1, by = 0.1),
    support_y = seq(0, 1, by = 0.1)
  )
  expect_equal(nrow(result), 121)
  expect_lt(abs(sum(result$prob) - 1), 0.01)
})

test_that("shade_hdr assigns the smallest HDR with at least target coverage", {
  result <- suppressMessages(StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom2,
    xlim = c(0, 10),
    ylim = c(0, 10),
    shade_hdr = 0.8
  ))
  expect_s3_class(result$probs, "ordered")
  expect_equal(levels(result$probs), c(">80%", "80%"))
  in_hdr <- result$probs == "80%"
  expect_true(any(in_hdr) && any(!in_hdr))
  expect_gte(sum(result$prob[in_hdr]), 0.8)
  expect_gte(min(result$prob[in_hdr]), max(result$prob[!in_hdr]))
})

test_that("multi-level shade_hdr yields nested ordered levels", {
  result <- suppressMessages(StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dbinom2,
    xlim = c(0, 10),
    ylim = c(0, 10),
    shade_hdr = c(0.5, 0.8, 0.95)
  ))
  expect_equal(levels(result$probs), c(">95%", "95%", "80%", "50%"))
  mass_at <- function(lvls) sum(result$prob[result$probs %in% lvls])
  expect_gte(mass_at("50%"), 0.5)
  expect_gte(mass_at(c("50%", "80%")), 0.8)
  expect_gte(mass_at(c("50%", "80%", "95%")), 0.95)
  expect_gte(min(result$prob[result$probs == "50%"]),
             max(result$prob[result$probs == "80%"]))
})

test_that("shade_hdr maps probs to alpha by default", {
  l_hdr <- geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
    shade_hdr = 0.8)
  l_plain <- geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10))
  expect_equal(rlang::as_label(l_hdr$mapping$alpha), "after_stat(probs)")
  expect_null(l_plain$mapping$alpha)
})

test_that("invalid shade_hdr aborts with a clear message", {
  expect_error(
    StatPMF2d$compute_group(
      data = data.frame(group = 1),
      scales = list(),
      fun = dbinom2,
      xlim = c(0, 10),
      ylim = c(0, 10),
      shade_hdr = 1.2
    ),
    "between 0 and 1"
  )
})

test_that("geom_pmf_2d with shade_hdr builds in both modes", {
  p_point <- ggplot() +
    geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
      shade_hdr = c(0.5, 0.8, 0.95))
  p_tile <- ggplot() +
    geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
      shade_hdr = c(0.5, 0.8, 0.95), type = "tile")
  expect_no_error(suppressMessages(ggplot_build(p_point)))
  expect_no_error(suppressMessages(ggplot_build(p_tile)))
})

test_that("drop_zeros removes off-support cells of a bounding lattice", {
  kept <- StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dtrinom,
    xlim = c(0, 8),
    ylim = c(0, 8)
  )
  full <- StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = dtrinom,
    xlim = c(0, 8),
    ylim = c(0, 8),
    drop_zeros = FALSE
  )
  expect_equal(nrow(kept), 45)
  expect_equal(nrow(full), 81)
  expect_true(all(kept$x + kept$y <= 8))
  expect_lt(abs(sum(kept$prob) - 1), 0.01)
})

test_that("normalization check alerts for non-normalized fun", {
  f_bad <- function(v) 2 * dbinom2(v)
  expect_message(
    StatPMF2d$compute_group(
      data = data.frame(group = 1),
      scales = list(),
      fun = f_bad,
      xlim = c(0, 10),
      ylim = c(0, 10)
    ),
    "sums to"
  )

  old <- options(ggfunction.check = FALSE)
  on.exit(options(old), add = TRUE)
  expect_silent(StatPMF2d$compute_group(
    data = data.frame(group = 1),
    scales = list(),
    fun = f_bad,
    xlim = c(0, 10),
    ylim = c(0, 10)
  ))
})

test_that("geom_pmf_2d maps prob to size (point) or fill (tile) by default", {
  l_point <- geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10))
  l_tile <- geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
    type = "tile")
  expect_equal(rlang::as_label(l_point$mapping$size), "after_stat(prob)")
  expect_null(l_point$mapping$fill)
  expect_equal(rlang::as_label(l_tile$mapping$fill), "after_stat(prob)")
  expect_null(l_tile$mapping$size)

  # User mappings override the defaults
  l_custom <- geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
    mapping = aes(alpha = after_stat(prob)))
  expect_equal(rlang::as_label(l_custom$mapping$alpha), "after_stat(prob)")
  expect_equal(rlang::as_label(l_custom$mapping$size), "after_stat(prob)")
})

test_that("non-scalar fun return aborts with a clear message", {
  f_bad <- function(v) c(0.1, 0.2)
  expect_error(
    StatPMF2d$compute_group(
      data = data.frame(group = 1),
      scales = list(),
      fun = f_bad,
      xlim = c(0, 2),
      ylim = c(0, 2)
    ),
    "mass value per lattice point"
  )
})
