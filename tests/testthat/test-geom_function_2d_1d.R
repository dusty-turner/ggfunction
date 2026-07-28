test_that("StatFunction2d computes grid correctly", {
  f <- function(v) sqrt(v[1]^2 + v[2]^2)
  scales <- list(x = NULL, y = NULL)
  result <- StatFunction2d$compute_group(
    data = data.frame(x = NA_real_, y = NA_real_),
    scales = scales,
    fun = f,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    n = 10
  )
  expect_equal(nrow(result), 100)
  expect_true("z" %in% names(result))
  expect_true(all(result$z >= 0))
})

test_that("geom_function_2d_1d builds raster without error", {
  f <- function(v) sqrt(v[1]^2 + v[2]^2)
  p <- ggplot() + geom_function_2d_1d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_2d_1d raster defaults to fill scale", {
  f <- function(v) exp(-(v[1]^2 + v[2]^2))
  l_raster <- geom_function_2d_1d(
    fun = f, xlim = c(-3, 3), ylim = c(-3, 3),
    n = 20, type = "raster"
  )
  expect_s3_class(l_raster$stat, "StatFunction2d")
  expect_s3_class(l_raster$geom, "GeomFunction2d")
  expect_equal(rlang::as_label(l_raster$mapping$fill), "after_stat(z)")
  expect_null(l_raster$mapping$alpha)

  built <- ggplot_build(ggplot() + l_raster)
  expect_equal(nrow(built$data[[1]]), 20^2)
  expect_true("z" %in% names(built$data[[1]]))
  expect_gt(length(unique(built$data[[1]]$fill)), 1)
  expect_equal(unique(built$data[[1]]$alpha), 1)
})

test_that("geom_function_2d_1d raster can use alpha with fixed fill", {
  f <- function(v) exp(-(v[1]^2 + v[2]^2))
  l_raster <- geom_function_2d_1d(
    fun = f, xlim = c(-3, 3), ylim = c(-3, 3),
    n = 20, type = "raster", raster_aes = "alpha"
  )
  expect_s3_class(l_raster$stat, "StatFunction2d")
  expect_s3_class(l_raster$geom, "GeomFunction2d")
  expect_equal(
    rlang::as_label(l_raster$mapping$alpha),
    "after_stat(function2d_alpha_rescale(z))"
  )

  built <- ggplot_build(ggplot() + l_raster)
  expect_equal(nrow(built$data[[1]]), 20^2)
  expect_true("z" %in% names(built$data[[1]]))
  expect_equal(unique(built$data[[1]]$fill), "grey20")
  expect_gt(length(unique(built$data[[1]]$alpha)), 1)
  expect_equal(range(built$data[[1]]$alpha), c(0, 1))
  expect_s3_class(built$data[[1]]$alpha, "AsIs")
})

test_that("geom_function_2d_1d alpha raster handles degenerate ranges", {
  p_zero <- ggplot() +
    geom_function_2d_1d(
      fun = function(v) 0,
      xlim = c(-1, 1), ylim = c(-1, 1), n = 5,
      raster_aes = "alpha"
    )
  b_zero <- ggplot_build(p_zero)
  expect_equal(unique(b_zero$data[[1]]$alpha), 0)

  p_const <- ggplot() +
    geom_function_2d_1d(
      fun = function(v) 1,
      xlim = c(-1, 1), ylim = c(-1, 1), n = 5,
      raster_aes = "alpha"
    )
  b_const <- ggplot_build(p_const)
  expect_equal(unique(b_const$data[[1]]$alpha), 1)
})

test_that("geom_function_2d_1d alpha raster allows mapping and aesthetic overrides", {
  f <- function(v) exp(-(v[1]^2 + v[2]^2))

  p_mapped <- ggplot() +
    geom_function_2d_1d(
      fun = f, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
      raster_aes = "alpha",
      mapping = aes(fill = after_stat(z), alpha = after_stat(sqrt(z)))
    )
  b_mapped <- ggplot_build(p_mapped)
  expect_gt(length(unique(b_mapped$data[[1]]$fill)), 1)
  expect_gt(length(unique(b_mapped$data[[1]]$alpha)), 1)
  expect_false(all(b_mapped$data[[1]]$fill == "grey20"))

  p_fixed <- ggplot() +
    geom_function_2d_1d(
      fun = f, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
      raster_aes = "alpha",
      fill = "steelblue"
    )
  b_fixed <- ggplot_build(p_fixed)
  expect_equal(unique(b_fixed$data[[1]]$fill), "steelblue")

  p_alpha <- ggplot() +
    geom_function_2d_1d(
      fun = f, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
      raster_aes = "alpha",
      alpha = 0.4
    )
  b_alpha <- ggplot_build(p_alpha)
  expect_equal(unique(b_alpha$data[[1]]$alpha), 0.4)
})

test_that("geom_function_2d_1d builds contour without error", {
  f <- function(v) sqrt(v[1]^2 + v[2]^2)
  p <- ggplot() + geom_function_2d_1d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), type = "contour")
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))
})

test_that("geom_function_2d_1d builds contour_filled without error", {
  f <- function(v) sqrt(v[1]^2 + v[2]^2)
  p <- ggplot() + geom_function_2d_1d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), type = "contour_filled")
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))
})

test_that("geom_function_2d_1d defaults xlim/ylim to c(-1,1)", {
  f <- function(v) sqrt(v[1]^2 + v[2]^2)
  p <- ggplot() + geom_function_2d_1d(fun = f)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("stat_function_2d_1d builds without error", {
  f <- function(v) sqrt(v[1]^2 + v[2]^2)
  p <- ggplot() + stat_function_2d_1d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), n = 10)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_2d_1d with contour and custom mapping", {
  f <- function(v) sqrt(v[1]^2 + v[2]^2)
  p <- ggplot() + geom_function_2d_1d(
    fun = f, xlim = c(-1, 1), ylim = c(-1, 1), type = "contour",
    mapping = aes(colour = after_stat(level))
  )
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))
})

test_that("StatFunction2d passes args to fun", {
  f <- function(v, a = 1, b = 1) a * v[1] + b * v[2]
  result <- StatFunction2d$compute_group(
    data = data.frame(x = NA_real_, y = NA_real_),
    scales = list(x = NULL, y = NULL),
    fun = f,
    xlim = c(0, 1),
    ylim = c(0, 1),
    n = 2,
    args = list(a = 2, b = 3)
  )
  expect_equal(result$z, c(0, 2, 3, 5))
})

test_that("geom_function_2d_1d accepts args without unknown-parameter warning", {
  f <- function(v, a = 1, b = 1) a * v[1] + b * v[2]
  expect_no_warning(
    p <- ggplot() +
      geom_function_2d_1d(
        fun = f, xlim = c(0, 1), ylim = c(0, 1), n = 5,
        args = list(a = 2, b = 3)
      )
  )
  expect_silent(ggplot_build(p))
})

test_that("stat_function_2d_1d accepts args without warnings", {
  f <- function(v, a = 1, b = 1) a * v[1] + b * v[2]
  expect_no_warning(
    p <- ggplot() +
      stat_function_2d_1d(
        fun = f, xlim = c(0, 1), ylim = c(0, 1), n = 5,
        args = list(a = 2, b = 3)
      )
  )
  expect_silent(ggplot_build(p))
})

test_that("raster, contour, and contour_filled all use args", {
  f_args <- function(v, a = 1, b = 1) a * sin(v[1]) + b * cos(v[2])
  f_fixed <- function(v) 2 * sin(v[1]) + 0.5 * cos(v[2])

  for (type in c("raster", "contour", "contour_filled")) {
    built_args <- ggplot_build(
      ggplot() +
        geom_function_2d_1d(
          fun = f_args, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
          args = list(a = 2, b = 0.5), type = type
        )
    )
    built_fixed <- ggplot_build(
      ggplot() +
        geom_function_2d_1d(
          fun = f_fixed, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
          type = type
        )
    )
    expect_equal(built_args$data[[1]], built_fixed$data[[1]], label = type)
  }
})

# --- E-01: panel-aware contours and precomputed data ---

test_that("contours populate every facet with equal grids (E-01)", {
  d <- data.frame(facet = c("a", "b"))
  f <- function(v) v[1]^2 + v[2]^2

  for (type in c("contour", "contour_filled")) {
    b <- ggplot_build(
      ggplot(d) +
        facet_wrap(~facet) +
        geom_function_2d_1d(
          fun = f, n = 10, type = type,
          xlim = c(-2, 2), ylim = c(-2, 2)
        )
    )
    counts <- table(factor(b$data[[1]]$PANEL, levels = 1:2))
    expect_true(all(counts > 0))
    expect_identical(unname(counts[1]), unname(counts[2]))
  }
})

test_that("precomputed x/y/z contours match native geom_contour (E-01)", {
  d <- expand.grid(x = seq(-2, 2, length.out = 15), y = seq(-2, 2, length.out = 15))
  d$z <- d$x^2 + d$y^2
  breaks <- c(1, 2, 3)

  b_ours <- ggplot_build(
    ggplot(d, aes(x, y, z = z)) +
      geom_function_2d_1d(fun = NULL, type = "contour", breaks = breaks)
  )$data[[1]]
  b_native <- ggplot_build(
    ggplot(d, aes(x, y, z = z)) + geom_contour(breaks = breaks)
  )$data[[1]]

  expect_equal(sort(unique(b_ours$level)), sort(unique(b_native$level)))
  ord <- function(df) df[order(df$level, df$piece, df$x, df$y), c("x", "y", "level")]
  expect_equal(ord(b_ours), ord(b_native), ignore_attr = TRUE)
})

test_that("function-only contour layers error without a domain source (E-01/E-07)", {
  expect_error(
    ggplot_build(
      ggplot() +
        layer(
          stat = StatFunction2d, geom = GeomFunction2d,
          data = data.frame(x = NA_real_, y = NA_real_),
          mapping = aes(fill = after_stat(z)),
          position = "identity",
          params = list(fun = function(v) sum(v), xlim = NULL, ylim = NULL)
        )
    ),
    "xlim"
  )
})

# --- E-02: default scalar-field z encoding ---

test_that("stat_function_2d_1d maps fill to after_stat(z) by default (E-02)", {
  l <- stat_function_2d_1d(fun = function(v) sum(v^2), n = 5)
  expect_equal(rlang::as_label(l$mapping$fill), "after_stat(z)")

  b <- ggplot_build(ggplot() + l)
  expect_gt(length(unique(b$data[[1]]$fill)), 1)
})

test_that("auxiliary mappings do not displace the z fill default (E-02)", {
  l <- geom_function_2d_1d(
    fun = function(v) sum(v^2), n = 5,
    mapping = aes(alpha = 0.5)
  )
  expect_equal(rlang::as_label(l$mapping$fill), "after_stat(z)")
  expect_gt(
    length(unique(ggplot_build(ggplot() + l)$data[[1]]$fill)),
    1
  )
})

test_that("an explicit user fill mapping overrides the z default (E-02)", {
  l <- geom_function_2d_1d(
    fun = function(v) sum(v^2), n = 5,
    mapping = aes(fill = after_stat(-z))
  )
  expect_equal(rlang::as_label(l$mapping$fill), "after_stat(-z)")
})

test_that("alpha-raster mode keeps fixed fill with z-scaled alpha (E-02)", {
  l <- geom_function_2d_1d(
    fun = function(v) sum(v^2), n = 5, raster_aes = "alpha"
  )
  expect_false("fill" %in% names(l$mapping))
  b <- ggplot_build(ggplot() + l)$data[[1]]
  expect_identical(unique(b$fill), "grey20")
  expect_gt(length(unique(b$alpha)), 1)
})

# --- E-03: precomputed scalar-field aesthetics ---

test_that("precomputed scalar fields preserve mapped aesthetics (E-03)", {
  d <- expand.grid(x = 1:3, y = 1:3)
  d$z <- d$x + d$y
  d$a <- seq(0.1, 0.9, length.out = nrow(d))

  expect_no_warning(
    b <- ggplot_build(
      ggplot(d, aes(x, y, z = z, alpha = I(a))) +
        stat_function_2d_1d(fun = NULL)
    )$data[[1]]
  )
  expect_equal(sort(unique(b$alpha)), sort(unique(d$a)))
})

test_that("grouped, faceted precomputed grids keep group and panel structure (E-03)", {
  d <- rbind(
    transform(expand.grid(x = 1:3, y = 1:3), g = "a", facet = "f1"),
    transform(expand.grid(x = 1:3, y = 1:3), g = "b", facet = "f2")
  )
  d$z <- d$x * d$y

  b <- ggplot_build(
    ggplot(d, aes(x, y, z = z, group = g)) +
      facet_wrap(~facet) +
      stat_function_2d_1d(fun = NULL)
  )$data[[1]]

  counts <- table(factor(b$PANEL, levels = 1:2))
  expect_true(all(counts == 9))
  expect_equal(length(unique(b$group)), 2)
})

# --- A-01: transformed scales for 2D grids ---

test_that("2D grids are panel-uniform and evaluate in data coordinates (A-01)", {
  f <- function(v) v[1] + v[2]
  b <- ggplot_build(
    ggplot() +
      geom_function_2d_1d(fun = f, xlim = c(1, 100), ylim = c(1, 100), n = 3) +
      scale_x_log10() +
      scale_y_log10()
  )$data[[1]]

  expect_equal(sort(unique(b$x)), c(0, 1, 2))
  expect_equal(sort(unique(b$y)), c(0, 1, 2))
  expect_equal(b$z, c(2, 11, 101, 11, 20, 110, 101, 110, 200))
})
