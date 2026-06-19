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
