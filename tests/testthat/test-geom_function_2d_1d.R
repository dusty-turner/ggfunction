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
