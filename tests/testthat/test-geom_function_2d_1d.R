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
