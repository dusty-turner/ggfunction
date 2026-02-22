test_that("geom_function_2d_2d builds without error", {
  f <- function(u) c(-u[2], u[1])
  p <- ggplot() + geom_function_2d_2d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_2d_2d with type='stream' builds without error", {
  f <- function(u) c(-u[2], u[1])
  p <- ggplot() + geom_function_2d_2d(
    fun = f, xlim = c(-1, 1), ylim = c(-1, 1), type = "stream"
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_2d_2d errors on invalid type", {
  f <- function(u) c(-u[2], u[1])
  expect_error(
    geom_function_2d_2d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), type = "invalid"),
    "vector.*stream"
  )
})

test_that("stat_function_2d_2d builds without error", {
  f <- function(u) c(-u[2], u[1])
  p <- ggplot() + stat_function_2d_2d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
