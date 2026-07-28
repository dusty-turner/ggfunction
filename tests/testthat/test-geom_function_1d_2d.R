test_that("Stat_1d_2d computes trajectory correctly", {
  f <- function(t) c(sin(t), cos(t))
  scales <- list(x = NULL, y = NULL)
  result <- Stat_1d_2d$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = f,
    tlim = c(0, 2 * pi),
    dt = 0.1,
    args = list()
  )
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true("t" %in% names(result))
  expect_true(nrow(result) > 10)
})

test_that("geom_function_1d_2d builds without error", {
  f <- function(t) c(sin(t), cos(t))
  p <- ggplot() + geom_function_1d_2d(fun = f)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_2d with tail_point builds without error", {
  f <- function(t) c(sin(t), cos(t))
  p <- ggplot() + geom_function_1d_2d(fun = f, tlim = c(0, 5), tail_point = TRUE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_2d with args builds without error", {
  lissajous <- function(t, a = 3, b = 2, delta = pi/2) {
    c(sin(a * t + delta), sin(b * t))
  }
  p <- ggplot() + geom_function_1d_2d(
    fun = lissajous, tlim = c(0, 2 * pi),
    args = list(a = 3, b = 2, delta = pi/2)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_2d with arrow builds without error", {
  f <- function(t) c(sin(t), cos(t))
  p <- ggplot() + geom_function_1d_2d(
    fun = f, tlim = c(0, 5),
    arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_2d with custom colour mapping", {
  f <- function(t) c(sin(t), cos(t))
  p <- ggplot() + geom_function_1d_2d(
    fun = f, tlim = c(0, 5), mapping = aes(colour = "blue")
  )
  expect_s3_class(p, "gg")
})

test_that("stat_function_1d_2d builds without error", {
  f <- function(t) c(sin(t), cos(t))
  p <- ggplot() + stat_function_1d_2d(fun = f, tlim = c(0, 5))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

# --- dt as an exact positive step magnitude ---

test_that("parameter_grid uses dt as an exact step with terminal endpoint", {
  expect_equal(parameter_grid(c(0, 1), 0.3), c(0, 0.3, 0.6, 0.9, 1))
  expect_equal(
    parameter_grid(c(1, 0), 0.3),
    c(1, 0.7, 0.4, 0.1, 0),
    tolerance = 1e-12
  )
  expect_equal(parameter_grid(c(0, 0.2), 1), c(0, 0.2))
  expect_equal(parameter_grid(c(1, 1), 0.1), 1)
})

test_that("invalid dt and tlim error clearly", {
  expect_error(parameter_grid(c(0, 1), 0), "positive")
  expect_error(parameter_grid(c(0, 1), -0.1), "positive")
  expect_error(parameter_grid(c(0, 1), Inf), "positive")
  expect_error(parameter_grid(c(0, 1), NA_real_), "positive")
  expect_error(parameter_grid(c(0, 1), c(0.1, 0.2)), "single")
  expect_error(parameter_grid(c(0, Inf), 0.1), "finite")
  expect_error(geom_function_1d_2d(fun = function(t) c(t, t), dt = 0), "positive")
})

# --- parametric output positions under transformed scales ---

test_that("parametric curves transform output positions exactly once", {
  f <- function(t) c(10^t, 10^(2 * t))
  expect_no_warning(
    b <- ggplot_build(
      ggplot() +
        geom_function_1d_2d(fun = f, tlim = c(0, 1), dt = 0.5, colour = "black") +
        scale_x_log10() +
        scale_y_log10()
    )$data[[1]]
  )
  expect_equal(b$t, c(0, 0.5, 1))
  expect_equal(b$x, c(0, 0.5, 1))
  expect_equal(b$y, c(0, 1, 2))
  expect_equal(b$x_eval, 10^b$t)
})


test_that("the final endpoint of tlim is always evaluated (closed curves close)", {
  f <- function(t) c(cos(t), sin(t))
  d <- Stat_1d_2d$compute_group(
    data.frame(group = 1), scales = list(),
    fun = f, tlim = c(0, 2 * pi), dt = 0.1, args = list()
  )
  expect_equal(max(d$t), 2 * pi)
  # First and last points coincide for a closed unit circle.
  expect_equal(d$x[nrow(d)], d$x[1], tolerance = 1e-8)
  expect_equal(d$y[nrow(d)], d$y[1], tolerance = 1e-8)
})

test_that("the parametric trajectory is evaluated exactly, with args injected", {
  traj <- Stat_1d_2d$compute_group(
    data.frame(group = 1), scales = list(),
    fun = function(t) c(sin(t), cos(t)),
    tlim = c(0, pi), dt = 0.1, args = list()
  )
  expect_equal(traj$x, sin(traj$t))
  expect_equal(traj$y, cos(traj$t))

  traj2 <- Stat_1d_2d$compute_group(
    data.frame(group = 1), scales = list(),
    fun = function(t, a = 1) c(a * t, t), tlim = c(0, 1), dt = 0.25,
    args = list(a = 3)
  )
  expect_equal(traj2$x, 3 * traj2$t)
})
