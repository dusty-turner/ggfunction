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
