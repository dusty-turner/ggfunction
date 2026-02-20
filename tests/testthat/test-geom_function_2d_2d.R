test_that("geom_function_2d_2d builds without error", {
  f <- function(u) c(-u[2], u[1])
  p <- ggplot() + geom_function_2d_2d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
