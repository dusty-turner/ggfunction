test_that("StatQF computes correct quantile values", {
  scales <- list(x = NULL)
  result <- StatQF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = qnorm,
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  # Median should be near 0 for standard normal
  mid_idx <- ceiling(101 / 2)
  expect_true(abs(result$q[mid_idx]) < 0.1)
})

test_that("geom_qf builds a ggplot without error", {
  p <- ggplot() + geom_qf(fun = qnorm)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
