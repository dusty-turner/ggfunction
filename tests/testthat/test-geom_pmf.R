test_that("StatPMF computes correct PMF values", {
  scales <- list(x = NULL)
  result <- StatPMF$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = dbinom,
    xlim = c(0, 10),
    args = list(size = 10, prob = 0.5)
  )
  expect_equal(nrow(result), 11)
  expect_true(all(result$y >= 0))
  expect_true(abs(sum(result$y) - 1) < 0.01)
})

test_that("geom_pmf builds a ggplot without error", {
  p <- ggplot() + geom_pmf(fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})
