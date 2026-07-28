test_that("StatFunction1d computes correct values", {
  scales <- list(x = NULL)
  result <- StatFunction1d$compute_group(
    data = data.frame(group = 1),
    scales = scales,
    fun = sin,
    xlim = c(0, 2 * pi),
    n = 101,
    args = list()
  )
  expect_equal(nrow(result), 101)
  expect_equal(result$x[1], 0)
  expect_equal(result$x[101], 2 * pi)
  # sin(0) = 0
  expect_equal(result$y[1], 0, tolerance = 1e-10)
})

test_that("geom_function_1d_1d builds without error", {
  p <- ggplot() + geom_function_1d_1d(fun = sin, xlim = c(0, 2 * pi))
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_1d with shade_from/shade_to builds without error", {
  p <- ggplot() + geom_function_1d_1d(
    fun = dnorm, xlim = c(-3, 3),
    shade_from = -1, shade_to = 1
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_1d with args builds without error", {
  p <- ggplot() + geom_function_1d_1d(
    fun = dnorm, xlim = c(-5, 15),
    args = list(mean = 5, sd = 2)
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_1d with shade_from only builds without error", {
  p <- ggplot() + geom_function_1d_1d(
    fun = dnorm, xlim = c(-3, 3), shade_from = -1
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_1d with shade_to only builds without error", {
  p <- ggplot() + geom_function_1d_1d(
    fun = dnorm, xlim = c(-3, 3), shade_to = 1
  )
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_function_1d_1d with custom mapping builds without error", {
  p <- ggplot() + geom_function_1d_1d(
    fun = sin, xlim = c(0, 2 * pi), mapping = aes(linetype = "solid")
  )
  expect_s3_class(p, "gg")
})

test_that("fixed colour via constructor arg or ... is honored (E-04)", {
  expect_no_warning(l <- geom_function_1d_1d(fun = sin, colour = "red"))
  b <- ggplot_build(ggplot() + l)
  expect_identical(unique(b$data[[1]]$colour), "red")

  expect_no_warning(l2 <- geom_function_1d_1d(fun = sin, color = "blue"))
  b2 <- ggplot_build(ggplot() + l2)
  expect_identical(unique(b2$data[[1]]$colour), "blue")
})

test_that("mapped colour is honored and no fixed default overrides it (E-04)", {
  expect_no_warning(
    l <- geom_function_1d_1d(fun = sin, mapping = aes(colour = after_stat(x)))
  )
  b <- ggplot_build(ggplot() + l)
  expect_gt(length(unique(b$data[[1]]$colour)), 1)
  expect_null(l$aes_params$colour)
})

test_that("default line colour remains black and default fill grey20 (E-04)", {
  b <- ggplot_build(ggplot() + geom_function_1d_1d(fun = sin, xlim = c(0, 1)))
  expect_identical(unique(b$data[[1]]$colour), "black")
  expect_identical(unique(b$data[[1]]$fill), "grey20")
})

test_that("group-constant mapped fill reaches the shading polygon (E-04)", {
  l <- geom_function_1d_1d(
    fun = sin, xlim = c(0, pi), shade_from = 1, shade_to = 2,
    mapping = aes(fill = "grp")
  )
  p <- ggplot() + l
  fill_hex <- ggplot_build(p)$data[[1]]$fill[1]
  polys <- layer_grobs(p, 1, "polygon")
  expect_gt(length(polys), 0)
  expect_identical(as.character(polys[[1]]$gp$fill), fill_hex)
})

test_that("within-group varying fill is rejected for shaded functions (E-04)", {
  l <- geom_function_1d_1d(
    fun = sin, xlim = c(0, pi), shade_from = 1, shade_to = 2,
    mapping = aes(fill = after_stat(x))
  )
  expect_error(ggplotGrob(ggplot() + l), "varies within")
})

test_that("shading trains the zero baseline; no shading leaves it untrained (E-05)", {
  p <- ggplot() +
    geom_function_1d_1d(
      fun = function(x) 100 + x, xlim = c(0, 1),
      shade_from = 0.2, shade_to = 0.8
    )
  rng <- plot_y_range(p)
  expect_lte(rng[1], 0)
  expect_gte(rng[2], 101)

  p2 <- ggplot() +
    geom_function_1d_1d(fun = function(x) 100 + x, xlim = c(0, 1))
  rng2 <- plot_y_range(p2)
  expect_gt(rng2[1], 90)
})

test_that("exact off-grid shade boundaries are evaluated (E-05)", {
  d <- ggplot_build(
    ggplot() +
      geom_function_1d_1d(
        fun = function(x) 100 + x, xlim = c(0, 1), n = 3,
        shade_from = 0.2, shade_to = 0.8
      )
  )$data[[1]]

  expect_true(all(c(0.2, 0.8) %in% d$x_eval))
  expect_equal(d$y_raw[match(c(0.2, 0.8), d$x_eval)], c(100.2, 100.8))
})

test_that("log-y shading clips the baseline with one documented warning (E-05)", {
  p <- ggplot() +
    geom_function_1d_1d(
      fun = function(x) 100 + x, xlim = c(0, 1),
      shade_from = 0.2, shade_to = 0.8
    ) +
    scale_y_log10()
  w <- testthat::capture_warnings(g <- ggplotGrob(p))
  expect_length(w, 1)
  expect_match(w, "baseline", ignore.case = TRUE)

  polys <- find_grobs(g, "polygon")
  expect_gt(length(polys), 0)
  expect_true(all(is.finite(as.numeric(polys[[1]]$x))))
  expect_true(all(is.finite(as.numeric(polys[[1]]$y))))
})

test_that("invalid shade bounds abort clearly (E-05)", {
  expect_error(
    ggplot_build(
      ggplot() +
        geom_function_1d_1d(fun = sin, xlim = c(0, 1), shade_from = 0.8, shade_to = 0.2)
    ),
    "shade_from"
  )
  expect_error(
    ggplot_build(
      ggplot() +
        geom_function_1d_1d(fun = sin, xlim = c(0, 1), shade_from = Inf)
    ),
    "finite"
  )
})
