# Package default mappings never conflict with user aesthetics.

test_that("static overrides of package default mappings are silent", {
  # PMF HDR alpha default vs static alpha
  expect_no_warning(
    l <- geom_pmf(
      fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3),
      shade_hdr = c(0.5, 0.8, 0.95),
      mapping = aes(colour = after_stat(probs)),
      alpha = 1
    )
  )

  # PMF2D HDR alpha default vs static alpha
  expect_no_warning(
    geom_pmf_2d(
      fun = function(v) dbinom(v[1], 5, 0.5) * dbinom(v[2], 5, 0.5),
      shade_hdr = 0.8,
      alpha = 0.9
    )
  )

  # PMF2D point-size default vs static size
  expect_no_warning(
    geom_pmf_2d(
      fun = function(v) dbinom(v[1], 5, 0.5) * dbinom(v[2], 5, 0.5),
      size = 3
    )
  )

  # 2D alpha-raster default vs static alpha
  expect_no_warning(
    geom_function_2d_1d(
      fun = function(v) sum(v^2), n = 5,
      raster_aes = "alpha", alpha = 0.5
    )
  )

  # PDF-2D raster (delegated alpha raster) vs static alpha
  expect_no_warning(
    geom_pdf_2d(
      fun = function(v) exp(-0.5 * sum(v^2)) / (2 * pi),
      xlim = c(-2, 2), ylim = c(-2, 2),
      type = "raster", alpha = 0.5
    )
  )

  # 1D-to-2D default colour vs static colour
  expect_no_warning(
    geom_function_1d_2d(fun = function(t) c(sin(t), cos(t)), colour = "black")
  )
})

test_that("layer mapping plus static value keeps the single native warning", {
  # When the USER supplies both a mapping and a static value on the same
  # layer, ggplot2's own defined-twice warning fires exactly once.
  w <- testthat::capture_warnings(
    geom_pmf(
      fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3),
      mapping = aes(alpha = after_stat(y)),
      alpha = 0.5
    )
  )
  expect_length(w, 1)
  expect_match(w, "defined twice")
})

test_that("plot-global mapping plus layer static override is silent", {
  d <- data.frame(x = 1:5, a = seq(0.2, 1, length.out = 5))
  expect_no_warning(
    ggplot_build(
      ggplot(d, aes(x = x, alpha = a)) +
        geom_ecdf(conf_int = FALSE, alpha = 1)
    )
  )
})

test_that("both colour alias spellings keep ggplot2's diagnostic", {
  expect_warning(
    geom_function_1d_1d(fun = sin, xlim = c(0, 1), color = "red", colour = "blue"),
    "[Dd]uplicated aesthetics"
  )
})
