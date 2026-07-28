dbvn <- function(v, mu = c(0, 0), Sigma = diag(2)) {
  x <- matrix(v - mu, ncol = 1)
  Sinv <- solve(Sigma)
  1 / (2 * pi * sqrt(det(Sigma))) *
    exp(-0.5 * as.numeric(t(x) %*% Sinv %*% x))
}

dbvn_xy <- function(x, y) dnorm(x) * dnorm(y)

test_that("geom_pdf_2d builds filled HDRs without error", {
  p <- ggplot() +
    geom_pdf_2d(fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 50) +
    coord_equal()
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))
})

test_that("geom_pdf_2d builds HDR lines without error", {
  p <- ggplot() +
    geom_pdf_2d(
      fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 50,
      type = "hdr_lines"
    )
  expect_no_error(ggplot_build(p))
})

test_that("geom_pdf_2d exposes honest HDR computation-domain controls", {
  expect_warning(
    layer <- geom_pdf_2d(
      fun = dbvn,
      xlim = c(-2, 2), ylim = c(-2, 2),
      hdr_xlim = c(-3, 3), hdr_ylim = c(-3, 3),
      n = 30
    ),
    "computed and evaluated over"
  )
  p <- ggplot() + layer

  expect_silent(built <- ggplot_build(p))
  expect_true(max(abs(built$data[[1]]$x), na.rm = TRUE) > 2)
})

test_that("geom_pdf_2d builds density rasters without error", {
  p <- ggplot() +
    geom_pdf_2d(
      fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 30,
      type = "raster"
    ) +
    coord_equal()
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))
})

test_that("geom_pdf_2d defaults to filled HDRs", {
  l_default <- geom_pdf_2d(fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3))
  l_lines <- geom_pdf_2d(
    fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3),
    type = "hdr_lines"
  )
  expect_s3_class(l_default$stat, "StatHdrFun")
  expect_s3_class(l_lines$stat, "StatHdrLinesFun")
})

test_that("geom_pdf_2d raster uses density alpha on a dark-gray raster", {
  l_raster <- geom_pdf_2d(
    fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3),
    n = 20, type = "raster"
  )
  expect_s3_class(l_raster$stat, "StatFunction2d")
  expect_s3_class(l_raster$geom, "GeomFunction2d")
  expect_equal(
    rlang::as_label(l_raster$mapping$alpha),
    "after_stat(function2d_alpha_rescale(z))"
  )

  built <- ggplot_build(ggplot() + l_raster)
  expect_equal(nrow(built$data[[1]]), 20^2)
  expect_true("z" %in% names(built$data[[1]]))
  expect_equal(unique(built$data[[1]]$fill), "grey20")
  expect_gt(length(unique(built$data[[1]]$alpha)), 1)
  expect_equal(range(built$data[[1]]$alpha), c(0, 1))
  expect_s3_class(built$data[[1]]$alpha, "AsIs")
})

test_that("geom_pdf_2d raster handles degenerate alpha ranges", {
  p_zero <- ggplot() +
    geom_pdf_2d(
      fun = function(v) 0,
      xlim = c(-1, 1), ylim = c(-1, 1), n = 5, type = "raster"
    )
  b_zero <- ggplot_build(p_zero)
  expect_equal(unique(b_zero$data[[1]]$alpha), 0)

  p_const <- ggplot() +
    geom_pdf_2d(
      fun = function(v) 1,
      xlim = c(-1, 1), ylim = c(-1, 1), n = 5, type = "raster"
    )
  b_const <- ggplot_build(p_const)
  expect_equal(unique(b_const$data[[1]]$alpha), 1)
})

test_that("geom_pdf_2d raster allows mapping and fill overrides", {
  p_mapped <- ggplot() +
    geom_pdf_2d(
      fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
      type = "raster",
      mapping = aes(fill = after_stat(z), alpha = after_stat(sqrt(z)))
    )
  b_mapped <- ggplot_build(p_mapped)
  expect_gt(length(unique(b_mapped$data[[1]]$fill)), 1)
  expect_gt(length(unique(b_mapped$data[[1]]$alpha)), 1)
  expect_false(all(b_mapped$data[[1]]$fill == "grey20"))

  p_fixed <- ggplot() +
    geom_pdf_2d(
      fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
      type = "raster",
      fill = "steelblue"
    )
  b_fixed <- ggplot_build(p_fixed)
  expect_equal(unique(b_fixed$data[[1]]$fill), "steelblue")

  p_alpha <- ggplot() +
    geom_pdf_2d(
      fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 20,
      type = "raster",
      alpha = 0.4
    )
  b_alpha <- ggplot_build(p_alpha)
  expect_equal(unique(b_alpha$data[[1]]$alpha), 0.4)
})

test_that("pdf2d_vector_fun_to_xy_fun adapts and vectorizes fun", {
  fun_xy <- ggfunction:::pdf2d_vector_fun_to_xy_fun(dbvn)
  x <- c(-1, 0, 1)
  y <- c(0.5, 0, -0.5)
  expect_equal(fun_xy(x, y), dnorm(x) * dnorm(y))
  # Recycles the shorter argument
  expect_equal(fun_xy(0, y), dnorm(0) * dnorm(y))
})

test_that("args are passed through the adapter", {
  Sigma <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
  fun_xy <- ggfunction:::pdf2d_vector_fun_to_xy_fun(
    dbvn, args = list(Sigma = Sigma)
  )
  expect_equal(fun_xy(1, 1), dbvn(c(1, 1), Sigma = Sigma))

  dbvn_fixed <- function(v) dbvn(v, Sigma = Sigma)
  b_args <- ggplot_build(
    ggplot() +
      geom_pdf_2d(
        fun = dbvn, args = list(Sigma = Sigma),
        xlim = c(-3, 3), ylim = c(-3, 3), n = 40
      )
  )
  b_fixed <- ggplot_build(
    ggplot() +
      geom_pdf_2d(
        fun = dbvn_fixed,
        xlim = c(-3, 3), ylim = c(-3, 3), n = 40
      )
  )
  expect_equal(b_args$data[[1]], b_fixed$data[[1]])
})

test_that("args are passed through in raster mode", {
  Sigma <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
  dbvn_fixed <- function(v) dbvn(v, Sigma = Sigma)

  b_args <- ggplot_build(
    ggplot() +
      geom_pdf_2d(
        fun = dbvn, args = list(Sigma = Sigma),
        xlim = c(-3, 3), ylim = c(-3, 3), n = 20, type = "raster"
      )
  )
  b_fixed <- ggplot_build(
    ggplot() +
      geom_pdf_2d(
        fun = dbvn_fixed,
        xlim = c(-3, 3), ylim = c(-3, 3), n = 20, type = "raster"
      )
  )

  expect_equal(b_args$data[[1]]$z, b_fixed$data[[1]]$z)
  expect_equal(b_args$data[[1]]$alpha, b_fixed$data[[1]]$alpha)
})

test_that("invalid type errors via match.arg", {
  expect_error(
    geom_pdf_2d(fun = dbvn, type = "contour"),
    "should be one of"
  )
})

test_that("non-scalar fun return aborts with a clear message", {
  fun_xy <- ggfunction:::pdf2d_vector_fun_to_xy_fun(function(v) c(1, 2))
  expect_error(fun_xy(0, 0), "must return one numeric density value")

  fun_xy_chr <- ggfunction:::pdf2d_vector_fun_to_xy_fun(function(v) "a")
  expect_error(fun_xy_chr(0, 0), "must return one numeric density value")
})

test_that("wrapper matches a direct ggdensity call", {
  p_wrapped <- ggplot() +
    geom_pdf_2d(
      fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3),
      probs = c(0.5, 0.8), n = 50
    )
  p_direct <- ggplot() +
    ggdensity::geom_hdr_fun(
      fun = dbvn_xy, xlim = c(-3, 3), ylim = c(-3, 3),
      probs = c(0.5, 0.8), n = 50
    )

  b_wrapped <- ggplot_build(p_wrapped)
  b_direct <- ggplot_build(p_direct)
  expect_equal(
    levels(b_wrapped$data[[1]]$probs),
    levels(b_direct$data[[1]]$probs)
  )
})

test_that("static aesthetics pass through to ggdensity", {
  expect_no_error(ggplot_build(
    ggplot() +
      geom_pdf_2d(
        fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 30,
        fill = "steelblue"
      )
  ))
  expect_no_error(ggplot_build(
    ggplot() +
      geom_pdf_2d(
        fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), n = 30,
        type = "hdr_lines", colour = "black", linewidth = 1
      )
  ))
})

test_that("bimodal mixture HDR lines produce multiple pieces", {
  mix <- function(v) {
    0.55 * dbvn(v, mu = c(-1.5, 0), Sigma = diag(c(0.35, 0.6))) +
      0.45 * dbvn(v, mu = c(1.4, 0.3), Sigma = diag(c(0.5, 0.4)))
  }
  built <- ggplot_build(
    ggplot() +
      geom_pdf_2d(
        fun = mix, xlim = c(-4, 4), ylim = c(-3, 3),
        probs = c(0.5, 0.8), n = 50, type = "hdr_lines"
      )
  )
  pieces <- unique(interaction(
    built$data[[1]]$probs, built$data[[1]]$group, drop = TRUE
  ))
  expect_gt(length(pieces), 2)
})

# --- E-07: finite function domain requirement ---

test_that("function-only geom_pdf_2d requires a finite domain (E-07)", {
  dbvn <- function(v) exp(-0.5 * sum(v^2)) / (2 * pi)
  expect_error(
    ggplot_build(ggplot() + geom_pdf_2d(fun = dbvn)),
    "xlim.*ylim"
  )
  expect_error(
    ggplot_build(ggplot() + geom_pdf_2d(fun = dbvn, type = "raster")),
    "xlim.*ylim"
  )
  expect_error(
    ggplot_build(ggplot() + geom_pdf_2d(fun = dbvn, type = "hdr_lines")),
    "xlim.*ylim"
  )
})

test_that("hdr_xlim/hdr_ylim satisfy the HDR domain requirement (E-07)", {
  dbvn <- function(v) exp(-0.5 * sum(v^2)) / (2 * pi)
  expect_no_error(
    l <- geom_pdf_2d(fun = dbvn, hdr_xlim = c(-3, 3), hdr_ylim = c(-3, 3))
  )
  expect_gt(nrow(ggplot_build(ggplot() + l)$data[[1]]), 0)
})

test_that("malformed limits abort with a clear message (E-07)", {
  dbvn <- function(v) exp(-0.5 * sum(v^2)) / (2 * pi)
  expect_error(geom_pdf_2d(fun = dbvn, xlim = c(3, -3), ylim = c(-3, 3)), "increasing")
  expect_error(geom_pdf_2d(fun = dbvn, xlim = c(-3, 3), ylim = c(0, Inf)), "increasing")
})

test_that("inherited plot mappings satisfy the delayed domain validation (E-07)", {
  dbvn <- function(v) exp(-0.5 * sum(v^2)) / (2 * pi)
  set.seed(1)
  d <- data.frame(x = rnorm(20), y = rnorm(20))
  expect_no_error(
    b <- ggplot_build(
      ggplot(d, aes(x, y)) +
        geom_pdf_2d(fun = dbvn, type = "raster", inherit.aes = TRUE)
    )
  )
  expect_gt(nrow(b$data[[1]]), 0)
})

test_that("HDR types reject transformed position scales (A-01)", {
  dbvn <- function(v) exp(-0.5 * sum(v^2)) / (2 * pi)
  expect_error(
    ggplot_build(
      ggplot() +
        geom_pdf_2d(fun = dbvn, xlim = c(1, 10), ylim = c(1, 10)) +
        scale_x_log10()
    ),
    "transformed position scales"
  )
})
