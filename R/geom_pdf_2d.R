#' Plot a Bivariate PDF
#'
#' `geom_pdf_2d()` visualizes a theoretical bivariate probability density
#' function either through highest density regions (HDRs) or as a raw density
#' raster. For HDRs it is a thin, probability-facing wrapper around
#' \pkg{ggdensity}: HDR computation, contour construction, probability labels,
#' and default aesthetics are delegated to [ggdensity::geom_hdr_fun()] for
#' `type = "hdr"` (filled regions) and [ggdensity::geom_hdr_lines_fun()] for
#' `type = "hdr_lines"` (boundary contours). For `type = "raster"`, the density
#' is evaluated on the requested grid and drawn with [ggplot2::geom_raster()].
#'
#' @details
#' The supplied density uses ggfunction's 2D function convention: `fun`
#' receives a single numeric vector `v = c(x, y)` and returns one density
#' value. \pkg{ggdensity} expects a function of two vectorized arguments
#' `fun(x, y)`; `geom_pdf_2d()` adapts between the two interfaces internally,
#' closing over `args` in the process.
#'
#' Raw density rasters use [geom_function_2d_1d()] with
#' `raster_aes = "alpha"`: the evaluated density is mapped to literal alpha
#' values scaled from 0 to 1 by default, with a fixed dark gray fill. `probs`
#' is ignored for `type = "raster"`.
#'
#' For arbitrary iso-density contours (level sets not calibrated to probability
#' content), use [geom_function_2d_1d()] with `type = "contour"` or
#' `"contour_filled"`.
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A bivariate density function accepting a length-2 numeric vector
#'   `v = c(x, y)` and returning one numeric density value.
#' @param xlim,ylim Numeric vectors of length 2 specifying the evaluation
#'   range.
#' @param n Grid resolution. Defaults to `100`.
#' @param args A named list of additional arguments passed to `fun`.
#' @param probs HDR probabilities passed to ggdensity. Defaults to
#'   `c(0.99, 0.95, 0.8, 0.5)`. Ignored when `type = "raster"`.
#' @param type Character. `"hdr"` (default) draws filled highest density
#'   regions; `"hdr_lines"` draws HDR boundary contour lines; `"raster"` draws
#'   the evaluated density as a dark-gray raster with alpha scaled from 0 to 1.
#'
#' @section Computed variables:
#' HDR computed variables and default aesthetics are those supplied by the
#' delegated \pkg{ggdensity} stat. In particular, the built data includes an
#' ordered factor `probs`, which is mapped to `alpha` by default for filled
#' HDRs. Raster layers expose `after_stat(z)`, the evaluated density value,
#' and scale it to literal 0--1 alpha values by default.
#'
#' @return A ggplot2 layer.
#'
#' @seealso [ggdensity::geom_hdr_fun()] and [ggdensity::geom_hdr_lines_fun()]
#'   for the underlying HDR machinery; [geom_function_2d_1d()] for raw
#'   iso-density contours; [geom_pdf()] for univariate densities.
#'
#' @examples
#' dbvn <- function(v, mu = c(0, 0), Sigma = diag(2)) {
#'   x <- matrix(v - mu, ncol = 1)
#'   Sinv <- solve(Sigma)
#'   1 / (2 * pi * sqrt(det(Sigma))) *
#'     exp(-0.5 * as.numeric(t(x) %*% Sinv %*% x))
#' }
#'
#' ggplot() +
#'   geom_pdf_2d(
#'     fun = dbvn,
#'     xlim = c(-3, 3),
#'     ylim = c(-3, 3),
#'     probs = c(0.5, 0.8, 0.95)
#'   ) +
#'   coord_equal()
#'
#' ggplot() +
#'   geom_pdf_2d(
#'     fun = dbvn,
#'     xlim = c(-3, 3),
#'     ylim = c(-3, 3),
#'     probs = c(0.5, 0.8, 0.95),
#'     type = "hdr_lines"
#'   ) +
#'   coord_equal()
#'
#' # Parameterized via `args`
#' Sigma <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
#' ggplot() +
#'   geom_pdf_2d(
#'     fun = dbvn,
#'     args = list(Sigma = Sigma),
#'     xlim = c(-3, 3),
#'     ylim = c(-3, 3),
#'     type = "hdr_lines"
#'   ) +
#'   coord_equal()
#'
#' ggplot() +
#'   geom_pdf_2d(fun = dbvn, xlim = c(-3, 3), ylim = c(-3, 3), type = "raster") +
#'   coord_equal()
#'
#' @name geom_pdf_2d
#' @export
#' @importFrom ggdensity geom_hdr_fun geom_hdr_lines_fun
geom_pdf_2d <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 100,
    args = list(),
    probs = c(0.99, 0.95, 0.8, 0.5),
    type = c("hdr", "hdr_lines", "raster")
) {
  type <- match.arg(type)

  if (identical(type, "raster")) {
    dots <- list(...)
    dots$na.rm <- na.rm

    do.call(geom_function_2d_1d, c(
      list(
        mapping = mapping,
        data = data,
        position = position,
        fun = fun,
        xlim = xlim,
        ylim = ylim,
        n = n,
        args = args,
        type = "raster",
        raster_aes = "alpha",
        show.legend = show.legend,
        inherit.aes = inherit.aes
      ),
      dots
    ))

  } else if (identical(type, "hdr")) {
    fun_xy <- pdf2d_vector_fun_to_xy_fun(fun, args)
    ggdensity::geom_hdr_fun(
      mapping = mapping,
      data = data,
      position = position,
      fun = fun_xy,
      args = list(),
      probs = probs,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  } else {
    fun_xy <- pdf2d_vector_fun_to_xy_fun(fun, args)
    ggdensity::geom_hdr_lines_fun(
      mapping = mapping,
      data = data,
      position = position,
      fun = fun_xy,
      args = list(),
      probs = probs,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  }
}

#' @noRd
pdf2d_vector_fun_to_xy_fun <- function(fun, args = list()) {
  force(fun)
  force(args)

  function(x, y) {
    n <- max(length(x), length(y))
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)

    vapply(seq_len(n), function(i) {
      value <- do.call(fun, c(list(c(x[i], y[i])), args))
      if (!is.numeric(value) || length(value) != 1L) {
        cli::cli_abort("{.arg fun} must return one numeric density value.")
      }
      value
    }, numeric(1))
  }
}
