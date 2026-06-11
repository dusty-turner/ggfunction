#' Plot a Bivariate Probability Mass Function
#'
#' `geom_pmf_2d()` evaluates a bivariate probability mass function on a
#' discrete lattice and renders it either as a heatmap of tiles
#' (`type = "tile"`, the default), with fill encoding the probability mass, or
#' as a balloon plot of points (`type = "point"`), with size encoding the
#' probability mass. An optional highest density region can be highlighted via
#' `shade_hdr`, mirroring [geom_pmf()]: lattice points outside the HDR are
#' rendered in grey.
#'
#' @details
#' The supplied mass function uses ggfunction's 2D function convention, shared
#' with [geom_pdf_2d()] and [geom_function_2d_1d()]: `fun` receives a single
#' numeric vector `v = c(x, y)` and returns one probability mass.
#'
#' The evaluation lattice is the product of the per-axis supports: integers
#' spanning `xlim` and `ylim` (each defaulting to `0:10`), or the exact values
#' in `support_x` and `support_y` when provided. Distributions with
#' non-product support, such as a trinomial whose support is a simplex, can be
#' plotted by evaluating over a bounding lattice and returning `0` off the
#' support; cells with zero mass are removed by default (`drop_zeros = TRUE`).
#'
#' The total mass over the lattice is checked to be 1 (within `1e-2`), with a
#' [cli::cli_alert()] otherwise; disable via
#' `options(ggfunction.check = FALSE)`. As in [geom_pmf()], the exact
#' `shade_hdr` coverage may not be achievable for a discrete distribution, in
#' which case the smallest HDR with coverage at least `shade_hdr` is used and
#' a message reports the actual coverage.
#'
#' Unlike [geom_pmf()], `geom_pmf_2d()` defaults to `inherit.aes = FALSE`
#' since the layer is driven entirely by `fun`. For point mode,
#' [ggplot2::scale_size_area()] is recommended so that mass is proportional to
#' point area and zero mass maps to zero area.
#'
#' @inheritParams ggplot2::geom_tile
#' @param fun A bivariate probability mass function accepting a length-2
#'   numeric vector `v = c(x, y)` and returning one numeric mass value.
#' @param xlim,ylim Numeric vectors of length 2 specifying the lattice range
#'   on each axis; integers spanning the range are used. Each defaults to
#'   `c(0, 10)`.
#' @param support_x,support_y Optional numeric vectors of exact support points
#'   for each axis, overriding `xlim`/`ylim`. Non-integer values are allowed.
#' @param args A named list of additional arguments passed to `fun`.
#' @param shade_hdr (Optional) A numeric value between 0 and 1 specifying the
#'   target coverage of the highest density region (HDR) to highlight: the
#'   smallest set of lattice points containing at least the specified
#'   probability mass. Points outside the HDR are rendered in grey. Because a
#'   discrete distribution may not achieve the exact coverage, the smallest
#'   HDR with coverage >= `shade_hdr` is used and a message is issued via
#'   [cli::cli_inform()] reporting both the specified and actual coverage
#'   whenever they differ.
#' @param drop_zeros Logical. If `TRUE` (default), lattice points with zero
#'   mass are removed before rendering. Useful for distributions with
#'   non-product support evaluated over a bounding lattice.
#' @param type Character. Either `"tile"` (default) for a heatmap with
#'   `fill = after_stat(prob)`, or `"point"` for a balloon plot with
#'   `size = after_stat(prob)`.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)` and `after_stat(y)`}{Lattice coordinates.}
#'   \item{`after_stat(prob)`}{Probability mass at each lattice point.}
#'   \item{`after_stat(hdr)`}{Logical HDR membership; all `TRUE` when
#'   `shade_hdr` is `NULL`.}
#' }
#'
#' @section Aesthetics:
#' Tile mode understands the aesthetics of [ggplot2::geom_tile()] (notably
#' `fill`, `alpha`, `colour`, `linewidth`, `width`, `height`); point mode
#' those of [ggplot2::geom_point()] (notably `size`, `colour`, `shape`,
#' `alpha`, `stroke`). The probability mass is mapped to `fill` (tile) or
#' `size` (point) by default. Note that legend keys are drawn from the scale
#' and are not greyed by `shade_hdr`.
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_pmf()] for univariate mass functions; [geom_pdf_2d()] for
#'   bivariate densities.
#'
#' @examples
#' # Independent product binomial
#' dbinom2 <- function(v, sizes = c(10, 10), probs = c(0.5, 0.5)) {
#'   dbinom(v[1], sizes[1], probs[1]) * dbinom(v[2], sizes[2], probs[2])
#' }
#'
#' ggplot() +
#'   geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10))
#'
#' # Parameterized via `args`
#' ggplot() +
#'   geom_pmf_2d(
#'     fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
#'     args = list(probs = c(0.3, 0.7))
#'   )
#'
#' # Balloon plot
#' ggplot() +
#'   geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
#'     type = "point") +
#'   scale_size_area()
#'
#' # Highlight the 80% highest density region
#' ggplot() +
#'   geom_pmf_2d(fun = dbinom2, xlim = c(0, 10), ylim = c(0, 10),
#'     shade_hdr = 0.8)
#'
#' # Non-product support: trinomial over a bounding lattice
#' dtrinom <- function(v, size = 8, prob = c(0.3, 0.3, 0.4)) {
#'   if (sum(v) > size) return(0)
#'   dmultinom(c(v, size - sum(v)), prob = prob)
#' }
#'
#' ggplot() +
#'   geom_pmf_2d(fun = dtrinom, xlim = c(0, 8), ylim = c(0, 8)) +
#'   coord_equal()
#'
#' @name geom_pmf_2d
#' @aliases StatPMF2d GeomPMF2dTile GeomPMF2dPoint
#' @importFrom ggplot2 GeomTile
#' @export
geom_pmf_2d <- function(
    mapping = NULL,
    data = NULL,
    stat = StatPMF2d,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    ylim = NULL,
    support_x = NULL,
    support_y = NULL,
    args = list(),
    shade_hdr = NULL,
    drop_zeros = TRUE,
    type = c("tile", "point")
) {
  type <- match.arg(type)

  if (is.null(data)) data <- ensure_nonempty_data(data)

  if (identical(type, "tile")) {
    geom <- GeomPMF2dTile
    default_mapping <- aes(
      x = after_stat(x), y = after_stat(y), fill = after_stat(prob)
    )
  } else {
    geom <- GeomPMF2dPoint
    default_mapping <- aes(
      x = after_stat(x), y = after_stat(y), size = after_stat(prob)
    )
  }

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      support_x = support_x,
      support_y = support_y,
      args = args,
      shade_hdr = shade_hdr,
      drop_zeros = drop_zeros,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_pmf_2d
#' @export
StatPMF2d <- ggproto("StatPMF2d", Stat,

  compute_group = function(data, scales, fun, xlim = NULL, ylim = NULL,
                           support_x = NULL, support_y = NULL, args = NULL,
                           shade_hdr = NULL, drop_zeros = TRUE, ...) {

    x_vals <- discrete_support(xlim, support_x)
    y_vals <- discrete_support(ylim, support_y)
    grid <- expand.grid(x = x_vals, y = y_vals)

    args <- args %||% list()
    fun_injected <- function(v) rlang::inject(fun(v, !!!args))
    prob <- vectorize(fun_injected)(as.matrix(grid[, c("x", "y")]))

    if (!is.numeric(prob) || length(prob) != nrow(grid)) {
      cli::cli_abort("{.arg fun} must return one numeric mass value per lattice point.")
    }
    grid$prob <- as.numeric(prob)

    invisible(check_pmf_mass_normalization(grid$prob, tol = 1e-2))

    grid$hdr <- discrete_hdr_indicator(grid$prob, shade_hdr)

    if (isTRUE(drop_zeros)) {
      grid <- grid[grid$prob > 0, , drop = FALSE]
    }

    grid
  }
)

#' @rdname geom_pmf_2d
#' @export
GeomPMF2dTile <- ggproto("GeomPMF2dTile", GeomTile,

  draw_panel = function(self, data, panel_params, coord,
                        lineend = "butt", linejoin = "mitre") {
    if (!is.null(data$hdr)) {
      data$fill <- ifelse(data$hdr, data$fill, "grey70")
    }
    ggproto_parent(GeomTile, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
  }
)

#' @rdname geom_pmf_2d
#' @export
GeomPMF2dPoint <- ggproto("GeomPMF2dPoint", GeomPoint,

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    if (!is.null(data$hdr)) {
      data$colour <- ifelse(data$hdr, data$colour, "grey70")
      if (!is.null(data$fill)) {
        data$fill <- ifelse(data$hdr, data$fill, "grey70")
      }
    }
    ggproto_parent(GeomPoint, self)$draw_panel(
      data, panel_params, coord, na.rm = na.rm
    )
  }
)
