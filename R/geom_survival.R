#' Plot a Survival Function S(x) = 1 - F(x)
#'
#' `geom_survival()` creates a ggplot2 layer that plots a survival function,
#' computed as \eqn{S(x) = 1 - F(x)} where \eqn{F(x)} is a CDF.
#' By default only the line is drawn (no fill).
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A CDF function (e.g. [pnorm]). The function must accept a numeric vector
#'   and return values between 0 and 1. The survival function is computed as `1 - fun(x)`.
#' @param n Number of points at which to evaluate `fun`. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 giving the x-range.
#' @param color Line color for the survival curve.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   ggplot() +
#'     geom_survival(fun = pnorm, xlim = c(-3, 3))
#'
#'   ggplot() +
#'     geom_survival(fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))
#'
#' @name geom_survival
#' @aliases StatSurvival GeomSurvival
#' @export
geom_survival <- function(
    mapping = NULL,
    data = NULL,
    stat = StatSurvival,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    n = 101,
    args = list(),
    xlim = NULL,
    color = "black"
    ) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  default_mapping <- aes(x = after_stat(x), y = after_stat(y))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSurvival,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      n = n,
      xlim = xlim,
      args = args,
      na.rm = na.rm,
      color = color,
      ...
    )
  )
}

#' @rdname geom_survival
#' @export
StatSurvival <- ggproto("StatSurvival", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, n = 101, args = NULL) {

    range <- if (is.null(scales$x)) {
      xlim %||% c(0, 1)
    } else {
      xlim %||% scales$x$dimension()
    }

    fun_injected <- function(x) {
      rlang::inject(fun(x, !!!args))
    }

    xseq <- seq(range[1], range[2], length.out = n)
    cdf_vals <- fun_injected(xseq)
    y_out <- 1 - cdf_vals

    data.frame(x = xseq, y = y_out)
  }
)

#' @rdname geom_survival
#' @export
GeomSurvival <- ggproto("GeomSurvival", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE
                        ) {

    ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )
  }
)
