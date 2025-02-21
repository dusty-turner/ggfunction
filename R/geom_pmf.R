#' Plot a Probability Mass Function as Lollipops
#'
#' `geom_pmf()` creates a ggplot2 layer that plots a probability mass function (PMF)
#' using a lollipop representation. Vertical segments extend from zero up to the probability
#' value at each integer support value and a point is drawn at the top.
#'
#' @inheritParams ggplot2::geom_point
#' @param fun A function to compute the PMF (e.g. [dbinom] or [dpois]). The function must
#'   accept a numeric vector as its first argument and return non-negative probability values.
#'   (Ideally, the probabilities sum to 1 over the support.)
#' @param xlim A numeric vector of length 2 specifying the range (of x values) over which to
#'   evaluate the PMF. If not provided, a default range of 0 to 10 is used.
#' @param linewidth linewidth of the points (defaults to 2).
#' @param color Color for the points and for the segments (defaults to `"black"`).
#' @param args A named list of additional arguments to pass to `fun`.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   # Plot a binomial PMF with n = 10 and p = 0.5 over 0 to 10.
#'   ggplot() +
#'     geom_pmf(fun = dbinom, args = list(size = 10, prob = 0.25), xlim = c(0, 10))
#'
#'   # now a Poisson
#'   ggplot() +
#'     geom_pmf(fun = dpois, args = list(lambda = 6), xlim = c(0, 15))
#'
#' @name geom_pmf
#' @aliases StatPMF GeomPMF
#' @export
geom_pmf <- function(mapping = NULL,
                     data = NULL,
                     stat = StatPMF,
                     position = "identity",
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE,
                     fun,
                     xlim = NULL,
                     linewidth = 2,
                     color = "black",
                     args = list()) {

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
    geom = GeomPMF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      linewidth = linewidth,
      color = color,
      args = args,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_pmf
#' @export
StatPMF <- ggproto("StatPMF", Stat,

  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, args = NULL, ...) {

    if (is.null(xlim)) {
      x_vals <- 0:10
    } else {
      x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
    }

    fun_injected <- function(x) {
      rlang::inject(fun(x, !!!args))
    }

    # check to make sure pmf is a real pmf
    invisible(check_pmf_normalization(fun_injected, support = x_vals, tol = 1e-2))

    y_vals <- fun_injected(x_vals)
    data.frame(x = x_vals, y = y_vals)
  }
)

#' @rdname geom_pmf
#' @export
GeomPMF <- ggproto("GeomPMF", GeomPoint,

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE, linewidth) {

    segment_data <- transform(data, yend = y, y = 0)

     segment_data$linewidth <- segment_data$size
     segment_data$size <- NULL
    ## can seperate color of segment with lollipop like this
    # segment_data$colour <- "red"

    seg_grob <- ggproto_parent(GeomSegment, self)$draw_panel(
      segment_data, panel_params, coord, na.rm = na.rm
    )

    pt_grob <- ggproto_parent(GeomPoint, self)$draw_panel(
      data, panel_params, coord, na.rm = na.rm)

    grid::grobTree(seg_grob, pt_grob)
  }
)
