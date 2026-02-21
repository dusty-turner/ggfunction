#' Plot a Discrete Quantile Function as a Step Function
#'
#' `geom_qf_discrete()` takes a PMF function, computes the cumulative sum, and
#' renders the inverse (quantile) function as a left-continuous step function
#' with horizontal segments, dashed vertical jumps, closed circles at the lower
#' limit of each jump, and open circles at the upper limit.
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A PMF function (e.g. [dbinom]). The function must accept a numeric
#'   vector as its first argument and return non-negative probability values that
#'   sum to 1.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 specifying the range of integer support
#'   values.
#' @param open_fill Fill color for the open (hollow) endpoint circles. Defaults to
#'   `NULL`, which uses the active theme's panel background color.
#' @param vert_type Line type for the vertical jump segments. Defaults to
#'   `"dashed"`.
#' @param show_points Logical. If `FALSE`, suppresses all endpoint circles (open
#'   and closed). Defaults to `TRUE`.
#' @param show_vert Logical. If `FALSE`, suppresses the vertical jump segments.
#'   Defaults to `TRUE`.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   ggplot() +
#'     geom_qf_discrete(fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10))
#'
#'   ggplot() +
#'     geom_qf_discrete(fun = dpois, args = list(lambda = 5), xlim = c(0, 15))
#'
#' @name geom_qf_discrete
#' @aliases StatQFDiscrete GeomQFDiscrete
#' @export
geom_qf_discrete <- function(
    mapping = NULL,
    data = NULL,
    stat = StatQFDiscrete,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    args = list(),
    xlim = NULL,
    open_fill = NULL,
    vert_type = "dashed",
    show_points = TRUE,
    show_vert = TRUE
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
    geom = GeomQFDiscrete,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      args = args,
      xlim = xlim,
      na.rm = na.rm,
      open_fill = open_fill,
      vert_type = vert_type,
      show_points = show_points,
      show_vert = show_vert,
      ...
    )
  )
}

#' @rdname geom_qf_discrete
#' @export
StatQFDiscrete <- ggproto("StatQFDiscrete", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, args = NULL) {

    if (is.null(xlim)) {
      x_vals <- 0:10
    } else {
      x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
    }

    fun_injected <- function(x) rlang::inject(fun(x, !!!args))

    invisible(check_pmf_normalization(fun_injected, support = x_vals, tol = 1e-2))

    pmf_vals <- fun_injected(x_vals)
    cdf_vals <- cumsum(pmf_vals)

    # x = F(x_k) (probability axis), y = x_k (support value axis)
    data.frame(x = cdf_vals, y = x_vals)
  }
)

#' @rdname geom_qf_discrete
#' @export
GeomQFDiscrete <- ggproto("GeomQFDiscrete", Geom,

  required_aes = c("x", "y"),

  default_aes = aes(
    colour    = "black",
    alpha     = NA,
    linewidth = 0.5,
    linetype  = 1,
    shape     = 19,
    size      = 1.5,
    fill      = NA,
    stroke    = 0.5
  ),

  draw_group = function(data, panel_params, coord,
                        open_fill = NULL, vert_type = "dashed",
                        show_points = TRUE, show_vert = TRUE) {
    if (is.null(open_fill)) {
      bg <- ggplot2::theme_get()$panel.background
      open_fill <- if (!inherits(bg, "element_blank") && !is.null(bg$fill) && !is.na(bg$fill)) bg$fill else "white"
    }
    n <- nrow(data)

    # Horizontal segments (n total, defined only on [0, 1]):
    #   [0 → x[1]] at height y[1],
    #   [x[k] → x[k+1]] at height y[k+1], ...,
    #   [x[n-1] → x[n]] at height y[n]
    # where x = F(x_k) and y = x_k (support value)
    data_hori        <- data
    data_hori$x      <- c(0, data$x[-n])
    data_hori$xend   <- data$x
    data_hori$y      <- data$y
    data_hori$yend   <- data$y

    coord_hori <- coord$transform(data_hori, panel_params)

    grobs <- list()

    grobs$hori <- grid::segmentsGrob(
      coord_hori$x, coord_hori$y, coord_hori$xend, coord_hori$yend,
      default.units = "native",
      gp = grid::gpar(
        col = scales::alpha(coord_hori$colour, coord_hori$alpha),
        lwd = coord_hori$linewidth * .pt,
        lty = coord_hori$linetype
      )
    )

    if (n > 1) {
      # Vertical jump segments at each p = x[k] for k = 1 to n-1:
      # from y[k] (closed, achieved) up to y[k+1] (open, not yet achieved)
      data_vert        <- data[-n, ]   # n-1 rows: x = F(x_k), y = x_k
      data_vert$xend   <- data_vert$x  # same p (vertical segment)
      data_vert$yend   <- data$y[-1]   # top of jump = x_{k+1}

      coord_vert <- coord$transform(data_vert, panel_params)

      if (show_vert) {
        grobs$vert <- grid::segmentsGrob(
          coord_vert$x, coord_vert$y, coord_vert$xend, coord_vert$yend,
          default.units = "native",
          gp = grid::gpar(
            col = scales::alpha(coord_vert$colour, coord_vert$alpha),
            lwd = coord_vert$linewidth * .pt,
            lty = vert_type
          )
        )
      }

      if (show_points) {
        # Closed circle at bottom of each jump (QF achieves this value at p = F(x_k))
        grobs$closed <- grid::pointsGrob(
          coord_vert$x, coord_vert$y,
          pch = coord_vert$shape,
          default.units = "native",
          gp = grid::gpar(
            col      = scales::alpha(coord_vert$colour, coord_vert$alpha),
            fill     = scales::alpha(coord_vert$colour, coord_vert$alpha),
            fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
            lwd      = coord_vert$stroke * .stroke / 2
          )
        )

        # Open circle at top of each jump (next value not yet achieved)
        grobs$open <- grid::pointsGrob(
          coord_vert$xend, coord_vert$yend,
          default.units = "native",
          pch = 21,
          gp = grid::gpar(
            col      = coord_vert$colour,
            fill     = open_fill,
            fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
            lwd      = coord_vert$stroke * .stroke / 2
          )
        )
      }
    }

    grid::gTree(children = do.call(grid::gList, grobs))
  },

  draw_key = draw_key_path
)
