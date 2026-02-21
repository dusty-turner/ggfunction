#' Plot a Discrete Survival Function as a Step Function
#'
#' `geom_survival_discrete()` takes a PMF function, computes the cumulative sum,
#' and renders the complement \eqn{S(x) = 1 - F(x)} as a right-continuous step
#' function with horizontal segments, dashed vertical jumps, open circles at the
#' lower limit of each jump, and closed circles at the upper limit.
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A PMF function (e.g. [dbinom]). The function must accept a numeric
#'   vector as its first argument and return non-negative probability values that
#'   sum to 1.
#' @param xlim A numeric vector of length 2 specifying the range of integer support
#'   values.
#' @param args A named list of additional arguments to pass to `fun`.
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
#'     geom_survival_discrete(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))
#'
#'   ggplot() +
#'     geom_survival_discrete(fun = dpois, xlim = c(0, 15), args = list(lambda = 5))
#'
#' @name geom_survival_discrete
#' @aliases StatSurvivalDiscrete GeomSurvivalDiscrete
#' @export
geom_survival_discrete <- function(
    mapping = NULL,
    data = NULL,
    stat = StatSurvivalDiscrete,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    args = list(),
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
    geom = GeomSurvivalDiscrete,
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

#' @rdname geom_survival_discrete
#' @export
StatSurvivalDiscrete <- ggproto("StatSurvivalDiscrete", Stat,
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
    survival_vals <- 1 - cdf_vals

    data.frame(x = x_vals, y = survival_vals)
  }
)

#' @rdname geom_survival_discrete
#' @export
GeomSurvivalDiscrete <- ggproto("GeomSurvivalDiscrete", Geom,

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

    # Horizontal segments (right-continuous):
    #   [left_boundary → x[1]] at height y[1]  (S before any mass is removed)
    #   Wait — S is right-continuous: S(x) = P(X > x), so:
    #   Before x[1]: S = 1; at x[1] it drops. We use CDF convention:
    #   segment at height S(x[k]) from x[k] to x[k+1], plus
    #   leftmost extension from panel left to x[1] at height 1 (or S[0] = 1),
    #   and rightmost extension from x[n] to panel right at height S(x[n]).
    data_hori        <- data[c(1, 1:n), ]
    data_hori$x      <- c(panel_params$x.range[1], data$x)
    data_hori$xend   <- c(data$x, panel_params$x.range[2])
    data_hori$y      <- c(1, data$y)
    data_hori$yend   <- c(1, data$y)

    # Vertical jump segments at each x[k]: from S(x[k-1]) (or 1) down to S(x[k])
    data_vert        <- data
    data_vert$xend   <- data$x
    data_vert$y      <- c(1, data$y[-n])
    data_vert$yend   <- data$y

    coord_hori <- coord$transform(data_hori, panel_params)
    coord_vert <- coord$transform(data_vert, panel_params)

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
      # Open circle at top of each jump (S just before the drop — left limit)
      grobs$open <- grid::pointsGrob(
        coord_vert$x, coord_vert$y,
        default.units = "native",
        pch = 21,
        gp = grid::gpar(
          col      = coord_vert$colour,
          fill     = open_fill,
          fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
          lwd      = coord_vert$stroke * .stroke / 2
        )
      )

      # Closed circle at bottom of each jump (S achieves this value at x[k])
      grobs$closed <- grid::pointsGrob(
        coord_vert$xend, coord_vert$yend,
        pch = coord_vert$shape,
        default.units = "native",
        gp = grid::gpar(
          col      = scales::alpha(coord_vert$colour, coord_vert$alpha),
          fill     = scales::alpha(coord_vert$colour, coord_vert$alpha),
          fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
          lwd      = coord_vert$stroke * .stroke / 2
        )
      )
    }

    grid::gTree(children = do.call(grid::gList, grobs))
  },

  draw_key = draw_key_path
)
