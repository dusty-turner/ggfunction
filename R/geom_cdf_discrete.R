#' Plot a Discrete CDF as a Step Function
#'
#' `geom_cdf_discrete()` renders a discrete CDF as a right-continuous step
#' function with horizontal segments, dashed vertical jumps, open circles at
#' the lower limit of each jump, and closed circles at the upper limit.
#'
#' Supply **either** `pmf_fun` (a PMF such as [dbinom], from which the CDF is
#' computed via cumulative summation) **or** `fun` (a CDF such as [pbinom],
#' evaluated directly on the integer support).
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A discrete CDF function (e.g. [pbinom]). Evaluated directly on
#'   the integer support derived from `xlim` or `support`. Exactly one of
#'   `fun` or `pmf_fun` must be provided.
#' @param pmf_fun A PMF function (e.g. [dbinom]). The CDF is computed
#'   internally via cumulative summation. Exactly one of `fun` or `pmf_fun`
#'   must be provided.
#' @param args A named list of additional arguments to pass to `fun` or
#'   `pmf_fun`.
#' @param xlim A numeric vector of length 2 specifying the range of integer
#'   support values.
#' @param support An optional integer or numeric vector giving the exact support
#'   points to evaluate. When supplied, `xlim` is ignored.
#' @param open_fill Fill color for the open (hollow) endpoint circles. Defaults
#'   to `NULL`, which uses the active theme's panel background color.
#' @param vert_type Line type for the vertical jump segments. Defaults to
#'   `"dashed"`.
#' @param show_points Logical. If `FALSE`, suppresses all endpoint circles (open
#'   and closed). If `NULL` (the default), circles are shown when there are
#'   50 or fewer points and hidden otherwise.
#' @param show_vert Logical. If `FALSE`, suppresses the vertical jump segments.
#'   If `NULL` (the default), segments are shown when there are 50 or fewer
#'   points and hidden otherwise.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   # via PMF
#'   ggplot() +
#'     geom_cdf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))
#'
#'   # via CDF directly
#'   ggplot() +
#'     geom_cdf_discrete(fun = pbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))
#'
#'   ggplot() +
#'     geom_cdf_discrete(pmf_fun = dpois, xlim = c(0, 15), args = list(lambda = 5))
#'
#' @name geom_cdf_discrete
#' @aliases StatCDFDiscrete GeomCDFDiscrete
#' @export
geom_cdf_discrete <- function(
    mapping = NULL,
    data = NULL,
    stat = StatCDFDiscrete,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    pmf_fun = NULL,
    xlim = NULL,
    support = NULL,
    args = list(),
    open_fill = NULL,
    vert_type = "dashed",
    show_points = NULL,
    show_vert = NULL
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
    geom = GeomCDFDiscrete,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      pmf_fun = pmf_fun,
      args = args,
      xlim = xlim,
      support = support,
      na.rm = na.rm,
      open_fill = open_fill,
      vert_type = vert_type,
      show_points = show_points,
      show_vert = show_vert,
      ...
    )
  )
}

#' @rdname geom_cdf_discrete
#' @export
StatCDFDiscrete <- ggproto("StatCDFDiscrete", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, pmf_fun = NULL,
                           xlim = NULL, support = NULL, args = NULL) {

    if (!is.null(support)) {
      x_vals <- sort(support)
    } else if (is.null(xlim)) {
      x_vals <- 0:10
    } else {
      x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
    }

    if (!is.null(fun)) {
      fun_injected <- function(x) rlang::inject(fun(x, !!!args))
      cdf_vals <- fun_injected(x_vals)
      return(data.frame(x = x_vals, y = cdf_vals))
    }

    if (!is.null(pmf_fun)) {
      fun_injected <- function(x) rlang::inject(pmf_fun(x, !!!args))
      invisible(check_pmf_normalization(fun_injected, support = x_vals, tol = 1e-2))
      pmf_vals <- fun_injected(x_vals)
      cdf_vals <- cumsum(pmf_vals)
      return(data.frame(x = x_vals, y = cdf_vals))
    }

    cli::cli_abort("One of {.arg fun} or {.arg pmf_fun} must be provided.")
  }
)

#' @rdname geom_cdf_discrete
#' @export
GeomCDFDiscrete <- ggproto("GeomCDFDiscrete", Geom,

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
                        show_points = NULL, show_vert = NULL) {
    if (is.null(open_fill)) {
      bg <- ggplot2::theme_get()$panel.background
      open_fill <- if (!inherits(bg, "element_blank") && !is.null(bg$fill) && !is.na(bg$fill)) bg$fill else "white"
    }
    n <- nrow(data)
    if (is.null(show_points)) show_points <- n <= 50
    if (is.null(show_vert))   show_vert   <- n <= 50

    # Horizontal segments:
    #   [left_boundary → x[1]] at height 0,
    #   [x[k] → x[k+1]] at height y[k],
    #   [x[n] → right_boundary] at height y[n]
    data_hori        <- data[c(1, 1:n), ]
    data_hori$x      <- c(panel_params$x.range[1], data$x)
    data_hori$xend   <- c(data$x, panel_params$x.range[2])
    data_hori$y      <- c(0, data$y)
    data_hori$yend   <- c(0, data$y)

    # Vertical jump segments at each x[k]: from y[k-1] (or 0) up to y[k]
    data_vert        <- data
    data_vert$xend   <- data$x
    data_vert$y      <- c(0, data$y[-n])
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
      # Open circle at bottom of each jump (left-limit of F just before x[k])
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

      # Closed circle at top of each jump (F achieves this value at x[k])
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
