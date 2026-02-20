#' Plot a General R to R Function with Optional Shading
#'
#' `geom_function_1d_1d()` computes a function \eqn{f: \mathbb{R} \to \mathbb{R}} and plots it
#' as a line (like [ggplot2::geom_function()]) with an optional shaded region between two x-values.
#'
#' @inheritParams ggplot2::geom_function
#' @importFrom ggplot2 ggproto Stat GeomArea GeomPath
#' @param fun A function to compute. The function must accept a numeric vector as its first argument.
#' @param n Number of points at which to evaluate `fun`. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 giving the x-range over which to evaluate the function.
#' @param fill Fill color for the shaded area (only used when `shade_from`/`shade_to` are specified).
#' @param color Line color for the curve.
#' @param shade_from (Optional) Numeric. Left boundary of the x-interval to shade.
#' @param shade_to (Optional) Numeric. Right boundary of the x-interval to shade.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   ggplot() +
#'     geom_function_1d_1d(fun = sin, xlim = c(0, 2 * pi))
#'
#'   ggplot() +
#'     geom_function_1d_1d(fun = dnorm, xlim = c(-3, 3),
#'       shade_from = -1, shade_to = 1)
#'
#' @name geom_function_1d_1d
#' @aliases StatFunction1d GeomFunction1d
#' @export
geom_function_1d_1d <- function(
    mapping = NULL,
    data = NULL,
    stat = StatFunction1d,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    n = 101,
    args = list(),
    xlim = NULL,
    fill = "grey20",
    color = "black",
    shade_from = NULL,
    shade_to = NULL
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
    geom = GeomFunction1d,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      n = n,
      xlim = xlim,
      args = args,
      na.rm = na.rm,
      fill = fill,
      color = color,
      shade_from = shade_from,
      shade_to = shade_to,
      ...
    )
  )
}

#' @rdname geom_function_1d_1d
#' @export
StatFunction1d <- ggproto("StatFunction1d", Stat,
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
    y_out <- fun_injected(xseq)

    data.frame(x = xseq, y = y_out)
  }
)

#' @rdname geom_function_1d_1d
#' @export
GeomFunction1d <- ggproto("GeomFunction1d", GeomPath,
  extra_params = c("na.rm", "shade_from", "shade_to", "fill"),
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, shade_from = NULL, shade_to = NULL,
                        fill = "grey20"
                        ) {

    x_vals <- data$x
    y_vals <- data$y
    has_shading <- !is.null(shade_from) || !is.null(shade_to)

    grobs <- list()

    if (has_shading) {
      clip_left <- shade_from %||% min(x_vals)
      clip_right <- shade_to %||% max(x_vals)

      # Interpolate y at exact shade boundaries if they fall between grid points
      fun_approx <- stats::approxfun(x_vals, y_vals, rule = 2)
      y_left <- fun_approx(clip_left)
      y_right <- fun_approx(clip_right)

      # Get points within the shading range
      in_range <- x_vals >= clip_left & x_vals <= clip_right
      clip_x <- x_vals[in_range]
      clip_y <- y_vals[in_range]

      # Add interpolated boundary points if needed
      if (length(clip_x) == 0 || clip_x[1] != clip_left) {
        clip_x <- c(clip_left, clip_x)
        clip_y <- c(y_left, clip_y)
      }
      if (clip_x[length(clip_x)] != clip_right) {
        clip_x <- c(clip_x, clip_right)
        clip_y <- c(clip_y, y_right)
      }

      # Build closed polygon: curve points then back along baseline
      poly_x <- c(clip_x, rev(clip_x))
      poly_y <- c(clip_y, rep(0, length(clip_x)))

      # Transform to panel coordinates
      poly_df <- data.frame(x = poly_x, y = poly_y)
      poly_coords <- coord$transform(poly_df, panel_params)

      fill_col <- fill
      alpha_val <- if (!is.null(data$alpha[1]) && !is.na(data$alpha[1])) data$alpha[1] else 0.4

      poly_grob <- grid::polygonGrob(
        x = poly_coords$x,
        y = poly_coords$y,
        gp = grid::gpar(fill = fill_col, alpha = alpha_val, col = NA)
      )
      grobs <- c(grobs, list(poly_grob))
    }

    # Always draw the line for the entire function
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )
    grobs <- c(grobs, list(line_grob))

    do.call(grid::grobTree, grobs)
  }
)
