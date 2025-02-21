#' Plot a Probability Density Function with a Filled Area
#'
#' `geom_pdf()` computes a probability density function and plots it as a filled area.
#' This function is similar to [geom_function()], but it shades the area corresponding to
#' a given proportion of the total density.
#'
#' @inheritParams ggplot2::geom_function
#' @importFrom stats integrate
#' @importFrom cli cli_alert
#' @param fun A function to compute the density (e.g. [dnorm]). The function must accept a
#'   numeric vector as its first argument and return density values that integrate (approximately)
#'   to 1.
#' @param n (defaults to 101)Number of points at which to evaluate `fun`.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 giving the x-range over which to evaluate the PDF.
#' @param fill Fill color for the shaded area.
#' @param color Line color for the outline of the density curve.
#' @param p (Optional) A numeric value between 0 and 1 specifying the cumulative probability
#'   threshold. The area will be shaded up until the point where the cumulative density reaches
#'   this value.
#' @param lower.tail Logical; if `TRUE` (the default) the shaded area extends from the left end
#'   of the density up to the threshold. If `FALSE`, the shading extends from the threshold to the
#'   right end.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   ggplot() +
#'     geom_pdf(fun = dnorm, xlim = c(-3, 3),  p = .975, lower.tail = TRUE)
#'
#' @name geom_pdf
#' @aliases StatPDF GeomPDF
#' @export
geom_pdf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatPDF,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    n = 101,
    args = list(),
    xlim = NULL,
    fill = "#734F60",
    color = "black",
    p = NULL,
    lower.tail = TRUE
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
    geom = GeomPDF,
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
      p = p,
      lower.tail = lower.tail,
      ...
    )
  )
}

#' @rdname geom_pdf
#' @export
StatPDF <- ggproto("StatPDF", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, n = 101, args = NULL) {

    range <- if (is.null(scales$x)) {
      xlim %||% c(0, 1)
    } else {
      xlim %||% scales$x$dimension()
    }

    # Create an injected version of fun that includes the additional arguments.
    fun_injected <- function(x) {
      rlang::inject(fun(x, !!!args))
    }

    # Check that the injected function integrates to 1 over the specified range.
    invisible(check_pdf_normalization(fun_injected, lower = range[1], upper = range[2], tol = 1e-2))

    xseq <- seq(range[1], range[2], length.out = n)
    y_out <- fun_injected(xseq)

    tibble::tibble(x = xseq, y = y_out)
  }
)

#' @rdname geom_pdf
#' @export
GeomPDF <- ggproto("GeomPDF", GeomArea,
  draw_panel = function(self, data, panel_params, coord,
                        arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE,
                        p = NULL, lower.tail = TRUE) {


    x_vals <- data$x
    y_vals <- data$y

    # Compute cumulative area using the trapezoidal rule.
    dx <- diff(x_vals)
    seg_area <- (y_vals[-length(y_vals)] + y_vals[-1]) / 2 * dx
    cum_area <- c(0, cumsum(seg_area))

    total_area <- max(cum_area)
    norm_cum <- cum_area / total_area  # normalized cumulative area

    # Determine the clipping range based on p (if provided)
    if (!is.null(p)) {
      if (lower.tail) {
        # Find first x where cumulative area >= p
        idx <- which(norm_cum >= p)[1]
        if (is.na(idx)) idx <- length(norm_cum)
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x <= threshold_x, , drop = FALSE]
        clip_range <- c(min(x_vals), threshold_x)
      } else {
        # For the upper tail, find first x where cumulative area >= (1 - p)
        idx <- which(norm_cum >= (1 - p))[1]
        if (is.na(idx)) idx <- 1
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x >= threshold_x, , drop = FALSE]
        clip_range <- c(threshold_x, max(x_vals))
      }
    } else {
      # Fall back on xlim attached as an attribute (or the full range)
        clip_range <- range(x_vals, na.rm = TRUE)
        clip_data <- data[data$x >= clip_range[1] & data$x <= clip_range[2], , drop = FALSE]
    }

    # We "close" the polygon by adding baseline (y=0) points at the boundaries.
      poly_data <- rbind(
        transform(clip_data[1, , drop = FALSE], x = clip_range[1], y = 0),
        clip_data,
        transform(clip_data[nrow(clip_data), , drop = FALSE], x = clip_range[2], y = 0)
      )

    # Create the filled area grob using GeomArea’s draw_panel.
    area_grob <- ggproto_parent(GeomArea, self)$draw_panel(
      poly_data, panel_params, coord, na.rm = na.rm
    )

    # Create the line grob for the entire function using GeomPath’s draw_panel.
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )

    grid::grobTree(area_grob, line_grob)
  }
)

