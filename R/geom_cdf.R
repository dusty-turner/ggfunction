#' Plot a Cumulative Distribution Function
#'
#' `geom_cdf()` creates a ggplot2 layer that plots a cumulative distribution function (CDF)
#' as a line. You can optionally shade a region by specifying a cumulative probability
#' threshold via `p`, or a two-sided interval via `p_lower` and `p_upper`.
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A function to compute the CDF (e.g. [pnorm]). The function must accept a numeric
#'   vector as its first argument and return values between 0 and 1.
#' @param n Number of points at which to evaluate `fun`.
#' @param args A named list of additional arguments passed on to `fun`.
#' @param xlim A numeric vector of length 2 specifying the x-range over which to evaluate the CDF.
#' @param fill Fill color for the shaded area.
#' @param color Line color for the CDF curve.
#' @param p (Optional) A numeric value between 0 and 1 specifying the threshold value of the CDF.
#'   The area will be shaded up until (if `lower.tail = TRUE`) or from (if `lower.tail = FALSE`)
#'   the point where the CDF reaches this value.
#' @param lower.tail Logical; if `TRUE` (the default) shading is applied from the left end of the
#'   curve up to the threshold; if `FALSE`, shading is applied from the threshold to the right end.
#' @param p_lower (Optional) A numeric value between 0 and 1 specifying the lower CDF threshold
#'   for two-sided shading. Used with `p_upper`.
#' @param p_upper (Optional) A numeric value between 0 and 1 specifying the upper CDF threshold
#'   for two-sided shading. Used with `p_lower`.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   # Plot the standard normal CDF, shading up to the 97.5th percentile.
#'   ggplot() +
#'     geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = TRUE)
#'
#' @name geom_cdf
#' @aliases StatCDF GeomCDF
#' @export
geom_cdf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatCDF,
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
    p = NULL,
    lower.tail = TRUE,
    p_lower = NULL,
    p_upper = NULL
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
    geom = GeomCDF,
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
      p_lower = p_lower,
      p_upper = p_upper,
      ...
    )
  )
}

#' @rdname geom_cdf
#' @export
StatCDF <- ggproto("StatCDF", Stat,

  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, n = 101, args = NULL) {

    range <- if (is.null(scales$x)) {
      xlim %||% c(-Inf, Inf)
    } else {
      xlim %||% scales$x$dimension()
    }

    # Create an injected version of fun that includes additional arguments.
    fun_injected <- function(x) {
      rlang::inject(fun(x, !!!args))
    }

    # check for a CDF: lower value should be near 0 and upper value near 1.
    lower_val <- fun_injected(range[1])
    upper_val <- fun_injected(range[2])

    if (abs(lower_val) > 0.01 || abs(upper_val - 1) > 0.01) {
      cli::cli_alert(sprintf("The provided function appears not to be a valid CDF over the range [%g, %g]: it returns %g at the lower bound and %g at the upper bound.",
           range[1], range[2], lower_val, upper_val)
           )
    }

    xseq <- seq(range[1], range[2], length.out = n)
    y_out <- fun_injected(xseq)

    data.frame(x = xseq, y = y_out)
  }
)

#' @rdname geom_cdf
#' @export
GeomCDF <- ggproto("GeomCDF", GeomArea,

  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL
                        ) {

    x_vals <- data$x
    y_vals <- data$y

    if (!is.null(p_lower) && !is.null(p_upper)) {
      # Two-sided shading: shade between x where CDF = p_lower and CDF = p_upper
      idx_lower <- which(y_vals >= p_lower)[1]
      if (is.na(idx_lower)) idx_lower <- length(y_vals)
      idx_upper <- which(y_vals >= p_upper)[1]
      if (is.na(idx_upper)) idx_upper <- length(y_vals)
      threshold_lower <- x_vals[idx_lower]
      threshold_upper <- x_vals[idx_upper]
      clip_data <- data[data$x >= threshold_lower & data$x <= threshold_upper, , drop = FALSE]
      clip_range <- c(threshold_lower, threshold_upper)
    } else if (!is.null(p)) {
      if (lower.tail) {
        idx <- which(y_vals >= p)[1]
        if (is.na(idx)) idx <- length(y_vals)
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x <= threshold_x, , drop = FALSE]
        clip_range <- c(min(x_vals), threshold_x)
      } else {
        idx <- which(y_vals >= (1 - p))[1]
        if (is.na(idx)) idx <- 1
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x >= threshold_x, , drop = FALSE]
        clip_range <- c(threshold_x, max(x_vals))
      }
    } else {
      clip_data <- NULL
      clip_range <- NULL
    }

    # Create the line grob for the entire function using GeomPath’s draw_panel.
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord, arrow = arrow, lineend = lineend,
      linejoin = linejoin, linemitre = linemitre, na.rm = na.rm
    )

    if (is.null(clip_data)) {
      return(line_grob)
    }

    # Close the polygon by adding baseline (y=0) points at the boundaries.
    poly_data <- rbind(
      transform(clip_data[1, , drop = FALSE], x = clip_range[1], y = 0),
      clip_data,
      transform(clip_data[nrow(clip_data), , drop = FALSE], x = clip_range[2], y = 0)
    )

    poly_data$colour <- NA

    # Draw the filled area using GeomArea’s draw_panel.
    area_grob <- ggproto_parent(GeomArea, self)$draw_panel(
      poly_data, panel_params, coord, na.rm = na.rm
    )

    grid::grobTree(area_grob, line_grob)
  }
)
