#' Plot a Discrete CDF (Step Function)
#'
#' `geom_discrete_cdf()` creates a ggplot2 layer that plots a discrete cumulative distribution
#' function as a step function. It takes a PMF function, computes the cumulative sum, and
#' renders as steps.
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A PMF function (e.g. [dbinom]). The function must accept a numeric vector as its
#'   first argument and return non-negative probability values that sum to 1.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 specifying the range of integer support values.
#'   Defaults to `c(0, 10)`.
#' @param fill Fill color for the shaded area.
#' @param color Line color for the step function.
#' @param p (Optional) A numeric value between 0 and 1 specifying the CDF threshold for shading.
#' @param lower.tail Logical; if `TRUE` (the default) shading extends from the left;
#'   if `FALSE`, from the threshold to the right.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   ggplot() +
#'     geom_discrete_cdf(fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10))
#'
#'   ggplot() +
#'     geom_discrete_cdf(fun = dpois, args = list(lambda = 5), xlim = c(0, 15), p = 0.9)
#'
#' @name geom_discrete_cdf
#' @aliases StatDiscreteCDF GeomDiscreteCDF
#' @export
geom_discrete_cdf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatDiscreteCDF,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    args = list(),
    xlim = NULL,
    fill = "grey20",
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
    geom = GeomDiscreteCDF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      args = args,
      xlim = xlim,
      na.rm = na.rm,
      fill = fill,
      color = color,
      p = p,
      lower.tail = lower.tail,
      ...
    )
  )
}

#' @rdname geom_discrete_cdf
#' @export
StatDiscreteCDF <- ggproto("StatDiscreteCDF", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, args = NULL) {

    if (is.null(xlim)) {
      x_vals <- 0:10
    } else {
      x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
    }

    fun_injected <- function(x) {
      rlang::inject(fun(x, !!!args))
    }

    invisible(check_pmf_normalization(fun_injected, support = x_vals, tol = 1e-2))

    pmf_vals <- fun_injected(x_vals)
    cdf_vals <- cumsum(pmf_vals)

    data.frame(x = x_vals, y = cdf_vals)
  }
)

#' @rdname geom_discrete_cdf
#' @export
GeomDiscreteCDF <- ggproto("GeomDiscreteCDF", GeomArea,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, p = NULL, lower.tail = TRUE
                        ) {

    x_vals <- data$x
    y_vals <- data$y

    # Build step polygon for the line
    step_data <- build_step_polygon(x_vals, y_vals)

    # Create step line data with all needed columns from original data row
    line_data <- data[rep(1, nrow(step_data)), , drop = FALSE]
    line_data$x <- step_data$x
    line_data$y <- step_data$y
    rownames(line_data) <- NULL

    # Determine shading region
    if (!is.null(p)) {
      if (lower.tail) {
        idx <- which(y_vals >= p)[1]
        if (is.na(idx)) idx <- length(y_vals)
        threshold_x <- x_vals[idx]
        clip_step <- line_data[line_data$x <= threshold_x, , drop = FALSE]
        clip_range <- c(min(x_vals), threshold_x)
      } else {
        idx <- which(y_vals >= (1 - p))[1]
        if (is.na(idx)) idx <- 1
        threshold_x <- x_vals[idx]
        clip_step <- line_data[line_data$x >= threshold_x, , drop = FALSE]
        clip_range <- c(threshold_x, max(x_vals))
      }
    } else {
      clip_range <- range(x_vals, na.rm = TRUE)
      clip_step <- line_data
    }

    # Close the polygon
    poly_data <- rbind(
      transform(clip_step[1, , drop = FALSE], x = clip_range[1], y = 0),
      clip_step,
      transform(clip_step[nrow(clip_step), , drop = FALSE], x = clip_range[2], y = 0)
    )

    poly_data$colour <- NA

    area_grob <- ggproto_parent(GeomArea, self)$draw_panel(
      poly_data, panel_params, coord, na.rm = na.rm
    )

    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      line_data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )

    grid::grobTree(area_grob, line_grob)
  }
)
