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
#' @param xlim A numeric vector of length 2 giving the x-range over which to
#'   evaluate the function, in data coordinates. Under a transformed x scale
#'   the evaluation grid is evenly spaced in panel (transformed) space and the
#'   function is evaluated at the corresponding data-space values. (This
#'   deliberately differs from [ggplot2::stat_function()], which interprets
#'   `xlim` in transformed scale space.)
#' @param fill Fill color for the shaded area (only used when `shade_from`/`shade_to` are
#'   specified). Defaults to `"grey20"`; supply a value to override a fill mapping.
#' @param color Line color for the curve. Defaults to `"black"`; supply a
#'   value to override a colour mapping.
#' @param shade_from (Optional) Numeric. Left boundary of the x-interval to shade, in data
#'   coordinates.
#' @param shade_to (Optional) Numeric. Right boundary of the x-interval to shade, in data
#'   coordinates.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Points at which `fun` is evaluated (data
#'   coordinates).}
#'   \item{`after_stat(y)`}{Function values.}
#'   \item{`after_stat(x_eval)`}{Data-space evaluation points (equal to the
#'   raw `x` grid).}
#'   \item{`after_stat(y_raw)`}{Raw (untransformed) function values.}
#'   \item{`after_stat(in_shade)`}{Logical indicator for grid points inside
#'   the requested `shade_from`/`shade_to` region.}
#' }
#'
#' @section Aesthetics:
#' `geom_function_1d_1d()` does not require any input aesthetics when `fun` is
#' supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(x)` and `after_stat(y)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `fill`, `group`,
#'   `linetype`, and `linewidth` for the line and optional shaded region. A
#'   mapped `fill` must be constant within each shaded group.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [ggplot2::geom_function()] for ggplot2's built-in one-dimensional
#'   function layer.
#'
#' @examples
#'   ggplot() +
#'     geom_function_1d_1d(fun = sin, xlim = c(0, 2 * pi))
#'
#'   ggplot() +
#'     geom_function_1d_1d(fun = dnorm, xlim = c(-3, 3),
#'       shade_from = -1, shade_to = 1)
#'
#'   # Parameterized via `args`
#'   ggplot() +
#'     geom_function_1d_1d(fun = dnorm, xlim = c(-3, 9),
#'       args = list(mean = 3, sd = 2))
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
    xlim = NULL,
    n = 101,
    args = list(),
    fill = NULL,
    color = NULL,
    shade_from = NULL,
    shade_to = NULL
    ) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  validate_data_limits(xlim)
  validate_shade_bounds(shade_from, shade_to)

  default_mapping <- aes(x = after_stat(x), y = after_stat(y))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  params <- list(
    fun = fun,
    n = n,
    xlim = xlim,
    args = args,
    na.rm = na.rm,
    shade_from = shade_from,
    shade_to = shade_to,
    ...
  )
  # Fixed colour/fill are forwarded only when the user supplied them; the
  # defaults (black line, grey20 shade) live in GeomFunction1d$default_aes so
  # mapped aesthetics are honored (E-04).
  if (!is.null(color)) params$color <- color
  if (!is.null(fill)) params$fill <- fill
  params <- normalise_colour_params(params)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFunction1d,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @noRd
validate_shade_bounds <- function(shade_from = NULL, shade_to = NULL) {
  for (nm in c("shade_from", "shade_to")) {
    val <- get(nm)
    if (!is.null(val) &&
        (!is.numeric(val) || length(val) != 1L || !is.finite(val))) {
      cli::cli_abort("{.arg {nm}} must be a single finite number (data coordinates).")
    }
  }
  if (!is.null(shade_from) && !is.null(shade_to) && shade_from >= shade_to) {
    cli::cli_abort("{.arg shade_from} must be less than {.arg shade_to}.")
  }
  invisible(NULL)
}

#' @rdname geom_function_1d_1d
#' @export
StatFunction1d <- ggproto("StatFunction1d", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, n = 101,
                           args = NULL,
                           shade_from = NULL, shade_to = NULL) {

    validate_shade_bounds(shade_from, shade_to)
    grid <- resolve_stat_grid_1d(scales$x, xlim, n = n)
    x_eval <- grid$eval
    x_panel <- grid$panel

    has_shading <- !is.null(shade_from) || !is.null(shade_to)

    # Insert exact shade boundary evaluation rows (data space) when they fall
    # inside the evaluation window (E-05).
    if (has_shading) {
      window <- range(x_eval)
      bounds <- c(shade_from, shade_to)
      bounds <- bounds[bounds >= window[1] & bounds <= window[2]]
      bounds <- setdiff(bounds, x_eval)
      if (length(bounds) > 0L) {
        bounds_panel <- scale_forward(scales$x, bounds)
        ord <- order(c(x_eval, bounds))
        x_eval <- c(x_eval, bounds)[ord]
        x_panel <- c(x_panel, bounds_panel)[ord]
      }
    }

    fun_injected <- function(x) {
      rlang::inject(fun(x, !!!args))
    }

    y_raw <- fun_injected(x_eval)
    y_panel <- scale_forward(scales$y, y_raw)

    out <- data.frame(
      x = x_panel,
      x_eval = x_eval,
      y = y_panel,
      y_raw = y_raw
    )

    if (has_shading) {
      out$in_shade <- x_eval >= (shade_from %||% -Inf) &
        x_eval <= (shade_to %||% Inf)
      baseline <- resolve_stat_baseline(scales$y, 0)
      out$baseline_panel <- baseline$panel
      # The zero baseline participates in scale training only when it is
      # finite in the active transformation domain (E-05).
      if (baseline$finite) out$ymin <- baseline$panel
    } else {
      out$in_shade <- FALSE
    }

    out
  }
)

#' @rdname geom_function_1d_1d
#' @export
GeomFunction1d <- ggproto("GeomFunction1d", GeomPath,
  default_aes = aes(
    colour = "black",
    fill = "grey20",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  extra_params = c("na.rm", "shade_from", "shade_to"),

  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, shade_from = NULL, shade_to = NULL) {

    has_shading <- !is.null(shade_from) || !is.null(shade_to)
    grobs <- list()

    if (has_shading && any(data$in_shade, na.rm = TRUE)) {
      baseline_panel <- if ("baseline_panel" %in% names(data)) {
        data$baseline_panel[1]
      } else {
        NA_real_
      }
      base_y <- baseline_draw_value(baseline_panel, panel_params)

      for (g in split(data, data$group)) {
        seg <- g[which(g$in_shade), , drop = FALSE]
        if (nrow(seg) < 2L) next

        fills <- unique(seg$fill)
        if (length(fills) > 1L) {
          cli::cli_abort(c(
            "{.field fill} varies within a single shaded group.",
            "i" = "Map {.field fill} to a group-constant value, or supply a fixed {.arg fill}."
          ))
        }
        alpha_val <- seg$alpha[1]
        if (is.na(alpha_val)) alpha_val <- 0.4

        poly_df <- data.frame(
          x = c(seg$x, rev(seg$x)),
          y = c(seg$y, rep(base_y, nrow(seg)))
        )
        poly_coords <- coord$transform(poly_df, panel_params)

        grobs <- c(grobs, list(grid::polygonGrob(
          x = poly_coords$x,
          y = poly_coords$y,
          gp = grid::gpar(fill = fills, alpha = alpha_val, col = NA)
        )))
      }
    }

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
