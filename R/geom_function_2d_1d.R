#' Visualize a Scalar Field z = f(x, y)
#'
#' `geom_function_2d_1d` adds a layer that visualizes a scalar-valued function
#' of two variables, \eqn{z = f(x, y)}, over a grid as a raster or contour plot.
#'
#' @param mapping Aesthetic mappings, created using `aes()`. If `NULL`, defaults are used.
#' @param data Optional data frame to override the default data source.
#' @param stat Statistical transformation to use. Defaults to `StatFunction2d`.
#' @param geom Geom used for rendering. Defaults to `GeomFunction2d`.
#' @param ... Other arguments passed to the layer, such as additional parameters.
#' @param position Position adjustment for the layer. Defaults to `"identity"`.
#' @param fun A function evaluated at each grid point. It takes a length-2
#'   numeric vector `c(x, y)` and returns a single numeric value `z`.
#' @param xlim Numeric vector of length 2 specifying the x-range of the grid. Required if `fun` is provided.
#' @param ylim Numeric vector of length 2 specifying the y-range of the grid. Required if `fun` is provided.
#' @param n Number of points in the grid along each axis. Defaults to `50` in `stat_function_2d_1d`.
#' @param args A named list of additional arguments passed to `fun`.
#' @param type Character. Type of visualization: `"raster"` (default), `"contour"`, or `"contour_filled"`.
#' @param raster_aes Character. Default raster aesthetic encoding. `"fill"`
#'   (default) maps `after_stat(z)` to `fill`, preserving the usual fill scale
#'   and legend. `"alpha"` uses a fixed dark gray fill and scales
#'   `after_stat(z)` to literal 0--1 alpha values.
#' @param bins Number of contour bins. Only used when `type` is `"contour"` or `"contour_filled"`.
#' @param binwidth Width of contour bins. Only used when `type` is `"contour"` or `"contour_filled"`.
#' @param breaks Numeric vector of specific contour break values. Only used when `type` is `"contour"` or `"contour_filled"`.
#' @param na.rm Logical. Should missing values be removed? Defaults to `FALSE`.
#' @param show.legend Logical. Should this layer be included in the legends? `NA` includes if aesthetics are mapped.
#' @param inherit.aes If `FALSE`, overrides default aesthetics rather than combining them.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Grid x coordinates.}
#'   \item{`after_stat(y)`}{Grid y coordinates.}
#'   \item{`after_stat(z)`}{Function values on the grid for raster and contour
#'   inputs. The raster display maps `fill = after_stat(z)` by default, or
#'   scales it to literal alpha values when `raster_aes = "alpha"`.}
#'   \item{`after_stat(level)`}{Contour level for `type = "contour"` or
#'   `type = "contour_filled"`.}
#'   \item{`after_stat(nlevel)`}{Contour level scaled to a maximum of 1.}
#'   \item{`after_stat(piece)`}{Contour piece identifier.}
#'   \item{`after_stat(level_low)`}{Lower boundary of each filled-contour band.}
#'   \item{`after_stat(level_high)`}{Upper boundary of each filled-contour band.}
#'   \item{`after_stat(level_mid)`}{Midpoint of each filled-contour band.}
#' }
#'
#' @section Dropped variables:
#' `z` is used to compute contour lines or filled contour bands and is not
#' available after contouring.
#'
#' @section Aesthetics:
#' `geom_function_2d_1d()` does not require input aesthetics when `fun` is
#' supplied. Raster layers understand `x`, `y`, `fill`, `alpha`, and `group`;
#' contour layers use ggplot2's contour aesthetics, including `colour`,
#' `linetype`, `linewidth`, `fill`, and `group` depending on `type`.
#'
#' @return A `ggplot2` layer.
#'
#' @seealso [ggplot2::geom_raster()] and [ggplot2::geom_contour()] for the
#'   underlying raster and contour drawing conventions.
#' @export
#'
#' @examples
#' # Function that calculates the norm
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(sqrt(x^2 + y^2))
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 25)
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 25, raster_aes = "alpha")
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 25, type = "contour")
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 25, type = "contour_filled")
#'
#' # Sinusoidal combination of sine and cosine
#' f_sin_cos <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   sin(x) * cos(y)
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f_sin_cos, xlim = c(-5, 5), ylim = c(-5, 5), n = 25)
#'
#' # Gaussian bump function
#' f_gaussian <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   exp(-(x^2 + y^2) / 2)
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f_gaussian, xlim = c(-5, 5), ylim = c(-5, 5), n = 25)
#'
#' # Radial sine wave function
#' f_radial_wave <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   r <- sqrt(x^2 + y^2)
#'   sin(r)
#' }
#'
#' # Some functions need more resolution for clarity
#' ggplot() +
#'   geom_function_2d_1d(fun = f_radial_wave, xlim = c(-12, 12), ylim = c(-12, 12), n = 40)
#'
#' # Complex combination of radial and angular components
#' f_complex <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   r <- sqrt(x^2 + y^2)
#'   theta <- atan2(y, x)
#'   sin(r) * cos(theta)
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f_complex, xlim = c(-12, 12), ylim = c(-12, 12), n = 40)
#'
#' # Spiral pattern function
#' f_spiral <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   r <- sqrt(x^2 + y^2)
#'   theta <- atan2(y, x)
#'   sin(r + theta)
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f_spiral, xlim = c(-12, 12), ylim = c(-12, 12), n = 40)
#'
#' # Parameterized scalar field via `args`
#' f <- function(v, a = 1, b = 1) {
#'   a * sin(v[1]) + b * cos(v[2])
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(
#'     fun = f,
#'     xlim = c(-5, 5),
#'     ylim = c(-5, 5),
#'     n = 25,
#'     args = list(a = 2, b = 0.5)
#'   )
#'
#' # Alpha raster with a fixed fill
#' ggplot() +
#'   geom_function_2d_1d(
#'     fun = f_gaussian,
#'     xlim = c(-5, 5),
#'     ylim = c(-5, 5),
#'     n = 25,
#'     raster_aes = "alpha"
#'   )

#' @rdname geom_function_2d_1d
#' @export
geom_function_2d_1d <- function(mapping = NULL, data = NULL,
                                stat = StatFunction2d, geom = GeomFunction2d,
                                ...,
                                position = "identity",
                                fun = NULL,
                                xlim = NULL,
                                ylim = NULL,
                                n = NULL,
                                args = list(),
                                type = "raster",
                                raster_aes = c("fill", "alpha"),
                                bins = NULL,
                                binwidth = NULL,
                                breaks = NULL,
                                show.legend = TRUE,
                                inherit.aes = TRUE) {

  validate_data_limits(xlim)
  validate_data_limits(ylim, arg = "ylim")

  # A function-only layer (no data to inherit positions from) gets dummy data
  # to trigger compute_group() and the documented default panel-space domain
  # c(-1, 1) on each axis. With layer/plot data present, omitted limits are
  # inferred from the mapped positions instead.
  if (is.null(data) && !is.null(fun)) {
    if (is.null(xlim)) xlim <- c(-1, 1)
    if (is.null(ylim)) ylim <- c(-1, 1)
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  type <- match.arg(type, c("raster", "contour", "contour_filled"))
  raster_aes <- match.arg(raster_aes)
  dots <- list(...)

  if (type == "contour") {
    stat <- StatFunction2dContour
    geom <- ggplot2::GeomContour
    if (is.null(mapping)) {
      mapping <- aes(colour = after_stat(level))
    } else if (is.null(mapping$colour)) {
      mapping <- modifyList(mapping, aes(colour = after_stat(level)))
    }
  } else if (type == "contour_filled") {
    stat <- StatFunction2dContourFilled
    geom <- ggplot2::GeomContourFilled
    if (is.null(mapping)) {
      mapping <- aes()
    }
  } else {
    # raster (default)
    if (identical(raster_aes, "fill")) {
      # The scalar field is encoded through fill = after_stat(z) by default;
      # auxiliary mappings must not displace it, and an explicit user fill
      # mapping overrides it (E-02).
      if (is.null(mapping)) {
        mapping <- aes(fill = after_stat(z))
      } else if (!("fill" %in% names(mapping))) {
        mapping <- modifyList(mapping, aes(fill = after_stat(z)))
      }
    } else {
      default_mapping <- aes(alpha = after_stat(function2d_alpha_rescale(z)))
      if (is.null(mapping)) {
        mapping <- default_mapping
      } else {
        mapping <- modifyList(default_mapping, mapping)
      }
      if (!"fill" %in% names(dots) && is.null(mapping$fill)) dots$fill <- "grey20"
    }
  }

  params <- list(
    fun = fun,
    xlim = xlim,
    ylim = ylim,
    n = n,
    args = args
  )
  params <- c(params, dots)

  if (!is.null(bins)) params$bins <- bins
  if (!is.null(binwidth)) params$binwidth <- binwidth
  if (!is.null(breaks)) params$breaks <- breaks

  layer(
    stat = stat,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}


#' @noRd
function2d_alpha_rescale <- function(z) {
  out <- rep(NA_real_, length(z))
  finite <- is.finite(z)
  if (!any(finite)) return(I(out))

  z_min <- min(z[finite])
  z_max <- max(z[finite])
  if (z_max <= z_min) {
    out[finite] <- if (z_max == 0) 0 else 1
  } else {
    out[finite] <- (z[finite] - z_min) / (z_max - z_min)
  }

  I(out)
}


#' @rdname geom_function_2d_1d
#' @export
stat_function_2d_1d <- function(mapping = NULL, data = NULL,
                              geom = GeomFunction2d, position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              fun = NULL,
                              xlim = c(-1, 1),
                              ylim = c(-1, 1),
                              n = 50,
                              args = list()) {

  validate_data_limits(xlim)
  validate_data_limits(ylim, arg = "ylim")

  if (is.null(data) && !is.null(fun)) {
    if (is.null(xlim)) xlim <- c(-1, 1)
    if (is.null(ylim)) ylim <- c(-1, 1)
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  # The scalar field is encoded through fill = after_stat(z) by default (E-02).
  if (is.null(mapping)) {
    mapping <- aes(fill = after_stat(z))
  } else if (!("fill" %in% names(mapping))) {
    mapping <- modifyList(mapping, aes(fill = after_stat(z)))
  }

  # Pass the parameters via `params` only
  layer(
    stat = StatFunction2d,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      ...
    )
  )
}


#' @rdname geom_function_2d_1d
#' @export
StatFunction2d <- ggproto(
  "StatFunction2d",
  Stat,
  # required_aes = character(0), # No required aesthetics to allow flexibility
  default_aes = aes(x = NA, y = NA, fill = "black", alpha = 1),

  setup_params = function(data, params) {
    # Domain requirement surfaces as a build error, not a swallowed
    # computation warning (E-07 policy for delegating callers).
    if (!is.null(params$fun) &&
        (is.null(params$xlim) || is.null(params$ylim))) {
      has_xy <- all(c("x", "y") %in% names(data)) &&
        any(is.finite(data$x)) && any(is.finite(data$y))
      if (!has_xy) {
        cli::cli_abort(
          "A function-only scalar-field layer requires finite {.arg xlim} and {.arg ylim} (or mapped {.field x}/{.field y} data) to establish the evaluation domain."
        )
      }
    }
    params
  },

  compute_group = function(data, scales, fun, xlim = NULL, ylim = NULL, n = NULL,
                           args = NULL, ...) {

    if (is.null(fun)) {
      # Precomputed scalar field: pass the mapped data through unchanged so
      # every incoming aesthetic (alpha, colour, group, PANEL, ...) survives
      # (E-03).
      if (!all(c("x", "y") %in% names(data))) {
        cli::cli_abort("`stat_function_2d_1d()` requires `x` and `y` aesthetics or a `fun` with `xlim`/`ylim`.")
      }
      if (!any(c("z", "fill", "alpha") %in% names(data))) {
        cli::cli_abort("Precomputed scalar fields require a mapped `z` (or `fill`/`alpha`) aesthetic.")
      }
      return(data)
    }

    if (is.null(n)) n <- 50

    # Omitted limits fall back to the incoming mapped positions, which are
    # already panel-space; explicit limits are data-space (A-01).
    has_xy <- all(c("x", "y") %in% names(data)) &&
      any(is.finite(data$x)) && any(is.finite(data$y))
    panel_xlim <- if (is.null(xlim) && has_xy) range(data$x, na.rm = TRUE)
    panel_ylim <- if (is.null(ylim) && has_xy) range(data$y, na.rm = TRUE)

    grid <- resolve_stat_grid_2d(
      scales$x, scales$y, xlim, ylim, n,
      default_panel_limits = c(-1, 1),
      panel_xlim = panel_xlim, panel_ylim = panel_ylim
    )

    args <- args %||% list()
    fun_injected <- function(v) rlang::inject(fun(v, !!!args))
    grid$z <- vectorize(fun_injected)(as.matrix(grid[, c("x_eval", "y_eval")]))
    grid
  }
)

#' @rdname geom_function_2d_1d
#' @export
GeomFunction2d <- ggplot2::ggproto(
  "GeomFunction2d",
  ggplot2::GeomRaster,
  default_aes = ggplot2::aes(fill = "black", alpha = 1)
)

#' Generate per-panel, per-group scalar-field grids for contouring (E-01).
#'
#' Each (PANEL, group) combination present in the layer data receives its own
#' scale-aware grid, so facets are populated independently and grouped layers
#' are not collapsed. Grids are evenly spaced in panel coordinates and the
#' function is evaluated at the inverse-transformed data-space image (A-01).
#' @noRd
function2d_contour_grids <- function(data, params, layout) {
  n <- params$n %||% 50
  args <- params$args %||% list()
  fun <- params$fun
  fun_injected <- function(v) rlang::inject(fun(v, !!!args))

  keys <- unique(data[c("PANEL", "group")])
  pieces <- lapply(seq_len(nrow(keys)), function(i) {
    panel <- keys$PANEL[i]
    scales <- layout$get_scales(panel)
    sub <- data[data$PANEL == panel & data$group == keys$group[i], , drop = FALSE]

    has_xy <- all(c("x", "y") %in% names(sub)) &&
      any(is.finite(sub$x)) && any(is.finite(sub$y))
    panel_xlim <- if (is.null(params$xlim) && has_xy) range(sub$x, na.rm = TRUE)
    panel_ylim <- if (is.null(params$ylim) && has_xy) range(sub$y, na.rm = TRUE)

    grid <- resolve_stat_grid_2d(
      scales$x, scales$y, params$xlim, params$ylim, n,
      default_panel_limits = c(-1, 1),
      panel_xlim = panel_xlim, panel_ylim = panel_ylim
    )
    grid$z <- vectorize(fun_injected)(as.matrix(grid[, c("x_eval", "y_eval")]))
    grid$x_eval <- NULL
    grid$y_eval <- NULL
    grid$PANEL <- panel
    grid$group <- keys$group[i]
    grid
  })
  do.call(rbind, pieces)
}

#' @noRd
contour_stat_setup_params <- function(parent, self, data, params) {
  if (is.null(params$fun)) {
    # Precomputed x/y/z data delegate to the parent contour stat (E-01).
    if (!all(c("x", "y", "z") %in% names(data))) {
      cli::cli_abort(
        "Contour layers require {.arg fun} or mapped {.field x}, {.field y}, and {.field z} aesthetics."
      )
    }
    return(ggproto_parent(parent, self)$setup_params(data, params))
  }
  # z.range is computed in compute_layer, once the per-panel grids exist.
  params$z.range <- NULL
  params
}

#' @noRd
contour_stat_compute_layer <- function(parent, self, data, params, layout) {
  if (!is.null(params$fun)) {
    data <- function2d_contour_grids(data, params, layout)
    params$z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
  }
  ggproto_parent(parent, self)$compute_layer(data, params, layout)
}

#' @rdname geom_function_2d_1d
#' @export
StatFunction2dContour <- ggproto(
  "StatFunction2dContour",
  ggplot2::StatContour,

  required_aes = character(0),
  extra_params = c("na.rm", "fun", "xlim", "ylim", "n", "args"),

  setup_params = function(self, data, params) {
    contour_stat_setup_params(ggplot2::StatContour, self, data, params)
  },

  compute_layer = function(self, data, params, layout) {
    contour_stat_compute_layer(ggplot2::StatContour, self, data, params, layout)
  }
)

#' @rdname geom_function_2d_1d
#' @export
StatFunction2dContourFilled <- ggproto(
  "StatFunction2dContourFilled",
  ggplot2::StatContourFilled,

  required_aes = character(0),
  extra_params = c("na.rm", "fun", "xlim", "ylim", "n", "args"),

  setup_params = function(self, data, params) {
    contour_stat_setup_params(ggplot2::StatContourFilled, self, data, params)
  },

  compute_layer = function(self, data, params, layout) {
    contour_stat_compute_layer(
      ggplot2::StatContourFilled, self, data, params, layout
    )
  }
)
