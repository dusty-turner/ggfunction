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
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5))
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), raster_aes = "alpha")
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), type = "contour")
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), type = "contour_filled")
#'
#' # Sinusoidal combination of sine and cosine
#' f_sin_cos <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   sin(x) * cos(y)
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f_sin_cos, xlim = c(-5, 5), ylim = c(-5, 5))
#'
#' # Gaussian bump function
#' f_gaussian <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   exp(-(x^2 + y^2) / 2)
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f_gaussian, xlim = c(-5, 5), ylim = c(-5, 5))
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
#'   geom_function_2d_1d(fun = f_radial_wave, xlim = c(-50, 50), ylim = c(-50, 50), n = 100)
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
#'   geom_function_2d_1d(fun = f_complex, xlim = c(-50, 50), ylim = c(-50, 50), n = 500)
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
#'   geom_function_2d_1d(fun = f_spiral, xlim = c(-50, 50), ylim = c(-50, 50), n = 500)
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
#'     args = list(a = 2, b = 0.5)
#'   )
#'
#' # Alpha raster with a fixed fill
#' ggplot() +
#'   geom_function_2d_1d(
#'     fun = f_gaussian,
#'     xlim = c(-5, 5),
#'     ylim = c(-5, 5),
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

  if (is.null(xlim)) {
    xlim <- c(-1, 1)
  }
  if (is.null(ylim)) {
    ylim <- c(-1, 1)
  }

  # If no data and no x,y aesthetics are specified, but we have fun, xlim, ylim,
  # we need dummy data to trigger compute_group().
  if (is.null(data) && !is.null(fun) && !is.null(xlim) && !is.null(ylim)) {
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
      if (is.null(mapping)) {
        mapping <- aes(fill = after_stat(z))
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

  if (is.null(xlim)) {
    xlim <- c(-1, 1)
  }
  if (is.null(ylim)) {
    ylim <- c(-1, 1)
  }

  # If no data and no x,y aesthetics are specified, but we have fun, xlim, ylim,
  # we need dummy data to trigger compute_group().
  if (is.null(data) && !is.null(fun) && !is.null(xlim) && !is.null(ylim)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
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

  compute_group = function(data, scales, fun, xlim = NULL, ylim = NULL, n = NULL,
                           args = NULL, ...) {

    # Scenario: Using a function to generate the vector field
    if (!is.null(fun)) {
      # If xlim and ylim provided, generate grid from those
      # If not provided, try to infer from data
      if (is.null(xlim) || is.null(ylim)) {
        if (nrow(data) > 0 && all(c("x", "y") %in% names(data))) {
          xlim <- xlim %||% range(data$x, na.rm = TRUE)
          ylim <- ylim %||% range(data$y, na.rm = TRUE)
        } else {
          stop("When using `fun` without specifying aes `x, y` from data, you must supply `xlim` and `ylim` or specify `x, y` using aes()")
        }
      }

      if (is.null(n)) n <- 50

      data <- expand.grid(
        x = seq(xlim[1], xlim[2], length.out = n),
        y = seq(ylim[1], ylim[2], length.out = n)
      )

      args <- args %||% list()
      fun_injected <- function(v) rlang::inject(fun(v, !!!args))
      data$z <- vectorize(fun_injected)(as.matrix(data[, c("x", "y")]))

    } else {
      # fun is NULL, expecting user-provided data with x,y and dx,dy or angle/distance
      if (!all(c("x", "y") %in% names(data))) {
        stop("`stat_function_2d_1d()` requires `x` and `y` aesthetics or a `fun` with `xlim`/`ylim`.")
      }

    }

    if(is.numeric(data$z)) {
      data <- data.frame(x = data$x, y = data$y, z = data$z)
    }
    if(is.numeric(data$fill)) {
      data <- data.frame(x = data$x, y = data$y, fill = data$fill)
    }

    data
  }
)

#' @rdname geom_function_2d_1d
#' @export
GeomFunction2d <- ggplot2::ggproto(
  "GeomFunction2d",
  ggplot2::GeomRaster,
  default_aes = ggplot2::aes(fill = "black", alpha = 1)
)

#' @rdname geom_function_2d_1d
#' @export
StatFunction2dContour <- ggproto(
  "StatFunction2dContour",
  ggplot2::StatContour,

  required_aes = character(0),
  extra_params = c("na.rm", "fun", "xlim", "ylim", "n", "args"),

  setup_params = function(data, params) {
    # Generate grid early so z.range is available for contour break computation
    fun <- params$fun
    xlim <- params$xlim %||% c(-1, 1)
    ylim <- params$ylim %||% c(-1, 1)
    n <- params$n %||% 50
    args <- params$args %||% list()
    fun_injected <- function(v) rlang::inject(fun(v, !!!args))

    grid_data <- expand.grid(
      x = seq(xlim[1], xlim[2], length.out = n),
      y = seq(ylim[1], ylim[2], length.out = n)
    )
    grid_data$z <- vectorize(fun_injected)(as.matrix(grid_data[, c("x", "y")]))
    params$z.range <- range(grid_data$z, na.rm = TRUE, finite = TRUE)
    params$.grid_data <- grid_data
    params
  },

  setup_data = function(data, params) {
    grid_data <- params$.grid_data
    grid_data$PANEL <- data$PANEL[1]
    grid_data$group <- data$group[1]
    grid_data
  }
)

#' @rdname geom_function_2d_1d
#' @export
StatFunction2dContourFilled <- ggproto(
  "StatFunction2dContourFilled",
  ggplot2::StatContourFilled,

  required_aes = character(0),
  extra_params = c("na.rm", "fun", "xlim", "ylim", "n", "args"),

  setup_params = function(data, params) {
    fun <- params$fun
    xlim <- params$xlim %||% c(-1, 1)
    ylim <- params$ylim %||% c(-1, 1)
    n <- params$n %||% 50
    args <- params$args %||% list()
    fun_injected <- function(v) rlang::inject(fun(v, !!!args))

    grid_data <- expand.grid(
      x = seq(xlim[1], xlim[2], length.out = n),
      y = seq(ylim[1], ylim[2], length.out = n)
    )
    grid_data$z <- vectorize(fun_injected)(as.matrix(grid_data[, c("x", "y")]))
    params$z.range <- range(grid_data$z, na.rm = TRUE, finite = TRUE)
    params$.grid_data <- grid_data
    params
  },

  setup_data = function(data, params) {
    grid_data <- params$.grid_data
    grid_data$PANEL <- data$PANEL[1]
    grid_data$group <- data$group[1]
    grid_data
  }
)
