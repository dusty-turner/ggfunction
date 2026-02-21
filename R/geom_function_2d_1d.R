#' Create a 2D Function Visualization Layer of the Norm of a Vector Field
#'
#' `geom_function_2d_1d` adds a layer to visualize 2D functions or vector fields in a `ggplot2` plot.
#'
#' @param mapping Aesthetic mappings, created using `aes()`. If `NULL`, defaults are used.
#' @param data Optional data frame to override the default data source.
#' @param stat Statistical transformation to use. Defaults to `StatFunction2d`.
#' @param geom Geom used for rendering. Defaults to `GeomFunction2d`.
#' @param ... Other arguments passed to the layer, such as additional parameters.
#' @param position Position adjustment for the layer. Defaults to `"identity"`.
#' @param fun A function that takes a matrix of x, y values and returns a matrix of dx, dy values.
#' @param xlim Numeric vector of length 2 specifying the x-range of the grid. Required if `fun` is provided.
#' @param ylim Numeric vector of length 2 specifying the y-range of the grid. Required if `fun` is provided.
#' @param n Number of points in the grid along each axis. Defaults to `50` in `stat_function_2d_1d`.
#' @param type Character. Type of visualization: `"raster"` (default), `"contour"`, or `"contour_filled"`.
#' @param bins Number of contour bins. Only used when `type` is `"contour"` or `"contour_filled"`.
#' @param binwidth Width of contour bins. Only used when `type` is `"contour"` or `"contour_filled"`.
#' @param breaks Numeric vector of specific contour break values. Only used when `type` is `"contour"` or `"contour_filled"`.
#' @param na.rm Logical. Should missing values be removed? Defaults to `FALSE`.
#' @param show.legend Logical. Should this layer be included in the legends? `NA` includes if aesthetics are mapped.
#' @param inherit.aes If `FALSE`, overrides default aesthetics rather than combining them.
#'
#' @return A `ggplot2` layer.
#' @export
#'
#' @examples
#' # Function that calculates the norm
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(sqrt(x^2 + y^2))
#' }
#'
#' ggplot() +
#'   geom_function_2d_1d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5))
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
                                type = "raster",
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
    if (is.null(mapping)) {
      mapping <- aes(fill = after_stat(z))
    }
  }

  params <- list(
    fun = fun,
    xlim = xlim,
    ylim = ylim,
    n = n,
    ...
  )

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
                              n = 50) {

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

  compute_group = function(data, scales, fun, xlim = NULL, ylim = NULL, n = NULL, ...) {

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

      data$z <- vectorize(fun)(as.matrix(data))

    } else {
      # fun is NULL, expecting user-provided data with x,y and dx,dy or angle/distance
      if (!all(c("x", "y") %in% names(data))) {
        stop("`stat_vector()` requires `x` and `y` aesthetics or a `fun` with `xlim`/`ylim`.")
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
  extra_params = c("na.rm", "fun", "xlim", "ylim", "n"),

  setup_params = function(data, params) {
    # Generate grid early so z.range is available for contour break computation
    fun <- params$fun
    xlim <- params$xlim %||% c(-1, 1)
    ylim <- params$ylim %||% c(-1, 1)
    n <- params$n %||% 50

    grid_data <- expand.grid(
      x = seq(xlim[1], xlim[2], length.out = n),
      y = seq(ylim[1], ylim[2], length.out = n)
    )
    grid_data$z <- vectorize(fun)(as.matrix(grid_data[, c("x", "y")]))
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
  extra_params = c("na.rm", "fun", "xlim", "ylim", "n"),

  setup_params = function(data, params) {
    fun <- params$fun
    xlim <- params$xlim %||% c(-1, 1)
    ylim <- params$ylim %||% c(-1, 1)
    n <- params$n %||% 50

    grid_data <- expand.grid(
      x = seq(xlim[1], xlim[2], length.out = n),
      y = seq(ylim[1], ylim[2], length.out = n)
    )
    grid_data$z <- vectorize(fun)(as.matrix(grid_data[, c("x", "y")]))
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
