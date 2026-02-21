#' Plot a Vector or Stream Field from a 2D Vector-Valued Function
#'
#' `geom_function_2d_2d()` evaluates a user-defined vector field
#' \eqn{\mathbf{F}(x,y) = (dx, dy)} on a grid and renders it as either a
#' vector field (short arrows at each grid point, the default) or a stream
#' field (integral curves). It wraps [ggvfields::geom_vector_field()] for
#' `type = "vector"` and [ggvfields::geom_stream_field()] for
#' `type = "stream"`.
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A function accepting a length-2 numeric vector and returning a
#'   length-2 numeric vector \eqn{(dx, dy)}.
#' @param xlim,ylim Numeric vectors of length 2 specifying the domain limits.
#'   Default to \eqn{c(-1, 1)}.
#' @param n Integer or two-element integer vector. Number of seed points along
#'   each axis. Defaults to `11`.
#' @param args A named list of additional arguments passed to `fun`.
#' @param type Character. Either `"vector"` (default) for short arrows at each
#'   grid point, or `"stream"` for full integral-curve streamlines.
#' @param center Logical. If `TRUE` (default), centers each arrow/streamline on
#'   its seed point.
#' @param normalize Logical. If `TRUE` (default), normalizes arrow/streamline
#'   lengths relative to grid spacing.
#' @param tail_point Logical. If `TRUE`, draws a point at the tail of each
#'   arrow or streamline. Defaults to `FALSE`.
#' @param eval_point Logical. If `TRUE`, draws a point at the seed point.
#'   Defaults to `FALSE`.
#' @param grid A data frame of precomputed seed points. If `NULL` (default), a
#'   regular grid is generated from `xlim`, `ylim`, and `n`.
#' @param arrow A [grid::arrow()] specification. Defaults to a closed arrow at
#'   30° with length `0.02 npc`.
#' @param max_it Integer. Maximum integration steps per streamline
#'   (`type = "stream"` only). Defaults to `1000`.
#' @param T Numeric. Maximum integration time (`type = "stream"` only).
#' @param L Numeric. Maximum arc length (`type = "stream"` only, when
#'   `normalize = TRUE`).
#' @param method Character. Integration method, e.g. `"rk4"` or `"euler"`
#'   (`type = "stream"` only). Defaults to `"rk4"`.
#' @param geom The geometric object to use to display the data. Defaults to
#'   [ggvfields::GeomStream].
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @seealso \pkg{ggvfields} (Turner, Kahle, and Sturdivant) for a richer
#'   collection of vector field tools including gradient fields, stream plots,
#'   and potential functions: <https://github.com/dusty-turner/ggvfields>.
#'
#' @examples
#' library(ggfunction)
#' f <- function(u) c(-u[2], u[1])
#'
#' # Vector field (default)
#' ggplot() + geom_function_2d_2d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1))
#'
#' # Stream field
#' ggplot() + geom_function_2d_2d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1),
#'   type = "stream")
#'
#' @name geom_function_2d_2d
#' @aliases stat_function_2d_2d
#' @export
#' @importFrom ggvfields geom_vector_field geom_stream_field
geom_function_2d_2d <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 11,
    args = list(),
    type = "vector",
    center = TRUE,
    normalize = TRUE,
    tail_point = FALSE,
    eval_point = FALSE,
    grid = NULL,
    arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed"),
    max_it = 1000,
    T = NULL,
    L = NULL,
    method = "rk4"
) {
  if (type == "vector") {
    ggvfields::geom_vector_field(
      mapping = mapping,
      data = data,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      arrow = arrow,
      ...
    )
  } else if (type == "stream") {
    ggvfields::geom_stream_field(
      mapping = mapping,
      data = data,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      max_it = max_it,
      T = T,
      L = L,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      method = method,
      arrow = arrow,
      ...
    )
  } else {
    stop('`type` must be "vector" or "stream"', call. = FALSE)
  }
}

#' @rdname geom_function_2d_2d
#' @export
stat_function_2d_2d <- function(
    mapping = NULL,
    data = NULL,
    geom = ggvfields::GeomStream,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 11,
    args = list(),
    max_it = 1000,
    T = NULL,
    L = NULL,
    center = TRUE,
    normalize = TRUE,
    tail_point = FALSE,
    eval_point = FALSE,
    grid = NULL,
    method = "rk4",
    arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
) {
  ggvfields::stat_stream_field(
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = fun,
    xlim = xlim,
    ylim = ylim,
    n = n,
    args = args,
    max_it = max_it,
    T = T,
    L = L,
    center = center,
    normalize = normalize,
    tail_point = tail_point,
    eval_point = eval_point,
    grid = grid,
    method = method,
    arrow = arrow,
    ...
  )
}
