#' Plot a Discrete Quantile Function as a Step Function
#'
#' `geom_qf_discrete()` renders a discrete quantile function as a
#' left-continuous step function with horizontal segments, dashed vertical
#' jumps, closed circles at the lower limit of each jump, and open circles at
#' the upper limit.
#'
#' Supply exactly one of `fun` (a quantile function such as [qbinom],
#' evaluated directly on a dense probability grid), `pmf_fun` (a PMF such as
#' [dbinom], from which the CDF is computed via cumulative summation and then
#' inverted), `cdf_fun` (a CDF such as [pbinom], evaluated on the integer
#' support and inverted), or `survival_fun` (a discrete survival function,
#' from which the CDF is computed as \eqn{F(x) = 1 - S(x)} and then inverted).
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A discrete quantile function (e.g. [qbinom]). Evaluated on a
#'   dense grid of probabilities in \eqn{(0, 1)}. Use `xlim` to restrict the
#'   range of support values shown. Exactly one of `fun`, `pmf_fun`, `cdf_fun`,
#'   or `survival_fun` must be provided.
#' @param pmf_fun A PMF function (e.g. [dbinom]). The quantile function is
#'   derived internally by inverting the cumulative sum. Exactly one of `fun`,
#'   `pmf_fun`, `cdf_fun`, or `survival_fun` must be provided.
#' @param cdf_fun A discrete CDF function (e.g. [pbinom]). Evaluated on the
#'   integer support and inverted to produce the quantile function. Exactly one
#'   of `fun`, `pmf_fun`, `cdf_fun`, or `survival_fun` must be provided.
#' @param survival_fun A discrete survival function. The CDF is computed as
#'   \eqn{F(x) = 1 - S(x)} on the integer support and then inverted. Exactly
#'   one of `fun`, `pmf_fun`, `cdf_fun`, or `survival_fun` must be provided.
#' @param args A named list of additional arguments to pass to `fun` or
#'   `pmf_fun`.
#' @param xlim A numeric vector of length 2 specifying the range of support
#'   values to display (y-axis of the quantile function). For the `pmf_fun`
#'   path this also defines the integer support to evaluate.
#' @param support An optional integer or numeric vector giving the exact support
#'   points. When supplied, `xlim` is ignored.
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
#'     geom_qf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))
#'
#'   # via quantile function directly
#'   ggplot() +
#'     geom_qf_discrete(fun = qbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))
#'
#'   ggplot() +
#'     geom_qf_discrete(pmf_fun = dpois, xlim = c(0, 15), args = list(lambda = 5))
#'
#' @name geom_qf_discrete
#' @aliases StatQFDiscrete GeomQFDiscrete
#' @export
geom_qf_discrete <- function(
    mapping = NULL,
    data = NULL,
    stat = StatQFDiscrete,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    pmf_fun = NULL,
    cdf_fun = NULL,
    survival_fun = NULL,
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
    geom = GeomQFDiscrete,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      pmf_fun = pmf_fun,
      cdf_fun = cdf_fun,
      survival_fun = survival_fun,
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

#' @rdname geom_qf_discrete
#' @export
StatQFDiscrete <- ggproto("StatQFDiscrete", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, pmf_fun = NULL,
                           cdf_fun = NULL, survival_fun = NULL,
                           xlim = NULL, support = NULL, args = NULL) {

    # Validate: exactly one source
    n_provided <- (!is.null(fun)) + (!is.null(pmf_fun)) + (!is.null(cdf_fun)) +
      (!is.null(survival_fun))
    if (n_provided == 0L) {
      cli::cli_abort("One of {.arg fun}, {.arg pmf_fun}, {.arg cdf_fun}, or {.arg survival_fun} must be provided.")
    }
    if (n_provided > 1L) {
      cli::cli_abort("Supply only one of {.arg fun}, {.arg pmf_fun}, {.arg cdf_fun}, or {.arg survival_fun}.")
    }

    if (!is.null(fun)) {
      fun_injected <- function(p) rlang::inject(fun(p, !!!args))
      p_grid <- seq(0.0001, 0.9999, length.out = 5000)
      q_vals <- fun_injected(p_grid)

      if (!is.null(support)) {
        keep   <- q_vals %in% support
        p_grid <- p_grid[keep]
        q_vals <- q_vals[keep]
      } else if (!is.null(xlim)) {
        keep   <- q_vals >= xlim[1] & q_vals <= xlim[2]
        p_grid <- p_grid[keep]
        q_vals <- q_vals[keep]
      }

      # For each unique support value, the right boundary is the largest p
      # where Q(p) equals that value (= F(x_k) from the CDF).
      q_unique <- sort(unique(q_vals))
      p_right  <- vapply(q_unique,
                         function(xk) max(p_grid[q_vals == xk]),
                         numeric(1))
      return(data.frame(x = p_right, y = q_unique))
    }

    if (!is.null(cdf_fun)) {
      if (!is.null(support)) {
        x_vals <- sort(support)
      } else if (is.null(xlim)) {
        x_vals <- 0:10
      } else {
        x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
      }

      cdf_injected <- function(x) rlang::inject(cdf_fun(x, !!!args))
      cdf_vals <- cdf_injected(x_vals)

      # x = F(x_k) (probability axis), y = x_k (support value axis)
      return(data.frame(x = cdf_vals, y = x_vals))
    }

    if (!is.null(pmf_fun)) {
      if (!is.null(support)) {
        x_vals <- sort(support)
      } else if (is.null(xlim)) {
        x_vals <- 0:10
      } else {
        x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
      }

      fun_injected <- function(x) rlang::inject(pmf_fun(x, !!!args))
      invisible(check_pmf_normalization(fun_injected, support = x_vals, tol = 1e-2))
      pmf_vals <- fun_injected(x_vals)
      cdf_vals <- cumsum(pmf_vals)

      # x = F(x_k) (probability axis), y = x_k (support value axis)
      return(data.frame(x = cdf_vals, y = x_vals))
    }

    if (!is.null(survival_fun)) {
      if (!is.null(support)) {
        x_vals <- sort(support)
      } else if (is.null(xlim)) {
        x_vals <- 0:10
      } else {
        x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
      }

      surv_injected <- function(x) rlang::inject(survival_fun(x, !!!args))
      surv_vals <- surv_injected(x_vals)
      cdf_vals <- 1 - surv_vals

      # x = F(x_k) (probability axis), y = x_k (support value axis)
      return(data.frame(x = cdf_vals, y = x_vals))
    }
  }
)

#' @rdname geom_qf_discrete
#' @export
GeomQFDiscrete <- ggproto("GeomQFDiscrete", Geom,

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

  use_defaults = function(self, data, params = list(), modifiers = aes(),
                          default_aes = NULL, theme = NULL, ...) {
    data <- ggproto_parent(Geom, self)$use_defaults(
      data, params, modifiers, default_aes = default_aes, theme = theme, ...
    )
    inject_open_fill(data, theme)
  },

  draw_group = function(data, panel_params, coord,
                        open_fill = NULL, vert_type = "dashed",
                        show_points = NULL, show_vert = NULL) {
    open_fill <- resolve_open_fill(open_fill, data)
    n <- nrow(data)
    if (is.null(show_points)) show_points <- n <= 50
    if (is.null(show_vert))   show_vert   <- n <= 50

    # Horizontal segments (n total, defined only on [0, 1]):
    #   [0 → x[1]] at height y[1],
    #   [x[k] → x[k+1]] at height y[k+1], ...,
    #   [x[n-1] → x[n]] at height y[n]
    # where x = F(x_k) and y = x_k (support value)
    data_hori        <- data
    data_hori$x      <- c(0, data$x[-n])
    data_hori$xend   <- data$x
    data_hori$y      <- data$y
    data_hori$yend   <- data$y

    coord_hori <- coord$transform(data_hori, panel_params)

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

    if (n > 1) {
      # Vertical jump segments at each p = x[k] for k = 1 to n-1:
      # from y[k] (closed, achieved) up to y[k+1] (open, not yet achieved)
      data_vert        <- data[-n, ]   # n-1 rows: x = F(x_k), y = x_k
      data_vert$xend   <- data_vert$x  # same p (vertical segment)
      data_vert$yend   <- data$y[-1]   # top of jump = x_{k+1}

      coord_vert <- coord$transform(data_vert, panel_params)

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
        # Closed circle at bottom of each jump (QF achieves this value at p = F(x_k))
        grobs$closed <- grid::pointsGrob(
          coord_vert$x, coord_vert$y,
          pch = coord_vert$shape,
          default.units = "native",
          gp = grid::gpar(
            col      = scales::alpha(coord_vert$colour, coord_vert$alpha),
            fill     = scales::alpha(coord_vert$colour, coord_vert$alpha),
            fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
            lwd      = coord_vert$stroke * .stroke / 2
          )
        )

        # Open circle at top of each jump (next value not yet achieved)
        grobs$open <- grid::pointsGrob(
          coord_vert$xend, coord_vert$yend,
          default.units = "native",
          pch = 21,
          gp = grid::gpar(
            col      = coord_vert$colour,
            fill     = open_fill,
            fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
            lwd      = coord_vert$stroke * .stroke / 2
          )
        )
      }
    }

    grid::gTree(children = do.call(grid::gList, grobs))
  },

  draw_key = draw_key_path
)
