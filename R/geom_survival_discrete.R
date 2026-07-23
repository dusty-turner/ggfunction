#' Plot a Discrete Survival Function as a Step Function
#'
#' `geom_survival_discrete()` renders the discrete survival function
#' \eqn{S(x) = 1 - F(x)} as a right-continuous step function with horizontal
#' segments, dashed vertical jumps, open circles at the lower limit of each
#' jump, and closed circles at the upper limit.
#'
#' Supply exactly one of `fun` (a discrete survival function evaluated
#' directly), `cdf_fun` (a discrete CDF such as [pbinom], from which
#' \eqn{S(x) = 1 - F(x)} is computed), or `pmf_fun` (a PMF such as [dbinom],
#' from which the CDF is computed via cumulative summation and then
#' \eqn{S(x) = 1 - F(x)}).
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A discrete survival function evaluated directly on the integer
#'   support. Exactly one of `fun`, `cdf_fun`, or `pmf_fun` must be provided.
#' @param cdf_fun A discrete CDF function (e.g. [pbinom]). \eqn{S(x) = 1 - F(x)}
#'   is computed from this function on the integer support. Exactly one of
#'   `fun`, `cdf_fun`, or `pmf_fun` must be provided.
#' @param pmf_fun A PMF function (e.g. [dbinom]). The survival function is
#'   derived as \eqn{1 - \mathrm{cumsum}(\mathrm{pmf})}. Exactly one of
#'   `fun`, `cdf_fun`, or `pmf_fun` must be provided.
#' @param xlim A numeric vector of length 2 specifying the range of support
#'   values to display. When `support` is not supplied, this range is also used
#'   as the computational support.
#' @param support An optional integer or numeric vector giving the exact support
#'   points used for cumulative computation. When supplied with `xlim`, the
#'   survival probabilities are computed on the full `support` and then filtered
#'   to the displayed `xlim`.
#' @param args A named list of additional arguments to pass to `fun`,
#'   `cdf_fun`, or `pmf_fun`.
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
#' @param p (Optional) A numeric value between 0 and 1 specifying a cumulative
#'   probability threshold from the left (that is, on \eqn{F = 1 - S}). When
#'   `lower.tail = TRUE` (the default), steps and endpoints up to the
#'   corresponding quantile are highlighted and the rest are dimmed; when
#'   `FALSE`, the upper tail is highlighted.
#' @param lower.tail Logical; controls the direction of `p`-based shading.
#'   Defaults to `TRUE`.
#' @param p_lower (Optional) Lower cumulative probability bound for two-sided
#'   shading. Used with `p_upper`.
#' @param p_upper (Optional) Upper cumulative probability bound for two-sided
#'   shading. Used with `p_lower`.
#' @param shade_outside Logical; if `TRUE`, shading is applied to the tails
#'   outside the `p_lower`/`p_upper` interval rather than inside. Defaults to
#'   `FALSE`.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Support points at which the discrete survival
#'   function is evaluated.}
#'   \item{`after_stat(y)`}{Survival probabilities.}
#' }
#'
#' @section Aesthetics:
#' `geom_survival_discrete()` does not require any input aesthetics when a
#' function source is supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(x)` and `after_stat(y)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `fill`, `group`,
#'   `linetype`, `linewidth`, `shape`, `size`, and `stroke` for steps,
#'   jump segments, and endpoints.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_survival()], [geom_cdf_discrete()], [geom_qf_discrete()], and
#'   [geom_pmf()] for related discrete distribution-function layers.
#'
#' @examples
#'   # via PMF
#'   ggplot() +
#'     geom_survival_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))
#'
#'   # via CDF
#'   ggplot() +
#'     geom_survival_discrete(cdf_fun = pbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))
#'
#'   ggplot() +
#'     geom_survival_discrete(pmf_fun = dpois, xlim = c(0, 15), args = list(lambda = 5))
#'
#'   # highlight the upper quartile
#'   ggplot() +
#'     geom_survival_discrete(pmf_fun = dbinom, xlim = c(0, 10),
#'                            args = list(size = 10, prob = 0.5),
#'                            p = 0.25, lower.tail = FALSE)
#'
#' @name geom_survival_discrete
#' @aliases StatSurvivalDiscrete GeomSurvivalDiscrete
#' @export
geom_survival_discrete <- function(
    mapping = NULL,
    data = NULL,
    stat = StatSurvivalDiscrete,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    cdf_fun = NULL,
    pmf_fun = NULL,
    xlim = NULL,
    support = NULL,
    args = list(),
    open_fill = NULL,
    vert_type = "dashed",
    show_points = NULL,
    show_vert = NULL,
    p = NULL,
    lower.tail = TRUE,
    p_lower = NULL,
    p_upper = NULL,
    shade_outside = FALSE
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
    geom = GeomSurvivalDiscrete,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      cdf_fun = cdf_fun,
      pmf_fun = pmf_fun,
      args = args,
      xlim = xlim,
      support = support,
      na.rm = na.rm,
      open_fill = open_fill,
      vert_type = vert_type,
      show_points = show_points,
      show_vert = show_vert,
      p = p,
      lower.tail = lower.tail,
      p_lower = p_lower,
      p_upper = p_upper,
      shade_outside = shade_outside,
      ...
    )
  )
}

#' @rdname geom_survival_discrete
#' @export
StatSurvivalDiscrete <- ggproto("StatSurvivalDiscrete", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           pmf_fun = NULL, xlim = NULL, support = NULL,
                           args = NULL,
                           p = NULL, lower.tail = TRUE,
                           p_lower = NULL, p_upper = NULL,
                           shade_outside = FALSE) {

    # Validate: exactly one source
    n_provided <- (!is.null(fun)) + (!is.null(cdf_fun)) + (!is.null(pmf_fun))
    if (n_provided == 0L) {
      cli::cli_abort("One of {.arg fun}, {.arg cdf_fun}, or {.arg pmf_fun} must be provided.")
    }
    if (n_provided > 1L) {
      cli::cli_abort("Supply only one of {.arg fun}, {.arg cdf_fun}, or {.arg pmf_fun}.")
    }

    x_vals <- discrete_support(xlim = xlim, support = support)

    if (!is.null(fun)) {
      fun_injected   <- function(x) rlang::inject(fun(x, !!!args))
      survival_vals  <- fun_injected(x_vals)
      if (length(survival_vals) > 1 && any(diff(survival_vals) > 0, na.rm = TRUE)) {
        cli::cli_warn(c(
          "The resulting survival function is not monotonically non-increasing.",
          "i" = "Check the function supplied to {.arg fun}."
        ))
      }
      pmf_vals <- diff(c(0, 1 - survival_vals))
    } else if (!is.null(cdf_fun)) {
      cdf_injected   <- function(x) rlang::inject(cdf_fun(x, !!!args))
      survival_vals  <- 1 - cdf_injected(x_vals)
      if (length(survival_vals) > 1 && any(diff(survival_vals) > 0, na.rm = TRUE)) {
        cli::cli_warn(c(
          "The resulting survival function is not monotonically non-increasing.",
          "i" = "Check the function supplied to {.arg cdf_fun}."
        ))
      }
      pmf_vals <- diff(c(0, 1 - survival_vals))
    } else {
      fun_injected  <- function(x) rlang::inject(pmf_fun(x, !!!args))
      invisible(check_pmf_normalization(
        fun_injected, support = x_vals, tol = 1e-2, action = "abort"
      ))
      pmf_vals      <- fun_injected(x_vals)
      cdf_vals      <- cumsum(pmf_vals)
      survival_vals <- 1 - cdf_vals
    }

    out <- data.frame(x = x_vals, y = survival_vals)
    # Shading membership is computed on the full support, before any xlim
    # filtering, so cumulative probabilities are not distorted by the display
    # window.
    out$in_shade <- pmf_shade_index(
      pmf_vals, p = p, lower.tail = lower.tail,
      p_lower = p_lower, p_upper = p_upper,
      shade_outside = shade_outside
    )
    filter_discrete_xlim(out, xlim = xlim)
  }
)

#' @rdname geom_survival_discrete
#' @export
GeomSurvivalDiscrete <- ggproto("GeomSurvivalDiscrete", Geom,

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

    # Horizontal segments (right-continuous):
    #   Before x[1]: S = 1; at x[1] it drops.
    #   Segment at height S(x[k]) from x[k] to x[k+1], plus
    #   leftmost extension from panel left to x[1] at height 1,
    #   and rightmost extension from x[n] to panel right at height S(x[n]).
    in_shade <- if ("in_shade" %in% names(data)) data$in_shade else rep(TRUE, n)

    data_hori        <- data[c(1, 1:n), ]
    data_hori$x      <- c(panel_params$x.range[1], data$x)
    data_hori$xend   <- c(data$x, panel_params$x.range[2])
    data_hori$y      <- c(1, data$y)
    data_hori$yend   <- c(1, data$y)
    # The step at height S(x[k]) belongs to atom k; the leading segment at
    # height 1 inherits atom 1's membership.
    data_hori$alpha  <- ifelse(c(in_shade[1], in_shade), data_hori$alpha, 0.3)

    # Vertical jump segments at each x[k]: from S(x[k-1]) (or 1) down to S(x[k])
    data_vert        <- data
    data_vert$xend   <- data$x
    data_vert$y      <- c(1, data$y[-n])
    data_vert$yend   <- data$y
    data_vert$alpha  <- ifelse(in_shade, data_vert$alpha, 0.3)

    coord_hori <- coord$transform(data_hori, panel_params)
    coord_vert <- coord$transform(data_vert, panel_params)

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
      # Open circle at top of each jump (S just before the drop — left limit)
      grobs$open <- grid::pointsGrob(
        coord_vert$x, coord_vert$y,
        default.units = "native",
        pch = 21,
        gp = grid::gpar(
          col      = scales::alpha(coord_vert$colour, coord_vert$alpha),
          fill     = open_fill,
          fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
          lwd      = coord_vert$stroke * .stroke / 2
        )
      )

      # Closed circle at bottom of each jump (S achieves this value at x[k])
      grobs$closed <- grid::pointsGrob(
        coord_vert$xend, coord_vert$yend,
        pch = coord_vert$shape,
        default.units = "native",
        gp = grid::gpar(
          col      = scales::alpha(coord_vert$colour, coord_vert$alpha),
          fill     = scales::alpha(coord_vert$colour, coord_vert$alpha),
          fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
          lwd      = coord_vert$stroke * .stroke / 2
        )
      )
    }

    grid::gTree(children = do.call(grid::gList, grobs))
  },

  draw_key = draw_key_path
)
