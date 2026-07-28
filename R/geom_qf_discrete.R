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
#' @param args A named list of additional arguments to pass to `fun`,
#'   `pmf_fun`, `cdf_fun`, or `survival_fun`.
#' @param xlim A numeric vector of length 2 specifying the range of support
#'   values to display (y-axis of the quantile function). When `support` is not
#'   supplied for a cumulative input path, this range is also used as the
#'   computational support.
#' @param support An optional integer or numeric vector giving the exact support
#'   points used for cumulative computation. When supplied with `xlim`, the
#'   quantile steps are computed on the full `support` and then filtered to the
#'   displayed `xlim`.
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
#'   probability threshold. When `lower.tail = TRUE` (the default), steps and
#'   endpoints up to the corresponding quantile are highlighted and the rest
#'   are dimmed; when `FALSE`, the upper tail is highlighted. For the direct
#'   `fun` path the cumulative probabilities are recovered from a dense grid,
#'   so membership is approximate at grid resolution.
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
#'   \item{`after_stat(p)`}{Cumulative probabilities that define the quantile
#'   step boundaries.}
#'   \item{`after_stat(x)`}{Support values; the default y aesthetic maps to
#'   this variable.}
#' }
#'
#' @section Aesthetics:
#' `geom_qf_discrete()` does not require any input aesthetics when a function
#' source is supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(p)` and `after_stat(x)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `fill`, `group`,
#'   `linetype`, `linewidth`, `shape`, `size`, and `stroke` for steps,
#'   jump segments, and endpoints.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_qf()], [geom_cdf_discrete()], [geom_survival_discrete()], and
#'   [geom_pmf()] for related discrete distribution-function layers.
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
#'     geom_qf_discrete(pmf_fun = dpois, xlim = c(0, 15), support = 0:50,
#'                      args = list(lambda = 5))
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
    show_vert = NULL,
    p = NULL,
    lower.tail = TRUE,
    p_lower = NULL,
    p_upper = NULL,
    shade_outside = FALSE
) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  default_mapping <- aes(x = after_stat(p), y = after_stat(x))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  main_layer <- layer(
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
      p = p,
      lower.tail = lower.tail,
      p_lower = p_lower,
      p_upper = p_upper,
      shade_outside = shade_outside,
      ...
    )
  )

  list(main_layer, probability_axis_anchor())
}

#' Infer a bounded integer support from a black-box discrete quantile
#' function via Q(0)/Q(1), or return NULL (with a warning) when exact
#' enumeration is impossible (C-02).
#' @noRd
infer_qf_integer_support <- function(qf, cap = 10000L) {
  q0 <- tryCatch(suppressWarnings(qf(0)), error = function(e) NA_real_)
  q1 <- tryCatch(suppressWarnings(qf(1)), error = function(e) NA_real_)
  valid_endpoints <- length(q0) == 1L && length(q1) == 1L &&
    is.finite(q0) && is.finite(q1) && q1 >= q0 &&
    abs(q0 - round(q0)) < 1e-8 && abs(q1 - round(q1)) < 1e-8
  if (!valid_endpoints) {
    cli::cli_warn(c(
      "Exact boundary enumeration of a black-box discrete quantile function requires a known support.",
      "i" = "Supply {.arg support}, {.arg pmf_fun}, or {.arg cdf_fun} for exact boundaries; falling back to a grid approximation."
    ))
    return(NULL)
  }
  span <- round(q1) - round(q0) + 1
  if (span > cap) {
    cli::cli_abort(c(
      "The integer support inferred from Q(0) and Q(1) spans {format(span, big.mark = ',')} points, exceeding the internal cap of {format(cap, big.mark = ',')}.",
      "i" = "Supply an explicit {.arg support} to opt into a larger computation, or use {.arg pmf_fun}/{.arg cdf_fun}."
    ))
  }
  seq.int(round(q0), round(q1))
}

#' Recover the exact right boundary F(x_k) = sup{p : Q(p) <= x_k} for each
#' support point by monotone bisection (C-02).
#' @noRd
qf_right_boundaries <- function(qf, support, tol = 1e-15) {
  vapply(support, function(xk) {
    q1 <- suppressWarnings(qf(1))
    if (is.finite(q1) && q1 <= xk) return(1)
    lo <- 0
    hi <- 1
    while (hi - lo > tol) {
      mid <- (lo + hi) / 2
      q_mid <- suppressWarnings(qf(mid))
      if (is.finite(q_mid) && q_mid <= xk) lo <- mid else hi <- mid
    }
    lo
  }, numeric(1))
}

#' @rdname geom_qf_discrete
#' @export
StatQFDiscrete <- ggproto("StatQFDiscrete", Stat,
  default_aes = aes(x = NULL, y = after_stat(x)),

  compute_group = function(data, scales, fun = NULL, pmf_fun = NULL,
                           cdf_fun = NULL, survival_fun = NULL,
                           xlim = NULL, support = NULL, args = NULL,
                           p = NULL, lower.tail = TRUE,
                           p_lower = NULL, p_upper = NULL,
                           shade_outside = FALSE) {

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
      support_use <- if (!is.null(support)) {
        sort(unique(support))
      } else {
        infer_qf_integer_support(fun_injected)
      }

      if (!is.null(support_use)) {
        # Exact right boundaries by monotone bisection; rare atoms are
        # recovered no matter how small their mass (C-02).
        p_right <- qf_right_boundaries(fun_injected, support_use)
        qdf <- data.frame(q = support_use, p_right = p_right)
      } else {
        # Black-box fallback: grid approximation with no false terminal
        # boundary — an unverified observed maximum is not pinned to 1.
        p_grid <- seq(1e-4, 1 - 1e-4, length.out = 5000)
        q_vals <- fun_injected(p_grid)
        q_unique <- sort(unique(q_vals))
        p_right <- vapply(q_unique,
                          function(xk) max(p_grid[q_vals == xk]),
                          numeric(1))
        qdf <- data.frame(q = q_unique, p_right = p_right)
      }
    } else if (!is.null(cdf_fun)) {
      x_vals <- discrete_support(xlim = xlim, support = support)
      cdf_injected <- function(x) rlang::inject(cdf_fun(x, !!!args))
      cdf_vals <- validate_discrete_cdf_values(
        cdf_injected(x_vals), x_vals, arg = "cdf_fun"
      )
      qdf <- data.frame(q = x_vals, p_right = cdf_vals)
    } else if (!is.null(pmf_fun)) {
      x_vals <- discrete_support(xlim = xlim, support = support)
      # Evaluated and structurally validated exactly once (C-03).
      pmf_vals <- evaluate_pmf(
        pmf_fun, x_vals, args = args, arg = "pmf_fun", normalization = "abort"
      )
      qdf <- data.frame(q = x_vals, p_right = cumsum(pmf_vals))
    } else {
      x_vals <- discrete_support(xlim = xlim, support = support)
      surv_injected <- function(x) rlang::inject(survival_fun(x, !!!args))
      surv_vals <- validate_discrete_survival(
        surv_injected(x_vals), x_vals, arg = "survival_fun"
      )
      qdf <- data.frame(q = x_vals, p_right = 1 - surv_vals)
    }

    # True predecessor boundaries on the full computational set (C-01), then
    # zero-mass rows are dropped before constructing QF geometry, keeping the
    # earliest support value for a duplicated cumulative boundary (C-02).
    qdf$p_left <- c(0, qdf$p_right[-nrow(qdf)])
    mass <- qdf$p_right - qdf$p_left
    qdf$in_shade <- pmf_shade_index(
      mass, p = p, lower.tail = lower.tail,
      p_lower = p_lower, p_upper = p_upper,
      shade_outside = shade_outside
    )
    qdf <- qdf[mass > 1e-12, , drop = FALSE]

    out <- data.frame(
      # The quantile column rides the after_stat(p)/after_stat(x)
      # cross-mapping round trip, so it carries x-panel-space values; raw
      # quantiles are retained in q (A-01, as in geom_qf).
      x = scale_forward(scales$x, qdf$q),
      q = qdf$q,
      p = qdf$p_right,
      p_right = qdf$p_right,
      p_left = qdf$p_left,
      p_left_panel = scale_forward(scales$x, qdf$p_left),
      in_shade = qdf$in_shade
    )
    filter_discrete_xlim(out, xlim = xlim, x_col = "q")
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

    in_shade   <- if ("in_shade" %in% names(data)) data$in_shade else rep(TRUE, n)
    orig_alpha <- data$alpha

    # Horizontal segments (n total, defined only on [0, 1]):
    #   [p_left[k] → p_right[k]] at height y[k] = x_k (support value).
    # p_left carries the true predecessor boundary, so a narrowed window
    # starts atom k's segment at F(x_{k-1}) rather than 0 (C-01).
    left_bound <- if ("p_left_panel" %in% names(data)) {
      data$p_left_panel
    } else {
      c(0, data$x[-n])
    }
    data_hori        <- data
    data_hori$x      <- left_bound
    data_hori$xend   <- data$x
    data_hori$y      <- data$y
    data_hori$yend   <- data$y
    data_hori$alpha  <- dim_alpha(orig_alpha, in_shade)

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
      # The jump and its closed (bottom) circle belong to atom k; the open
      # (top) circle previews atom k + 1's membership.
      data_vert$alpha  <- dim_alpha(orig_alpha[-n], in_shade[-n])
      open_alpha       <- dim_alpha(orig_alpha[-1], in_shade[-1])

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
            col      = scales::alpha(coord_vert$colour, open_alpha),
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
