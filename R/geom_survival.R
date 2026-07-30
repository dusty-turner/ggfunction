#' Plot a Survival Function S(x) = 1 - F(x)
#'
#' `geom_survival()` creates a ggplot2 layer that plots a survival function.
#' By default only the line is drawn (no fill).
#'
#' Supply exactly one of `fun` (a survival function), `cdf_fun` (a CDF),
#' `pdf_fun` (a PDF), `qf_fun` (a quantile function), or `hf_fun` (a hazard
#' function). When `cdf_fun` is supplied, \eqn{S(x) = 1 - F(x)}. When
#' `pdf_fun` is supplied, the CDF is first derived by numerical integration and
#' then \eqn{S(x) = 1 - F(x)}. When `qf_fun` is supplied, the CDF is derived
#' via interpolation and then \eqn{S(x) = 1 - F(x)}. When `hf_fun` is
#' supplied, the survival function is derived via numerical integration of the
#' cumulative hazard as \eqn{S(x) = \exp(-H(x))}.
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A survival function \eqn{S(x)} returning values between 0 and 1
#'   (e.g. `function(x) 1 - pnorm(x)`). Evaluated directly. Exactly one of
#'   `fun`, `cdf_fun`, `pdf_fun`, `qf_fun`, or `hf_fun` must be provided.
#' @param cdf_fun A CDF function (e.g. [pnorm]). The survival function is
#'   computed as `1 - cdf_fun(x)`. Exactly one of `fun`, `cdf_fun`, `pdf_fun`,
#'   `qf_fun`, or `hf_fun` must be provided.
#' @param pdf_fun A PDF function (e.g. [dnorm]). The CDF is derived by
#'   numerical integration and the survival function is computed as
#'   `1 - F(x)`. Exactly one of `fun`, `cdf_fun`, `pdf_fun`, `qf_fun`, or
#'   `hf_fun` must be provided.
#' @param qf_fun A quantile function (e.g. [qnorm]). The CDF is derived via
#'   interpolation and the survival function is computed as `1 - F(x)`.
#'   Exactly one of `fun`, `cdf_fun`, `pdf_fun`, `qf_fun`, or `hf_fun` must be
#'   provided.
#' @param hf_fun A hazard function (e.g. a Weibull hazard). The survival
#'   function is derived via numerical integration of the cumulative hazard as
#'   \eqn{S(x) = \exp(-H(x))}. Exactly one of `fun`, `cdf_fun`, `pdf_fun`,
#'   `qf_fun`, or `hf_fun` must be provided.
#' @param hf_lower Lower limit for integrating `hf_fun`. Defaults to `-Inf`.
#'   When `support` has a finite lower endpoint and `hf_lower` is left at
#'   `-Inf`, the lower support endpoint is used. For finite-support hazards,
#'   set `support` or `hf_lower` to the lower support point (for example, `0`
#'   for Weibull or exponential hazards).
#' @param n Number of points at which to evaluate. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`,
#'   `cdf_fun`, `pdf_fun`, `qf_fun`, or `hf_fun`.
#' @param xlim A numeric vector of length 2 giving the x-range.
#' @param support A numeric vector of length 2 giving the computational support
#'   of the distribution. Defaults to `c(-Inf, Inf)`. It is used when deriving
#'   survival values from a PDF, quantile, or hazard function.
#' @param color Line color for the survival curve.
#' @param fill Fill color for the shaded probability region. The region is
#'   drawn at alpha 0.35 unless an `alpha` aesthetic is supplied; the curve
#'   itself stays at full opacity.
#' @param p A single probability. When supplied, the region of the survival
#'   curve corresponding to cumulative probability `p` from the left (that is,
#'   x-values where \eqn{S(x) \ge 1 - p}) is shaded. Interpreted with
#'   `lower.tail`.
#' @param lower.tail Logical; if `TRUE` (default), `p` shades the lower tail
#'   (x-values up to the `p`-th quantile). If `FALSE`, `p` shades the upper
#'   tail (x-values where \eqn{S(x) \le p}).
#' @param p_lower,p_upper Two cumulative probabilities with
#'   `p_lower < p_upper`. When both are supplied, the region between the
#'   corresponding quantiles is shaded. Cannot be combined with `p`.
#' @param check Logical; if `TRUE`, issue a diagnostic when the survival
#'   function is not near 1 and 0 at the lower and upper ends of the support,
#'   or when the computed values are not monotonically non-increasing. Use
#'   `FALSE` to suppress this check.
#' @param check_tol Numeric tolerance used by the survival validity check.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Points at which the survival function is evaluated.}
#'   \item{`after_stat(y)`}{Survival probabilities.}
#' }
#'
#' @section Aesthetics:
#' `geom_survival()` does not require any input aesthetics when a function
#' source is supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(x)` and `after_stat(y)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `group`,
#'   `linetype`, and `linewidth` for the line.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_cdf()], [geom_chf()], [geom_hf()],
#'   [geom_survival_discrete()], and [geom_ecdf_km()] for related survival and
#'   distribution-function layers.
#'
#' @examples
#'   # Direct survival function
#'   ggplot() +
#'     geom_survival(fun = function(x) 1 - pnorm(x), xlim = c(-3, 3))
#'
#'   # Via CDF
#'   ggplot() +
#'     geom_survival(cdf_fun = pnorm, xlim = c(-3, 3))
#'
#'   ggplot() +
#'     geom_survival(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))
#'
#'   # Via a Weibull hazard
#'   ggplot() +
#'     geom_survival(hf_fun = function(t) 2 * t, support = c(0, Inf), xlim = c(0, 3))
#'
#'   # Shade the region up to the median survival time
#'   ggplot() +
#'     geom_survival(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10),
#'                   p = 0.5)
#'
#' @name geom_survival
#' @aliases StatSurvival GeomSurvival
#' @export
geom_survival <- function(
    mapping = NULL,
    data = NULL,
    stat = StatSurvival,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    cdf_fun = NULL,
    pdf_fun = NULL,
    qf_fun = NULL,
    hf_fun = NULL,
    hf_lower = -Inf,
    xlim = NULL,
    support = c(-Inf, Inf),
    n = 101,
    args = list(),
    color = "black",
    fill = "grey20",
    p = NULL,
    lower.tail = TRUE,
    p_lower = NULL,
    p_upper = NULL,
    check = TRUE,
    check_tol = 1e-2
    ) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  validate_data_limits(xlim)
  validate_probability_shading(p = p, p_lower = p_lower, p_upper = p_upper)

  default_mapping <- aes(x = after_stat(x), y = after_stat(y))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  params <- list(
    fun = fun,
    cdf_fun = cdf_fun,
    pdf_fun = pdf_fun,
    qf_fun = qf_fun,
    hf_fun = hf_fun,
    hf_lower = hf_lower,
    n = n,
    xlim = xlim,
    support = support,
    args = args,
    na.rm = na.rm,
    color = color,
    fill = fill,
    p = p,
    lower.tail = lower.tail,
    p_lower = p_lower,
    p_upper = p_upper,
    check = check,
    check_tol = check_tol,
    ...
  )
  params <- drop_overridden_aes_defaults(params, mapping)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSurvival,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @noRd
check_survival_sources <- function(fun, cdf_fun, pdf_fun, qf_fun, hf_fun) {
  n_provided <- (!is.null(fun)) + (!is.null(cdf_fun)) + (!is.null(pdf_fun)) +
    (!is.null(qf_fun)) + (!is.null(hf_fun))
  if (n_provided == 0L) {
    cli::cli_abort("One of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, {.arg qf_fun}, or {.arg hf_fun} must be provided.")
  }
  if (n_provided > 1L) {
    cli::cli_abort("Supply only one of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, {.arg qf_fun}, or {.arg hf_fun}.")
  }
}

#' @rdname geom_survival
#' @export
StatSurvival <- ggproto("StatSurvival", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           pdf_fun = NULL, qf_fun = NULL,
                           hf_fun = NULL, hf_lower = -Inf,
                           xlim = NULL, support = c(-Inf, Inf),
                           n = 101, args = NULL,
                           p = NULL, lower.tail = TRUE,
                           p_lower = NULL, p_upper = NULL,
                           check = TRUE, check_tol = 1e-2) {

    check_survival_sources(fun, cdf_fun, pdf_fun, qf_fun, hf_fun)
    support <- validate_support_1d(support)
    validate_probability_shading(p = p, p_lower = p_lower, p_upper = p_upper)

    grid <- resolve_stat_grid_1d(scales$x, xlim, support = support, n = n)

    fun_injected <- as_survival_1d(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      qf_fun = qf_fun,
      hf_fun = hf_fun,
      hf_lower = hf_lower,
      args = args,
      support = support
    )
    survival_raw <- fun_injected(grid$eval)

    if (ggfunction_check_enabled(check)) {
      invisible(check_survival_validity(
        fun_injected, survival_raw,
        lower = support[1], upper = support[2],
        tol = check_tol
      ))
    }

    out <- data.frame(
      x = grid$panel,
      x_eval = grid$eval,
      y = scale_forward(scales$y, survival_raw),
      survival = survival_raw
    )

    # Shading boundaries are distributional quantiles resolved on raw
    # probabilities (`p` counts cumulative mass from the left, F = 1 - S),
    # independent of grid resolution and y scale.
    out$in_shade <- FALSE
    out$shade_x_lower_raw <- NA_real_
    out$shade_x_upper_raw <- NA_real_
    if (!is.null(p) || (!is.null(p_lower) && !is.null(p_upper))) {
      qf_injected <- as_qf_1d(
        fun = qf_fun, cdf_fun = cdf_fun, pdf_fun = pdf_fun,
        survival_fun = fun, hf_fun = hf_fun, hf_lower = hf_lower,
        args = validate_named_args(args), support = support
      )
      meta <- cdf_shading_meta(
        qf_injected,
        p = p, lower.tail = lower.tail,
        p_lower = p_lower, p_upper = p_upper
      )
      out <- stat_insert_boundary_rows(
        out, c(meta$lower, meta$upper), fun_injected,
        x_scale = scales$x, y_scale = scales$y,
        value_col = "survival"
      )
      out$in_shade <- cdf_mark_in_shade(out$x_eval, meta$lower, meta$upper)
      out$shade_x_lower_raw <- meta$lower
      out$shade_x_upper_raw <- meta$upper
    }

    # Probability endpoints: raw zero and one, transformed once when finite
    # in the transformation domain.
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel
    out$top_panel <- resolve_stat_baseline(scales$y, 1)$panel
    out
  }
)

#' @rdname geom_survival
#' @export
GeomSurvival <- ggproto("GeomSurvival", GeomArea,
  setup_data = function(data, params) {
    # Probability endpoints train the y scale when finite under the active
    # transformation; the shading baseline is the transformed raw
    # survival zero, never panel zero.
    base <- if ("baseline_panel" %in% names(data)) data$baseline_panel else 0
    top <- if ("top_panel" %in% names(data)) data$top_panel else 1
    transform(data, ymin = base, ymax = top)
  },

  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL
                        ) {

    # Create the line grob for the entire function using GeomPath's draw_panel.
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord, arrow = arrow, lineend = lineend,
      linejoin = linejoin, linemitre = linemitre, na.rm = na.rm
    )

    if (!("in_shade" %in% names(data)) || !any(data$in_shade, na.rm = TRUE)) {
      return(line_grob)
    }

    baseline_panel <- if ("baseline_panel" %in% names(data)) {
      data$baseline_panel[1]
    } else {
      NA_real_
    }
    base_y <- baseline_draw_value(baseline_panel, panel_params)

    build_poly <- function(clip_data, clip_range) {
      pd <- rbind(
        transform(clip_data[1, , drop = FALSE], x = clip_range[1], y = base_y),
        clip_data,
        transform(clip_data[nrow(clip_data), , drop = FALSE], x = clip_range[2], y = base_y)
      )
      pd$colour <- NA
      # Shading defaults to translucent (matching geom_pdf); a supplied
      # alpha aesthetic overrides. The curve itself keeps its own alpha.
      pd$alpha <- ifelse(is.na(pd$alpha), 0.35, pd$alpha)
      pd$ymin <- base_y
      pd$ymax <- pd$y
      pd
    }

    area_grobs <- list()
    for (g in split(data, data$group)) {
      shade <- g[which(g$in_shade), , drop = FALSE]
      if (nrow(shade) < 2L) next
      clip_range <- range(shade$x, na.rm = TRUE)
      area_grobs <- c(area_grobs, list(
        ggproto_parent(GeomArea, self)$draw_panel(
          build_poly(shade, clip_range), panel_params, coord, na.rm = na.rm
        )
      ))
    }

    do.call(grid::grobTree, c(area_grobs, list(line_grob)))
  }
)
