#' Plot a Cumulative Distribution Function
#'
#' `geom_cdf()` creates a ggplot2 layer that plots a cumulative distribution function (CDF)
#' as a line. You can optionally shade a region by specifying a cumulative probability
#' threshold via `p`, or a two-sided interval via `p_lower` and `p_upper`.
#'
#' @inheritParams ggplot2::geom_function
#' @param data Optional data frame. Not required when a function is supplied via
#'   `fun` (or `pdf_fun`/`survival_fun`/`qf_fun`/`hf_fun`); a placeholder is
#'   created internally so the layer can render from the function alone.
#' @param fun A function to compute the CDF (e.g. [pnorm]). The function must accept a numeric
#'   vector as its first argument and return values between 0 and 1. Exactly one of `fun`,
#'   `pdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun` must be provided.
#' @param pdf_fun A PDF function (e.g. [dnorm]). When supplied, the CDF is derived
#'   numerically via integration. Exactly one of `fun`, `pdf_fun`, `survival_fun`,
#'   `qf_fun`, or `hf_fun` must be provided.
#' @param survival_fun A survival function (e.g. `function(x) 1 - pnorm(x)`).
#'   When supplied, the CDF is computed as \eqn{F(x) = 1 - S(x)}. Exactly one of
#'   `fun`, `pdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun` must be provided.
#' @param qf_fun A quantile function (e.g. [qnorm]). When supplied, the CDF is
#'   derived via interpolation on a dense grid. Exactly one of `fun`, `pdf_fun`,
#'   `survival_fun`, `qf_fun`, or `hf_fun` must be provided.
#' @param hf_fun A hazard function (e.g. a Weibull hazard). When supplied, the
#'   CDF is derived via numerical integration of the cumulative hazard as
#'   \eqn{F(x) = 1 - \exp(-H(x))}. Exactly one of `fun`, `pdf_fun`,
#'   `survival_fun`, `qf_fun`, or `hf_fun` must be provided.
#' @param hf_lower Lower limit for integrating `hf_fun`. Defaults to `-Inf`.
#'   When `support` has a finite lower endpoint and `hf_lower` is left at
#'   `-Inf`, the lower support endpoint is used. For finite-support hazards,
#'   set `support` or `hf_lower` to the lower support point (for example, `0`
#'   for Weibull or exponential hazards); values below the hazard origin return
#'   CDF `0`.
#' @param n Number of points at which to evaluate `fun`.
#' @param args A named list of additional arguments passed on to `fun`.
#' @param xlim A numeric vector of length 2 specifying the visible x-range over
#'   which to draw the CDF.
#' @param support A numeric vector of length 2 giving the computational support
#'   of the distribution. Defaults to `c(-Inf, Inf)`. It is used for numerical
#'   PDF-to-CDF integration, CDF-to-quantile inversion, and endpoint checks.
#' @param fill Fill color for the shaded area. The area is drawn at alpha
#'   0.35 unless an `alpha` aesthetic is supplied; the curve itself stays at
#'   full opacity.
#' @param color Line color for the CDF curve.
#' @param p (Optional) A numeric value between 0 and 1 specifying the threshold value of the CDF.
#'   The area will be shaded up until (if `lower.tail = TRUE`) or from (if `lower.tail = FALSE`)
#'   the point where the CDF reaches this value.
#' @param lower.tail Logical; if `TRUE` (the default) shading is applied from the left end of the
#'   curve up to the threshold; if `FALSE`, shading is applied from the threshold to the right end.
#' @param p_lower (Optional) A numeric value between 0 and 1 specifying the lower CDF threshold
#'   for two-sided shading. Used with `p_upper`.
#' @param p_upper (Optional) A numeric value between 0 and 1 specifying the upper CDF threshold
#'   for two-sided shading. Used with `p_lower`.
#' @param check Logical; if `TRUE`, issue a diagnostic when the CDF is not near
#'   0 and 1 at the lower and upper ends of the drawn x-range. Use `FALSE` to
#'   suppress this check.
#' @param check_tol Numeric tolerance used by the CDF endpoint check.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Points at which the CDF is evaluated (data
#'   coordinates).}
#'   \item{`after_stat(y)`}{CDF values.}
#'   \item{`after_stat(p)`}{CDF values; the default y aesthetic maps to this
#'   variable.}
#'   \item{`after_stat(cdf)`}{Raw CDF values (canonical alias of `p`).}
#'   \item{`after_stat(x_eval)`}{Data-space evaluation points.}
#'   \item{`after_stat(in_shade)`}{Logical indicator for grid points inside
#'   the requested probability shading region.}
#'   \item{`after_stat(shade_x_lower_raw)` and
#'   `after_stat(shade_x_upper_raw)`}{Raw (data-space) distributional shading
#'   boundaries, i.e. quantiles of the requested probabilities. Retained even
#'   when a boundary lies outside `xlim` (the visible shading is then simply
#'   clipped by the plotted window).}
#' }
#'
#' @section Aesthetics:
#' `geom_cdf()` does not require any input aesthetics when a function source is
#' supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(x)` and `after_stat(p)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `fill`, `group`,
#'   `linetype`, and `linewidth` for the area and outline.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_pdf()], [geom_qf()], [geom_survival()], [geom_hf()],
#'   [geom_chf()], and [geom_cdf_discrete()] for related distribution-function
#'   layers.
#'
#' @examples
#'   # Plot the standard normal CDF, shading up to the 97.5th percentile.
#'   ggplot() +
#'     geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = TRUE)
#'
#'   # Parameterized via `args`
#'   ggplot() +
#'     geom_cdf(fun = pexp, xlim = c(0, 10), args = list(rate = 0.5))
#'
#' @name geom_cdf
#' @aliases StatCDF GeomCDF
#' @export
geom_cdf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatCDF,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    pdf_fun = NULL,
    survival_fun = NULL,
    qf_fun = NULL,
    hf_fun = NULL,
    hf_lower = -Inf,
    xlim = NULL,
    support = c(-Inf, Inf),
    n = 101,
    args = list(),
    fill = "grey20",
    color = "black",
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

  default_mapping <- aes(x = after_stat(x), y = after_stat(p))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  params <- list(
    fun = fun,
    pdf_fun = pdf_fun,
    survival_fun = survival_fun,
    qf_fun = qf_fun,
    hf_fun = hf_fun,
    hf_lower = hf_lower,
    n = n,
    xlim = xlim,
    support = support,
    args = args,
    na.rm = na.rm,
    fill = fill,
    color = color,
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
    geom = GeomCDF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @noRd
check_cdf_sources <- function(fun, pdf_fun, survival_fun, qf_fun, hf_fun) {
  n_provided <- (!is.null(fun)) + (!is.null(pdf_fun)) +
    (!is.null(survival_fun)) + (!is.null(qf_fun)) + (!is.null(hf_fun))
  if (n_provided == 0L) {
    cli::cli_abort("One of {.arg fun}, {.arg pdf_fun}, {.arg survival_fun}, {.arg qf_fun}, or {.arg hf_fun} must be provided.")
  }
  if (n_provided > 1L) {
    cli::cli_abort("Supply only one of {.arg fun}, {.arg pdf_fun}, {.arg survival_fun}, {.arg qf_fun}, or {.arg hf_fun}.")
  }
}

#' @noRd
make_cdf_function <- function(fun = NULL, pdf_fun = NULL, survival_fun = NULL,
                              qf_fun = NULL, hf_fun = NULL, hf_lower = -Inf,
                              args = NULL, support = c(-Inf, Inf)) {
  args <- args %||% list()
  as_cdf_1d(
    fun = fun,
    pdf_fun = pdf_fun,
    survival_fun = survival_fun,
    qf_fun = qf_fun,
    hf_fun = hf_fun,
    hf_lower = hf_lower,
    args = args,
    support = support
  )
}

#' Resolve raw shading boundaries as distributional quantiles, once per
#' group, in the Stat. `lower`/`upper` are data-space x values;
#' `NA` means the region extends to the corresponding end of the window.
#' @noRd
cdf_shading_meta <- function(qf, p = NULL, lower.tail = TRUE,
                             p_lower = NULL, p_upper = NULL) {
  if (!is.null(p_lower) && !is.null(p_upper)) {
    list(lower = qf(p_lower), upper = qf(p_upper))
  } else if (!is.null(p)) {
    if (isTRUE(lower.tail)) {
      list(lower = NA_real_, upper = qf(p))
    } else {
      list(lower = qf(1 - p), upper = NA_real_)
    }
  } else {
    NULL
  }
}

#' @noRd
cdf_mark_in_shade <- function(x_eval, lower, upper) {
  x_eval >= (if (is.na(lower)) -Inf else lower) &
    x_eval <= (if (is.na(upper)) Inf else upper)
}

#' Insert exact in-window boundary evaluation rows. Positions are
#' transformed exactly once; raw metadata is retained even when no row is
#' inserted.
#' @noRd
cdf_insert_boundary_rows <- function(data, boundaries, fun,
                                     x_scale = NULL, y_scale = NULL) {
  stat_insert_boundary_rows(
    data, boundaries, fun,
    x_scale = x_scale, y_scale = y_scale,
    value_col = "cdf"
  )
}

#' Draw-time regrid: extend the curve over the full visible panel when other
#' layers or expansion widened it beyond the stat's grid. Geometry only —
#' shading membership is reconstructed from raw boundary metadata retained by
#' the Stat; no checks and no quantile solving happen here.
#' @noRd
cdf_panel_data <- function(data, panel_params, fun, n = 101) {
  panel_range <- panel_params$x.range %||% panel_params$x$limits
  if (length(panel_range) != 2L || any(!is.finite(panel_range))) {
    return(data)
  }
  data_range <- range(data$x, na.rm = TRUE)
  if (isTRUE(all.equal(data_range, panel_range, tolerance = sqrt(.Machine$double.eps)))) {
    return(data)
  }

  xseq <- seq(panel_range[1], panel_range[2], length.out = n)
  x_eval <- scale_inverse(panel_params$x, xseq)
  cdf_raw <- fun(x_eval)

  out <- data[rep(1L, n), , drop = FALSE]
  out$x <- xseq
  out$x_eval <- x_eval
  out$y <- scale_forward(panel_params$y, cdf_raw)
  out$cdf <- cdf_raw
  if ("p" %in% names(out)) out$p <- cdf_raw

  lower <- if ("shade_x_lower_raw" %in% names(data)) data$shade_x_lower_raw[1] else NA_real_
  upper <- if ("shade_x_upper_raw" %in% names(data)) data$shade_x_upper_raw[1] else NA_real_
  had_shading <- ("in_shade" %in% names(data)) &&
    (any(data$in_shade) || !is.na(lower) || !is.na(upper))

  if (had_shading) {
    out <- cdf_insert_boundary_rows(
      out, c(lower, upper), fun,
      x_scale = panel_params$x, y_scale = panel_params$y
    )
    out$in_shade <- cdf_mark_in_shade(out$x_eval, lower, upper)
  } else if ("in_shade" %in% names(out)) {
    out$in_shade <- FALSE
  }
  out
}

#' @rdname geom_cdf
#' @export
StatCDF <- ggproto("StatCDF", Stat,

  default_aes = aes(x = NULL, y = after_stat(p)),

  compute_group = function(data, scales, fun = NULL, pdf_fun = NULL,
                           survival_fun = NULL, qf_fun = NULL,
                           hf_fun = NULL,
                           hf_lower = -Inf,
                           xlim = NULL, support = c(-Inf, Inf),
                           n = 101, args = NULL,
                           p = NULL, lower.tail = TRUE,
                           p_lower = NULL, p_upper = NULL,
                           check = TRUE, check_tol = 1e-2) {

    # Validate: exactly one source
    check_cdf_sources(fun, pdf_fun, survival_fun, qf_fun, hf_fun)
    support <- validate_support_1d(support)
    validate_probability_shading(p = p, p_lower = p_lower, p_upper = p_upper)

    grid <- resolve_stat_grid_1d(scales$x, xlim, support = support, n = n)
    fun_injected <- make_cdf_function(
      fun, pdf_fun, survival_fun, qf_fun, hf_fun,
      hf_lower = hf_lower, args = args, support = support
    )

    cdf_raw <- fun_injected(grid$eval)

    out <- data.frame(
      x = grid$panel,
      x_eval = grid$eval,
      y = scale_forward(scales$y, cdf_raw),
      cdf = cdf_raw,
      p = cdf_raw
    )

    if (ggfunction_check_enabled(check)) {
      invisible(check_cdf_normalization(
        fun_injected,
        lower = support[1],
        upper = support[2],
        tol = check_tol
      ))
    }

    # Shading boundaries are distributional quantiles resolved on raw
    # probabilities, independent of grid resolution and y scale.
    out$in_shade <- FALSE
    out$shade_x_lower_raw <- NA_real_
    out$shade_x_upper_raw <- NA_real_
    meta <- NULL
    if (!is.null(p) || (!is.null(p_lower) && !is.null(p_upper))) {
      qf_injected <- make_qf_function(
        cdf_fun = fun, pdf_fun = pdf_fun, survival_fun = survival_fun,
        fun = qf_fun, hf_fun = hf_fun, hf_lower = hf_lower,
        args = args, support = support
      )
      meta <- cdf_shading_meta(
        qf_injected,
        p = p, lower.tail = lower.tail,
        p_lower = p_lower, p_upper = p_upper
      )
    }
    if (!is.null(meta)) {
      out <- cdf_insert_boundary_rows(
        out, c(meta$lower, meta$upper), fun_injected,
        x_scale = scales$x, y_scale = scales$y
      )
      out$in_shade <- cdf_mark_in_shade(out$x_eval, meta$lower, meta$upper)
      out$shade_x_lower_raw <- meta$lower
      out$shade_x_upper_raw <- meta$upper
    }

    # Probability endpoints: raw zero and one, transformed once when finite
    # in the transformation domain; retained as metadata otherwise, so the
    # probability axis trains on the mathematical endpoints.
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel
    out$top_panel <- resolve_stat_baseline(scales$y, 1)$panel
    out
  }
)

#' @rdname geom_cdf
#' @export
GeomCDF <- ggproto("GeomCDF", GeomArea,

  setup_data = function(data, params) {
    # Probability endpoints train the y scale via ymin/ymax when they are
    # finite under the active transformation. The shading baseline is
    # the transformed raw probability zero, never panel zero; when the
    # transformation excludes it, draw_panel clips to the visible panel floor
    # instead.
    base <- if ("baseline_panel" %in% names(data)) data$baseline_panel else 0
    top <- if ("top_panel" %in% names(data)) data$top_panel else 1
    transform(data, ymin = base, ymax = top)
  },

  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL, fun = NULL,
                        pdf_fun = NULL, survival_fun = NULL, qf_fun = NULL,
                        hf_fun = NULL, hf_lower = -Inf, xlim = NULL,
                        support = c(-Inf, Inf),
                        n = 101, args = NULL,
                        check = TRUE, check_tol = 1e-2
                        ) {

    if (is.null(xlim)) {
      # Panel-coverage regrid only; no checks and no quantile solving.
      fun_injected <- make_cdf_function(
        fun, pdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args, support = support
      )
      data <- cdf_panel_data(data, panel_params, fun_injected, n)
    }

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
