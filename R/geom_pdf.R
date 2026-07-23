#' Plot a Probability Density Function with a Filled Area
#'
#' `geom_pdf()` computes a probability density function and plots it as a filled area.
#' This function is similar to [geom_function()], but it shades the area corresponding to
#' a given proportion of the total density.
#'
#' @inheritParams ggplot2::geom_function
#' @importFrom stats integrate
#' @importFrom cli cli_alert
#' @param fun A function to compute the density (e.g. [dnorm]). The function must accept a
#'   numeric vector as its first argument and return density values that integrate (approximately)
#'   to 1. Exactly one of `fun`, `cdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun`
#'   must be provided.
#' @param cdf_fun A CDF function (e.g. [pnorm]). When supplied, the PDF is derived
#'   numerically via central finite differences. Exactly one of `fun`, `cdf_fun`,
#'   `survival_fun`, `qf_fun`, or `hf_fun` must be provided.
#' @param survival_fun A survival function (e.g. `function(x) 1 - pnorm(x)`).
#'   When supplied, the CDF is computed as \eqn{F(x) = 1 - S(x)} and then
#'   differentiated to obtain the PDF. Exactly one of `fun`, `cdf_fun`,
#'   `survival_fun`, `qf_fun`, or `hf_fun` must be provided.
#' @param qf_fun A quantile function (e.g. [qnorm]). When supplied, the CDF is
#'   derived via interpolation and then differentiated to obtain the PDF. Exactly
#'   one of `fun`, `cdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun` must be
#'   provided.
#' @param hf_fun A hazard function (e.g. a Weibull hazard). When supplied, the
#'   PDF is derived via numerical integration of the cumulative hazard. Exactly
#'   one of `fun`, `cdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun` must be
#'   provided.
#' @param hf_lower Lower limit for integrating `hf_fun`. Defaults to `-Inf`;
#'   when `support` has a finite lower endpoint and `hf_lower` is left at
#'   `-Inf`, the lower support endpoint is used. For finite-support hazards,
#'   set `support` or `hf_lower` to the lower support point (for example, `0`
#'   for Weibull or exponential hazards); values below the hazard origin return
#'   density `0`.
#' @param n Number of points at which to evaluate the density. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 giving the visible x-range over
#'   which to draw the PDF. Probability calculations use `support`, not the
#'   displayed interval.
#' @param support A numeric vector of length 2 giving the computational support
#'   of the distribution. Defaults to `c(-Inf, Inf)`. It is used for
#'   normalization checks, PDF-to-CDF integration, CDF-to-quantile inversion,
#'   and support-aware probability shading.
#' @param fill Fill color for the shaded area.
#' @param color Line color for the outline of the density curve.
#' @param linewidth Line width for the outline of the density curve.
#' @param alpha Alpha transparency for the shaded area.
#' @param p (Optional) A numeric value between 0 and 1 specifying the cumulative probability
#'   threshold. The area will be shaded up until the point where the cumulative density reaches
#'   this value. The probability is distributional: it is computed over
#'   `support`, not renormalized over `xlim`. If the corresponding quantile
#'   lies outside `xlim`, the visible shading is simply clipped by the plotted
#'   window.
#' @param lower.tail Logical; if `TRUE` (the default) the shaded area extends from the left end
#'   of the density up to the threshold. If `FALSE`, the shading extends from the threshold to the
#'   right end.
#' @param p_lower (Optional) A numeric value between 0 and 1 specifying the lower cumulative
#'   probability bound. Used with `p_upper` for two-sided shading.
#' @param p_upper (Optional) A numeric value between 0 and 1 specifying the upper cumulative
#'   probability bound. Used with `p_lower` for two-sided shading.
#' @param shade_outside Logical; if `FALSE` (the default) shading is applied between `p_lower`
#'   and `p_upper`. If `TRUE`, shading is applied to the tails outside that range.
#' @param shade_hdr (Optional) A numeric value between 0 and 1 specifying the coverage of the
#'   [highest density region](https://en.wikipedia.org/wiki/Credible_interval#Highest_density_interval)
#'   (HDR) to shade. The HDR is the smallest region of the domain containing the specified
#'   probability mass; for multimodal densities it may be disconnected, producing multiple
#'   shaded intervals. The threshold is approximated on an equally spaced grid
#'   over `hdr_xlim` (or over `xlim` when `hdr_xlim = NULL`), so HDRs are
#'   grid-based approximations rather than analytic regions. Takes precedence
#'   over `p`, `p_lower`, and `p_upper` if specified.
#' @param hdr_xlim Optional numeric vector of length 2 giving the computational
#'   interval for the gridded HDR approximation. When `NULL`, the visible
#'   `xlim` is used. Supplying a wider `hdr_xlim` lets the HDR threshold be
#'   computed over more of the distribution while only the portion intersecting
#'   `xlim` is displayed.
#' @param check Logical; if `TRUE`, issue a diagnostic when the density does not
#'   integrate to 1 over `support`, or when an HDR computation interval omits
#'   substantial probability mass. Use `FALSE` to suppress these checks.
#' @param check_tol Numeric tolerance used by the normalization check.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Points at which the density is evaluated.}
#'   \item{`after_stat(y)`}{Density values.}
#'   \item{`after_stat(in_shade)`}{Logical indicator for grid points inside
#'   the requested probability or HDR shading region.}
#'   \item{`after_stat(shade_lower)` and `after_stat(shade_upper)`}{The
#'   distributional shading boundaries for `p`/`p_lower`/`p_upper` requests,
#'   when applicable.}
#' }
#'
#' @section Aesthetics:
#' `geom_pdf()` does not require any input aesthetics when a function source is
#' supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(x)` and `after_stat(y)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `fill`, `group`,
#'   `linetype`, and `linewidth` for the area and outline.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_cdf()], [geom_qf()], [geom_survival()], [geom_hf()],
#'   [geom_chf()], and [geom_pmf()] for related distribution-function layers.
#'
#' @examples
#' ggplot() +
#'   geom_pdf(fun = dnorm, xlim = c(-3, 3), p = .975, lower.tail = TRUE)
#'
#' ggplot() +
#'   geom_pdf(fun = dbeta, xlim = c(0, 1), support = c(0, 1),
#'     args = list(shape1 = 2, shape2 = 5))
#'
#' # Highest density region of a bimodal density
#' f_bim <- function(x) 0.5 * dnorm(x, -2, 0.5) + 0.5 * dnorm(x, 2, 0.5)
#' ggplot() +
#'   geom_pdf(fun = f_bim, xlim = c(-3, 3), hdr_xlim = c(-5, 5),
#'     shade_hdr = 0.9)
#'
#' @name geom_pdf
#' @aliases StatPDF GeomPDF
#' @export
geom_pdf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatPDF,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    cdf_fun = NULL,
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
    linewidth = NULL,
    alpha = 0.35,
    p = NULL,
    lower.tail = TRUE,
    p_lower = NULL,
    p_upper = NULL,
    shade_outside = FALSE,
    shade_hdr = NULL,
    hdr_xlim = NULL,
    check = TRUE,
    check_tol = 1e-2
    ) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  default_mapping <- aes(x = after_stat(x), y = after_stat(y))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  linewidth_params <- if (is.null(linewidth)) {
    list()
  } else if (utils::packageVersion("ggplot2") < "3.4.0") {
    list(size = linewidth)
  } else {
    list(linewidth = linewidth)
  }

  params <- c(
    list(
      fun = fun,
      cdf_fun = cdf_fun,
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
      alpha = alpha,
      p = p,
      lower.tail = lower.tail,
      p_lower = p_lower,
      p_upper = p_upper,
      shade_outside = shade_outside,
      shade_hdr = shade_hdr,
      hdr_xlim = hdr_xlim,
      check = check,
      check_tol = check_tol
    ),
    linewidth_params,
    list(...)
  )
  params <- drop_overridden_aes_defaults(params, mapping)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPDF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @noRd
check_pdf_sources <- function(fun, cdf_fun, survival_fun, qf_fun, hf_fun) {
  n_provided <- (!is.null(fun)) + (!is.null(cdf_fun)) +
    (!is.null(survival_fun)) + (!is.null(qf_fun)) + (!is.null(hf_fun))
  if (n_provided == 0L) {
    cli::cli_abort("One of {.arg fun}, {.arg cdf_fun}, {.arg survival_fun}, {.arg qf_fun}, or {.arg hf_fun} must be provided.")
  }
  if (n_provided > 1L) {
    cli::cli_abort("Supply only one of {.arg fun}, {.arg cdf_fun}, {.arg survival_fun}, {.arg qf_fun}, or {.arg hf_fun}.")
  }
}

#' @noRd
make_pdf_function <- function(fun = NULL, cdf_fun = NULL, survival_fun = NULL,
                              qf_fun = NULL, hf_fun = NULL, hf_lower = -Inf,
                              args = NULL, support = c(-Inf, Inf)) {
  args <- args %||% list()
  as_pdf_1d(
    fun = fun,
    cdf_fun = cdf_fun,
    survival_fun = survival_fun,
    qf_fun = qf_fun,
    hf_fun = hf_fun,
    hf_lower = hf_lower,
    args = args,
    support = support
  )
}

#' @noRd
make_pdf_cdf_function <- function(fun = NULL, cdf_fun = NULL,
                                  survival_fun = NULL, qf_fun = NULL,
                                  hf_fun = NULL, hf_lower = -Inf,
                                  args = NULL, support = c(-Inf, Inf)) {
  args <- args %||% list()

  if (!is.null(cdf_fun)) {
    as_cdf_1d(fun = cdf_fun, args = args, support = support)
  } else {
    as_cdf_1d(
      pdf_fun = fun,
      survival_fun = survival_fun,
      qf_fun = qf_fun,
      hf_fun = hf_fun,
      hf_lower = hf_lower,
      args = args,
      support = support
    )
  }
}

#' @noRd
make_pdf_qf_function <- function(fun = NULL, cdf_fun = NULL,
                                 survival_fun = NULL, qf_fun = NULL,
                                 hf_fun = NULL, hf_lower = -Inf,
                                 args = NULL, support = c(-Inf, Inf)) {
  args <- args %||% list()

  as_qf_1d(
    fun = qf_fun,
    cdf_fun = cdf_fun,
    pdf_fun = fun,
    survival_fun = survival_fun,
    hf_fun = hf_fun,
    hf_lower = hf_lower,
    args = args,
    support = support
  )
}

#' @noRd
pdf_scale_inverse <- function(x_scale, x) {
  if (is.null(x_scale) || x_scale$is_discrete()) return(x)
  x_scale$get_transformation()$inverse(x)
}

#' @noRd
pdf_scale_transform <- function(y_scale, y) {
  if (is.null(y_scale) || y_scale$is_discrete()) return(y)
  y_scale$get_transformation()$transform(y)
}

#' @noRd
pdf_x_scale_transform <- function(x_scale, x) {
  if (is.null(x_scale) || x_scale$is_discrete()) return(x)
  x_scale$get_transformation()$transform(x)
}

#' @noRd
pdf_stat_range <- function(scales, xlim = NULL, n = 101,
                           support = c(-Inf, Inf)) {
  support <- validate_support_1d(support)
  if (is.null(scales$x)) {
    x_range <- xlim %||% if (all(is.finite(support))) support else c(0, 1)
    xseq <- seq(x_range[1], x_range[2], length.out = n)
    x_eval <- xseq
  } else {
    x_range <- xlim %||% scales$x$dimension()
    xseq <- seq(x_range[1], x_range[2], length.out = n)
    x_eval <- pdf_scale_inverse(scales$x, xseq)
  }

  list(x = xseq, x_eval = x_eval)
}

#' @noRd
pdf_eval_range <- function(x, x_scale = NULL) {
  x_range <- range(x, na.rm = TRUE)
  pdf_scale_inverse(x_scale, x_range)
}

#' @noRd
pdf_panel_data <- function(data, panel_params, fun, n = 101) {
  panel_range <- panel_params$x$limits %||% panel_params$x.range
  if (length(panel_range) != 2L || any(!is.finite(panel_range))) {
    return(data)
  }
  data_range <- range(data$x, na.rm = TRUE)
  if (isTRUE(all.equal(data_range, panel_range, tolerance = sqrt(.Machine$double.eps)))) {
    return(data)
  }

  xseq <- seq(panel_range[1], panel_range[2], length.out = n)
  x_eval <- pdf_scale_inverse(panel_params$x, xseq)
  y_out <- fun(x_eval)
  y_out <- pdf_scale_transform(panel_params$y, y_out)

  out <- data[rep(1L, n), , drop = FALSE]
  out$x <- xseq
  out$x_eval <- x_eval
  out$y <- y_out
  if ("ymin" %in% names(out)) out$ymin <- 0
  if ("ymax" %in% names(out)) out$ymax <- y_out
  out
}

#' @noRd
validate_probability_shading <- function(p = NULL, p_lower = NULL,
                                         p_upper = NULL,
                                         shade_hdr = NULL) {
  probs <- c(p = p, p_lower = p_lower, p_upper = p_upper, shade_hdr = shade_hdr)
  if (length(probs) > 0L &&
      any(!is.finite(probs) | probs <= 0 | probs >= 1)) {
    cli::cli_abort("Probability shading arguments must be strictly between 0 and 1.")
  }
  if ((is.null(p_lower) && !is.null(p_upper)) ||
      (!is.null(p_lower) && is.null(p_upper))) {
    cli::cli_abort("{.arg p_lower} and {.arg p_upper} must be supplied together.")
  }
  if (!is.null(p_lower) && p_lower >= p_upper) {
    cli::cli_abort("{.arg p_lower} must be less than {.arg p_upper}.")
  }
}

#' @noRd
insert_pdf_boundary_rows <- function(data, boundaries, fun, scales) {
  boundaries <- unique(boundaries[is.finite(boundaries)])
  if (length(boundaries) == 0L) return(data)

  display_range <- range(data$x_eval, na.rm = TRUE)
  boundaries <- boundaries[boundaries >= display_range[1] & boundaries <= display_range[2]]
  if (length(boundaries) == 0L) return(data)

  rows <- data[rep(1L, length(boundaries)), , drop = FALSE]
  rows$x_eval <- boundaries
  rows$x <- pdf_x_scale_transform(scales$x, boundaries)
  rows$y <- fun(boundaries)

  out <- rbind(data, rows)
  out[order(out$x), , drop = FALSE]
}

#' @noRd
pdf_probability_intervals <- function(qf, support = c(-Inf, Inf),
                                      p = NULL, lower.tail = TRUE,
                                      p_lower = NULL, p_upper = NULL,
                                      shade_outside = FALSE) {
  support <- validate_support_1d(support)
  if (!is.null(p_lower) && !is.null(p_upper)) {
    q_lower <- qf(p_lower)
    q_upper <- qf(p_upper)
    if (isTRUE(shade_outside)) {
      return(list(
        intervals = rbind(c(support[1], q_lower), c(q_upper, support[2])),
        boundaries = c(q_lower, q_upper),
        shade_lower = q_lower,
        shade_upper = q_upper
      ))
    }
    return(list(
      intervals = rbind(c(q_lower, q_upper)),
      boundaries = c(q_lower, q_upper),
      shade_lower = q_lower,
      shade_upper = q_upper
    ))
  }

  if (!is.null(p)) {
    if (isTRUE(lower.tail)) {
      q <- qf(p)
      return(list(
        intervals = rbind(c(support[1], q)),
        boundaries = q,
        shade_lower = support[1],
        shade_upper = q
      ))
    }
    q <- qf(1 - p)
    return(list(
      intervals = rbind(c(q, support[2])),
      boundaries = q,
      shade_lower = q,
      shade_upper = support[2]
    ))
  }

  list(
    intervals = rbind(support),
    boundaries = numeric(0),
    shade_lower = support[1],
    shade_upper = support[2]
  )
}

#' @noRd
pdf_mark_intervals <- function(x, intervals) {
  in_region <- rep(FALSE, length(x))
  for (i in seq_len(nrow(intervals))) {
    in_region <- in_region | (x >= intervals[i, 1] & x <= intervals[i, 2])
  }
  in_region
}

#' @noRd
pdf_hdr_cutoff <- function(fun, hdr_xlim, n = 101) {
  hdr_grid <- seq(hdr_xlim[1], hdr_xlim[2], length.out = n)
  y <- fun(hdr_grid)
  if (any(!is.finite(y) | y < 0)) {
    cli::cli_abort("HDR shading requires finite, non-negative density values over {.arg hdr_xlim}.")
  }
  if (sum(y) <= 0) {
    cli::cli_abort("HDR shading requires positive density somewhere over {.arg hdr_xlim}.")
  }
  ord <- order(y, decreasing = TRUE)
  weights <- y / sum(y)
  cutoff_idx <- which(cumsum(weights[ord]) >= attr(hdr_xlim, "shade_hdr"))[1L]
  if (is.na(cutoff_idx)) cutoff_idx <- length(y)
  y[ord[cutoff_idx]]
}

#' @noRd
pdf_add_shading_columns <- function(data, scales, fun, cdf, qf,
                                    support = c(-Inf, Inf), xlim = NULL,
                                    p = NULL, lower.tail = TRUE,
                                    p_lower = NULL, p_upper = NULL,
                                    shade_outside = FALSE, shade_hdr = NULL,
                                    hdr_xlim = NULL, n = 101,
                                    check = TRUE, check_tol = 1e-2) {
  validate_probability_shading(p, p_lower, p_upper, shade_hdr)
  support <- validate_support_1d(support)
  data$in_shade <- TRUE
  data$shade_lower <- NA_real_
  data$shade_upper <- NA_real_
  data$hdr_cutoff <- NA_real_

  if (!is.null(shade_hdr)) {
    hdr_xlim <- validate_support_1d(hdr_xlim %||% xlim %||% range(data$x_eval, na.rm = TRUE), "hdr_xlim")
    attr(hdr_xlim, "shade_hdr") <- shade_hdr
    cutoff <- pdf_hdr_cutoff(fun, hdr_xlim, n = n)
    data$in_shade <- data$y >= cutoff
    data$hdr_cutoff <- cutoff

    if (ggfunction_check_enabled(check)) {
      mass_inside <- cdf(hdr_xlim[2]) - cdf(hdr_xlim[1])
      if (is.finite(mass_inside) && 1 - mass_inside > check_tol) {
        cli::cli_warn(c(
          "The HDR computation interval omits substantial probability mass.",
          "i" = "Estimated mass inside {.arg hdr_xlim} is {round(mass_inside, 4)}; set a wider {.arg hdr_xlim} or {.arg check = FALSE}."
        ))
      }
    }
    return(data)
  }

  if (!is.null(p) || (!is.null(p_lower) && !is.null(p_upper))) {
    region <- pdf_probability_intervals(
      qf = qf,
      support = support,
      p = p,
      lower.tail = lower.tail,
      p_lower = p_lower,
      p_upper = p_upper,
      shade_outside = shade_outside
    )
    data <- insert_pdf_boundary_rows(data, region$boundaries, fun, scales)
    data$in_shade <- pdf_mark_intervals(data$x_eval, region$intervals)
    data$shade_lower <- region$shade_lower
    data$shade_upper <- region$shade_upper
  }

  data
}

#' @rdname geom_pdf
#' @export
StatPDF <- ggproto("StatPDF", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           survival_fun = NULL, qf_fun = NULL,
                           hf_fun = NULL,
                           hf_lower = -Inf,
                           xlim = NULL, support = c(-Inf, Inf),
                           n = 101, args = NULL,
                           p = NULL, lower.tail = TRUE,
                           p_lower = NULL, p_upper = NULL,
                           shade_outside = FALSE, shade_hdr = NULL,
                           hdr_xlim = NULL,
                           check = TRUE, check_tol = 1e-2) {

    # Validate: exactly one source
    check_pdf_sources(fun, cdf_fun, survival_fun, qf_fun, hf_fun)
    support <- validate_support_1d(support)

    range <- pdf_stat_range(scales, xlim, n, support = support)
    fun_injected <- make_pdf_function(
      fun, cdf_fun, survival_fun, qf_fun, hf_fun,
      hf_lower = hf_lower, args = args, support = support
    )
    cdf_injected <- make_pdf_cdf_function(
      fun, cdf_fun, survival_fun, qf_fun, hf_fun,
      hf_lower = hf_lower, args = args, support = support
    )
    qf_injected <- make_pdf_qf_function(
      fun, cdf_fun, survival_fun, qf_fun, hf_fun,
      hf_lower = hf_lower, args = args, support = support
    )

    y_out <- fun_injected(range$x_eval)

    out <- data.frame(x = range$x, x_eval = range$x_eval, y = y_out)

    if (ggfunction_check_enabled(check)) {
      invisible(check_pdf_normalization(
        fun_injected,
        lower = support[1],
        upper = support[2],
        tol = check_tol
      ))
    }

    pdf_add_shading_columns(
      out,
      scales = scales,
      fun = fun_injected,
      cdf = cdf_injected,
      qf = qf_injected,
      support = support,
      xlim = xlim %||% range(range$x_eval, na.rm = TRUE),
      p = p,
      lower.tail = lower.tail,
      p_lower = p_lower,
      p_upper = p_upper,
      shade_outside = shade_outside,
      shade_hdr = shade_hdr,
      hdr_xlim = hdr_xlim,
      n = n,
      check = check,
      check_tol = check_tol
    )
  }
)

#' @rdname geom_pdf
#' @export
GeomPDF <- ggproto("GeomPDF", GeomArea,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL, shade_outside = FALSE,
                        shade_hdr = NULL, fun = NULL, cdf_fun = NULL,
                        survival_fun = NULL, qf_fun = NULL, hf_fun = NULL,
                        hf_lower = -Inf, xlim = NULL,
                        support = c(-Inf, Inf), n = 101,
                        args = NULL, hdr_xlim = NULL,
                        check = TRUE, check_tol = 1e-2
                        ) {

    if (is.null(xlim)) {
      support <- validate_support_1d(support)
      fun_injected <- make_pdf_function(
        fun, cdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args, support = support
      )
      cdf_injected <- make_pdf_cdf_function(
        fun, cdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args, support = support
      )
      qf_injected <- make_pdf_qf_function(
        fun, cdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args, support = support
      )
      data <- pdf_panel_data(data, panel_params, fun_injected, n)
      data <- pdf_add_shading_columns(
        data,
        scales = list(x = panel_params$x),
        fun = fun_injected,
        cdf = cdf_injected,
        qf = qf_injected,
        support = support,
        xlim = range(data$x_eval, na.rm = TRUE),
        p = p,
        lower.tail = lower.tail,
        p_lower = p_lower,
        p_upper = p_upper,
        shade_outside = shade_outside,
        shade_hdr = shade_hdr,
        hdr_xlim = hdr_xlim,
        n = n,
        check = FALSE,
        check_tol = check_tol
      )
    }

    # Helper to build a closed polygon from clipped data
    build_poly <- function(clip_data, clip_range) {
      pd <- rbind(
        transform(clip_data[1, , drop = FALSE], x = clip_range[1], y = 0),
        clip_data,
        transform(clip_data[nrow(clip_data), , drop = FALSE], x = clip_range[2], y = 0)
      )
      pd$colour <- NA
      pd
    }

    area_grobs <- list()

    in_shade <- if ("in_shade" %in% names(data)) data$in_shade else rep(TRUE, nrow(data))
    if (any(in_shade, na.rm = TRUE)) {
      runs <- rle(in_shade)
      idx_end <- cumsum(runs$lengths)
      idx_start <- c(1L, head(idx_end, -1L) + 1L)

      for (i in seq_along(runs$values)) {
        if (!runs$values[i]) next
        clip_data <- data[idx_start[i]:idx_end[i], , drop = FALSE]
        clip_range <- range(clip_data$x, na.rm = TRUE)
        area_grobs <- c(area_grobs, list(
          ggproto_parent(GeomArea, self)$draw_panel(
            build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
          )
        ))
      }
    }

    # Create the line grob for the entire function using GeomPath's draw_panel.
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )

    do.call(grid::grobTree, c(area_grobs, list(line_grob)))
  }
)
