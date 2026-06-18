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
#' @param hf_lower Lower limit for integrating `hf_fun`. Defaults to `-Inf`.
#'   For finite-support hazards, set this to the lower support point (for
#'   example, `0` for Weibull or exponential hazards); values below `hf_lower`
#'   return density `0`.
#' @param n Number of points at which to evaluate the density. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 giving the x-range over which to evaluate the PDF.
#' @param fill Fill color for the shaded area.
#' @param color Line color for the outline of the density curve.
#' @param linewidth Line width for the outline of the density curve.
#' @param alpha Alpha transparency for the shaded area.
#' @param p (Optional) A numeric value between 0 and 1 specifying the cumulative probability
#'   threshold. The area will be shaded up until the point where the cumulative density reaches
#'   this value. The cumulative density is measured relative to the mass within the drawn
#'   `xlim` window (the density is renormalized over `xlim`), so when `xlim` is narrower than
#'   the support, `p` refers to the conditional probability within `xlim` rather than the
#'   unconditional CDF. Widen `xlim` to cover the full support for `p` to match the
#'   unconditional CDF.
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
#'   shaded intervals. Computed following the approach of \doi{10.32614/RJ-2023-048}: density
#'   values are evaluated on the grid, normalized to sum to 1, sorted in descending order, and
#'   cumulated until the target coverage is reached; the density at that threshold determines
#'   which regions are shaded. Takes precedence over `p`, `p_lower`, and `p_upper` if specified.
#' @param check Logical; if `TRUE`, issue a diagnostic when the density does not
#'   integrate to 1 over the drawn x-range. Use `FALSE` to suppress this check.
#' @param check_tol Numeric tolerance used by the normalization check.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Points at which the density is evaluated.}
#'   \item{`after_stat(y)`}{Density values.}
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
#' # Highest density region of a bimodal density
#' f_bim <- function(x) 0.5 * dnorm(x, -2, 0.5) + 0.5 * dnorm(x, 2, 0.5)
#' ggplot() +
#'   geom_pdf(fun = f_bim, xlim = c(-4, 4), shade_hdr = 0.9)
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

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPDF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        fun = fun,
        cdf_fun = cdf_fun,
        survival_fun = survival_fun,
        qf_fun = qf_fun,
        hf_fun = hf_fun,
        hf_lower = hf_lower,
        n = n,
        xlim = xlim,
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
        check = check,
        check_tol = check_tol
      ),
      linewidth_params,
      list(...)
    )
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
                              args = NULL) {
  args <- args %||% list()

  if (!is.null(cdf_fun)) {
    cdf_injected <- function(x) rlang::inject(cdf_fun(x, !!!args))
    cdf_to_pdf(cdf_injected)
  } else if (!is.null(survival_fun)) {
    surv_injected <- function(x) rlang::inject(survival_fun(x, !!!args))
    cdf_derived <- survival_to_cdf(surv_injected)
    cdf_to_pdf(cdf_derived)
  } else if (!is.null(qf_fun)) {
    qf_injected <- function(p) rlang::inject(qf_fun(p, !!!args))
    cdf_derived <- qf_to_cdf(qf_injected)
    cdf_to_pdf(cdf_derived)
  } else if (!is.null(hf_fun)) {
    hf_injected <- function(x) rlang::inject(hf_fun(x, !!!args))
    hf_to_pdf(hf_injected, lower = hf_lower)
  } else {
    function(x) rlang::inject(fun(x, !!!args))
  }
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
pdf_stat_range <- function(scales, xlim = NULL, n = 101) {
  if (is.null(scales$x)) {
    x_range <- xlim %||% c(0, 1)
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
  y_out <- fun(pdf_scale_inverse(panel_params$x, xseq))
  y_out <- pdf_scale_transform(panel_params$y, y_out)

  out <- data[rep(1L, n), , drop = FALSE]
  out$x <- xseq
  out$y <- y_out
  if ("ymin" %in% names(out)) out$ymin <- 0
  if ("ymax" %in% names(out)) out$ymax <- y_out
  out
}

#' @rdname geom_pdf
#' @export
StatPDF <- ggproto("StatPDF", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           survival_fun = NULL, qf_fun = NULL,
                           hf_fun = NULL,
                           hf_lower = -Inf,
                           xlim = NULL, n = 101, args = NULL,
                           check = TRUE, check_tol = 1e-2) {

    # Validate: exactly one source
    check_pdf_sources(fun, cdf_fun, survival_fun, qf_fun, hf_fun)

    range <- pdf_stat_range(scales, xlim, n)
    fun_injected <- make_pdf_function(
      fun, cdf_fun, survival_fun, qf_fun, hf_fun,
      hf_lower = hf_lower, args = args
    )

    y_out <- fun_injected(range$x_eval)

    data.frame(x = range$x, y = y_out)
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
                        hf_lower = -Inf, xlim = NULL, n = 101,
                        args = NULL, check = TRUE, check_tol = 1e-2
                        ) {

    if (is.null(xlim)) {
      fun_injected <- make_pdf_function(
        fun, cdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args
      )
      data <- pdf_panel_data(data, panel_params, fun_injected, n)
    }
    if (ggfunction_check_enabled(check)) {
      check_range <- pdf_eval_range(data$x, panel_params$x)
      check_lower <- if (is.finite(check_range[1])) check_range[1] else -Inf
      check_upper <- if (is.finite(check_range[2])) check_range[2] else Inf
      fun_injected <- make_pdf_function(
        fun, cdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args
      )
      invisible(check_pdf_normalization(
        fun_injected,
        lower = check_lower,
        upper = check_upper,
        tol = check_tol
      ))
    }

    x_vals <- data$x
    y_vals <- data$y

    # Compute cumulative area using the trapezoidal rule.
    dx <- diff(x_vals)
    seg_area <- (y_vals[-length(y_vals)] + y_vals[-1]) / 2 * dx
    cum_area <- c(0, cumsum(seg_area))

    total_area <- max(cum_area)
    norm_cum <- cum_area / total_area  # normalized cumulative area

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

    # Determine the clipping range based on shade_hdr, p_lower/p_upper, or p.
    if (!is.null(shade_hdr)) {
      # Highest density region (HDR) shading, following ggdensity's approach:
      # normalize f(x) values to sum to 1, sort descending, cumsum until
      # coverage is reached, shade all connected intervals above the cutoff.
      fhat_discretized <- y_vals / sum(y_vals)
      ord <- order(y_vals, decreasing = TRUE)
      cumprob <- cumsum(fhat_discretized[ord])
      cutoff_idx <- which(cumprob >= shade_hdr)[1]
      if (is.na(cutoff_idx)) cutoff_idx <- length(y_vals)
      cutoff <- y_vals[ord[cutoff_idx]]

      # Identify connected runs of grid points at or above the cutoff
      above <- y_vals >= cutoff
      runs <- rle(above)
      idx_end <- cumsum(runs$lengths)
      idx_start <- c(1L, head(idx_end, -1L) + 1L)

      for (i in seq_along(runs$values)) {
        if (runs$values[i]) {
          clip_data  <- data[idx_start[i]:idx_end[i], , drop = FALSE]
          clip_range <- c(x_vals[idx_start[i]], x_vals[idx_end[i]])
          area_grobs <- c(area_grobs, list(
            ggproto_parent(GeomArea, self)$draw_panel(
              build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
            )
          ))
        }
      }
    } else if (!is.null(p_lower) && !is.null(p_upper)) {
      idx_lower <- which(norm_cum >= p_lower)[1]
      if (is.na(idx_lower)) idx_lower <- length(norm_cum)
      idx_upper <- which(norm_cum >= p_upper)[1]
      if (is.na(idx_upper)) idx_upper <- length(norm_cum)
      threshold_lower <- x_vals[idx_lower]
      threshold_upper <- x_vals[idx_upper]

      if (shade_outside) {
        # Shade both tails: left of p_lower and right of p_upper
        left_data <- data[data$x <= threshold_lower, , drop = FALSE]
        if (nrow(left_data) > 0) {
          left_range <- c(min(x_vals), threshold_lower)
          area_grobs <- c(area_grobs, list(
            ggproto_parent(GeomArea, self)$draw_panel(
              build_poly(left_data, left_range), panel_params, coord, na.rm = na.rm
            )
          ))
        }
        right_data <- data[data$x >= threshold_upper, , drop = FALSE]
        if (nrow(right_data) > 0) {
          right_range <- c(threshold_upper, max(x_vals))
          area_grobs <- c(area_grobs, list(
            ggproto_parent(GeomArea, self)$draw_panel(
              build_poly(right_data, right_range), panel_params, coord, na.rm = na.rm
            )
          ))
        }
      } else {
        # Shade between p_lower and p_upper
        clip_data <- data[data$x >= threshold_lower & data$x <= threshold_upper, , drop = FALSE]
        clip_range <- c(threshold_lower, threshold_upper)
        area_grobs <- c(area_grobs, list(
          ggproto_parent(GeomArea, self)$draw_panel(
            build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
          )
        ))
      }
    } else if (!is.null(p)) {
      if (lower.tail) {
        idx <- which(norm_cum >= p)[1]
        if (is.na(idx)) idx <- length(norm_cum)
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x <= threshold_x, , drop = FALSE]
        clip_range <- c(min(x_vals), threshold_x)
      } else {
        idx <- which(norm_cum >= (1 - p))[1]
        if (is.na(idx)) idx <- 1
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x >= threshold_x, , drop = FALSE]
        clip_range <- c(threshold_x, max(x_vals))
      }
      area_grobs <- c(area_grobs, list(
        ggproto_parent(GeomArea, self)$draw_panel(
          build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
        )
      ))
    } else {
      clip_range <- range(x_vals, na.rm = TRUE)
      clip_data <- data[data$x >= clip_range[1] & data$x <= clip_range[2], , drop = FALSE]
      area_grobs <- c(area_grobs, list(
        ggproto_parent(GeomArea, self)$draw_panel(
          build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
        )
      ))
    }

    # Create the line grob for the entire function using GeomPath’s draw_panel.
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )

    do.call(grid::grobTree, c(area_grobs, list(line_grob)))
  }
)
