#' Plot a Cumulative Distribution Function
#'
#' `geom_cdf()` creates a ggplot2 layer that plots a cumulative distribution function (CDF)
#' as a line. You can optionally shade a region by specifying a cumulative probability
#' threshold via `p`, or a two-sided interval via `p_lower` and `p_upper`.
#'
#' @inheritParams ggplot2::geom_function
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
#'   For finite-support hazards, set this to the lower support point (for
#'   example, `0` for Weibull or exponential hazards); values below `hf_lower`
#'   return CDF `0`.
#' @param n Number of points at which to evaluate `fun`.
#' @param args A named list of additional arguments passed on to `fun`.
#' @param xlim A numeric vector of length 2 specifying the x-range over which to evaluate the CDF.
#' @param fill Fill color for the shaded area.
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
#' @return A ggplot2 layer.
#'
#' @examples
#'   # Plot the standard normal CDF, shading up to the 97.5th percentile.
#'   ggplot() +
#'     geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = TRUE)
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

  default_mapping <- aes(x = after_stat(x), y = after_stat(p))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCDF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      pdf_fun = pdf_fun,
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
      p = p,
      lower.tail = lower.tail,
      p_lower = p_lower,
      p_upper = p_upper,
      check = check,
      check_tol = check_tol,
      ...
    )
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
                              args = NULL) {
  args <- args %||% list()

  if (!is.null(pdf_fun)) {
    pdf_injected <- function(x) rlang::inject(pdf_fun(x, !!!args))
    pdf_to_cdf(pdf_injected)
  } else if (!is.null(survival_fun)) {
    surv_injected <- function(x) rlang::inject(survival_fun(x, !!!args))
    survival_to_cdf(surv_injected)
  } else if (!is.null(qf_fun)) {
    qf_injected <- function(p) rlang::inject(qf_fun(p, !!!args))
    qf_to_cdf(qf_injected)
  } else if (!is.null(hf_fun)) {
    hf_injected <- function(x) rlang::inject(hf_fun(x, !!!args))
    hf_to_cdf(hf_injected, lower = hf_lower)
  } else {
    function(x) rlang::inject(fun(x, !!!args))
  }
}

#' @noRd
cdf_scale_inverse <- function(x_scale, x) {
  if (is.null(x_scale) || x_scale$is_discrete()) return(x)
  x_scale$get_transformation()$inverse(x)
}

#' @noRd
cdf_scale_transform <- function(y_scale, y) {
  if (is.null(y_scale) || y_scale$is_discrete()) return(y)
  y_scale$get_transformation()$transform(y)
}

#' @noRd
cdf_stat_range <- function(scales, xlim = NULL, n = 101) {
  if (is.null(scales$x)) {
    x_range <- xlim %||% c(-Inf, Inf)
    xseq <- seq(x_range[1], x_range[2], length.out = n)
    x_eval <- xseq
  } else {
    x_range <- xlim %||% scales$x$dimension()
    xseq <- seq(x_range[1], x_range[2], length.out = n)
    x_eval <- cdf_scale_inverse(scales$x, xseq)
  }

  list(x = xseq, x_eval = x_eval)
}

#' @noRd
cdf_eval_range <- function(x, x_scale = NULL) {
  x_range <- range(x, na.rm = TRUE)
  cdf_scale_inverse(x_scale, x_range)
}

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
  y_out <- fun(cdf_scale_inverse(panel_params$x, xseq))
  y_out <- cdf_scale_transform(panel_params$y, y_out)

  out <- data[rep(1L, n), , drop = FALSE]
  out$x <- xseq
  out$y <- y_out
  if ("p" %in% names(out)) out$p <- y_out
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
                           xlim = NULL, n = 101, args = NULL,
                           check = TRUE, check_tol = 1e-2) {

    # Validate: exactly one source
    check_cdf_sources(fun, pdf_fun, survival_fun, qf_fun, hf_fun)

    range <- cdf_stat_range(scales, xlim, n)
    fun_injected <- make_cdf_function(
      fun, pdf_fun, survival_fun, qf_fun, hf_fun,
      hf_lower = hf_lower, args = args
    )

    y_out <- fun_injected(range$x_eval)

    data.frame(x = range$x, y = y_out, p = y_out)
  }
)

#' @rdname geom_cdf
#' @export
GeomCDF <- ggproto("GeomCDF", GeomArea,

  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL, fun = NULL,
                        pdf_fun = NULL, survival_fun = NULL, qf_fun = NULL,
                        hf_fun = NULL, hf_lower = -Inf, xlim = NULL,
                        n = 101, args = NULL,
                        check = TRUE, check_tol = 1e-2
                        ) {

    if (is.null(xlim)) {
      fun_injected <- make_cdf_function(
        fun, pdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args
      )
      data <- cdf_panel_data(data, panel_params, fun_injected, n)
    }
    if (ggfunction_check_enabled(check)) {
      check_range <- cdf_eval_range(data$x, panel_params$x)
      fun_injected <- make_cdf_function(
        fun, pdf_fun, survival_fun, qf_fun, hf_fun,
        hf_lower = hf_lower, args = args
      )
      invisible(check_cdf_normalization(
        fun_injected,
        lower = check_range[1],
        upper = check_range[2],
        tol = check_tol
      ))
    }

    x_vals <- data$x
    y_vals <- data$y

    if (!is.null(p_lower) && !is.null(p_upper)) {
      # Two-sided shading: shade between x where CDF = p_lower and CDF = p_upper
      idx_lower <- which(y_vals >= p_lower)[1]
      if (is.na(idx_lower)) idx_lower <- length(y_vals)
      idx_upper <- which(y_vals >= p_upper)[1]
      if (is.na(idx_upper)) idx_upper <- length(y_vals)
      threshold_lower <- x_vals[idx_lower]
      threshold_upper <- x_vals[idx_upper]
      clip_data <- data[data$x >= threshold_lower & data$x <= threshold_upper, , drop = FALSE]
      clip_range <- c(threshold_lower, threshold_upper)
    } else if (!is.null(p)) {
      if (lower.tail) {
        idx <- which(y_vals >= p)[1]
        if (is.na(idx)) idx <- length(y_vals)
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x <= threshold_x, , drop = FALSE]
        clip_range <- c(min(x_vals), threshold_x)
      } else {
        idx <- which(y_vals >= (1 - p))[1]
        if (is.na(idx)) idx <- 1
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x >= threshold_x, , drop = FALSE]
        clip_range <- c(threshold_x, max(x_vals))
      }
    } else {
      clip_data <- NULL
      clip_range <- NULL
    }

    # Create the line grob for the entire function using GeomPath’s draw_panel.
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord, arrow = arrow, lineend = lineend,
      linejoin = linejoin, linemitre = linemitre, na.rm = na.rm
    )

    if (is.null(clip_data)) {
      return(line_grob)
    }

    # Close the polygon by adding baseline (y=0) points at the boundaries.
    poly_data <- rbind(
      transform(clip_data[1, , drop = FALSE], x = clip_range[1], y = 0),
      clip_data,
      transform(clip_data[nrow(clip_data), , drop = FALSE], x = clip_range[2], y = 0)
    )

    poly_data$colour <- NA

    # Draw the filled area using GeomArea’s draw_panel.
    area_grob <- ggproto_parent(GeomArea, self)$draw_panel(
      poly_data, panel_params, coord, na.rm = na.rm
    )

    grid::grobTree(area_grob, line_grob)
  }
)
