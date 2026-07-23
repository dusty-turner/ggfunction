#' Plot a Cumulative Hazard Function H(x)
#'
#' `geom_chf()` creates a ggplot2 layer that plots a cumulative hazard
#' function. By default only the line is drawn (no fill).
#'
#' Supply exactly one of `fun` (a cumulative hazard function, evaluated
#' directly), `hf_fun` (a hazard function, integrated numerically),
#' `cdf_fun` (a CDF), `pdf_fun` (a PDF), `survival_fun` (a survival
#' function), or `qf_fun` (a quantile function).
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A cumulative hazard function \eqn{H(x)} (e.g.
#'   `function(x) -log(1 - pnorm(x))`). Evaluated directly. Exactly one of
#'   `fun`, `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun`
#'   must be provided.
#' @param hf_fun A hazard function (e.g. a Weibull hazard). The cumulative
#'   hazard is derived by numerical integration. Exactly one of `fun`,
#'   `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun` must be
#'   provided.
#' @param hf_lower Lower limit for integrating `hf_fun`. Defaults to `-Inf`.
#'   For finite-support hazards, set this to the lower support point (for
#'   example, `0` for Weibull or exponential hazards); values below `hf_lower`
#'   return cumulative hazard `0`.
#' @param cdf_fun A CDF function (e.g. [pnorm]). The cumulative hazard is
#'   computed as \eqn{H(x) = -\log(1 - F(x))}. Exactly one of `fun`,
#'   `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun` must be
#'   provided.
#' @param pdf_fun A PDF function (e.g. [dnorm]). The CDF is derived by
#'   numerical integration and then \eqn{H(x) = -\log(1 - F(x))}. Exactly
#'   one of `fun`, `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or
#'   `qf_fun` must be provided.
#' @param survival_fun A survival function (e.g. `function(x) 1 - pnorm(x)`).
#'   The cumulative hazard is computed as \eqn{H(x) = -\log(S(x))}. Exactly
#'   one of `fun`, `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or
#'   `qf_fun` must be provided.
#' @param qf_fun A quantile function (e.g. [qnorm]). The CDF is derived via
#'   interpolation and then \eqn{H(x) = -\log(1 - F(x))}. Exactly one of
#'   `fun`, `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun`
#'   must be provided.
#' @param n Number of points at which to evaluate. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`,
#'   `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun`.
#' @param xlim A numeric vector of length 2 giving the x-range.
#' @param support A numeric vector of length 2 giving the computational support
#'   of the distribution. Defaults to `c(-Inf, Inf)`. It is used for
#'   PDF-to-CDF and hazard integrations.
#' @param check Logical; if `TRUE`, issue a diagnostic when the computed
#'   cumulative hazard values are negative or not monotonically
#'   non-decreasing. Use `FALSE` to suppress this check.
#' @param check_tol Numeric tolerance used by the cumulative hazard validity
#'   check.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Points at which the cumulative hazard is evaluated.}
#'   \item{`after_stat(y)`}{Cumulative hazard values.}
#' }
#'
#' @section Aesthetics:
#' `geom_chf()` does not require any input aesthetics when a function source is
#' supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(x)` and `after_stat(y)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `group`,
#'   `linetype`, and `linewidth` for the line.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_hf()], [geom_survival()], [geom_cdf()], and [geom_echf()] for
#'   related hazard and survival-function layers.
#'
#' @examples
#'   # Via CDF
#'   ggplot() +
#'     geom_chf(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))
#'
#'   # Via hazard function (constant hazard = exponential)
#'   h_const <- function(x, rate) ifelse(x >= 0, rate, 0)
#'   ggplot() +
#'     geom_chf(hf_fun = h_const, args = list(rate = 0.5), xlim = c(0, 10))
#'
#'   # Via survival function
#'   ggplot() +
#'     geom_chf(survival_fun = function(x) 1 - pnorm(x), xlim = c(-3, 3))
#'
#' @name geom_chf
#' @aliases StatCHF GeomCHF
#' @export
geom_chf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatCHF,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    hf_fun = NULL,
    cdf_fun = NULL,
    pdf_fun = NULL,
    survival_fun = NULL,
    qf_fun = NULL,
    hf_lower = -Inf,
    xlim = NULL,
    support = c(-Inf, Inf),
    n = 101,
    args = list(),
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

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCHF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      hf_fun = hf_fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      qf_fun = qf_fun,
      hf_lower = hf_lower,
      n = n,
      xlim = xlim,
      support = support,
      args = args,
      check = check,
      check_tol = check_tol,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_chf
#' @export
StatCHF <- ggproto("StatCHF", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, hf_fun = NULL,
                           cdf_fun = NULL, pdf_fun = NULL,
                           survival_fun = NULL, qf_fun = NULL,
                           hf_lower = -Inf,
                           xlim = NULL, support = c(-Inf, Inf),
                           n = 101, args = NULL,
                           check = TRUE, check_tol = 1e-2) {

    # Validate: exactly one source
    n_provided <- (!is.null(fun)) + (!is.null(hf_fun)) + (!is.null(cdf_fun)) +
      (!is.null(pdf_fun)) + (!is.null(survival_fun)) + (!is.null(qf_fun))
    if (n_provided == 0L) {
      cli::cli_abort(
        "One of {.arg fun}, {.arg hf_fun}, {.arg cdf_fun}, {.arg pdf_fun}, {.arg survival_fun}, or {.arg qf_fun} must be provided."
      )
    }
    if (n_provided > 1L) {
      cli::cli_abort(
        "Supply only one of {.arg fun}, {.arg hf_fun}, {.arg cdf_fun}, {.arg pdf_fun}, {.arg survival_fun}, or {.arg qf_fun}."
      )
    }

    range <- if (is.null(scales$x)) {
      xlim %||% c(0, 1)
    } else {
      xlim %||% scales$x$dimension()
    }

    xseq <- seq(range[1], range[2], length.out = n)

    fun_injected <- as_chf_1d(
      fun = fun,
      hf_fun = hf_fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      qf_fun = qf_fun,
      hf_lower = hf_lower,
      args = args,
      support = support
    )
    y_out <- fun_injected(xseq)

    if (ggfunction_check_enabled(check)) {
      invisible(check_chf_validity(y_out, tol = check_tol))
    }

    data.frame(x = xseq, y = y_out)
  }
)

#' @rdname geom_chf
#' @export
GeomCHF <- ggproto("GeomCHF", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE
                        ) {

    x_vals <- data$x
    y_vals <- data$y

    # Remove NaN/Inf values (e.g. -log(0) at boundary)
    valid <- is.finite(y_vals)
    valid_data <- data[valid, , drop = FALSE]

    ggproto_parent(GeomPath, self)$draw_panel(
      valid_data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )
  }
)
