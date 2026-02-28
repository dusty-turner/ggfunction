#' Plot a Hazard Function h(x) = f(x) / S(x)
#'
#' `geom_hf()` creates a ggplot2 layer that plots a hazard function. Two
#' interfaces are supported:
#'
#' * **PDF + CDF interface**: supply `pdf_fun` and `cdf_fun`; the hazard is
#'   computed internally as \eqn{h(x) = f(x) / (1 - F(x))}.
#' * **PDF only**: supply just `pdf_fun`; the CDF is derived by numerical
#'   integration.
#' * **CDF only**: supply just `cdf_fun`; the PDF is derived by numerical
#'   differentiation.
#' * **Direct hazard interface**: supply `fun`, a function that returns
#'   \eqn{h(x)} directly (e.g. a closed-form expression).
#' * **Survival function**: supply `survival_fun`; the CDF is computed as
#'   \eqn{F = 1 - S} and the PDF by differentiation.
#' * **Quantile function**: supply `qf_fun`; the CDF is derived via
#'   interpolation and the PDF by differentiation.
#'
#' Supply either `fun` alone, one or both of `pdf_fun`/`cdf_fun`,
#' `survival_fun`, or `qf_fun`. By default only the line is drawn (no fill).
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A hazard function \eqn{h(x)} (optional). When supplied,
#'   no other function source must be provided.
#' @param pdf_fun A PDF function (e.g. [dnorm]). When supplied without
#'   `cdf_fun`, the CDF is derived by numerical integration.
#' @param cdf_fun A CDF function (e.g. [pnorm]). When supplied without
#'   `pdf_fun`, the PDF is derived by numerical differentiation.
#' @param survival_fun A survival function (e.g. `function(x) 1 - pnorm(x)`).
#'   The CDF is computed as \eqn{F(x) = 1 - S(x)} and the PDF by
#'   differentiation.
#' @param qf_fun A quantile function (e.g. [qnorm]). The CDF is derived via
#'   interpolation and the PDF by differentiation.
#' @param n Number of points at which to evaluate. Defaults to 101.
#' @param args A named list of arguments passed to `fun`, or shared by
#'   both `pdf_fun` and `cdf_fun`.
#' @param pdf_args A named list of additional arguments specific to `pdf_fun`
#'   (overrides `args`). Ignored when using the direct hazard interface.
#' @param cdf_args A named list of additional arguments specific to `cdf_fun`
#'   (overrides `args`). Ignored when using the direct hazard interface.
#' @param xlim A numeric vector of length 2 giving the x-range.
#' @param color Line color for the hazard curve.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   # PDF + CDF interface
#'   ggplot() +
#'     geom_hf(pdf_fun = dnorm, cdf_fun = pnorm, xlim = c(-3, 3))
#'
#'   ggplot() +
#'     geom_hf(pdf_fun = dexp, cdf_fun = pexp,
#'       args = list(rate = 0.5), xlim = c(0, 10))
#'
#'   # Direct hazard interface (Weibull closed-form hazard)
#'   h_weibull <- function(x, shape, scale) (shape / scale) * (x / scale)^(shape - 1)
#'   ggplot() +
#'     geom_hf(fun = h_weibull, xlim = c(0.01, 5),
#'       args = list(shape = 0.5, scale = 2))
#'
#' @name geom_hf
#' @aliases StatHF GeomHF
#' @export
geom_hf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatHF,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    pdf_fun = NULL,
    cdf_fun = NULL,
    survival_fun = NULL,
    qf_fun = NULL,
    xlim = NULL,
    n = 101,
    args = list(),
    pdf_args = NULL,
    cdf_args = NULL,
    color = "black"
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
    geom = GeomHF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      pdf_fun = pdf_fun,
      cdf_fun = cdf_fun,
      survival_fun = survival_fun,
      qf_fun = qf_fun,
      n = n,
      xlim = xlim,
      args = args,
      pdf_args = pdf_args,
      cdf_args = cdf_args,
      na.rm = na.rm,
      color = color,
      ...
    )
  )
}

#' @rdname geom_hf
#' @export
StatHF <- ggproto("StatHF", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, pdf_fun = NULL,
                           cdf_fun = NULL, survival_fun = NULL,
                           qf_fun = NULL, xlim = NULL, n = 101,
                           args = NULL, pdf_args = NULL, cdf_args = NULL) {

    # Validate interface
    using_fun <- !is.null(fun)
    using_pdf_cdf <- !is.null(pdf_fun) || !is.null(cdf_fun)
    using_survival <- !is.null(survival_fun)
    using_qf <- !is.null(qf_fun)

    n_sources <- using_fun + using_pdf_cdf + using_survival + using_qf
    if (n_sources == 0L) {
      cli::cli_abort(
        "Supply {.arg fun}, {.arg pdf_fun}/{.arg cdf_fun}, {.arg survival_fun}, or {.arg qf_fun}."
      )
    }
    if (n_sources > 1L) {
      cli::cli_abort(
        "Supply only one source: {.arg fun}, {.arg pdf_fun}/{.arg cdf_fun}, {.arg survival_fun}, or {.arg qf_fun}."
      )
    }

    range <- if (is.null(scales$x)) {
      xlim %||% c(0, 1)
    } else {
      xlim %||% scales$x$dimension()
    }

    xseq <- seq(range[1], range[2], length.out = n)

    if (using_fun) {
      fun_injected <- function(x) rlang::inject(fun(x, !!!args))
      y_out <- fun_injected(xseq)
    } else if (using_survival) {
      surv_injected <- function(x) rlang::inject(survival_fun(x, !!!args))
      cdf_derived <- survival_to_cdf(surv_injected)
      pdf_derived <- cdf_to_pdf(cdf_derived)

      f_vals <- pdf_derived(xseq)
      S_vals <- surv_injected(xseq)
      y_out <- ifelse(S_vals > 0, f_vals / S_vals, NaN)
    } else if (using_qf) {
      qf_injected <- function(p) rlang::inject(qf_fun(p, !!!args))
      cdf_derived <- qf_to_cdf(qf_injected)
      pdf_derived <- cdf_to_pdf(cdf_derived)

      f_vals <- pdf_derived(xseq)
      S_vals <- 1 - cdf_derived(xseq)
      y_out <- ifelse(S_vals > 0, f_vals / S_vals, NaN)
    } else {
      # Merge shared args with specific overrides
      pdf_a <- if (!is.null(pdf_args)) modifyList(args, pdf_args) else args
      cdf_a <- if (!is.null(cdf_args)) modifyList(args, cdf_args) else args

      # Build injected versions of whichever functions were supplied
      if (!is.null(pdf_fun)) {
        pdf_injected <- function(x) rlang::inject(pdf_fun(x, !!!pdf_a))
      }
      if (!is.null(cdf_fun)) {
        cdf_injected <- function(x) rlang::inject(cdf_fun(x, !!!cdf_a))
      }

      # Derive missing component
      if (is.null(cdf_fun)) {
        cdf_injected <- pdf_to_cdf(pdf_injected)
      }
      if (is.null(pdf_fun)) {
        pdf_injected <- cdf_to_pdf(cdf_injected)
      }

      f_vals <- pdf_injected(xseq)
      S_vals <- 1 - cdf_injected(xseq)

      # Compute hazard, guarding division by zero
      y_out <- ifelse(S_vals > 0, f_vals / S_vals, NaN)
    }

    data.frame(x = xseq, y = y_out)
  }
)

#' @rdname geom_hf
#' @export
GeomHF <- ggproto("GeomHF", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE
                        ) {

    x_vals <- data$x
    y_vals <- data$y

    # Remove NaN values for rendering
    valid <- !is.nan(y_vals)
    valid_data <- data[valid, , drop = FALSE]

    ggproto_parent(GeomPath, self)$draw_panel(
      valid_data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )
  }
)
