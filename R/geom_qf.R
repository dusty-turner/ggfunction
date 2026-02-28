#' Plot a Quantile Function
#'
#' `geom_qf()` creates a ggplot2 layer that plots a quantile function (inverse CDF)
#' as a line. It computes quantile values for a sequence of probabilities (from 0 to 1)
#' and connects them with a line.
#'
#' Supply exactly one of `fun` (a quantile function), `cdf_fun` (a CDF),
#' `pdf_fun` (a PDF), or `survival_fun` (a survival function). When `cdf_fun`
#' is supplied, the quantile function is derived by numerical root-finding.
#' When `pdf_fun` is supplied, the CDF is first derived by numerical
#' integration and then inverted. When `survival_fun` is supplied, the CDF is
#' computed as \eqn{F(x) = 1 - S(x)} and then inverted.
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A function to compute the quantile function (e.g. [qnorm]). The function must
#'   accept a numeric vector of probabilities (values in `[0,1]`) as its first argument.
#'   Exactly one of `fun`, `cdf_fun`, `pdf_fun`, or `survival_fun` must be provided.
#' @param cdf_fun A CDF function (e.g. [pnorm]). The quantile function is derived
#'   numerically via root-finding. Exactly one of `fun`, `cdf_fun`, `pdf_fun`,
#'   or `survival_fun` must be provided.
#' @param pdf_fun A PDF function (e.g. [dnorm]). The CDF is first derived by
#'   numerical integration, then the quantile function by root-finding. Exactly
#'   one of `fun`, `cdf_fun`, `pdf_fun`, or `survival_fun` must be provided.
#' @param survival_fun A survival function (e.g. `function(x) 1 - pnorm(x)`).
#'   The CDF is computed as \eqn{F(x) = 1 - S(x)} and then the quantile
#'   function is derived by root-finding. Exactly one of `fun`, `cdf_fun`,
#'   `pdf_fun`, or `survival_fun` must be provided.
#' @param n Number of probability points at which to evaluate `fun`. Defaults to 101.
#'   Points are placed at [Chebyshev nodes](https://en.wikipedia.org/wiki/Chebyshev_nodes)
#'   of the first kind on $(0, 1)$, which cluster
#'   near 0 and 1 where quantile functions are typically most curved, and never include
#'   the exact endpoints (avoiding \eqn{\pm\infty} for unbounded distributions).
#' @param args A named list of additional arguments to pass to `fun`, `cdf_fun`,
#'   or `pdf_fun`.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   ggplot() +
#'     geom_qf(fun = qnorm, args = list(mean = 3, sd = 2))
#'
#'   ggplot() +
#'     geom_qf(fun = qbeta, args = list(shape1 = 3, shape2 = 4))
#'
#' @name geom_qf
#' @aliases StatQF
#' @export
geom_qf <- function(mapping = NULL,
                    data = NULL,
                    stat = StatQF,
                    position = "identity",
                    ...,
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE,
                    fun = NULL,
                    cdf_fun = NULL,
                    pdf_fun = NULL,
                    survival_fun = NULL,
                    n = 101,
                    args = list()) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  default_mapping <- aes(x = after_stat(p), y = after_stat(q))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      n = n,
      args = args,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_qf
#' @export
StatQF <- ggproto("StatQF", Stat,

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           pdf_fun = NULL, survival_fun = NULL,
                           n = 101, args = NULL, ...) {

    # Validate: exactly one source
    n_provided <- (!is.null(fun)) + (!is.null(cdf_fun)) + (!is.null(pdf_fun)) +
      (!is.null(survival_fun))
    if (n_provided == 0L) {
      cli::cli_abort("One of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, or {.arg survival_fun} must be provided.")
    }
    if (n_provided > 1L) {
      cli::cli_abort("Supply only one of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, or {.arg survival_fun}.")
    }

    k <- seq_len(n)
    p_vals <- (1 - cos((2 * k - 1) * pi / (2 * n))) / 2

    if (!is.null(fun)) {
      fun_injected <- function(p) rlang::inject(fun(p, !!!args))
    } else if (!is.null(cdf_fun)) {
      cdf_injected <- function(x) rlang::inject(cdf_fun(x, !!!args))
      fun_injected <- cdf_to_qf(cdf_injected)
    } else if (!is.null(pdf_fun)) {
      pdf_injected <- function(x) rlang::inject(pdf_fun(x, !!!args))
      cdf_derived <- pdf_to_cdf(pdf_injected)
      fun_injected <- cdf_to_qf(cdf_derived)
    } else {
      surv_injected <- function(x) rlang::inject(survival_fun(x, !!!args))
      cdf_derived <- survival_to_cdf(surv_injected)
      fun_injected <- cdf_to_qf(cdf_derived)
    }

    q_vals <- fun_injected(p_vals)

    data.frame(p = p_vals, q = q_vals)

  }
)
