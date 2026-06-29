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
#'   `pdf_fun`, or `survival_fun`.
#' @param support A numeric vector of length 2 giving the computational support
#'   of the distribution. Defaults to `c(-Inf, Inf)`. It is used when a
#'   quantile function is derived by integrating a PDF or inverting a CDF.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(p)`}{Probability values at which the quantile function
#'   is evaluated.}
#'   \item{`after_stat(x)`}{Quantile values; the default y aesthetic maps to
#'   this variable.}
#'   \item{`after_stat(q)`}{Quantile values.}
#' }
#'
#' @section Aesthetics:
#' `geom_qf()` does not require any input aesthetics when a function source is
#' supplied. It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(p)` and `after_stat(x)`.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `group`,
#'   `linetype`, and `linewidth` for the line.}
#' }
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_cdf()], [geom_pdf()], and [geom_qf_discrete()] for related
#'   quantile and distribution-function layers.
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
                    support = c(-Inf, Inf),
                    n = 101,
                    args = list()) {

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
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      support = support,
      n = n,
      args = args,
      na.rm = na.rm,
      ...
    )
  )

  list(main_layer, probability_axis_anchor())
}

#' @rdname geom_qf
#' @export
StatQF <- ggproto("StatQF", Stat,

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           pdf_fun = NULL, survival_fun = NULL,
                           support = c(-Inf, Inf),
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

    fun_injected <- make_qf_function(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      args = args,
      support = support
    )

    q_vals <- fun_injected(p_vals)

    data.frame(p = p_vals, q = q_vals, x = q_vals)

  }
)
