#' Plot a Survival Function S(x) = 1 - F(x)
#'
#' `geom_survival()` creates a ggplot2 layer that plots a survival function.
#' By default only the line is drawn (no fill).
#'
#' Supply exactly one of `fun` (a survival function), `cdf_fun` (a CDF),
#' `pdf_fun` (a PDF), or `qf_fun` (a quantile function). When `cdf_fun` is
#' supplied, \eqn{S(x) = 1 - F(x)}. When `pdf_fun` is supplied, the CDF is
#' first derived by numerical integration and then \eqn{S(x) = 1 - F(x)}.
#' When `qf_fun` is supplied, the CDF is derived via interpolation and then
#' \eqn{S(x) = 1 - F(x)}.
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A survival function \eqn{S(x)} returning values between 0 and 1
#'   (e.g. `function(x) 1 - pnorm(x)`). Evaluated directly. Exactly one of
#'   `fun`, `cdf_fun`, `pdf_fun`, or `qf_fun` must be provided.
#' @param cdf_fun A CDF function (e.g. [pnorm]). The survival function is
#'   computed as `1 - cdf_fun(x)`. Exactly one of `fun`, `cdf_fun`, `pdf_fun`,
#'   or `qf_fun` must be provided.
#' @param pdf_fun A PDF function (e.g. [dnorm]). The CDF is derived by
#'   numerical integration and the survival function is computed as
#'   `1 - F(x)`. Exactly one of `fun`, `cdf_fun`, `pdf_fun`, or `qf_fun`
#'   must be provided.
#' @param qf_fun A quantile function (e.g. [qnorm]). The CDF is derived via
#'   interpolation and the survival function is computed as `1 - F(x)`.
#'   Exactly one of `fun`, `cdf_fun`, `pdf_fun`, or `qf_fun` must be provided.
#' @param n Number of points at which to evaluate. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`,
#'   `cdf_fun`, `pdf_fun`, or `qf_fun`.
#' @param xlim A numeric vector of length 2 giving the x-range.
#' @param support A numeric vector of length 2 giving the computational support
#'   of the distribution. Defaults to `c(-Inf, Inf)`. It is used when deriving
#'   survival values from a PDF or quantile function.
#' @param color Line color for the survival curve.
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
    xlim = NULL,
    support = c(-Inf, Inf),
    n = 101,
    args = list(),
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
    geom = GeomSurvival,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      qf_fun = qf_fun,
      n = n,
      xlim = xlim,
      support = support,
      args = args,
      na.rm = na.rm,
      color = color,
      ...
    )
  )
}

#' @rdname geom_survival
#' @export
StatSurvival <- ggproto("StatSurvival", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           pdf_fun = NULL, qf_fun = NULL,
                           xlim = NULL, support = c(-Inf, Inf),
                           n = 101, args = NULL) {

    # Validate: exactly one source
    n_provided <- (!is.null(fun)) + (!is.null(cdf_fun)) + (!is.null(pdf_fun)) +
      (!is.null(qf_fun))
    if (n_provided == 0L) {
      cli::cli_abort("One of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, or {.arg qf_fun} must be provided.")
    }
    if (n_provided > 1L) {
      cli::cli_abort("Supply only one of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, or {.arg qf_fun}.")
    }

    range <- if (is.null(scales$x)) {
      xlim %||% c(0, 1)
    } else {
      xlim %||% scales$x$dimension()
    }

    xseq <- seq(range[1], range[2], length.out = n)

    fun_injected <- as_survival_1d(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      qf_fun = qf_fun,
      args = args,
      support = support
    )
    y_out <- fun_injected(xseq)

    if (length(y_out) > 1 && any(diff(y_out) > 0, na.rm = TRUE)) {
      cli::cli_warn(c(
        "The resulting survival function is not monotonically non-increasing.",
        "i" = "Check the function supplied to {.arg fun}, {.arg cdf_fun}, or {.arg pdf_fun}."
      ))
    }

    data.frame(x = xseq, y = y_out)
  }
)

#' @rdname geom_survival
#' @export
GeomSurvival <- ggproto("GeomSurvival", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE
                        ) {

    ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )
  }
)
