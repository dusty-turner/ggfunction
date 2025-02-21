#' Plot a Quantile Function
#'
#' `geom_qf()` creates a ggplot2 layer that plots a quantile function (inverse CDF)
#' as a line. It computes quantile values for a sequence of probabilities (from 0 to 1)
#' and connects them with a line.
#'
#' @inheritParams ggplot2::geom_function
#' @param fun A function to compute the quantile function (e.g. [qnorm]). The function must
#'   accept a numeric vector of probabilities (values in `[0,1]`) as its first argument.
#' @param n Number of probability points at which to evaluate `fun`. Defaults to 101.
#' @param args A named list of additional arguments to pass to `fun`.
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
                    fun,
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

  compute_group = function(data, scales, fun, n = 101, args = NULL, ...) {

    p_vals <- seq(0, 1, length.out = n)

    fun_injected <- function(p) {
      rlang::inject(fun(p, !!!args))
    }

    q_vals <- fun_injected(p_vals)

    data.frame(p = p_vals, q = q_vals)

  }
)
