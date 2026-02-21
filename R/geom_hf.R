#' Plot a Hazard Function h(x) = f(x) / S(x)
#'
#' `geom_hf()` creates a ggplot2 layer that plots a hazard function,
#' computed as \eqn{h(x) = f(x) / (1 - F(x))} where \eqn{f(x)} is the PDF
#' and \eqn{F(x)} is the CDF. By default only the line is drawn (no fill).
#'
#' @inheritParams ggplot2::geom_function
#' @param pdf_fun A PDF function (e.g. [dnorm]).
#' @param cdf_fun A CDF function (e.g. [pnorm]).
#' @param n Number of points at which to evaluate. Defaults to 101.
#' @param args A named list of arguments shared by both `pdf_fun` and `cdf_fun`.
#' @param pdf_args A named list of additional arguments specific to `pdf_fun` (overrides `args`).
#' @param cdf_args A named list of additional arguments specific to `cdf_fun` (overrides `args`).
#' @param xlim A numeric vector of length 2 giving the x-range.
#' @param color Line color for the hazard curve.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#'   ggplot() +
#'     geom_hf(pdf_fun = dnorm, cdf_fun = pnorm, xlim = c(-3, 3))
#'
#'   ggplot() +
#'     geom_hf(pdf_fun = dexp, cdf_fun = pexp,
#'       args = list(rate = 0.5), xlim = c(0, 10))
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
    pdf_fun,
    cdf_fun,
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
      pdf_fun = pdf_fun,
      cdf_fun = cdf_fun,
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

  compute_group = function(data, scales, pdf_fun, cdf_fun, xlim = NULL, n = 101,
                           args = NULL, pdf_args = NULL, cdf_args = NULL) {

    range <- if (is.null(scales$x)) {
      xlim %||% c(0, 1)
    } else {
      xlim %||% scales$x$dimension()
    }

    # Merge shared args with specific overrides
    pdf_a <- if (!is.null(pdf_args)) modifyList(args, pdf_args) else args
    cdf_a <- if (!is.null(cdf_args)) modifyList(args, cdf_args) else args

    pdf_injected <- function(x) {
      rlang::inject(pdf_fun(x, !!!pdf_a))
    }

    cdf_injected <- function(x) {
      rlang::inject(cdf_fun(x, !!!cdf_a))
    }

    xseq <- seq(range[1], range[2], length.out = n)
    f_vals <- pdf_injected(xseq)
    F_vals <- cdf_injected(xseq)
    S_vals <- 1 - F_vals

    # Compute hazard, guarding division by zero
    y_out <- ifelse(S_vals > 0, f_vals / S_vals, NaN)

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
