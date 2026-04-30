#' PP and QQ Diagnostic Plots
#'
#' `geom_ppplot()` and `geom_qqplot()` create one-sample probability-probability
#' and quantile-quantile diagnostic layers for comparing a sample to a fully
#' specified null distribution. Both draw the order-statistic points, an optional
#' identity line, and a simultaneous 95% DKW/Massart confidence band by default.
#'
#' @details
#' Suppose \eqn{X_1, \ldots, X_n} are compared against a fully specified null
#' distribution \eqn{F_0}, with quantile function
#' \eqn{Q_0 = F_0^{\leftarrow}}. Let
#' \eqn{x_{(1)} \le \cdots \le x_{(n)}} denote the sample order statistics and
#' let \eqn{p_i} be the plotting positions returned by [stats::ppoints()], which
#' by default are \eqn{p_i = (i - 0.5) / n}.
#'
#' A probability-probability (PP) plot displays
#' \eqn{F_0(x_{(i)})} against \eqn{p_i}. A quantile-quantile (QQ) plot displays
#' \eqn{x_{(i)}} against \eqn{Q_0(p_i)}. In both cases, agreement with the null
#' model is represented by the identity line \eqn{y = x}. Michael (1983)
#' discusses acceptance regions for PP, QQ, and stabilized probability plots.
#'
#' The confidence band is based on the probability integral transform: under
#' \eqn{H_0: F_X = F_0}, the transformed observations
#' \eqn{U_i = F_0(X_i)} are iid \eqn{\mathrm{Uniform}(0, 1)}. The
#' Dvoretzky--Kiefer--Wolfowitz/Massart inequality gives, with
#' \eqn{\alpha = 1 - \mathrm{level}},
#' \deqn{
#'   \varepsilon_{n,\alpha} =
#'   \sqrt{\frac{\log(2/\alpha)}{2n}} .
#' }
#' Thus the PP band is drawn on the probability scale as
#' \deqn{
#'   \max(0, p - \varepsilon_{n,\alpha})
#'   \le F_0(x_{(i)}) \le
#'   \min(1, p + \varepsilon_{n,\alpha}),
#' }
#' and the QQ band is obtained by transforming these probability limits back to
#' the data scale:
#' \deqn{
#'   Q_0\{\max(0, p - \varepsilon_{n,\alpha})\}
#'   \le x_{(i)} \le
#'   Q_0\{\min(1, p + \varepsilon_{n,\alpha})\}.
#' }
#' Points far from the identity line, and especially points outside the band,
#' are visual evidence against the specified null distribution. The band is
#' finite-sample and distribution-free for a fully specified \eqn{F_0}; if
#' parameters are estimated from the same data, the display is best interpreted
#' as an informal diagnostic. The DKW bands are known to be conservative, so
#' power can be expected to be low.
#'
#' @inheritParams ggplot2::geom_point
#' @param fun Null distribution function. For `geom_ppplot()`, this is a CDF
#'   such as [pnorm]. For `geom_qqplot()`, this is a quantile function such as
#'   [qnorm].
#' @param pdf_fun,cdf_fun,survival_fun,qf_fun,hf_fun Alternate null
#'   distribution representations. `geom_ppplot()` accepts `pdf_fun`,
#'   `survival_fun`, `qf_fun`, or `hf_fun` and converts them to a CDF.
#'   `geom_qqplot()` accepts `cdf_fun`, `pdf_fun`, `survival_fun`, or `hf_fun`
#'   and converts them to a quantile function.
#' @param hf_lower Lower integration limit for `hf_fun`. Defaults to `-Inf`.
#' @param args A named list of additional arguments passed to the supplied
#'   distribution function.
#' @param conf_int Logical. If `TRUE` (the default), draw a simultaneous
#'   DKW/Massart confidence band.
#' @param level Confidence level for the DKW band. Defaults to `0.95`.
#' @param conf_alpha Alpha (transparency) of the confidence ribbon. Defaults
#'   to `0.25`.
#' @param band_n Number of points used to draw the DKW ribbon. Defaults to
#'   `501`.
#' @param a Plotting-position offset passed to [stats::ppoints()]. Defaults to
#'   `1 / 2`, giving \eqn{p_i = (i - 0.5) / n}.
#' @param identity_line Logical. If `TRUE` (the default), draw the reference
#'   line \eqn{y = x}.
#' @param line_color,line_linetype,line_linewidth Appearance of the identity
#'   line.
#' @param shape,size,stroke Optional fixed point appearance. Defaults to `NULL`
#'   so the corresponding [ggplot2::geom_point()] defaults are used.
#' @param color Optional fixed point colour. When `NULL`, PP plots map
#'   `colour` to the sorted sample value and QQ plots map `colour` to the
#'   plotting position.
#'
#' @return A list of ggplot2 layers.
#'
#' @references
#' Michael, J. R. (1983). The stabilized probability plot. *Biometrika*,
#' 70(1), 11--17. \doi{10.1093/biomet/70.1.11};
#' \url{https://www.jstor.org/stable/2335939}.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(50))
#'
#' ggplot(df, aes(x = x)) +
#'   geom_ppplot(fun = pnorm) +
#'   coord_equal()
#'
#' ggplot(df, aes(x = x)) +
#'   geom_qqplot(fun = qnorm) +
#'   coord_equal()
#'
#' # Use fixed black points by setting a fixed colour.
#' ggplot(df, aes(x = x)) +
#'   geom_qqplot(fun = qnorm, colour = "black") +
#'   coord_equal()
#'
#' # Or add a spectral/rainbow colour scale explicitly.
#' ggplot(df, aes(x = x)) +
#'   geom_qqplot(fun = qnorm) +
#'   scale_colour_gradientn(colors = grDevices::rainbow(10)) +
#'   coord_equal()
#'
#'
#' @name geom_ppplot
#' @aliases geom_qqplot StatPPPlot StatPPPlotBand StatQQPlot StatQQPlotBand
#' @importFrom ggplot2 GeomPoint GeomRibbon
#' @export
geom_ppplot <- function(
    mapping = NULL,
    data = NULL,
    stat = StatPPPlot,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun = NULL,
    pdf_fun = NULL,
    survival_fun = NULL,
    qf_fun = NULL,
    hf_fun = NULL,
    hf_lower = -Inf,
    args = list(),
    conf_int = TRUE,
    level = 0.95,
    conf_alpha = 0.25,
    band_n = 501,
    a = 1 / 2,
    identity_line = TRUE,
    line_color = "red",
    line_linetype = "dashed",
    line_linewidth = 0.5,
    shape = NULL,
    size = NULL,
    stroke = NULL,
    color = NULL
) {
  point_aes_params <- normalise_colour_params(list(...))
  has_fixed_colour <- "colour" %in% names(point_aes_params) || !is.null(color)

  default_mapping <- aes(y = after_stat(y))
  point_stat <- stat
  if (!has_fixed_colour) {
    if (inherits(stat, "ggproto")) {
      point_stat <- ggplot2::ggproto(
        NULL, stat,
        default_aes = aes(colour = after_stat(sample))
      )
    } else {
      default_mapping <- modifyList(default_mapping, aes(colour = after_stat(sample)))
    }
  }
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  point_params <- list(
    fun = fun,
    pdf_fun = pdf_fun,
    survival_fun = survival_fun,
    qf_fun = qf_fun,
    hf_fun = hf_fun,
    hf_lower = hf_lower,
    args = args,
    a = a,
    na.rm = na.rm
  )
  if (!is.null(shape)) point_params$shape <- shape
  if (!is.null(size)) point_params$size <- size
  if (!is.null(stroke)) point_params$stroke <- stroke
  point_params <- c(point_params, point_aes_params)
  if (!is.null(color)) point_params$colour <- color

  main_layer <- layer(
    data = data,
    mapping = mapping,
    stat = point_stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = point_params
  )

  layers <- list()

  if (conf_int) {
    layers <- c(layers, list(layer(
      data = data,
      mapping = aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
      stat = StatPPPlotBand,
      geom = GeomRibbon,
      position = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        level = level,
        band_n = band_n,
        a = a,
        fill = "grey70",
        linewidth = 0,
        alpha = conf_alpha
      )
    )))
  }

  if (identity_line) {
    layers <- c(layers, list(ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      colour = line_color,
      linetype = line_linetype,
      linewidth = line_linewidth,
      show.legend = FALSE
    )))
  }

  layers <- c(layers, list(main_layer))

  c(layers, list(default_labs_component(
    x = "Theoretical probabilities",
    y = "Observed probabilities",
    colour = if (has_fixed_colour) NULL else "x"
  )))
}

#' @rdname geom_ppplot
#' @export
geom_qqplot <- function(
    mapping = NULL,
    data = NULL,
    stat = StatQQPlot,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun = NULL,
    cdf_fun = NULL,
    pdf_fun = NULL,
    survival_fun = NULL,
    hf_fun = NULL,
    hf_lower = -Inf,
    args = list(),
    conf_int = TRUE,
    level = 0.95,
    conf_alpha = 0.25,
    band_n = 501,
    a = 1 / 2,
    identity_line = TRUE,
    line_color = "red",
    line_linetype = "dashed",
    line_linewidth = 0.5,
    shape = NULL,
    size = NULL,
    stroke = NULL,
    color = NULL
) {
  point_aes_params <- normalise_colour_params(list(...))
  has_fixed_colour <- "colour" %in% names(point_aes_params) || !is.null(color)

  default_mapping <- aes(y = after_stat(y))
  point_stat <- stat
  if (!has_fixed_colour) {
    if (inherits(stat, "ggproto")) {
      point_stat <- ggplot2::ggproto(
        NULL, stat,
        default_aes = aes(colour = after_stat(p))
      )
    } else {
      default_mapping <- modifyList(default_mapping, aes(colour = after_stat(p)))
    }
  }
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  point_params <- list(
    fun = fun,
    cdf_fun = cdf_fun,
    pdf_fun = pdf_fun,
    survival_fun = survival_fun,
    hf_fun = hf_fun,
    hf_lower = hf_lower,
    args = args,
    a = a,
    na.rm = na.rm
  )
  if (!is.null(shape)) point_params$shape <- shape
  if (!is.null(size)) point_params$size <- size
  if (!is.null(stroke)) point_params$stroke <- stroke
  point_params <- c(point_params, point_aes_params)
  if (!is.null(color)) point_params$colour <- color

  main_layer <- layer(
    data = data,
    mapping = mapping,
    stat = point_stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = point_params
  )

  layers <- list()

  if (conf_int) {
    layers <- c(layers, list(layer(
      data = data,
      mapping = aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
      stat = StatQQPlotBand,
      geom = GeomRibbon,
      position = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        fun = fun,
        cdf_fun = cdf_fun,
        pdf_fun = pdf_fun,
        survival_fun = survival_fun,
        hf_fun = hf_fun,
        hf_lower = hf_lower,
        args = args,
        na.rm = na.rm,
        level = level,
        band_n = band_n,
        a = a,
        fill = "grey70",
        linewidth = 0,
        alpha = conf_alpha
      )
    )))
  }

  if (identity_line) {
    layers <- c(layers, list(ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      colour = line_color,
      linetype = line_linetype,
      linewidth = line_linewidth,
      show.legend = FALSE
    )))
  }

  layers <- c(layers, list(main_layer))

  c(layers, list(default_labs_component(
    x = "Theoretical quantiles",
    y = "Observed quantiles",
    colour = if (has_fixed_colour) NULL else "p"
  )))
}

#' @rdname geom_ppplot
#' @export
StatPPPlot <- ggproto("StatPPPlot", Stat,
  required_aes = "x",

  compute_group = function(data, scales, fun = NULL, pdf_fun = NULL,
                           survival_fun = NULL, qf_fun = NULL, hf_fun = NULL,
                           hf_lower = -Inf, args = NULL, a = 1 / 2,
                           na.rm = FALSE) {
    check_cdf_sources(fun, pdf_fun, survival_fun, qf_fun, hf_fun)

    ord <- order_stat_sample(data$x, na.rm = na.rm, a = a)
    if (nrow(ord) == 0L) return(data.frame())

    cdf_fun <- make_cdf_function(
      fun = fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      qf_fun = qf_fun,
      hf_fun = hf_fun,
      hf_lower = hf_lower,
      args = args
    )
    observed <- cdf_fun(ord$sample)

    data.frame(
      x = ord$p,
      y = observed,
      p = ord$p,
      theoretical = ord$p,
      observed = observed,
      sample = ord$sample,
      n = ord$n
    )
  }
)

#' @rdname geom_ppplot
#' @export
StatPPPlotBand <- ggproto("StatPPPlotBand", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95,
                           band_n = 501, a = 1 / 2) {
    ord <- order_stat_sample(data$x, na.rm = na.rm, a = a)
    if (nrow(ord) == 0L) return(data.frame())

    n <- ord$n[1L]
    eps <- sqrt(log(2 / (1 - level)) / (2 * n))
    p_grid <- seq(0, 1, length.out = validate_ppqq_band_n(band_n))

    data.frame(
      x = p_grid,
      p = p_grid,
      ymin = pmax(0, p_grid - eps),
      ymax = pmin(1, p_grid + eps),
      n = n
    )
  }
)

#' @rdname geom_ppplot
#' @export
StatQQPlot <- ggproto("StatQQPlot", Stat,
  required_aes = "x",

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           pdf_fun = NULL, survival_fun = NULL, hf_fun = NULL,
                           hf_lower = -Inf, args = NULL, a = 1 / 2,
                           na.rm = FALSE) {
    qf_fun <- make_qf_function(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      hf_fun = hf_fun,
      hf_lower = hf_lower,
      args = args
    )

    ord <- order_stat_sample(data$x, na.rm = na.rm, a = a)
    if (nrow(ord) == 0L) return(data.frame())

    theoretical <- qf_fun(ord$p)

    data.frame(
      x = theoretical,
      y = ord$sample,
      p = ord$p,
      theoretical = theoretical,
      observed = ord$sample,
      sample = ord$sample,
      n = ord$n
    )
  }
)

#' @rdname geom_ppplot
#' @export
StatQQPlotBand <- ggproto("StatQQPlotBand", Stat,
  required_aes = "x",

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           pdf_fun = NULL, survival_fun = NULL, hf_fun = NULL,
                           hf_lower = -Inf, args = NULL, na.rm = FALSE,
                           level = 0.95, band_n = 501, a = 1 / 2) {
    qf_fun <- make_qf_function(
      fun = fun,
      cdf_fun = cdf_fun,
      pdf_fun = pdf_fun,
      survival_fun = survival_fun,
      hf_fun = hf_fun,
      hf_lower = hf_lower,
      args = args
    )

    ord <- order_stat_sample(data$x, na.rm = na.rm, a = a)
    if (nrow(ord) == 0L) return(data.frame())

    n <- ord$n[1L]
    eps <- sqrt(log(2 / (1 - level)) / (2 * n))
    p_grid <- seq(min(ord$p), max(ord$p), length.out = validate_ppqq_band_n(band_n))

    data.frame(
      x = qf_fun(p_grid),
      p = p_grid,
      ymin = qf_fun(pmax(0, p_grid - eps)),
      ymax = qf_fun(pmin(1, p_grid + eps)),
      n = n
    )
  }
)

#' @noRd
validate_ppqq_band_n <- function(band_n) {
  if (!is.numeric(band_n) || length(band_n) != 1L ||
      is.na(band_n) || band_n < 2) {
    cli::cli_abort("{.arg band_n} must be a single number greater than or equal to 2.")
  }
  as.integer(band_n)
}
