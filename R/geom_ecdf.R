#' @importFrom ggplot2 GeomRibbon
NULL

# ── Shared helpers ─────────────────────────────────────────────────────────────

# Tabulate a sample into unique values, empirical PMF, and empirical CDF.
# Returns data.frame(x, pmf, cdf, n) with one row per distinct value.
.tabulate_empirical <- function(x, na.rm) {
  keep <- is.finite(x)
  n_removed <- sum(!keep)
  if (n_removed > 0L && !na.rm) {
    cli::cli_warn(c(
      "Removed {n_removed} non-finite observation{?s}.",
      "i" = "Set {.arg na.rm = TRUE} to suppress this warning."
    ))
  }
  x <- x[keep]
  x <- sort(x)
  n <- length(x)
  if (n == 0L) return(data.frame(x = numeric(0), pmf = numeric(0), cdf = numeric(0), n = integer(0)))
  x_vals   <- unique(x)
  pmf_vals <- tabulate(match(x, x_vals)) / n
  data.frame(x = x_vals, pmf = pmf_vals, cdf = cumsum(pmf_vals), n = n)
}

# Expand (x, ymin, ymax) rows into a right-continuous step-ribbon shape.
# Each jump point is duplicated so the ribbon holds the previous level until
# the step, then jumps to the new level.
.expand_step_ribbon <- function(df) {
  m <- nrow(df)
  if (m <= 1L) return(df)
  x_out    <- c(df$x[1L], rep(df$x[-1L], each = 2L))
  ymin_out <- c(rep(df$ymin[-m], each = 2L), df$ymin[m])
  ymax_out <- c(rep(df$ymax[-m], each = 2L), df$ymax[m])
  data.frame(x = x_out, ymin = ymin_out, ymax = ymax_out)
}

# ── geom_ecdf ─────────────────────────────────────────────────────────────────

#' Plot an Empirical Cumulative Distribution Function
#'
#' `geom_ecdf()` computes the empirical CDF of a sample and renders it as a
#' right-continuous step function, using the same visual conventions as
#' [geom_cdf_discrete()]: horizontal segments, dashed vertical jumps, open
#' circles at the left limit of each jump, and closed circles at the achieved
#' value. An optional simultaneous confidence band (defaulting to 95%) is drawn
#' around the step function using the Dvoretzky--Kiefer--Wolfowitz (DKW)
#' inequality with Massart's sharp constant.
#'
#' The empirical distribution places mass \eqn{c_k / n} at each distinct
#' observed value \eqn{x_k}, where \eqn{c_k} is the count of occurrences and
#' \eqn{n} is the total sample size. Ties are handled correctly.
#'
#' The simultaneous confidence band is the DKW/Massart band.
#' The half-width is \eqn{\varepsilon = \sqrt{\log(2/\alpha) / (2n)}}, where
#' \eqn{\alpha = 1 - \texttt{level}}, giving pointwise bounds
#' \eqn{[\hat{F}_n(x) - \varepsilon,\, \hat{F}_n(x) + \varepsilon]} clipped to
#' \eqn{[0, 1]}.
#'
#' @inheritParams ggplot2::geom_path
#' @param na.rm If `TRUE`, silently remove missing values. Defaults to `FALSE`.
#' @param open_fill Fill color for the open (hollow) endpoint circles. Defaults
#'   to `NULL`, which uses the active theme's panel background color.
#' @param vert_type Line type for the vertical jump segments. Defaults to
#'   `"dashed"`.
#' @param show_points Logical. If `FALSE`, suppresses all endpoint circles.
#'   If `NULL` (the default), circles are shown when there are 50 or fewer
#'   points and hidden otherwise.
#' @param show_vert Logical. If `FALSE`, suppresses the vertical jump segments.
#'   If `NULL` (the default), segments are shown when there are 50 or fewer
#'   points and hidden otherwise.
#' @param conf_int Logical. If `TRUE` (the default), draws a simultaneous DKW
#'   confidence band around the ECDF.
#' @param level Confidence level for the band. Defaults to `0.95`.
#' @param conf_alpha Alpha (transparency) of the confidence ribbon. Defaults
#'   to `0.4`.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Distinct observed sample values.}
#'   \item{`after_stat(y)`}{Empirical cumulative probabilities.}
#'   \item{`after_stat(ymin)` and `after_stat(ymax)`}{Lower and upper confidence
#'   band limits when `conf_int = TRUE`.}
#' }
#'
#' @section Aesthetics:
#' `geom_ecdf()` requires the following aesthetic:
#' \describe{
#'   \item{`x`}{Observed sample values.}
#' }
#' It also understands `alpha`, `colour`/`color`, `fill`, `group`, `linetype`,
#' `linewidth`, `shape`, `size`, and `stroke`.
#'
#' @return A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.
#'
#' @seealso [geom_cdf()] for theoretical CDFs, [geom_eqf()] for empirical
#'   quantile functions, [geom_epmf()] for empirical probability masses, and
#'   [geom_ecdf_km()] for right-censored data.
#'
#' @examples
#' set.seed(1)
#'
#' df <- data.frame(x = rnorm(20))
#' ggplot(df, aes(x = x)) + geom_ecdf()
#'
#' df <- data.frame(x = rnorm(100))
#' ggplot(df, aes(x = x)) + geom_ecdf()
#'
#' # Overlaying multiple groups
#' df2 <- data.frame(
#'   x     = c(rnorm(40), rnorm(40, mean = 2)),
#'   group = rep(c("A", "B"), each = 40)
#' )
#' ggplot(df2, aes(x = x, colour = group)) + geom_ecdf()
#'
#' @name geom_ecdf
#' @aliases StatECDF StatECDFBand
#' @export
geom_ecdf <- function(
    mapping    = NULL,
    data       = NULL,
    stat       = StatECDF,
    position   = "identity",
    ...,
    na.rm      = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    open_fill  = NULL,
    vert_type  = "dashed",
    show_points = NULL,
    show_vert  = NULL,
    conf_int   = TRUE,
    level      = 0.95,
    conf_alpha = 0.4
) {
  default_mapping <- aes(y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  main_layer <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomCDFDiscrete,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm       = na.rm,
      open_fill   = open_fill,
      vert_type   = vert_type,
      show_points = show_points,
      show_vert   = show_vert,
      ...
    )
  )

  if (!conf_int) return(list(main_layer, default_labs_component(y = "p")))

  ribbon_layer <- layer(
    data        = data,
    mapping     = merge_input_mapping(
      mapping, aes(ymin = after_stat(ymin), ymax = after_stat(ymax))
    ),
    stat        = StatECDFBand,
    geom        = GeomRibbon,
    position    = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      level     = level,
      fill      = "grey60",
      linewidth = 0,
      alpha     = conf_alpha
    )
  )

  list(ribbon_layer, main_layer, default_labs_component(y = "p"))
}

#' @rdname geom_ecdf
#' @export
StatECDF <- ggproto("StatECDF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    df <- .tabulate_empirical(data$x, na.rm = na.rm)
    cdf <- df$cdf
    prev <- c(0, cdf[-length(cdf)])
    out <- data.frame(
      x = df$x,
      y = scale_forward(scales$y, cdf),
      cdf = cdf,
      y_prev = scale_forward(scales$y, prev)
    )
    # The mathematical endpoints 0 and 1 train the probability axis when
    # they are finite under the active transformation.
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel
    out$top_panel <- resolve_stat_baseline(scales$y, 1)$panel
    out
  }
)

#' @rdname geom_ecdf
#' @export
StatECDFBand <- ggproto("StatECDFBand", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95) {
    tab <- .tabulate_empirical(data$x, na.rm = na.rm)
    if (nrow(tab) == 0L) return(data.frame())
    n   <- tab$n[1L]
    eps <- sqrt(log(2 / (1 - level)) / (2 * n))

    cdf <- c(0, tab$cdf)
    df  <- data.frame(
      x    = c(-Inf, tab$x),
      ymin = pmax(0, cdf - eps),
      ymax = pmin(1, cdf + eps)
    )
    band <- .expand_step_ribbon(df)
    rbind(
      band,
      data.frame(
        x = Inf,
        ymin = df$ymin[nrow(df)],
        ymax = df$ymax[nrow(df)]
      )
    )
  }
)

# ── geom_eqf ──────────────────────────────────────────────────────────────────

#' Plot an Empirical Quantile Function
#'
#' `geom_eqf()` computes the empirical quantile function of a sample and renders
#' it as a left-continuous step function on \eqn{[0, 1]}, using the same visual
#' conventions as [geom_qf_discrete()]: horizontal segments, dashed vertical
#' jumps, closed circles at the bottom of each jump (value achieved), and open
#' circles at the top (next value not yet reached). An optional simultaneous
#' confidence band is drawn by inverting the DKW/Massart ECDF band.
#'
#' The empirical quantile function is the left-continuous inverse of the
#' empirical CDF: \eqn{Q(p) = \inf\{x : F_n(x) \geq p\}}.
#'
#' The two-sided confidence band at probability level \eqn{p} is
#' \eqn{[Q_n(p - \varepsilon),\, Q_n(p + \varepsilon)]}, where
#' \eqn{\varepsilon = \sqrt{\log(2/\alpha) / (2n)}} is the DKW/Massart half-width
#' (\eqn{\alpha = 1 - \texttt{level}}). In the extreme tails, DKW gives only
#' one-sided bounds unless known support bounds are supplied; the ribbon displays
#' these as open-ended, panel-clipped tails. This follows directly from inverting
#' the simultaneous ECDF confidence band.
#'
#' @inheritParams geom_ecdf
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Empirical cumulative probabilities.}
#'   \item{`after_stat(y)`}{Observed sample values.}
#'   \item{`after_stat(ymin)` and `after_stat(ymax)`}{Lower and upper confidence
#'   band limits when `conf_int = TRUE`.}
#' }
#'
#' @section Aesthetics:
#' `geom_eqf()` requires the following aesthetic:
#' \describe{
#'   \item{`x`}{Observed sample values.}
#' }
#' It also understands `alpha`, `colour`/`color`, `fill`, `group`, `linetype`,
#' `linewidth`, `shape`, `size`, and `stroke`.
#'
#' @return A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.
#'
#' @seealso [geom_qf()] and [geom_qf_discrete()] for theoretical quantile
#'   functions, and [geom_ecdf()] for empirical CDFs.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(50))
#'
#' ggplot(df, aes(x = x)) + geom_eqf()
#'
#' # Compare two groups
#' df2 <- data.frame(
#'   x     = c(rnorm(40), rnorm(40, mean = 2)),
#'   group = rep(c("A", "B"), each = 40)
#' )
#' ggplot(df2, aes(x = x, colour = group)) + geom_eqf()
#'
#' @name geom_eqf
#' @aliases StatEQF StatEQFBand
#' @export
geom_eqf <- function(
    mapping    = NULL,
    data       = NULL,
    stat       = StatEQF,
    position   = "identity",
    ...,
    na.rm      = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    open_fill  = NULL,
    vert_type  = "dashed",
    show_points = NULL,
    show_vert  = NULL,
    conf_int   = TRUE,
    level      = 0.95,
    conf_alpha = 0.4
) {
  default_mapping <- aes(y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  main_layer <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomQFDiscrete,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm       = na.rm,
      open_fill   = open_fill,
      vert_type   = vert_type,
      show_points = show_points,
      show_vert   = show_vert,
      ...
    )
  )

  if (!conf_int) {
    return(list(
      main_layer,
      probability_axis_anchor(),
      default_labs_component(x = "p", y = "x")
    ))
  }

  ribbon_layer <- layer(
    data        = data,
    mapping     = merge_input_mapping(
      mapping, aes(ymin = after_stat(ymin), ymax = after_stat(ymax))
    ),
    stat        = StatEQFBand,
    geom        = GeomRibbon,
    position    = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      level     = level,
      fill      = "grey60",
      linewidth = 0,
      alpha     = conf_alpha
    )
  )

  list(
    ribbon_layer,
    main_layer,
    probability_axis_anchor(),
    default_labs_component(x = "p", y = "x")
  )
}

#' @rdname geom_eqf
#' @export
StatEQF <- ggproto("StatEQF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    df <- .tabulate_empirical(data$x, na.rm = na.rm)
    # Swap axes: x = cumulative probability, y = observed value
    data.frame(x = df$cdf, y = df$x)
  }
)

#' @rdname geom_eqf
#' @export
StatEQFBand <- ggproto("StatEQFBand", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95) {
    tab <- .tabulate_empirical(data$x, na.rm = na.rm)
    if (nrow(tab) == 0L) return(data.frame())
    n      <- tab$n[1L]
    eps    <- sqrt(log(2 / (1 - level)) / (2 * n))
    p_vals <- tab$cdf
    x_vals <- tab$x

    # Q_n(p) lookup: smallest x_k such that F_n(x_k) >= p
    qn <- function(p_query) {
      p_query <- pmax(0, pmin(1, p_query))
      vapply(p_query, function(p) {
        k <- which(p_vals >= p)
        if (length(k) == 0L) x_vals[length(x_vals)] else x_vals[k[1L]]
      }, numeric(1L))
    }

    p_breaks <- sort(unique(c(
      0, 1,
      p_vals,
      pmax(0, p_vals - eps),
      pmin(1, p_vals + eps)
    )))
    p_eval <- c(
      head(p_breaks, -1L) + diff(p_breaks) / 2,
      1
    )

    lower_query <- p_eval - eps
    upper_query <- p_eval + eps

    df <- data.frame(
      x    = p_breaks,
      ymin = ifelse(lower_query <= 0, -Inf, qn(lower_query)),
      ymax = ifelse(upper_query >= 1, Inf, qn(upper_query))
    )
    .expand_step_ribbon(df)
  }
)

# ── geom_epmf ─────────────────────────────────────────────────────────────────

#' Plot an Empirical Probability Mass Function
#'
#' `geom_epmf()` computes the empirical PMF of a sample and renders it as a
#' lollipop chart, using the same visual conventions as [geom_pmf()]: vertical
#' segments from zero to each probability value, capped with points.
#'
#' The empirical distribution places mass \eqn{c_k / n} at each distinct
#' observed value \eqn{x_k}, where \eqn{c_k} is the count of occurrences and
#' \eqn{n} is the total sample size. Ties are handled correctly.
#'
#' @inheritParams geom_ecdf
#' @param point_size Size of the points at the top of each lollipop. Defaults
#'   to `2.5`.
#' @param stick_linewidth Linewidth of the vertical segments. Defaults to
#'   `0.25`.
#' @param stick_linetype Linetype of the vertical segments. Defaults to
#'   `"dashed"`.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Distinct observed sample values.}
#'   \item{`after_stat(y)`}{Empirical probability mass at each value.}
#' }
#'
#' @section Aesthetics:
#' `geom_epmf()` requires the following aesthetic:
#' \describe{
#'   \item{`x`}{Observed sample values.}
#' }
#' It also understands `alpha`, `colour`/`color`, `fill`, `group`, `linetype`,
#' `linewidth`, `shape`, `size`, and `stroke`.
#'
#' @return A list of two ggplot2 layers: a blank layer that anchors the y-axis
#'   at 0 and the main lollipop layer.
#'
#' @seealso [geom_pmf()] for theoretical PMFs, [geom_ecdf()] for empirical CDFs,
#'   and [geom_eqf()] for empirical quantile functions.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(50))
#'
#' ggplot(df, aes(x = x)) + geom_epmf()
#'
#' # Overlaying multiple groups
#' df2 <- data.frame(
#'   x     = c(rnorm(40), rnorm(40, mean = 2)),
#'   group = rep(c("A", "B"), each = 40)
#' )
#' ggplot(df2, aes(x = x, colour = group)) + geom_epmf()
#'
#' @name geom_epmf
#' @aliases StatEPMF
#' @export
geom_epmf <- function(
    mapping         = NULL,
    data            = NULL,
    stat            = StatEPMF,
    position        = "identity",
    ...,
    na.rm           = FALSE,
    show.legend     = NA,
    inherit.aes     = TRUE,
    point_size      = 2.5,
    stick_linewidth = 0.25,
    stick_linetype  = "dashed"
) {
  default_mapping <- aes(y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  main_layer <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPMF,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm           = na.rm,
      point_size      = point_size,
      stick_linewidth = stick_linewidth,
      stick_linetype  = stick_linetype,
      ...
    )
  )

  main_layer
}

#' @rdname geom_epmf
#' @export
StatEPMF <- ggproto("StatEPMF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    df <- .tabulate_empirical(data$x, na.rm = na.rm)
    out <- data.frame(
      x = df$x,
      y = scale_forward(scales$y, df$pmf),
      mass = df$pmf
    )
    # The mass baseline trains the y scale when it is finite under the
    # active transformation; lollipop sticks drop to it at draw time.
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel
    out
  }
)

# ── geom_echf ─────────────────────────────────────────────────────────────────

#' Plot an Empirical Cumulative Hazard Function
#'
#' `geom_echf()` computes the empirical cumulative hazard function of a sample
#' and renders it as a right-continuous step function. The cumulative hazard is
#' obtained by transforming the empirical CDF:
#' \eqn{\hat{H}_n(x) = -\log(1 - \hat{F}_n(x))}. An optional simultaneous
#' confidence band (defaulting to 95%) is drawn using a monotone transformation
#' of the DKW inequality.
#'
#' The final observation, where \eqn{\hat{F}_n(x) = 1} and
#' \eqn{\hat{H}_n(x) = \infty}, is dropped so the step function extends to the
#' right panel edge at the last finite value.
#'
#' The simultaneous confidence band transforms the DKW bounds on the CDF to the
#' cumulative hazard scale. The half-width on the CDF scale is
#' \eqn{\varepsilon = \sqrt{\log(2/\alpha) / (2n)}}, where
#' \eqn{\alpha = 1 - \texttt{level}}. The CDF bounds
#' \eqn{[\hat{F}_n(x) - \varepsilon,\, \hat{F}_n(x) + \varepsilon]} are
#' clipped to \eqn{[0, 1 - 1/(2n)]} and transformed via
#' \eqn{H = -\log(1 - F)} to give the band on the cumulative hazard scale.
#' This caps the displayed upper band at \eqn{\log(2n)}, since the ECDF has
#' resolution \eqn{1/n} and CDF values closer to 1 than \eqn{1/(2n)} are
#' below the estimator's resolution. When clipping occurs, an informational
#' message is emitted.
#'
#' @inheritParams geom_ecdf
#' @param band_max Maximum value of \eqn{H} for the upper confidence band.
#'   Defaults to `NULL`, which clips at \eqn{\log(2n)} and emits an
#'   informational message. Set to `Inf` to disable clipping entirely.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Distinct observed sample values except the final
#'   value when the empirical cumulative hazard would be infinite.}
#'   \item{`after_stat(y)`}{Empirical cumulative hazard values.}
#'   \item{`after_stat(ymin)` and `after_stat(ymax)`}{Lower and upper confidence
#'   band limits when `conf_int = TRUE`.}
#' }
#'
#' @section Aesthetics:
#' `geom_echf()` requires the following aesthetic:
#' \describe{
#'   \item{`x`}{Observed sample values.}
#' }
#' It also understands `alpha`, `colour`/`color`, `fill`, `group`, `linetype`,
#' `linewidth`, `shape`, `size`, and `stroke`.
#'
#' @return A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.
#'
#' @seealso [geom_chf()] for theoretical cumulative hazard functions,
#'   [geom_ecdf()] for empirical CDFs, and [geom_echf_na()] for censored data.
#'
#' @examples
#' set.seed(1)
#'
#' df <- data.frame(x = rexp(20))
#' ggplot(df, aes(x = x)) + geom_echf()
#' ggplot(df, aes(x = x)) + geom_echf(band_max = Inf)
#'
#' df <- data.frame(x = rexp(100))
#' ggplot(df, aes(x = x)) + geom_echf()
#'
#' # Overlaying multiple groups
#' df2 <- data.frame(
#'   x     = c(rexp(40, rate = 1), rexp(40, rate = 0.5)),
#'   group = rep(c("A", "B"), each = 40)
#' )
#' ggplot(df2, aes(x = x, colour = group)) + geom_echf()
#'
#' @name geom_echf
#' @aliases StatECHF StatECHFBand
#' @export
geom_echf <- function(
    mapping    = NULL,
    data       = NULL,
    stat       = StatECHF,
    position   = "identity",
    ...,
    na.rm      = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    open_fill  = NULL,
    vert_type  = "dashed",
    show_points = NULL,
    show_vert  = NULL,
    conf_int   = TRUE,
    level      = 0.95,
    conf_alpha = 0.4,
    band_max   = NULL
) {
  default_mapping <- aes(y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  main_layer <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomCDFDiscrete,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm       = na.rm,
      open_fill   = open_fill,
      vert_type   = vert_type,
      show_points = show_points,
      show_vert   = show_vert,
      ...
    )
  )

  if (!conf_int) return(main_layer)

  ribbon_layer <- layer(
    data        = data,
    mapping     = merge_input_mapping(
      mapping, aes(ymin = after_stat(ymin), ymax = after_stat(ymax))
    ),
    stat        = StatECHFBand,
    geom        = GeomECHFBandRibbon,
    position    = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      level     = level,
      band_max  = band_max,
      fill      = "grey60",
      linewidth = 0,
      alpha     = conf_alpha
    )
  )

  list(ribbon_layer, main_layer)
}

#' @rdname geom_echf
#' @export
StatECHF <- ggproto("StatECHF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    df <- .tabulate_empirical(data$x, na.rm = na.rm)
    if (nrow(df) == 0L) return(data.frame(x = numeric(0), y = numeric(0)))
    # Drop the last row: F_n(x_(n)) = 1, so H = -log(0) = Inf.
    # Floating-point cumsum may yield 1 - eps instead of exactly 1,
    # producing a huge but finite H; dropping avoids this artefact.
    df <- df[-nrow(df), , drop = FALSE]
    if (nrow(df) == 0L) return(data.frame(x = numeric(0), y = numeric(0)))
    h <- -log1p(-df$cdf)
    prev <- c(0, h[-length(h)])
    out <- data.frame(
      x = df$x,
      y = scale_forward(scales$y, h),
      cumhazard = h,
      y_prev = scale_forward(scales$y, prev)
    )
    # The cumulative-hazard baseline trains on raw zero when the transform
    # allows it; no artificial upper endpoint is forced.
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel
    out
  }
)

#' Validate the ECHF band cap: NULL (default log(2n) cap), Inf (genuinely
#' unbounded), or one finite non-negative scalar.
#' @noRd
validate_band_max <- function(band_max) {
  if (is.null(band_max)) return(NULL)
  if (!is.numeric(band_max) || length(band_max) != 1L || is.na(band_max) ||
      band_max < 0 || is.nan(band_max)) {
    cli::cli_abort("{.arg band_max} must be NULL, Inf, or a single finite non-negative number.")
  }
  band_max
}

#' @rdname geom_echf
#' @export
StatECHFBand <- ggproto("StatECHFBand", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95,
                           band_max = NULL) {
    validate_band_max(band_max)
    tab <- .tabulate_empirical(data$x, na.rm = na.rm)
    if (nrow(tab) == 0L) return(data.frame())
    n   <- tab$n[1L]
    eps <- sqrt(log(2 / (1 - level)) / (2 * n))
    cdf_lower <- pmax(0, tab$cdf - eps)
    cdf_upper <- tab$cdf + eps
    h_lower <- -log1p(-cdf_lower)
    # Where F_n + eps >= 1 the true upper bound is infinite. The log
    # branch is evaluated only where it is defined.
    h_upper <- rep(Inf, length(cdf_upper))
    defined <- cdf_upper < 1
    h_upper[defined] <- -log1p(-cdf_upper[defined])

    cap <- if (is.null(band_max)) log(2 * n) else band_max
    keep <- rep(TRUE, length(h_lower))
    if (is.finite(cap)) {
      if (is.null(band_max) && any(h_upper > cap)) {
        cli::cli_inform(
          "Upper confidence band clipped at {.val H} = {round(cap, 2)} ({.code log(2n)}); true upper bound is infinite where {.code F_n(x) + eps >= 1}. Set {.arg band_max = Inf} to disable."
        )
      }
      # Intersect the band with [0, cap]: rows whose lower bound exceeds the
      # cap are omitted; retained rows keep ymin <= ymax.
      keep <- h_lower <= cap
      h_upper <- pmin(h_upper, cap)
    }
    df  <- data.frame(
      x    = tab$x[keep],
      ymin = h_lower[keep],
      ymax = h_upper[keep]
    )
    .expand_step_ribbon(df)
  }
)

#' Ribbon that renders infinite upper bounds at the visible panel edge
#' instead of passing non-finite coordinates to grid.
#' @rdname geom_echf
#' @export
GeomECHFBandRibbon <- ggproto("GeomECHFBandRibbon", GeomRibbon,
  draw_group = function(self, data, panel_params, coord, ...) {
    inf_upper <- is.infinite(data$ymax) & data$ymax > 0
    if (any(inf_upper)) {
      data$ymax[inf_upper] <- panel_params$y.range[2]
    }
    ggproto_parent(GeomRibbon, self)$draw_group(data, panel_params, coord, ...)
  }
)
