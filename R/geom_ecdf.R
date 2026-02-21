#' @importFrom ggplot2 GeomRibbon
NULL

# ── Shared helpers ─────────────────────────────────────────────────────────────

# Tabulate a sample into unique values, empirical PMF, and empirical CDF.
# Returns data.frame(x, pmf, cdf, n) with one row per distinct value.
.tabulate_empirical <- function(x, na.rm) {
  if (na.rm) x <- x[!is.na(x)]
  x <- x[is.finite(x)]
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
#' around the step function using the Kolmogorov-Smirnov construction.
#'
#' The empirical distribution places mass \eqn{c_k / n} at each distinct
#' observed value \eqn{x_k}, where \eqn{c_k} is the count of occurrences and
#' \eqn{n} is the total sample size. Ties are handled correctly.
#'
#' The simultaneous confidence band inverts the Kolmogorov-Smirnov test.
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
#'   Defaults to `TRUE`.
#' @param show_vert Logical. If `FALSE`, suppresses the vertical jump segments.
#'   Defaults to `TRUE`.
#' @param conf_int Logical. If `TRUE` (the default), draws a simultaneous KS
#'   confidence band around the ECDF.
#' @param level Confidence level for the band. Defaults to `0.95`.
#' @param conf_alpha Alpha (transparency) of the confidence ribbon. Defaults
#'   to `0.3`.
#'
#' @return A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(50))
#'
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
    show_points = TRUE,
    show_vert  = TRUE,
    conf_int   = TRUE,
    level      = 0.95,
    conf_alpha = 0.3
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
    mapping     = aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
    stat        = StatECDFBand,
    geom        = GeomRibbon,
    position    = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      level     = level,
      fill      = "grey70",
      linewidth = 0,
      alpha     = conf_alpha
    )
  )

  list(ribbon_layer, main_layer)
}

#' @rdname geom_ecdf
#' @export
StatECDF <- ggproto("StatECDF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    df <- .tabulate_empirical(data$x, na.rm = na.rm)
    data.frame(x = df$x, y = df$cdf)
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
    df  <- data.frame(
      x    = tab$x,
      ymin = pmax(0, tab$cdf - eps),
      ymax = pmin(1, tab$cdf + eps)
    )
    .expand_step_ribbon(df)
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
#' confidence band is drawn using the Kolmogorov-Smirnov construction.
#'
#' The empirical quantile function is the left-continuous inverse of the
#' empirical CDF: \eqn{Q(p) = \inf\{x : F_n(x) \geq p\}}.
#'
#' The confidence band at probability level \eqn{p} is
#' \eqn{[Q_n(p - \varepsilon),\, Q_n(p + \varepsilon)]}, where
#' \eqn{\varepsilon = \sqrt{\log(2/\alpha) / (2n)}} is the KS half-width
#' (\eqn{\alpha = 1 - \texttt{level}}). This follows directly from inverting
#' the simultaneous ECDF confidence band.
#'
#' @inheritParams geom_ecdf
#'
#' @return A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.
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
    show_points = TRUE,
    show_vert  = TRUE,
    conf_int   = TRUE,
    level      = 0.95,
    conf_alpha = 0.3
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

  if (!conf_int) return(main_layer)

  ribbon_layer <- layer(
    data        = data,
    mapping     = aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
    stat        = StatEQFBand,
    geom        = GeomRibbon,
    position    = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      level     = level,
      fill      = "grey70",
      linewidth = 0,
      alpha     = conf_alpha
    )
  )

  list(ribbon_layer, main_layer)
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

    df <- data.frame(
      x    = p_vals,
      ymin = qn(p_vals - eps),
      ymax = qn(p_vals + eps)
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
#' @param color Color for the points and segments. Defaults to `"black"`.
#'
#' @return A ggplot2 layer.
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
    stick_linetype  = "dashed",
    color           = "black"
) {
  default_mapping <- aes(y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  layer(
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
      colour          = color,
      ...
    )
  )
}

#' @rdname geom_epmf
#' @export
StatEPMF <- ggproto("StatEPMF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    df <- .tabulate_empirical(data$x, na.rm = na.rm)
    data.frame(x = df$x, y = df$pmf)
  }
)
