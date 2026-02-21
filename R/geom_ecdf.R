# ── Shared helper ─────────────────────────────────────────────────────────────

# Tabulate a sample vector into (unique value, empirical CDF) pairs.
# Returns a data.frame(x = unique sorted values, y = cumulative probabilities).
.tabulate_ecdf <- function(x, na.rm) {
  if (na.rm) x <- x[!is.na(x)]
  x <- x[is.finite(x)]
  x <- sort(x)
  n <- length(x)
  if (n == 0L) return(data.frame(x = numeric(0), y = numeric(0)))
  x_vals  <- unique(x)
  pmf_vals <- tabulate(match(x, x_vals)) / n
  data.frame(x = x_vals, y = cumsum(pmf_vals))
}

# ── geom_ecdf ─────────────────────────────────────────────────────────────────

#' Plot an Empirical Cumulative Distribution Function
#'
#' `geom_ecdf()` computes the empirical CDF of a sample and renders it as a
#' right-continuous step function, using the same visual conventions as
#' [geom_cdf_discrete()]: horizontal segments, dashed vertical jumps, open
#' circles at the left limit of each jump, and closed circles at the achieved
#' value.
#'
#' The empirical distribution places mass \eqn{c_k / n} at each distinct
#' observed value \eqn{x_k}, where \eqn{c_k} is the count of occurrences and
#' \eqn{n} is the total sample size. Ties are handled correctly.
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
#'
#' @return A ggplot2 layer.
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
#' @aliases StatECDF
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
    show_vert  = TRUE
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
}

#' @rdname geom_ecdf
#' @export
StatECDF <- ggproto("StatECDF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    .tabulate_ecdf(data$x, na.rm = na.rm)
  }
)

# ── geom_eqf ──────────────────────────────────────────────────────────────────

#' Plot an Empirical Quantile Function
#'
#' `geom_eqf()` computes the empirical quantile function of a sample and renders
#' it as a left-continuous step function on \eqn{[0, 1]}, using the same visual
#' conventions as [geom_qf_discrete()]: horizontal segments, dashed vertical
#' jumps, closed circles at the bottom of each jump (value achieved), and open
#' circles at the top (next value not yet reached).
#'
#' The empirical quantile function is the left-continuous inverse of the
#' empirical CDF: \eqn{Q(p) = \inf\{x : F_n(x) \geq p\}}.
#'
#' @inheritParams geom_ecdf
#'
#' @return A ggplot2 layer.
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
#' @aliases StatEQF
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
    show_vert  = TRUE
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
    stat        = StatEQF,
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
}

#' @rdname geom_eqf
#' @export
StatEQF <- ggproto("StatEQF", Stat,
  required_aes = "x",

  compute_group = function(data, scales, na.rm = FALSE) {
    df <- .tabulate_ecdf(data$x, na.rm = na.rm)
    # Swap axes: x = cumulative probability, y = observed value
    data.frame(x = df$y, y = df$x)
  }
)
