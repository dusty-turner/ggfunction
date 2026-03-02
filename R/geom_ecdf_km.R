#' @importFrom ggplot2 GeomRibbon GeomPoint
NULL

# ── Shared helpers for censored data ─────────────────────────────────────────

#' Tabulate Kaplan-Meier / Nelson-Aalen estimates from censored data.
#'
#' Sorts by time, counts events and censorings at each unique time, computes
#' the risk set, then returns KM survival, Nelson-Aalen cumulative hazard,
#' and their variances at every distinct event time.
#'
#' @param time Numeric vector of observed times.
#' @param status Integer/logical vector: 1 = event, 0 = censored.
#' @param na.rm Logical; if `TRUE`, remove NA pairs.
#' @return A `data.frame` with columns `time`, `n_risk`, `n_event`,
#'   `n_censor`, `surv`, `chf`, `var_surv`, `var_chf`, `n`.
#' @noRd
.tabulate_km <- function(time, status, na.rm) {
  if (na.rm) {
    keep <- !is.na(time) & !is.na(status)
    time   <- time[keep]
    status <- status[keep]
  }
  keep <- is.finite(time)
  time   <- time[keep]
  status <- status[keep]

  n <- length(time)
  if (n == 0L) {
    return(data.frame(
      time = numeric(0), n_risk = integer(0), n_event = integer(0),
      n_censor = integer(0), surv = numeric(0), chf = numeric(0),
      var_surv = numeric(0), var_chf = numeric(0), n = integer(0)
    ))
  }

  # Sort by time

  ord    <- order(time)
  time   <- time[ord]
  status <- status[ord]

  # Tabulate at each unique time
  u_times  <- unique(time)
  k        <- length(u_times)
  d_i      <- integer(k)   # events at each time

  c_i      <- integer(k)   # censorings at each time

  for (j in seq_len(k)) {
    idx   <- which(time == u_times[j])
    d_i[j] <- sum(status[idx] == 1L)
    c_i[j] <- sum(status[idx] == 0L)
  }

  # Risk set: n_i = n - cumulative (events + censorings) before time i
  cum_removed <- cumsum(d_i + c_i)
  n_i <- c(n, n - cum_removed[-k])

  # Keep only event times (d_i > 0)
  event_mask <- d_i > 0L
  if (!any(event_mask)) {
    return(data.frame(
      time = numeric(0), n_risk = integer(0), n_event = integer(0),
      n_censor = integer(0), surv = numeric(0), chf = numeric(0),
      var_surv = numeric(0), var_chf = numeric(0), n = integer(0)
    ))
  }

  t_evt <- u_times[event_mask]
  d_evt <- d_i[event_mask]
  n_evt <- n_i[event_mask]
  c_evt <- c_i[event_mask]

  # Kaplan-Meier survival: S(t) = prod(1 - d_j / n_j)
  surv <- cumprod(1 - d_evt / n_evt)

  # Nelson-Aalen cumulative hazard: H(t) = sum(d_j / n_j)
  chf <- cumsum(d_evt / n_evt)

  # Greenwood variance of S(t): Var(S) = S^2 * sum(d_j / (n_j * (n_j - d_j)))

  greenwood_term <- ifelse(
    n_evt == d_evt,
    0,
    d_evt / (n_evt * (n_evt - d_evt))
  )
  var_surv <- surv^2 * cumsum(greenwood_term)

  # Nelson variance of H(t): Var(H) = sum(d_j / n_j^2)
  var_chf <- cumsum(d_evt / n_evt^2)

  data.frame(
    time     = t_evt,
    n_risk   = n_evt,
    n_event  = d_evt,
    n_censor = c_evt,
    surv     = surv,
    chf      = chf,
    var_surv = var_surv,
    var_chf  = var_chf,
    n        = n
  )
}

#' Equal-precision (EP) critical value for simultaneous confidence bands.
#'
#' Computes the critical value for the Nair (1984) EP band by interpolation
#' from a precomputed table.  The table was computed by Monte Carlo simulation
#' (200 000 replications of a stationary Ornstein-Uhlenbeck process, which is
#' the time-changed representation of the standardized KM process).
#'
#' @param a_L Ratio of Greenwood variance at the first event time to the last
#'   event time, clamped to `[0.001, 1]`.  When `a_L = 1`, the EP critical
#'   value reduces to the normal quantile (pointwise band).
#' @param alpha Significance level (1 - confidence level).
#' @return Scalar critical value.
#' @noRd
.ep_critical_value <- function(a_L, alpha) {
  # Precomputed EP table: rows = a values, columns = alpha values.
  # Critical value = (1 - alpha)-quantile of sup_{a <= u <= 1} |W(u)/sqrt(u)|.
  ep_a     <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.10, 0.15, 0.20, 0.25,
                0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 0.99, 1.00)
  ep_alpha <- c(0.01, 0.05, 0.10, 0.20)
  ep_table <- matrix(c(
    # alpha = 0.01
    3.6861, 3.5967, 3.5599, 3.5240, 3.4626, 3.3832, 3.3287, 3.2943,
    3.2646, 3.2278, 3.1686, 3.1092, 3.0380, 2.9697, 2.9014, 2.8086,
    2.7448, 2.6453, 2.5758,
    # alpha = 0.05
    3.1639, 3.0795, 3.0373, 2.9853, 2.9106, 2.8256, 2.7687, 2.7267,
    2.6925, 2.6512, 2.5846, 2.5156, 2.4512, 2.3816, 2.2915, 2.2022,
    2.1354, 2.0343, 1.9600,
    # alpha = 0.10
    2.9046, 2.8177, 2.7687, 2.7126, 2.6309, 2.5455, 2.4833, 2.4351,
    2.3979, 2.3516, 2.2764, 2.2135, 2.1437, 2.0681, 1.9818, 1.8838,
    1.8187, 1.7211, 1.6449,
    # alpha = 0.20
    2.6113, 2.5138, 2.4642, 2.4042, 2.3091, 2.2228, 2.1510, 2.0994,
    2.0556, 2.0124, 1.9325, 1.8635, 1.7899, 1.7118, 1.6274, 1.5258,
    1.4547, 1.3571, 1.2816
  ), nrow = length(ep_a), ncol = length(ep_alpha))

  # Clamp a_L
  a_L <- max(0.001, min(1.0, a_L))

  # Interpolate over alpha (log scale) then over a_L
  # For each bracketing alpha column, interpolate over a_L
  if (alpha <= ep_alpha[1L]) {
    c_vals <- ep_table[, 1L]
  } else if (alpha >= ep_alpha[length(ep_alpha)]) {
    c_vals <- ep_table[, length(ep_alpha)]
  } else {
    j <- findInterval(alpha, ep_alpha)
    w <- (alpha - ep_alpha[j]) / (ep_alpha[j + 1L] - ep_alpha[j])
    c_vals <- (1 - w) * ep_table[, j] + w * ep_table[, j + 1L]
  }

  # Interpolate over a_L
  if (a_L <= ep_a[1L]) return(c_vals[1L])
  if (a_L >= ep_a[length(ep_a)]) return(c_vals[length(ep_a)])
  i <- findInterval(a_L, ep_a)
  w <- (a_L - ep_a[i]) / (ep_a[i + 1L] - ep_a[i])
  (1 - w) * c_vals[i] + w * c_vals[i + 1L]
}


#' Return sorted censoring times for censor-mark rendering.
#'
#' @param time Numeric vector of observed times.
#' @param status Integer/logical vector: 1 = event, 0 = censored.
#' @param na.rm Logical; if `TRUE`, remove NA pairs.
#' @return Sorted numeric vector of times where `status == 0`.
#' @noRd
.censoring_times <- function(time, status, na.rm) {
  if (na.rm) {
    keep <- !is.na(time) & !is.na(status)
    time   <- time[keep]
    status <- status[keep]
  }
  sort(time[status == 0L])
}


# ── geom_ecdf_km ────────────────────────────────────────────────────────────

#' Plot a Kaplan-Meier Survival Curve for Censored Data
#'
#' `geom_ecdf_km()` computes the Kaplan-Meier product-limit survival estimator
#' from right-censored data and renders it as a decreasing step function
#' starting at 1, using the same visual conventions as
#' [geom_survival_discrete()]. An optional simultaneous confidence band
#' (defaulting to 95%) is drawn around the curve using the equal-precision (EP)
#' construction of Nair (1984), and censoring times are marked with "+"
#' symbols by default.
#'
#' The Kaplan-Meier estimator at event time \eqn{t_j} is
#' \deqn{\hat{S}(t) = \prod_{t_j \le t} \left(1 - \frac{d_j}{n_j}\right),}
#' where \eqn{d_j} is the number of events and \eqn{n_j} is the number at risk
#' just before \eqn{t_j}.
#'
#' The simultaneous confidence band uses the Greenwood variance estimator
#' \deqn{\widehat{\mathrm{Var}}[\hat{S}(t)] = \hat{S}(t)^2 \sum_{t_j \le t}
#' \frac{d_j}{n_j(n_j - d_j)}}
#' with the equal-precision (EP) critical value of Nair (1984), giving bounds
#' \eqn{\hat{S}(t) \pm c_{\mathrm{EP}}\,\mathrm{se}(t)} clipped to
#' \eqn{[0, 1]}. The EP critical value \eqn{c_{\mathrm{EP}}} is derived from
#' the asymptotic distribution of the standardized KM process and depends on
#' the ratio of Greenwood variances at the first and last event times. The
#' resulting band is simultaneous (valid at all \eqn{t} jointly), not merely
#' pointwise, and is asymptotically correct.
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
#' @param conf_int Logical. If `TRUE` (the default), draws a simultaneous
#'   EP confidence band around the KM curve.
#' @param level Confidence level for the band. Defaults to `0.95`.
#' @param conf_alpha Alpha (transparency) of the confidence ribbon. Defaults
#'   to `0.4`.
#' @param censor_marks Logical. If `TRUE` (the default), draws "+" marks at
#'   censoring times on the survival curve.
#' @param censor_shape Shape for censoring marks. Defaults to `3` ("+").
#' @param censor_size Size for censoring marks. Defaults to `2`.
#'
#' @section Aesthetics:
#' `geom_ecdf_km()` requires the following aesthetics:
#' \describe{
#'   \item{`x`}{Observed time (event or censoring time).}
#'   \item{`status`}{Event indicator: 1 = event occurred, 0 = censored.}
#' }
#' It also understands `colour`, `alpha`, `linewidth`, and `linetype`.
#'
#' @return A ggplot2 layer, or a list of layers when `conf_int = TRUE` or
#'   `censor_marks = TRUE`.
#'
#' @examples
#' set.seed(42)
#' n <- 50
#' true_time <- rexp(n, rate = 0.5)
#' cens_time <- rexp(n, rate = 0.2)
#' df <- data.frame(
#'   time   = pmin(true_time, cens_time),
#'   status = as.integer(true_time <= cens_time)
#' )
#'
#' ggplot(df, aes(x = time, status = status)) +
#'   geom_ecdf_km()
#'
#' # Without confidence band or censor marks
#' ggplot(df, aes(x = time, status = status)) +
#'   geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
#'
#' # Grouped data
#' df2 <- data.frame(
#'   time   = c(rexp(40, 0.5), rexp(40, 1)),
#'   status = sample(0:1, 80, replace = TRUE, prob = c(0.2, 0.8)),
#'   group  = rep(c("A", "B"), each = 40)
#' )
#' ggplot(df2, aes(x = time, status = status, colour = group)) +
#'   geom_ecdf_km()
#'
#' @seealso [geom_ecdf()] for complete (uncensored) data,
#'   [geom_survival()] and [geom_survival_discrete()] for theoretical survival
#'   functions, [geom_echf_na()] for the Nelson-Aalen cumulative hazard.
#'
#' @name geom_ecdf_km
#' @aliases StatECDFKM StatECDFKMBand StatCensorMarks
#' @export
geom_ecdf_km <- function(
    mapping      = NULL,
    data         = NULL,
    stat         = StatECDFKM,
    position     = "identity",
    ...,
    na.rm        = FALSE,
    show.legend  = NA,
    inherit.aes  = TRUE,
    open_fill    = NULL,
    vert_type    = "dashed",
    show_points  = NULL,
    show_vert    = NULL,
    conf_int     = TRUE,
    level        = 0.95,
    conf_alpha   = 0.4,
    censor_marks = TRUE,
    censor_shape = 3,
    censor_size  = 2
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
    geom        = GeomSurvivalDiscrete,
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

  layers <- list()

  if (conf_int) {
    ribbon_layer <- layer(
      data        = data,
      mapping     = aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
      stat        = StatECDFKMBand,
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
    layers <- c(layers, list(ribbon_layer))
  }

  layers <- c(layers, list(main_layer))

  if (censor_marks) {
    censor_layer <- layer(
      data        = data,
      mapping     = aes(y = after_stat(y)),
      stat        = StatCensorMarks,
      geom        = GeomPoint,
      position    = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params      = list(
        na.rm = na.rm,
        shape = censor_shape,
        size  = censor_size
      )
    )
    layers <- c(layers, list(censor_layer))
  }

  if (length(layers) == 1L) layers[[1L]] else layers
}


#' @rdname geom_ecdf_km
#' @export
StatECDFKM <- ggproto("StatECDFKM", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE) {
    tab <- .tabulate_km(data$x, data$status, na.rm = na.rm)
    if (nrow(tab) == 0L) return(data.frame(x = numeric(0), y = numeric(0)))
    data.frame(x = tab$time, y = tab$surv)
  }
)


#' @rdname geom_ecdf_km
#' @export
StatECDFKMBand <- ggproto("StatECDFKMBand", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95) {
    tab <- .tabulate_km(data$x, data$status, na.rm = na.rm)
    if (nrow(tab) == 0L) return(data.frame())
    se <- sqrt(tab$var_surv)
    # EP simultaneous critical value (Nair 1984)
    G  <- cumsum(ifelse(tab$n_risk == tab$n_event, 0,
                        tab$n_event / (tab$n_risk * (tab$n_risk - tab$n_event))))
    a_L <- if (length(G) >= 2L) G[1L] / G[length(G)] else 1
    c_ep <- .ep_critical_value(a_L, alpha = 1 - level)
    df <- data.frame(
      x    = tab$time,
      ymin = pmax(0, tab$surv - c_ep * se),
      ymax = pmin(1, tab$surv + c_ep * se)
    )
    .expand_step_ribbon(df)
  }
)


#' @rdname geom_ecdf_km
#' @export
StatCensorMarks <- ggproto("StatCensorMarks", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE) {
    ct <- .censoring_times(data$x, data$status, na.rm = na.rm)
    if (length(ct) == 0L) return(data.frame(x = numeric(0), y = numeric(0)))

    # Build the KM curve to look up S(t) at censoring times
    tab <- .tabulate_km(data$x, data$status, na.rm = na.rm)
    if (nrow(tab) == 0L) {
      # No events — survival is 1 everywhere
      return(data.frame(x = ct, y = rep(1, length(ct))))
    }

    # S(t) is right-continuous step: for each censoring time, find the most
    # recent event time <= ct[j] and use that S value; if none, S = 1.
    surv_at_censor <- vapply(ct, function(t_c) {
      idx <- which(tab$time <= t_c)
      if (length(idx) == 0L) 1 else tab$surv[max(idx)]
    }, numeric(1L))

    data.frame(x = ct, y = surv_at_censor)
  }
)


# ── geom_echf_na ─────────────────────────────────────────────────────────────

#' Plot a Nelson-Aalen Cumulative Hazard Estimate for Censored Data
#'
#' `geom_echf_na()` computes the Nelson-Aalen cumulative hazard estimator from
#' right-censored data and renders it as an increasing step function starting
#' at 0, using the same visual conventions as [geom_cdf_discrete()]. An
#' optional simultaneous confidence band (defaulting to 95%) is drawn around
#' the curve using the equal-precision (EP) construction of Nair (1984).
#'
#' The Nelson-Aalen estimator at event time \eqn{t_j} is
#' \deqn{\hat{H}(t) = \sum_{t_j \le t} \frac{d_j}{n_j},}
#' where \eqn{d_j} is the number of events and \eqn{n_j} is the number at risk
#' just before \eqn{t_j}.
#'
#' The simultaneous confidence band uses the Nelson variance estimator
#' \deqn{\widehat{\mathrm{Var}}[\hat{H}(t)] = \sum_{t_j \le t}
#' \frac{d_j}{n_j^2}}
#' with the equal-precision (EP) critical value of Nair (1984), giving bounds
#' \eqn{\hat{H}(t) \pm c_{\mathrm{EP}}\,\mathrm{se}(t)} with lower bound
#' clipped to 0. The EP critical value \eqn{c_{\mathrm{EP}}} is derived from
#' the asymptotic distribution of the standardized Nelson-Aalen process and
#' depends on the ratio of Nelson variances at the first and last event times.
#' The resulting band is simultaneous (valid at all \eqn{t} jointly), not
#' merely pointwise, and is asymptotically correct.
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
#' @param conf_int Logical. If `TRUE` (the default), draws a simultaneous
#'   EP confidence band around the Nelson-Aalen estimate.
#' @param level Confidence level for the band. Defaults to `0.95`.
#' @param conf_alpha Alpha (transparency) of the confidence ribbon. Defaults
#'   to `0.4`.
#'
#' @section Aesthetics:
#' `geom_echf_na()` requires the following aesthetics:
#' \describe{
#'   \item{`x`}{Observed time (event or censoring time).}
#'   \item{`status`}{Event indicator: 1 = event occurred, 0 = censored.}
#' }
#' It also understands `colour`, `alpha`, `linewidth`, and `linetype`.
#'
#' @return A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.
#'
#' @examples
#' set.seed(42)
#' n <- 50
#' true_time <- rexp(n, rate = 0.5)
#' cens_time <- rexp(n, rate = 0.2)
#' df <- data.frame(
#'   time   = pmin(true_time, cens_time),
#'   status = as.integer(true_time <= cens_time)
#' )
#'
#' ggplot(df, aes(x = time, status = status)) +
#'   geom_echf_na()
#'
#' # Without confidence band
#' ggplot(df, aes(x = time, status = status)) +
#'   geom_echf_na(conf_int = FALSE)
#'
#' # Grouped data
#' df2 <- data.frame(
#'   time   = c(rexp(40, 0.5), rexp(40, 1)),
#'   status = sample(0:1, 80, replace = TRUE, prob = c(0.2, 0.8)),
#'   group  = rep(c("A", "B"), each = 40)
#' )
#' ggplot(df2, aes(x = time, status = status, colour = group)) +
#'   geom_echf_na()
#'
#' @seealso [geom_echf()] for complete (uncensored) data,
#'   [geom_chf()] for theoretical cumulative hazard functions,
#'   [geom_ecdf_km()] for the Kaplan-Meier survival curve.
#'
#' @name geom_echf_na
#' @aliases StatECHFNA StatECHFNABand
#' @export
geom_echf_na <- function(
    mapping     = NULL,
    data        = NULL,
    stat        = StatECHFNA,
    position    = "identity",
    ...,
    na.rm       = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    open_fill   = NULL,
    vert_type   = "dashed",
    show_points = NULL,
    show_vert   = NULL,
    conf_int    = TRUE,
    level       = 0.95,
    conf_alpha  = 0.4
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
    stat        = StatECHFNABand,
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


#' @rdname geom_echf_na
#' @export
StatECHFNA <- ggproto("StatECHFNA", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE) {
    tab <- .tabulate_km(data$x, data$status, na.rm = na.rm)
    if (nrow(tab) == 0L) return(data.frame(x = numeric(0), y = numeric(0)))
    data.frame(x = tab$time, y = tab$chf)
  }
)


#' @rdname geom_echf_na
#' @export
StatECHFNABand <- ggproto("StatECHFNABand", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95) {
    tab <- .tabulate_km(data$x, data$status, na.rm = na.rm)
    if (nrow(tab) == 0L) return(data.frame())
    se <- sqrt(tab$var_chf)
    # EP simultaneous critical value (Nair 1984)
    a_L <- if (length(tab$var_chf) >= 2L) {
      tab$var_chf[1L] / tab$var_chf[length(tab$var_chf)]
    } else {
      1
    }
    c_ep <- .ep_critical_value(a_L, alpha = 1 - level)
    df <- data.frame(
      x    = tab$time,
      ymin = pmax(0, tab$chf - c_ep * se),
      ymax = tab$chf + c_ep * se
    )
    .expand_step_ribbon(df)
  }
)
