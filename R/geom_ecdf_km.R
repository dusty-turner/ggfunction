#' @importFrom ggplot2 GeomRibbon GeomPoint GeomErrorbar
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
  if (length(time) != length(status)) {
    cli::cli_abort("{.arg time} and {.arg status} must have the same length.")
  }

  keep <- is.finite(time) & !is.na(status)
  n_removed <- sum(!keep)
  if (n_removed > 0L && !na.rm) {
    cli::cli_warn(c(
      "Removed {n_removed} observation{?s} with missing or non-finite time/status values.",
      "i" = "Set {.arg na.rm = TRUE} to suppress this warning."
    ))
  }
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

  status <- normalize_status(status)

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

  # A singular Greenwood contribution (n_j == d_j) leaves the variance
  # undefined from that time on; it is never replaced by zero (D-01).
  greenwood_term <- ifelse(
    n_evt == d_evt,
    NA_real_,
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

#' Validate a prespecified equal-precision transformed-time domain (D-01).
#' @noRd
validate_ep_range <- function(ep_range) {
  if (is.null(ep_range)) return(NULL)
  if (!is.numeric(ep_range) || length(ep_range) != 2L ||
      any(!is.finite(ep_range)) || ep_range[1] <= 0 || ep_range[2] >= 1 ||
      ep_range[1] >= ep_range[2]) {
    cli::cli_abort("{.arg ep_range} must be two values with 0 < a_L < a_U < 1.")
  }
  ep_range
}

#' Observed follow-up range (event and censoring times alike), used for
#' non-inferential domain anchors (D-02).
#' @noRd
.observed_time_range <- function(time, status) {
  keep <- is.finite(time) & !is.na(status)
  if (!any(keep)) return(NULL)
  range(time[keep])
}

#' Equal-precision (EP) critical value for simultaneous confidence bands.
#'
#' Computes the Nair (1984) EP critical value using the Miller-Siegmund
#' approximation documented for SAS LIFETEST. The endpoints are
#' `a(t) = n * G(t) / (1 + n * G(t))`, where `G(t)` is Greenwood's cumulative
#' variance term.
#'
#' @param a_L Lower endpoint of the EP time scale.
#' @param a_U Upper endpoint of the EP time scale.
#' @param alpha Significance level (1 - confidence level).
#' @return Scalar critical value.
#' @noRd
.ep_critical_value <- function(a_L, a_U, alpha) {
  if (!is.finite(a_L) || !is.finite(a_U) || a_L <= 0 || a_U <= 0 ||
      a_L >= 1 || a_U >= 1 || a_U <= a_L) {
    # Never a pointwise-normal fallback: an invalid equal-precision domain
    # is a caller error (D-01).
    cli::cli_abort("Equal-precision endpoints must satisfy 0 < a_L < a_U < 1.")
  }

  log_term <- log((a_U * (1 - a_L)) / (a_L * (1 - a_U)))
  if (!is.finite(log_term) || log_term <= 0) {
    cli::cli_abort("Equal-precision endpoints must satisfy 0 < a_L < a_U < 1.")
  }

  ms_tail <- function(x) {
    phi <- stats::dnorm(x)
    4 * phi / x + phi * (x - 1 / x) * log_term
  }

  f <- function(x) ms_tail(x) - alpha
  lower <- stats::qnorm(1 - alpha / 2)
  upper <- max(lower * 2, 4)
  while (f(upper) > 0 && upper < 50) {
    upper <- upper * 2
  }
  if (f(upper) > 0) {
    return(upper)
  }
  stats::uniroot(f, lower = lower, upper = upper)$root
}


#' Return sorted censoring times for censor-mark rendering.
#'
#' @param time Numeric vector of observed times.
#' @param status Integer/logical vector: 1 = event, 0 = censored.
#' @param na.rm Logical; if `TRUE`, remove NA pairs.
#' @return Sorted numeric vector of times where `status == 0`.
#' @noRd
.censoring_times <- function(time, status, na.rm) {
  if (length(time) != length(status)) {
    cli::cli_abort("{.arg time} and {.arg status} must have the same length.")
  }

  keep <- is.finite(time) & !is.na(status)
  n_removed <- sum(!keep)
  if (n_removed > 0L && !na.rm) {
    cli::cli_warn(c(
      "Removed {n_removed} observation{?s} with missing or non-finite time/status values.",
      "i" = "Set {.arg na.rm = TRUE} to suppress this warning."
    ))
  }
  time   <- time[keep]
  status <- status[keep]
  status <- normalize_status(status)
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
#' the asymptotic distribution of the standardized KM process. It depends on
#' endpoints \eqn{a(t) = nG(t)/(1 + nG(t))}, where \eqn{G(t)} is Greenwood's
#' cumulative variance term, evaluated at the first and last valid event-time
#' values. The resulting band is simultaneous (valid at all \eqn{t} jointly),
#' not merely pointwise, and is asymptotically correct.
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
#' @param ep_range Optional numeric vector `c(a_L, a_U)` prespecifying the
#'   equal-precision transformed-time domain of the simultaneous confidence
#'   band, validated as `0 < a_L < a_U < 1`. The band is restricted to event
#'   times whose transformed times fall inside this range, and the critical
#'   value is computed from the supplied endpoints. When `NULL` (the
#'   default), the first and last valid transformed times are used — a
#'   data-adaptive plug-in band that is approximate rather than an
#'   unqualified nominal simultaneous-confidence procedure. Rows where the
#'   Greenwood variance is undefined (for example the terminal event when
#'   everyone remaining fails) are never part of the band; the last valid
#'   interval is carried to the end of follow-up as a non-inferential
#'   geometric anchor.
#' @param censor_marks Logical. If `TRUE` (the default), draws "+" marks at
#'   censoring times on the survival curve.
#' @param censor_shape Shape for censoring marks. Defaults to `3` ("+").
#' @param censor_size Size for censoring marks. Defaults to `2`.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Event or censoring times used by the displayed
#'   layer.}
#'   \item{`after_stat(y)`}{Kaplan-Meier survival estimates for the main curve,
#'   or survival estimates at censoring times for censor marks.}
#'   \item{`after_stat(ymin)` and `after_stat(ymax)`}{Lower and upper simultaneous
#'   confidence-band limits when `conf_int = TRUE`.}
#' }
#'
#' @section Dropped variables:
#' `status` is used to compute event times, censoring times, and risk sets, but
#' is not available after statistical transformation.
#'
#' @section Aesthetics:
#' `geom_ecdf_km()` requires the following aesthetics:
#' \describe{
#'   \item{`x`}{Observed time (event or censoring time).}
#'   \item{`status`}{Event indicator: 1 = event occurred, 0 = censored.}
#' }
#' It also understands `alpha`, `colour`/`color`, `fill`, `group`, `linetype`,
#' `linewidth`, `shape`, `size`, and `stroke`.
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
    ep_range     = NULL,
    censor_marks = TRUE,
    censor_shape = 3,
    censor_size  = 2
) {
  validate_ep_range(ep_range)
  default_mapping <- aes(y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  main_layer <- status_layer(
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
    ribbon_layer <- status_layer(
      data        = data,
      mapping     = merge_input_mapping(
        mapping, aes(ymin = after_stat(ymin), ymax = after_stat(ymax))
      ),
      stat        = StatECDFKMBand,
      geom        = GeomRibbon,
      position    = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params      = list(
        na.rm     = na.rm,
        level     = level,
        ep_range  = ep_range,
        fill      = "grey60",
        linewidth = 0,
        alpha     = conf_alpha
      )
    )
    layers <- c(layers, list(ribbon_layer))
  }

  layers <- c(layers, list(main_layer))

  if (censor_marks) {
    censor_layer <- status_layer(
      data        = data,
      mapping     = merge_input_mapping(mapping, aes(y = after_stat(y))),
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
    rng <- .observed_time_range(data$x, data$status)
    if (is.null(rng)) return(data.frame(x = numeric(0), y = numeric(0)))

    if (nrow(tab) == 0L) {
      # All censored: S(t) = 1 over the observation domain, no jumps (D-02).
      times <- unique(rng)
      surv_vals <- rep(1, length(times))
      prev <- surv_vals
      jump <- rep(FALSE, length(times))
      anchor <- rep(TRUE, length(times))
    } else {
      times <- tab$time
      surv_vals <- tab$surv
      prev <- c(1, surv_vals[-length(surv_vals)])
      jump <- rep(TRUE, length(times))
      anchor <- rep(FALSE, length(times))
      if (rng[2] > max(times)) {
        # Trailing follow-up (a censoring after the last event) extends the
        # curve horizontally; the anchor is not an event (D-02).
        s_last <- surv_vals[length(surv_vals)]
        times <- c(times, rng[2])
        surv_vals <- c(surv_vals, s_last)
        prev <- c(prev, s_last)
        jump <- c(jump, FALSE)
        anchor <- c(anchor, TRUE)
      }
    }

    out <- data.frame(
      x = times,
      y = scale_forward(scales$y, surv_vals),
      survival = surv_vals,
      y_prev = scale_forward(scales$y, prev),
      jump = jump,
      domain_anchor = anchor
    )
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel
    out$top_panel <- resolve_stat_baseline(scales$y, 1)$panel
    out
  }
)


#' @rdname geom_ecdf_km
#' @export
StatECDFKMBand <- ggproto("StatECDFKMBand", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95,
                           ep_range = NULL) {
    ep_range <- validate_ep_range(ep_range)
    tab <- .tabulate_km(data$x, data$status, na.rm = na.rm)
    rng <- .observed_time_range(data$x, data$status)
    if (nrow(tab) == 0L) return(data.frame())

    G <- ifelse(tab$surv > 0, tab$var_surv / tab$surv^2, NA_real_)
    se <- sqrt(tab$var_surv)
    a <- tab$n[1L] * G / (1 + tab$n[1L] * G)

    # The band is defined only where the Greenwood variance is finite and
    # positive and the equal-precision transformed time is valid; a singular
    # terminal contribution never produces a fake zero-width interval (D-01).
    valid <- is.finite(a) & a > 0 & a < 1 & is.finite(se) & se > 0
    if (!is.null(ep_range)) {
      keep <- valid & a >= ep_range[1] & a <= ep_range[2]
      a_L <- ep_range[1]
      a_U <- ep_range[2]
    } else {
      # Data-adaptive plug-in domain: first/last valid transformed times.
      # This is an approximate plug-in band, not an unqualified nominal
      # simultaneous-confidence procedure (D-01).
      keep <- valid
      if (any(valid)) {
        a_vals <- a[valid]
        a_L <- a_vals[1L]
        a_U <- a_vals[length(a_vals)]
      } else {
        a_L <- a_U <- NA_real_
      }
    }

    if (!any(keep) || !is.finite(a_L) || !is.finite(a_U) ||
        a_L <= 0 || a_U >= 1 || a_U <= a_L) {
      cli::cli_warn(c(
        "No valid domain exists for the equal-precision confidence band; the band is omitted.",
        "i" = "The Greenwood variance is undefined or degenerate at every event time (for example, a single event time or an all-terminal risk set)."
      ))
      return(data.frame())
    }

    c_ep <- .ep_critical_value(a_L, a_U, alpha = 1 - level)

    ymin_raw <- pmax(0, (tab$surv - c_ep * se)[keep])
    ymax_raw <- pmin(1, (tab$surv + c_ep * se)[keep])
    df <- data.frame(x = tab$time[keep], ymin = ymin_raw, ymax = ymax_raw)
    band <- .expand_step_ribbon(df)
    band$domain_anchor <- FALSE
    band$jump <- TRUE

    # Carry the last valid interval to the end of observed follow-up as
    # geometric metadata; this anchor is not a confidence interval (D-01,
    # D-02).
    t_end <- max(c(rng[2], tab$time))
    if (t_end > max(df$x)) {
      band <- rbind(band, data.frame(
        x = t_end,
        ymin = ymin_raw[length(ymin_raw)],
        ymax = ymax_raw[length(ymax_raw)],
        domain_anchor = TRUE,
        jump = FALSE
      ))
    }
    band$ymin <- scale_forward(scales$y, band$ymin)
    band$ymax <- scale_forward(scales$y, band$ymax)
    band
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
      return(data.frame(
        x = ct,
        y = scale_forward(scales$y, rep(1, length(ct))),
        survival = rep(1, length(ct))
      ))
    }

    # S(t) is right-continuous step: for each censoring time, find the most
    # recent event time <= ct[j] and use that S value; if none, S = 1.
    surv_at_censor <- vapply(ct, function(t_c) {
      idx <- which(tab$time <= t_c)
      if (length(idx) == 0L) 1 else tab$surv[max(idx)]
    }, numeric(1L))

    data.frame(
      x = ct,
      y = scale_forward(scales$y, surv_at_censor),
      survival = surv_at_censor
    )
  }
)


# ── geom_echf_na ─────────────────────────────────────────────────────────────

#' Plot a Nelson-Aalen Cumulative Hazard Estimate for Censored Data
#'
#' `geom_echf_na()` computes the Nelson-Aalen cumulative hazard estimator from
#' right-censored data and renders it as an increasing step function starting
#' at 0, using the same visual conventions as [geom_cdf_discrete()]. An
#' optional pointwise normal interval display (defaulting to 95%) is drawn at
#' event times using the Nelson variance estimator. These pointwise intervals
#' are drawn as gray error bars by default to distinguish them visually
#' from simultaneous confidence bands.
#'
#' The Nelson-Aalen estimator at event time \eqn{t_j} is
#' \deqn{\hat{H}(t) = \sum_{t_j \le t} \frac{d_j}{n_j},}
#' where \eqn{d_j} is the number of events and \eqn{n_j} is the number at risk
#' just before \eqn{t_j}.
#'
#' The pointwise confidence intervals use the Nelson variance estimator
#' \deqn{\widehat{\mathrm{Var}}[\hat{H}(t)] = \sum_{t_j \le t}
#' \frac{d_j}{n_j^2}}
#' with the normal critical value, giving pointwise bounds
#' \eqn{\hat{H}(t) \pm z_{1-\alpha/2}\,\mathrm{se}(t)} with lower bound
#' clipped to 0.
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
#' @param conf_int Logical. If `TRUE` (the default), draws pointwise normal
#'   confidence intervals for the Nelson-Aalen estimate.
#' @param level Confidence level for the intervals. Defaults to `0.95`.
#' @param conf_geom Confidence display to use when `conf_int = TRUE`.
#'   `"errorbar"` (the default) draws pointwise interval bars at event
#'   times. `"ribbon"` preserves the previous continuous ribbon display.
#'   `"none"` suppresses the confidence display.
#' @param conf_colour Colour or fill for the confidence display. Defaults
#'   to `"grey60"`.
#' @param conf_linewidth Line width for `conf_geom = "errorbar"`. Defaults
#'   to `0.4`.
#' @param conf_alpha Alpha (transparency) of the confidence display. Defaults
#'   to `0.4`.
#' @param conf_width Width of the error-bar caps when `conf_geom = "errorbar"`.
#'   Defaults to `NULL`, which uses 2% of the event-time range. Use `0` for
#'   capless vertical interval bars.
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Event times.}
#'   \item{`after_stat(y)`}{Nelson-Aalen cumulative hazard estimates.}
#'   \item{`after_stat(ymin)` and `after_stat(ymax)`}{Lower and upper confidence
#'   interval limits when `conf_int = TRUE`.}
#' }
#'
#' @section Dropped variables:
#' `status` is used to compute event times and risk sets, but is not available
#' after statistical transformation.
#'
#' @section Aesthetics:
#' `geom_echf_na()` requires the following aesthetics:
#' \describe{
#'   \item{`x`}{Observed time (event or censoring time).}
#'   \item{`status`}{Event indicator: 1 = event occurred, 0 = censored.}
#' }
#' It also understands `alpha`, `colour`/`color`, `fill`, `group`, `linetype`,
#' `linewidth`, `shape`, `size`, and `stroke`.
#'
#' @return A ggplot2 layer, or a list of two layers when a confidence display
#'   is drawn.
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
#' # Previous ribbon-style confidence display
#' ggplot(df, aes(x = time, status = status)) +
#'   geom_echf_na(conf_geom = "ribbon")
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
#' @aliases StatECHFNA StatECHFNABand StatECHFNAInterval
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
    conf_alpha  = 0.4,
    conf_geom   = c("errorbar", "ribbon", "none"),
    conf_colour = "grey60",
    conf_linewidth = 0.4,
    conf_width  = NULL
) {
  conf_geom <- match.arg(conf_geom)

  default_mapping <- aes(y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  main_layer <- status_layer(
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

  if (!conf_int || identical(conf_geom, "none")) return(main_layer)

  if (identical(conf_geom, "ribbon")) {
    conf_layer <- status_layer(
      data        = data,
      mapping     = merge_input_mapping(
        mapping, aes(ymin = after_stat(ymin), ymax = after_stat(ymax))
      ),
      stat        = StatECHFNABand,
      geom        = GeomRibbon,
      position    = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params      = list(
        na.rm     = na.rm,
        level     = level,
        fill      = conf_colour,
        linewidth = 0,
        alpha     = conf_alpha
      )
    )
  } else {
    conf_params <- list(
      na.rm     = na.rm,
      level     = level,
      colour    = conf_colour,
      linewidth = conf_linewidth,
      alpha     = conf_alpha
    )
    if (!is.null(conf_width)) conf_params$width <- conf_width

    conf_layer <- status_layer(
      data        = data,
      mapping     = merge_input_mapping(
        mapping, aes(ymin = after_stat(ymin), ymax = after_stat(ymax))
      ),
      stat        = StatECHFNAInterval,
      geom        = GeomECHFNAErrorbar,
      position    = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params      = conf_params
    )
  }

  list(conf_layer, main_layer)
}


GeomECHFNAErrorbar <- ggproto("GeomECHFNAErrorbar", GeomErrorbar,
  setup_data = function(self, data, params) {
    width <- params$width
    if (is.null(width)) width <- .echf_na_interval_width(data$x)
    data$width <- NULL
    params$width <- width
    data <- ggproto_parent(GeomErrorbar, self)$setup_data(data, params)
    data$width <- width
    data
  }
)


#' @rdname geom_echf_na
#' @export
StatECHFNA <- ggproto("StatECHFNA", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE) {
    tab <- .tabulate_km(data$x, data$status, na.rm = na.rm)
    rng <- .observed_time_range(data$x, data$status)
    if (is.null(rng)) return(data.frame(x = numeric(0), y = numeric(0)))

    if (nrow(tab) == 0L) {
      # All censored: H(t) = 0 over the observation domain, no jumps (D-02).
      times <- unique(rng)
      chf_vals <- rep(0, length(times))
      prev <- chf_vals
      jump <- rep(FALSE, length(times))
      anchor <- rep(TRUE, length(times))
    } else {
      times <- tab$time
      chf_vals <- tab$chf
      prev <- c(0, chf_vals[-length(chf_vals)])
      jump <- rep(TRUE, length(times))
      anchor <- rep(FALSE, length(times))
      if (rng[2] > max(times)) {
        h_last <- chf_vals[length(chf_vals)]
        times <- c(times, rng[2])
        chf_vals <- c(chf_vals, h_last)
        prev <- c(prev, h_last)
        jump <- c(jump, FALSE)
        anchor <- c(anchor, TRUE)
      }
    }

    out <- data.frame(
      x = times,
      y = scale_forward(scales$y, chf_vals),
      cumhazard = chf_vals,
      y_prev = scale_forward(scales$y, prev),
      jump = jump,
      domain_anchor = anchor
    )
    # The cumulative-hazard baseline trains on raw zero when the transform
    # allows it; no artificial upper endpoint is forced (C-05).
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel
    out
  }
)


#' @rdname geom_echf_na
#' @export
StatECHFNABand <- ggproto("StatECHFNABand", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95) {
    df <- .echf_na_intervals(data$x, data$status, na.rm = na.rm, level = level)
    if (nrow(df) == 0L) return(df)
    band <- .expand_step_ribbon(df)
    band$domain_anchor <- FALSE
    band$jump <- TRUE
    rng <- .observed_time_range(data$x, data$status)
    if (!is.null(rng) && rng[2] > max(df$x)) {
      band <- rbind(band, data.frame(
        x = rng[2],
        ymin = df$ymin[nrow(df)],
        ymax = df$ymax[nrow(df)],
        domain_anchor = TRUE,
        jump = FALSE
      ))
    }
    band
  }
)


#' @rdname geom_echf_na
#' @export
StatECHFNAInterval <- ggproto("StatECHFNAInterval", Stat,
  required_aes = c("x", "status"),
  dropped_aes  = "status",

  compute_group = function(data, scales, na.rm = FALSE, level = 0.95) {
    .echf_na_intervals(data$x, data$status, na.rm = na.rm, level = level)
  }
)


.echf_na_intervals <- function(time, status, na.rm = FALSE, level = 0.95) {
  tab <- .tabulate_km(time, status, na.rm = na.rm)
  if (nrow(tab) == 0L) return(data.frame())
  se <- sqrt(tab$var_chf)
  z <- stats::qnorm(1 - (1 - level) / 2)
  df <- data.frame(
    x    = tab$time,
    ymin = pmax(0, tab$chf - z * se),
    ymax = tab$chf + z * se
  )
  df[is.finite(df$x) & is.finite(df$ymin) & is.finite(df$ymax), , drop = FALSE]
}

.echf_na_interval_width <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(0)
  span <- diff(range(x))
  if (span > 0) {
    0.02 * span
  } else {
    0.02 * max(abs(x[1L]), 1)
  }
}
