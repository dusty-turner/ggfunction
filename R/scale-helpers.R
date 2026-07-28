# R/scale-helpers.R
#
# Shared scale-transformation architecture (spec A-01, section 5.1).
#
# Coordinate contract:
#   - Public `xlim`/`ylim`/`support` arguments are DATA coordinates.
#   - `scale$dimension()` is PANEL (transformed) coordinates.
#   - Stat output position columns (`x`, `y`, `ymin`, ...) must be PANEL
#     coordinates; ggplot2 4.x backtransforms them for `after_stat()`
#     expressions and retransforms calculated position aesthetics, so
#     `after_stat()` users see data-space values.
#   - Semantic raw columns (`x_eval`, `y_raw`, `density`, `cdf`, `survival`,
#     `hazard`, `cumhazard`, `mass`, `prob`, `sample`) are not position
#     aesthetics and stay raw.
#   - Raw values are transformed exactly once, in the Stat; never transform a
#     `dimension()` value or an incoming mapped position a second time.

#' @noRd
scale_is_transformable <- function(scale) {
  !is.null(scale) && !scale$is_discrete()
}

#' Transform data-space values to panel space, exactly once.
#' Identity and discrete scales pass through numerically unchanged.
#' @noRd
scale_forward <- function(scale, x) {
  if (!scale_is_transformable(scale)) return(x)
  scale$get_transformation()$transform(x)
}

#' Inverse-transform panel-space values to data space.
#' @noRd
scale_inverse <- function(scale, x) {
  if (!scale_is_transformable(scale)) return(x)
  scale$get_transformation()$inverse(x)
}

#' Has this scale an actual display window (trained range or explicit
#' limits)? An untrained scale reports a generic default `dimension()`, which
#' is not an inherited display window and must not outrank a finite declared
#' support (spec B-01).
#' @noRd
scale_has_window <- function(scale) {
  if (is.null(scale)) return(FALSE)
  has_trained <- !is.null(scale$range) && !is.null(scale$range$range) &&
    length(scale$range$range) > 0
  has_limits <- !is.null(scale$limits)
  isTRUE(has_trained || has_limits)
}

#' Validate a data-space limits argument: length-2, finite, strictly
#' increasing. A supplied but malformed limit aborts; it never falls through
#' to another range source (spec B-01).
#' @noRd
validate_data_limits <- function(limits, arg = "xlim") {
  if (is.null(limits)) return(NULL)
  if (!is.numeric(limits) || length(limits) != 2L || anyNA(limits) ||
      any(!is.finite(limits)) || limits[1] >= limits[2]) {
    cli::cli_abort(
      "{.arg {arg}} must be a numeric vector of two finite, strictly increasing values (data coordinates)."
    )
  }
  as.numeric(limits)
}

#' Resolve the 1D evaluation grid for a function-generated Stat.
#'
#' Range precedence (spec B-01):
#'   1. explicit, valid data-space `limits`;
#'   2. the panel window of a trained or explicitly limited scale;
#'   3. finite declared data-space `support`;
#'   4. the documented finite panel-space fallback `default_panel_limits`
#'      (c(0, 1)), inverse-transformed for evaluation.
#'
#' The grid is evenly spaced in panel space; evaluation happens at its
#' inverse-transformed data-space image.
#'
#' @return `list(panel = <panel grid>, eval = <data-space grid>)`.
#' @noRd
resolve_stat_grid_1d <- function(scale, limits = NULL, support = NULL,
                                 n = 101, default_panel_limits = c(0, 1),
                                 arg = "xlim") {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 2) {
    cli::cli_abort("{.arg n} must be a single number of at least 2.")
  }
  limits <- validate_data_limits(limits, arg = arg)

  panel_limits <- NULL
  if (!is.null(limits)) {
    panel_limits <- suppressWarnings(scale_forward(scale, limits))
    if (any(!is.finite(panel_limits))) {
      cli::cli_abort(
        "{.arg {arg}} must lie inside the domain of the x scale transformation."
      )
    }
  }
  if (is.null(panel_limits) && scale_has_window(scale)) {
    dim <- scale$dimension()
    if (length(dim) == 2L && all(is.finite(dim))) panel_limits <- dim
  }
  if (is.null(panel_limits) && !is.null(support) &&
      length(support) == 2L && all(is.finite(support))) {
    candidate <- suppressWarnings(scale_forward(scale, as.numeric(support)))
    if (all(is.finite(candidate))) panel_limits <- candidate
  }
  if (is.null(panel_limits)) panel_limits <- default_panel_limits

  panel <- seq(panel_limits[1], panel_limits[2], length.out = n)
  list(panel = panel, eval = scale_inverse(scale, panel))
}

#' Resolve a raw mathematical baseline (density/mass/hazard zero, survival
#' one, ...) against a possibly transforming scale (spec 5.1).
#'
#' When the transformation maps the baseline to a finite panel value, that
#' value may participate in scale training and drawing. When it does not
#' (e.g. zero on a log scale), the raw baseline is retained as metadata only:
#' no non-finite training value is emitted, and baseline-dependent geometry
#' is clipped to the visible panel boundary at draw time with one targeted
#' warning (`warn_baseline_clipped()`).
#' @noRd
resolve_stat_baseline <- function(scale, raw_baseline) {
  panel <- scale_forward(scale, raw_baseline)
  finite <- length(panel) == 1L && is.finite(panel)
  list(
    raw = raw_baseline,
    panel = if (finite) panel else NA_real_,
    finite = finite
  )
}

#' Panel-space value at which baseline-anchored geometry should be drawn:
#' the baseline itself when it is inside the transform domain, otherwise the
#' visible lower panel boundary (with the documented targeted warning).
#' @noRd
baseline_draw_value <- function(baseline_panel, panel_params, axis = "y") {
  rng <- panel_params[[paste0(axis, ".range")]]
  floor <- if (is.null(rng)) -Inf else rng[1]
  if (length(baseline_panel) == 1L && is.finite(baseline_panel)) {
    return(max(baseline_panel, floor))
  }
  warn_baseline_clipped(axis = axis)
  floor
}

#' @noRd
warn_baseline_clipped <- function(axis = "y") {
  cli::cli_warn(c(
    "The mathematical baseline is outside the domain of the {axis} scale transformation.",
    "i" = "Baseline-anchored geometry was clipped to the visible panel boundary."
  ))
}
