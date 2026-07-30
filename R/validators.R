# R/validators.R
#
# Shared structural validators. Structural invalidity always
# aborts and is never disabled by options(ggfunction.check = FALSE); that
# option only governs soft normalization/endpoint diagnostics.

#' @noRd
is_scalar_prob <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 && x < 1
}

#' Shared probability-argument validation for shading requests.
#'
#' Rules:
#' - `p` is a finite numeric scalar strictly between 0 and 1;
#' - `p_lower`/`p_upper` are supplied together, both finite scalars strictly
#'   between 0 and 1, with `p_lower < p_upper`;
#' - `p` cannot be combined with the pair;
#' - `shade_hdr` values are finite and strictly between 0 and 1;
#' - `shade_outside` is a single non-missing logical, and `TRUE` is valid
#'   only with a complete `p_lower`/`p_upper` pair.
#'
#' Called at construction wherever possible and again in the Stat, so direct
#' Stat usage gets the same message.
#' @noRd
validate_probability_shading <- function(p = NULL, p_lower = NULL,
                                         p_upper = NULL,
                                         shade_hdr = NULL,
                                         shade_outside = NULL) {
  for (nm in c("p", "p_lower", "p_upper")) {
    val <- get(nm)
    if (!is.null(val) && !is_scalar_prob(val)) {
      cli::cli_abort(
        "{.arg {nm}} must be a single finite number strictly between 0 and 1."
      )
    }
  }
  if (!is.null(shade_hdr)) {
    if (!is.numeric(shade_hdr) || length(shade_hdr) < 1L ||
        any(!is.finite(shade_hdr)) || any(shade_hdr <= 0) ||
        any(shade_hdr >= 1)) {
      cli::cli_abort(
        "{.arg shade_hdr} must contain finite values strictly between 0 and 1."
      )
    }
  }
  if ((is.null(p_lower) && !is.null(p_upper)) ||
      (!is.null(p_lower) && is.null(p_upper))) {
    cli::cli_abort("{.arg p_lower} and {.arg p_upper} must be supplied together.")
  }
  if (!is.null(p) && !is.null(p_lower)) {
    cli::cli_abort(
      "Supply either {.arg p} or the {.arg p_lower}/{.arg p_upper} pair, not both."
    )
  }
  if (!is.null(p_lower) && p_lower >= p_upper) {
    cli::cli_abort("{.arg p_lower} must be less than {.arg p_upper}.")
  }
  if (!is.null(shade_outside)) {
    if (!is.logical(shade_outside) || length(shade_outside) != 1L ||
        is.na(shade_outside)) {
      cli::cli_abort("{.arg shade_outside} must be a single non-missing logical value.")
    }
    if (isTRUE(shade_outside) && is.null(p_lower)) {
      cli::cli_abort(
        "{.arg shade_outside = TRUE} requires a complete {.arg p_lower}/{.arg p_upper} pair."
      )
    }
  }
  invisible(NULL)
}

#' Evaluate and structurally validate a PMF exactly once.
#'
#' Calls the PMF a single time over the support and validates the result:
#' numeric output, exactly one value per support point, all values finite and
#' non-negative, positive finite total. Structural invalidity always aborts —
#' independent of `options(ggfunction.check)` — because no coherent display
#' exists for such input. Normalization is a plausibility diagnostic: a total
#' off 1 by more than `tol` triggers a soft alert (gated by `check` and the
#' global option) and the computation proceeds with the declared, possibly
#' truncated, object.
#'
#' @return The validated (raw) mass vector, for reuse without re-evaluation.
#' @noRd
evaluate_pmf <- function(fun, support, args = NULL, arg = "fun",
                         tol = 1e-2, check = TRUE) {
  args <- args %||% list()
  fun_injected <- function(x) rlang::inject(fun(x, !!!args))

  mass <- try(fun_injected(support), silent = TRUE)
  if (inherits(mass, "try-error")) {
    cli::cli_abort("Error evaluating the PMF over the provided support. Please check your function definition.")
  }
  if (!is.numeric(mass)) {
    cli::cli_abort("{.arg {arg}} must return numeric mass values.")
  }
  if (length(mass) != length(support)) {
    cli::cli_abort(
      "{.arg {arg}} must return exactly one mass value per support point ({length(support)} points, got {length(mass)} value{?s})."
    )
  }
  if (any(!is.finite(mass))) {
    cli::cli_abort("{.arg {arg}} must return finite mass values over the support.")
  }
  if (any(mass < 0)) {
    cli::cli_abort("{.arg {arg}} must return non-negative mass values over the support.")
  }
  total <- sum(mass)
  if (!is.finite(total) || total <= 0) {
    cli::cli_abort("{.arg {arg}} must have positive finite total mass over the support.")
  }

  if (ggfunction_check_enabled(check) && abs(total - 1) > tol) {
    cli::cli_alert(sprintf(
      "The provided function sums to %.4f over the declared support [%g, %g], not ~1; supply the full computational support via `support` if the distribution is truncated, using `xlim` only to limit the displayed range.",
      total, min(support), max(support)
    ))
  }

  mass
}

#' Validate direct discrete survival values.
#'
#' Structure is hard: numeric output, one finite value per support point.
#' Shape is diagnostic: values outside the unit interval beyond roundoff and
#' non-monotone sequences trigger soft alerts (gated by `check` and the
#' global option) and are drawn as supplied; roundoff excursions within
#' sqrt(.Machine$double.eps) are clamped.
#' @noRd
validate_discrete_survival <- function(vals, support, arg = "fun",
                                       check = TRUE) {
  tol <- sqrt(.Machine$double.eps)
  if (!is.numeric(vals)) {
    cli::cli_abort("{.arg {arg}} must return numeric survival values.")
  }
  if (length(vals) != length(support)) {
    cli::cli_abort("{.arg {arg}} must return exactly one survival value per support point.")
  }
  if (any(!is.finite(vals))) {
    cli::cli_abort("{.arg {arg}} must return finite survival values over the support.")
  }
  in_round <- vals >= -tol & vals <= 1 + tol
  vals[in_round] <- pmin(1, pmax(0, vals[in_round]))
  if (ggfunction_check_enabled(check)) {
    if (any(!in_round)) {
      cli::cli_alert(sprintf(
        "The survival values from `%s` leave [0, 1] (range %.4f to %.4f); survival functions take values in [0, 1].",
        arg, min(vals), max(vals)
      ))
    }
    if (length(vals) > 1L && any(diff(vals) > tol)) {
      cli::cli_alert(sprintf(
        "The survival values from `%s` are not monotonically non-increasing; the provided function may not be a valid survival function.",
        arg
      ))
    }
  }
  vals
}

#' Validate direct discrete CDF values: structure hard, shape diagnostic
#' (see validate_discrete_survival).
#' @noRd
validate_discrete_cdf_values <- function(vals, support, arg = "fun",
                                         check = TRUE) {
  tol <- sqrt(.Machine$double.eps)
  if (!is.numeric(vals)) {
    cli::cli_abort("{.arg {arg}} must return numeric CDF values.")
  }
  if (length(vals) != length(support)) {
    cli::cli_abort("{.arg {arg}} must return exactly one CDF value per support point.")
  }
  if (any(!is.finite(vals))) {
    cli::cli_abort("{.arg {arg}} must return finite CDF values over the support.")
  }
  in_round <- vals >= -tol & vals <= 1 + tol
  vals[in_round] <- pmin(1, pmax(0, vals[in_round]))
  if (ggfunction_check_enabled(check)) {
    if (any(!in_round)) {
      cli::cli_alert(sprintf(
        "The CDF values from `%s` leave [0, 1] (range %.4f to %.4f); CDFs take values in [0, 1].",
        arg, min(vals), max(vals)
      ))
    }
    if (length(vals) > 1L && any(diff(vals) < -tol)) {
      cli::cli_alert(sprintf(
        "The CDF values from `%s` are not monotonically non-decreasing; the provided function may not be a valid CDF.",
        arg
      ))
    }
  }
  vals
}

#' Multiplicative dimming for unshaded discrete pieces.
#'
#' The dimming factor multiplies the resolved alpha (NA alpha resolves to 1),
#' so unhighlighted pieces are never more opaque than highlighted ones, and
#' an already-resolved HDR alpha is multiplied rather than replaced.
#' @noRd
dim_alpha <- function(alpha, in_shade, factor = 0.3) {
  base <- ifelse(is.na(alpha), 1, alpha)
  ifelse(in_shade, base, factor * base)
}
