# R/helper-functions.R

#' @noRd
ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    # A single-row, single-column placeholder so a function-only layer has data
    # to draw from. (Previously built via tibble0(group = 1, .size = 1), which
    # leaked `.size`/`.name_repair` as stray columns under base data.frame.)
    data.frame(group = 1L)
  } else {
    data
  }
}

#' @noRd
ensure_length_two <- function(n) {
  if (length(n) == 1) n <- rep(n, 2)
  if (length(n) != 2) stop("Length of 'n' must be 2")
  n
}

#' @noRd
times <- `*`

#' @noRd
empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

#' @noRd
probability_axis_anchor <- function() {
  ggplot2::geom_blank(
    data = data.frame(x = c(0, 1)),
    mapping = aes(x = x),
    inherit.aes = FALSE
  )
}

#' @noRd
default_labs_component <- function(x = NULL, y = NULL, fill = NULL, colour = NULL) {
  structure(
    list(x = x, y = y, fill = fill, colour = colour),
    class = "ggfunction_default_labs"
  )
}

#' @noRd
normalise_colour_params <- function(params) {
  if ("color" %in% names(params)) {
    color <- params$color
    params$color <- NULL
    if (!("colour" %in% names(params))) params$colour <- color
  }
  params
}

#' Drop constructor default aesthetic params the user has overridden
#'
#' Distribution geom constructors forward fixed defaults like
#' `color = "black"` as aesthetic params. Fixed aesthetic params override both
#' a user-supplied `colour` in `...` (which does not match the `color` formal)
#' and mapped aesthetics, so the default must be removed whenever the user
#' supplies the aesthetic either way.
#' @noRd
drop_overridden_aes_defaults <- function(params, mapping) {
  if (("colour" %in% names(params)) || ("colour" %in% names(mapping))) {
    params$color <- NULL
  }
  if (("fill" %in% names(mapping)) && ("fill" %in% names(params))) {
    params$fill <- NULL
  }
  params
}

#' Does a quosure reference a calculated (after-stat/after-scale) value?
#' @noRd
quo_is_calculated <- function(q) {
  detect <- function(e) {
    if (is.call(e)) {
      fn <- e[[1]]
      if (is.name(fn) &&
          as.character(fn) %in% c("after_stat", "stat", "after_scale", "stage")) {
        return(TRUE)
      }
      return(any(vapply(as.list(e)[-1], detect, logical(1))))
    }
    if (is.name(e)) {
      return(grepl("^\\.\\..+\\.\\.$", as.character(e)))
    }
    FALSE
  }
  detect(rlang::get_expr(q))
}

#' Merge a constructor-local user mapping into an auxiliary layer's computed
#' mapping (spec A-02).
#'
#' Composite geoms (ECDF/KM/Nelson-Aalen bands, censor marks, PP/SP/QQ
#' bands) must receive the input, grouping, and facet aesthetics supplied
#' directly to the composite constructor — `inherit.aes = TRUE` only recovers
#' plot-global mappings. The merger:
#' - preserves the user's input/grouping aesthetics;
#' - lets the auxiliary layer's computed output aesthetics (`ymin`, `ymax`,
#'   ...) override same-named user mappings;
#' - drops calculated (`after_stat()`) user aesthetics, which reference
#'   computed variables of the main stat that the auxiliary stat does not
#'   expose (point-only calculated aesthetics must not leak into ribbons or
#'   censor marks).
#' @noRd
merge_input_mapping <- function(user_mapping, computed_mapping) {
  if (is.null(user_mapping)) return(computed_mapping)
  keep <- user_mapping[!vapply(user_mapping, quo_is_calculated, logical(1))]
  merged <- modifyList(keep, computed_mapping)
  class(merged) <- "uneval"
  merged
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ggfunction_default_labs <- function(object, plot, object_name) {
  if (is.null(plot$labels$x) && !is.null(object$x)) {
    plot$labels$x <- object$x
  }
  if (is.null(plot$labels$y) && !is.null(object$y)) {
    plot$labels$y <- object$y
  }
  if (is.null(plot$labels$fill) && !is.null(object$fill)) {
    plot$labels$fill <- object$fill
  }
  if (is.null(plot$labels$colour) && !is.null(object$colour)) {
    plot$labels$colour <- object$colour
  }
  plot
}

#' @noRd
vectorize <- function(f, drop = TRUE) {
  function(v, ...) {
    stopifnot(is.numeric(v))
    if (is.vector(v)) v <- matrix(v, nrow = 1)
    out <- vector("list", nrow(v))
    for (i in seq_len(nrow(v))) {
      out[[i]] <- f(v[i, ], ...)
    }
    out <- t(simplify2array(out))
    if ((nrow(out) == 1L) && drop) out[1, ] else out
  }
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @noRd
ggfunction_check_enabled <- function(check = TRUE) {
  isTRUE(check) && !identical(getOption("ggfunction.check", TRUE), FALSE)
}

#' @noRd
check_pdf_normalization <- function(f, lower, upper, tol = 1e-3) {
  if (!ggfunction_check_enabled()) return(invisible(NA_real_))
  res <- try(integrate(f, lower, upper), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (!is.finite(lower) || !is.finite(upper)) {
      cli::cli_alert(sprintf(
        "The provided function could not be confirmed; it integrates to an unknown value over the support [%g, %g]. Set `support` to the distribution's finite domain when appropriate.",
        lower, upper
      ))
      return(invisible(NA_real_))
    }
    stop(sprintf("Error integrating the function over the range [%g, %g]. Please check your function definition.", lower, upper))
  }
  if (abs(res$value - 1) > tol) {
    cli::cli_alert(sprintf("The provided function integrates to %.4f over the range [%g, %g], which is not equal to 1 within a tolerance of %.3f.",
                           res$value, lower, upper, tol))
  }
  invisible(res$value)
}

#' @noRd
check_cdf_normalization <- function(f, lower, upper, tol = 1e-2) {
  if (!ggfunction_check_enabled()) return(invisible(c(lower = NA_real_, upper = NA_real_)))

  vals <- try(c(lower = f(lower), upper = f(upper)), silent = TRUE)
  if (inherits(vals, "try-error")) {
    stop(sprintf("Error evaluating the function over the range [%g, %g]. Please check your function definition.", lower, upper))
  }

  if (any(is.na(vals))) {
    # Numerically derived CDFs may not be evaluable at infinite support
    # endpoints; skip the endpoint check silently in that case (mirrors
    # check_survival_validity). A NA at a finite endpoint stays diagnostic.
    endpoints <- c(lower = lower, upper = upper)
    if (any(is.na(vals) & is.finite(endpoints))) {
      cli::cli_alert(sprintf(
        "The provided function could not be fully checked as a CDF over the range [%g, %g]: it returns %g at the lower bound and %g at the upper bound.",
        lower, upper, vals[["lower"]], vals[["upper"]]
      ))
    }
  } else if (abs(vals[["lower"]]) > tol || abs(vals[["upper"]] - 1) > tol) {
    cli::cli_alert(sprintf("The provided function appears not to be a valid CDF over the range [%g, %g]: it returns %g at the lower bound and %g at the upper bound.",
                           lower, upper, vals[["lower"]], vals[["upper"]]))
  }
  invisible(vals)
}

#' @noRd
check_pmf_normalization <- function(f, support, tol = 1e-3,
                                    action = c("warn", "abort")) {
  action <- match.arg(action)
  if (!ggfunction_check_enabled()) return(invisible(NA_real_))
  vals <- try(f(support), silent = TRUE)
  if (inherits(vals, "try-error")) {
    stop("Error evaluating the PMF over the provided support. Please check your function definition.")
  }
  total <- sum(vals)
  if (abs(total - 1) > tol) {
    msg <- sprintf(
      "The provided function sums to %.4f over the support [%g, %g], which is not equal to 1 within a tolerance of %.3f.",
      total, min(support), max(support), tol
    )
    if (identical(action, "abort")) {
      cli::cli_abort(c(
        msg,
        "i" = "For PMF-derived cumulative discrete functions, provide the full computational support via {.arg support}; use {.arg xlim} only to limit the displayed range."
      ))
    }
    cli::cli_alert(msg)
  }
  invisible(total)
}

#' Check normalization of precomputed probability masses.
#'
#' Structural invalidity (non-finite or negative masses) always aborts,
#' regardless of `options(ggfunction.check)`; only the soft non-unit-total
#' diagnostic is gated (spec 5.3, C-03).
#' @noRd
check_pmf_mass_normalization <- function(mass, tol = 1e-3) {
  if (any(!is.finite(mass)) || any(mass < 0)) {
    cli::cli_abort("{.arg fun} must return finite, non-negative mass values over the evaluation lattice.")
  }
  if (!ggfunction_check_enabled()) return(invisible(NA_real_))
  total <- sum(mass)
  if (abs(total - 1) > tol) {
    cli::cli_alert(sprintf(
      "The provided function sums to %.4f over the evaluation lattice, which is not equal to 1 within a tolerance of %.3f.",
      total, tol
    ))
  }
  invisible(total)
}

#' Resolve the computational support for discrete distribution geoms.
#' @noRd
discrete_support <- function(xlim = NULL, support = NULL, default = 0:10) {
  if (!is.null(support)) {
    return(sort(unique(support)))
  }
  if (is.null(xlim)) {
    return(default)
  }
  lo <- ceiling(xlim[1])
  hi <- floor(xlim[2])
  if (lo > hi) {
    return(numeric(0))
  }
  seq.int(lo, hi)
}

#' Filter already-computed discrete rows to the displayed support range.
#' @noRd
filter_discrete_xlim <- function(df, xlim = NULL, x_col = "x") {
  if (is.null(xlim) || nrow(df) == 0L) {
    return(df)
  }
  keep <- df[[x_col]] >= xlim[1] & df[[x_col]] <= xlim[2]
  df[keep, , drop = FALSE]
}

#' Smallest containing HDR level for discrete probability masses.
#'
#' Assigns each mass point the smallest of the requested highest density
#' regions that contains it, returned as an ordered factor whose first
#' (outermost) level collects points outside all requested regions, so that
#' an alpha mapping renders inner regions most opaque. HDRs are
#' threshold-based: all masses tied at the cutoff are included, so the
#' achieved coverage can exceed the target.
#' @noRd
discrete_hdr_probs <- function(mass, shade_hdr) {
  if (!is.numeric(shade_hdr) || length(shade_hdr) < 1 ||
      any(!is.finite(shade_hdr)) || any(shade_hdr <= 0) || any(shade_hdr >= 1)) {
    cli::cli_abort("{.arg shade_hdr} must be a numeric vector of coverage levels strictly between 0 and 1.")
  }
  total <- sum(mass)
  if (!is.finite(total) || total <= 0) {
    cli::cli_abort("{.arg shade_hdr}: mass values must have positive total mass.")
  }
  coverages <- sort(unique(shade_hdr))
  n <- length(mass)

  fhat_d  <- mass / total
  ord     <- order(mass, decreasing = TRUE)
  cumprob <- cumsum(fhat_d[ord])

  fmt <- format_hdr_coverages
  labels_in <- fmt(coverages)
  label_out <- paste0(">", labels_in[length(labels_in)])

  assigned <- rep(label_out, n)
  actual   <- numeric(length(coverages))
  for (i in rev(seq_along(coverages))) {
    k <- which(cumprob >= coverages[i])[1L]
    if (is.na(k)) k <- n
    cutoff    <- mass[ord[k]]
    # Use a relative tolerance so masses that are mathematically tied but
    # differ by floating-point rounding (e.g. symmetric products) are not
    # split across HDR levels.
    in_hdr    <- mass >= cutoff * (1 - 1e-9)
    actual[i] <- sum(fhat_d[in_hdr])
    assigned[in_hdr] <- labels_in[i]
  }

  if (any(abs(actual - coverages) > 0.005)) {
    pairs <- paste0(fmt(coverages), " -> ", fmt(pmin(actual, 1)), collapse = ", ")
    cli::cli_inform(c(
      "!" = "shade_hdr: exact coverage is not achievable for this discrete distribution.",
      "i" = "Using smallest HDRs with coverage >= each target: {pairs}."
    ))
  }

  factor(assigned, levels = c(label_out, rev(labels_in)), ordered = TRUE)
}

#' Collision-free labels for HDR coverage levels (spec C-07).
#'
#' Precision increases adaptively until every unique coverage has a unique
#' label and no strictly-interior coverage displays as a misleading 0% or
#' 100%. Common levels keep their familiar form (50%, 80%, 95%). Coverage
#' messages and factor levels share this formatter.
#' @noRd
format_hdr_coverages <- function(coverages) {
  distinct <- !duplicated(coverages)
  digits <- 0L
  repeat {
    shown <- round(coverages * 100, digits)
    labels <- paste0(formatC(shown, format = "f", digits = digits), "%")
    misleading <- (coverages > 0 & coverages < 1) & (shown <= 0 | shown >= 100)
    if ((!anyDuplicated(labels[distinct]) && !any(misleading)) || digits >= 10L) {
      return(labels)
    }
    digits <- digits + 1L
  }
}

#' Soft validity check for a discrete CDF over its computational support.
#'
#' Warns (without aborting) when the derived CDF leaves `[0, 1]`, is not
#' monotonically non-decreasing, or does not approach 1 at the top of the
#' support (e.g. a truncated support). Mirrors `check_cdf_normalization()` for
#' the discrete `fun`/`cdf_fun`/`survival_fun` paths, which otherwise accept
#' malformed input silently.
#' @noRd
check_discrete_cdf <- function(cdf_vals, tol = 1e-2, source = "fun") {
  if (!ggfunction_check_enabled()) return(invisible(cdf_vals))
  finite <- cdf_vals[is.finite(cdf_vals)]
  if (length(finite) == 0L) return(invisible(cdf_vals))
  if (any(finite < -tol) || any(finite > 1 + tol)) {
    cli::cli_alert(sprintf(
      "The CDF derived from `%s` leaves [0, 1] (range %.4f to %.4f).",
      source, min(finite), max(finite)
    ))
  }
  if (length(finite) > 1L && any(diff(finite) < -tol)) {
    cli::cli_alert(sprintf(
      "The CDF derived from `%s` is not monotonically non-decreasing over the support.",
      source
    ))
  }
  last <- finite[length(finite)]
  if (abs(last - 1) > tol) {
    cli::cli_alert(sprintf(
      "The CDF derived from `%s` reaches %.4f at the top of the support, not ~1; supply the full support via `support` if it is truncated.",
      source, last
    ))
  }
  invisible(cdf_vals)
}

#' Soft validity check for a survival function over the drawn grid.
#'
#' Warns (without aborting) when the survival function is not near 1 at the
#' lower support endpoint or near 0 at the upper support endpoint, or when the
#' computed values are not monotonically non-increasing. Mirrors
#' `check_cdf_normalization()` for the survival geoms.
#' @noRd
check_survival_validity <- function(f, y_vals, lower, upper, tol = 1e-2) {
  if (!ggfunction_check_enabled()) return(invisible(y_vals))

  # Numerically derived survival functions (e.g. hazard routes) may not be
  # evaluable at infinite support endpoints; skip the endpoint check silently
  # in that case rather than raising a spurious integration warning.
  vals <- suppressWarnings(
    try(c(lower = f(lower), upper = f(upper)), silent = TRUE)
  )
  if (!inherits(vals, "try-error") && !any(is.na(vals)) &&
      (abs(vals[["lower"]] - 1) > tol || abs(vals[["upper"]]) > tol)) {
    cli::cli_alert(sprintf("The provided function appears not to be a valid survival function over the range [%g, %g]: it returns %g at the lower bound and %g at the upper bound.",
                           lower, upper, vals[["lower"]], vals[["upper"]]))
  }

  finite <- y_vals[is.finite(y_vals)]
  if (length(finite) > 1L && any(diff(finite) > tol)) {
    cli::cli_alert("The computed survival values are not monotonically non-increasing; the provided function may not be a valid survival function.")
  }
  invisible(y_vals)
}

#' Soft validity check for computed quantile values.
#'
#' Warns (without aborting) when the quantile values are not monotonically
#' non-decreasing over the probability grid, or when finite values leave the
#' declared support.
#' @noRd
check_qf_validity <- function(q_vals, support = c(-Inf, Inf), tol = 1e-2) {
  if (!ggfunction_check_enabled()) return(invisible(q_vals))
  finite <- q_vals[is.finite(q_vals)]
  if (length(finite) == 0L) return(invisible(q_vals))
  if (length(finite) > 1L && any(diff(finite) < -tol)) {
    cli::cli_alert("The computed quantile values are not monotonically non-decreasing; the provided function may not be a valid quantile function.")
  }
  if (any(finite < support[1] - tol) || any(finite > support[2] + tol)) {
    cli::cli_alert(sprintf(
      "The computed quantile values leave the declared support [%g, %g] (range %.4f to %.4f).",
      support[1], support[2], min(finite), max(finite)
    ))
  }
  invisible(q_vals)
}

#' Soft validity check for computed hazard values.
#' @noRd
check_hf_validity <- function(y_vals, tol = 1e-2) {
  if (!ggfunction_check_enabled()) return(invisible(y_vals))
  finite <- y_vals[is.finite(y_vals)]
  if (length(finite) > 0L && any(finite < -tol)) {
    cli::cli_alert(sprintf(
      "The computed hazard values are negative (minimum %.4f); hazard functions must be non-negative.",
      min(finite)
    ))
  }
  invisible(y_vals)
}

#' Soft validity check for computed cumulative hazard values.
#' @noRd
check_chf_validity <- function(y_vals, tol = 1e-2) {
  if (!ggfunction_check_enabled()) return(invisible(y_vals))
  finite <- y_vals[is.finite(y_vals)]
  if (length(finite) == 0L) return(invisible(y_vals))
  if (any(finite < -tol)) {
    cli::cli_alert(sprintf(
      "The computed cumulative hazard values are negative (minimum %.4f); cumulative hazard functions must be non-negative.",
      min(finite)
    ))
  }
  if (length(finite) > 1L && any(diff(finite) < -tol)) {
    cli::cli_alert("The computed cumulative hazard values are not monotonically non-decreasing; the provided function may not be a valid cumulative hazard function.")
  }
  invisible(y_vals)
}

#' Per-group lollipop shading membership for a discrete PMF.
#'
#' Returns a logical vector marking which support points fall inside the
#' requested `p` / `p_lower`-`p_upper` shading region. Computed per group (on a
#' single group's masses), so the cumulative probabilities never cross group
#' boundaries. The upper tail (`lower.tail = FALSE`) includes the crossing
#' atom, mirroring the lower tail.
#' @noRd
pmf_shade_index <- function(y, p = NULL, lower.tail = TRUE,
                            p_lower = NULL, p_upper = NULL,
                            shade_outside = FALSE) {
  n <- length(y)
  if (n == 0L) return(logical(0))
  cum <- cumsum(y)
  if (!is.null(p_lower) && !is.null(p_upper)) {
    idx_lo <- which(cum >= p_lower)[1L]
    if (is.na(idx_lo)) idx_lo <- n
    idx_hi <- which(cum >= p_upper)[1L]
    if (is.na(idx_hi)) idx_hi <- n
    if (shade_outside) {
      seq_len(n) < idx_lo | seq_len(n) > idx_hi
    } else {
      seq_len(n) >= idx_lo & seq_len(n) <= idx_hi
    }
  } else if (!is.null(p)) {
    if (lower.tail) {
      idx <- which(cum >= p)[1L]
      if (is.na(idx)) idx <- n
      seq_len(n) <= idx
    } else {
      # Smallest suffix whose tail mass is >= p, inclusive of the crossing
      # atom, so the upper tail mirrors the lower tail.
      idx <- which(cum > (1 - p))[1L]
      if (is.na(idx)) idx <- n
      seq_len(n) >= idx
    }
  } else {
    rep(TRUE, n)
  }
}

#' @noRd
build_step_polygon <- function(x, y) {
  # Build step-function polygon vertices from (x, y) pairs.
  # Each step goes: (x[i], y[i]) -> (x[i+1], y[i]) -> (x[i+1], y[i+1])
  n <- length(x)
  if (n < 2) return(data.frame(x = x, y = y))

  px <- numeric(0)
  py <- numeric(0)

  for (i in seq_len(n - 1)) {
    px <- c(px, x[i], x[i + 1])
    py <- c(py, y[i], y[i])
  }
  px <- c(px, x[n])
  py <- c(py, y[n])

  data.frame(x = px, y = py)
}

#' @noRd
check_qf_sources <- function(fun, cdf_fun, pdf_fun, survival_fun,
                             hf_fun = NULL) {
  n_provided <- (!is.null(fun)) + (!is.null(cdf_fun)) +
    (!is.null(pdf_fun)) + (!is.null(survival_fun)) + (!is.null(hf_fun))
  if (n_provided == 0L) {
    cli::cli_abort("One of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, {.arg survival_fun}, or {.arg hf_fun} must be provided.")
  }
  if (n_provided > 1L) {
    cli::cli_abort("Supply only one of {.arg fun}, {.arg cdf_fun}, {.arg pdf_fun}, {.arg survival_fun}, or {.arg hf_fun}.")
  }
}

#' @noRd
make_qf_function <- function(fun = NULL, cdf_fun = NULL, pdf_fun = NULL,
                             survival_fun = NULL, hf_fun = NULL,
                             hf_lower = -Inf, args = NULL,
                             support = c(-Inf, Inf)) {
  args <- args %||% list()
  check_qf_sources(fun, cdf_fun, pdf_fun, survival_fun, hf_fun)

  as_qf_1d(
    fun = fun, cdf_fun = cdf_fun, pdf_fun = pdf_fun,
    survival_fun = survival_fun, hf_fun = hf_fun, args = args,
    support = support, hf_lower = hf_lower
  )
}

#' @noRd
order_stat_sample <- function(x, na.rm = FALSE, a = 1 / 2) {
  if (!is.numeric(a) || length(a) != 1L || !is.finite(a)) {
    cli::cli_abort("{.arg a} must be a single finite number.")
  }

  keep <- is.finite(x)
  n_removed <- sum(!keep)
  if (n_removed > 0L && !na.rm) {
    cli::cli_warn(c(
      "Removed {n_removed} non-finite observation{?s}.",
      "i" = "Set {.arg na.rm = TRUE} to suppress this warning."
    ))
  }

  x <- sort(x[keep])
  n <- length(x)
  if (n == 0L) {
    return(data.frame(sample = numeric(0), p = numeric(0), n = integer(0)))
  }

  p <- stats::ppoints(n, a = a)
  # Invalid offsets produce non-finite, out-of-range, or non-increasing
  # plotting positions; reject them before any band construction (D-05).
  if (any(!is.finite(p)) || any(p <= 0) || any(p >= 1) ||
      (n > 1L && any(diff(p) <= 0))) {
    cli::cli_abort(
      "{.arg a} = {a} produces invalid plotting positions; use an offset such as 1/2 or 3/8."
    )
  }

  data.frame(sample = x, p = p, n = n)
}

#' Resolve open_fill for discrete step-function geoms
#'
#' Returns the fill color for open (hollow) circles. Checks in order:
#' 1. Explicit user-supplied `open_fill`
#' 2. Theme-derived `.open_fill` column in data (ggplot2 >= 3.5)
#' 3. Global theme via `theme_get()` (fallback for older ggplot2)
#' @noRd
resolve_open_fill <- function(open_fill, data) {
  if (!is.null(open_fill)) return(open_fill)
  if (".open_fill" %in% names(data)) return(data$.open_fill[1])
  bg <- ggplot2::theme_get()$panel.background
  if (!inherits(bg, "element_blank") && !is.null(bg$fill) && !is.na(bg$fill)) bg$fill else "white"
}

#' Inject .open_fill column from the resolved plot theme
#'
#' Called from `use_defaults()` overrides in discrete geoms. In ggplot2 >= 3.5,
#' `use_defaults()` receives the fully resolved plot theme, so we can extract
#' the actual panel background color. This is a no-op when `theme` is NULL
#' (ggplot2 < 3.5 or missing theme).
#' @noRd
inject_open_fill <- function(data, theme) {
  if (!is.null(theme)) {
    bg <- ggplot2::calc_element("panel.background", theme)
    if (!inherits(bg, "element_blank") && !is.null(bg$fill) && !is.na(bg$fill)) {
      data$.open_fill <- bg$fill
    } else {
      data$.open_fill <- "white"
    }
  }
  data
}

#' @noRd
utils::globalVariables(c("x", "y", "z", "p", "level", "GeomLine", "pdf_fun", "cdf_fun",
                         "pmf_fun", "survival_fun", "qf_fun", "hf_fun", "ymin", "ymax",
                         "status", "prob", "probs", "qq_x", "qq_ymin", "qq_ymax"))

#' Pure step-segment construction for discrete distribution geoms (C-01).
#'
#' Builds the horizontal and vertical segment coordinates for a discrete
#' step function whose visible points are (x, y) with true predecessor
#' values y_prev (all in panel coordinates). The leading horizontal segment
#' sits at the first point's true predecessor value — which, in a narrowed
#' display window, is the value attained just before the window, not the
#' distribution's baseline.
#'
#' @return list(hori = data.frame(x, xend, y, yend, piece),
#'              vert = data.frame(x, xend, y, yend))
#'   `piece` indexes the atom each horizontal segment belongs to (the
#'   leading segment belongs to atom 1).
#' @noRd
discrete_step_segments <- function(x, y, y_prev, x_range) {
  n <- length(x)
  if (n == 0L) {
    empty <- data.frame(x = numeric(0), xend = numeric(0),
                        y = numeric(0), yend = numeric(0))
    return(list(hori = cbind(empty, piece = integer(0)), vert = empty))
  }
  hori <- data.frame(
    x = c(x_range[1], x),
    xend = c(x, x_range[2]),
    y = c(y_prev[1], y),
    yend = c(y_prev[1], y),
    piece = c(1L, seq_len(n))
  )
  vert <- data.frame(x = x, xend = x, y = y_prev, yend = y)
  list(hori = hori, vert = vert)
}

#' Shared draw_group implementation for the discrete CDF/survival step geoms.
#'
#' Consumes stat-provided predecessor metadata (`y_prev`, panel space) so the
#' first visible step of a narrowed window starts at the true predecessor
#' value (C-01); falls back to `baseline_default` (panel space) for stats
#' that do not provide metadata. Unshaded pieces are dimmed multiplicatively
#' (C-06). Non-finite predecessors (a transform-excluded baseline) are
#' clipped to the visible panel floor with one targeted warning.
#' @noRd
draw_discrete_step_group <- function(data, panel_params, coord,
                                     open_fill = NULL, vert_type = "dashed",
                                     show_points = NULL, show_vert = NULL,
                                     baseline_default = 0) {
  open_fill <- resolve_open_fill(open_fill, data)
  n <- nrow(data)
  if (is.null(show_points)) show_points <- n <= 50
  if (is.null(show_vert))   show_vert   <- n <= 50

  in_shade <- if ("in_shade" %in% names(data)) data$in_shade else rep(TRUE, n)
  jump <- if ("jump" %in% names(data)) data$jump else rep(TRUE, n)

  y_prev <- if ("y_prev" %in% names(data)) {
    data$y_prev
  } else {
    c(baseline_default, data$y[-n])
  }
  if (any(!is.finite(y_prev))) {
    floor_y <- baseline_draw_value(NA_real_, panel_params)
    y_prev[!is.finite(y_prev)] <- floor_y
  }

  # Empirical stats carry observation-domain anchors: the curve stops at its
  # own maximum follow-up rather than extending to the panel edge, and
  # anchors draw no jumps or endpoint circles (D-02).
  has_domain <- "domain_anchor" %in% names(data)
  x_hi <- if (has_domain) max(data$x) else panel_params$x.range[2]
  segs <- discrete_step_segments(data$x, data$y, y_prev,
                                 c(panel_params$x.range[1], x_hi))

  data_hori <- data[segs$hori$piece, , drop = FALSE]
  data_hori$x <- segs$hori$x
  data_hori$xend <- segs$hori$xend
  data_hori$y <- segs$hori$y
  data_hori$yend <- segs$hori$yend
  data_hori$alpha <- dim_alpha(data_hori$alpha, in_shade[segs$hori$piece])
  data_hori <- data_hori[data_hori$x != data_hori$xend, , drop = FALSE]

  data_vert <- data
  data_vert$xend <- segs$vert$xend
  data_vert$y <- segs$vert$y
  data_vert$yend <- segs$vert$yend
  data_vert$alpha <- dim_alpha(data_vert$alpha, in_shade)
  data_vert <- data_vert[jump, , drop = FALSE]

  coord_hori <- coord$transform(data_hori, panel_params)
  coord_vert <- coord$transform(data_vert, panel_params)

  grobs <- list()

  if (nrow(coord_hori) > 0L) {
    grobs$hori <- grid::segmentsGrob(
      coord_hori$x, coord_hori$y, coord_hori$xend, coord_hori$yend,
      default.units = "native",
      gp = grid::gpar(
        col = scales::alpha(coord_hori$colour, coord_hori$alpha),
        lwd = coord_hori$linewidth * .pt,
        lty = coord_hori$linetype
      )
    )
  }

  if (show_vert && nrow(coord_vert) > 0L) {
    grobs$vert <- grid::segmentsGrob(
      coord_vert$x, coord_vert$y, coord_vert$xend, coord_vert$yend,
      default.units = "native",
      gp = grid::gpar(
        col = scales::alpha(coord_vert$colour, coord_vert$alpha),
        lwd = coord_vert$linewidth * .pt,
        lty = vert_type
      )
    )
  }

  if (show_points && nrow(coord_vert) > 0L) {
    # Open circle at the pre-jump value (left limit); closed circle at the
    # value the function attains at x[k].
    grobs$open <- grid::pointsGrob(
      coord_vert$x, coord_vert$y,
      default.units = "native",
      pch = 21,
      gp = grid::gpar(
        col      = scales::alpha(coord_vert$colour, coord_vert$alpha),
        fill     = open_fill,
        fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
        lwd      = coord_vert$stroke * .stroke / 2
      )
    )

    grobs$closed <- grid::pointsGrob(
      coord_vert$xend, coord_vert$yend,
      pch = coord_vert$shape,
      default.units = "native",
      gp = grid::gpar(
        col      = scales::alpha(coord_vert$colour, coord_vert$alpha),
        fill     = scales::alpha(coord_vert$colour, coord_vert$alpha),
        fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke / 2,
        lwd      = coord_vert$stroke * .stroke / 2
      )
    )
  }

  grid::gTree(children = do.call(grid::gList, grobs))
}
