# R/internal_utils.R

#' @noRd
ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    tibble0(group = 1, .size = 1)
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
tibble0 <- function(...) {
  data.frame(..., .name_repair = "minimal")
}

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

  if (abs(vals[["lower"]]) > tol || abs(vals[["upper"]] - 1) > tol) {
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
#' @noRd
check_pmf_mass_normalization <- function(mass, tol = 1e-3) {
  if (!ggfunction_check_enabled()) return(invisible(NA_real_))
  if (any(!is.finite(mass)) || any(mass < 0)) {
    cli::cli_abort("{.arg fun} must return finite, non-negative mass values over the evaluation lattice.")
  }
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
  seq(ceiling(xlim[1]), floor(xlim[2]))
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

#' Smallest-HDR membership indicator for discrete probability masses.
#' @noRd
discrete_hdr_indicator <- function(mass, shade_hdr = NULL) {
  n <- length(mass)
  if (is.null(shade_hdr)) {
    return(rep(TRUE, n))
  }

  fhat_d  <- mass / sum(mass)
  ord     <- order(mass, decreasing = TRUE)
  cumprob <- cumsum(fhat_d[ord])
  k       <- which(cumprob >= shade_hdr)[1L]
  if (is.na(k)) k <- n
  actual  <- cumprob[k]
  cutoff  <- mass[ord[k]]

  if (abs(actual - shade_hdr) > 0.005) {
    fmt <- function(x) paste0(round(x * 100, 1), "%")
    cli::cli_inform(c(
      "!" = "shade_hdr: {fmt(shade_hdr)} is not exactly achievable for this discrete distribution.",
      "i" = "Using smallest HDR with coverage >= {fmt(shade_hdr)}: actual coverage = {fmt(actual)}."
    ))
  }

  mass >= cutoff
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

#' Convert a PDF function to a CDF function via numerical integration
#' @noRd
pdf_to_cdf <- function(pdf_fun, lower = -Inf) {
  function(x) {
    vapply(x, function(xi) {
      res <- try(
        stats::integrate(pdf_fun, lower = lower, upper = xi, stop.on.error = FALSE),
        silent = TRUE
      )
      if (inherits(res, "try-error")) NA_real_ else res$value
    }, numeric(1))
  }
}

#' Convert a CDF function to a PDF function via central finite differences
#' @noRd
cdf_to_pdf <- function(cdf_fun, h = 1e-5) {
  function(x) {
    (cdf_fun(x + h) - cdf_fun(x - h)) / (2 * h)
  }
}

#' Convert a CDF function to a quantile function via root-finding
#' @noRd
cdf_to_qf <- function(cdf_fun, search_lower = -10, search_upper = 10) {
  function(p) {
    vapply(p, function(pi) {
      if (pi <= 0) return(-Inf)
      if (pi >= 1) return(Inf)

      lo <- search_lower
      hi <- search_upper

      # Adaptively widen bounds until they bracket the target
      for (i in 1:25) {
        f_lo <- cdf_fun(lo)
        if (!is.na(f_lo) && f_lo <= pi) break
        lo <- lo * 2
      }
      for (i in 1:25) {
        f_hi <- cdf_fun(hi)
        if (!is.na(f_hi) && f_hi >= pi) break
        hi <- hi * 2
      }

      res <- try(
        stats::uniroot(function(x) cdf_fun(x) - pi, lower = lo, upper = hi,
                       tol = .Machine$double.eps^0.5),
        silent = TRUE
      )
      if (inherits(res, "try-error")) NA_real_ else res$root
    }, numeric(1))
  }
}

#' Convert a survival function to a CDF function via exact arithmetic
#' @noRd
survival_to_cdf <- function(survival_fun) {
  function(x) 1 - survival_fun(x)
}

#' Convert a quantile function to a CDF function via interpolation
#' @noRd
qf_to_cdf <- function(qf_fun, n = 10000) {
  p_grid <- seq(1 / (n + 1), n / (n + 1), length.out = n)
  x_grid <- qf_fun(p_grid)
  stats::approxfun(x_grid, p_grid, rule = 2)
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
                             hf_lower = -Inf, args = NULL) {
  args <- args %||% list()
  check_qf_sources(fun, cdf_fun, pdf_fun, survival_fun, hf_fun)

  if (!is.null(cdf_fun)) {
    cdf_injected <- function(x) rlang::inject(cdf_fun(x, !!!args))
    cdf_to_qf(cdf_injected)
  } else if (!is.null(pdf_fun)) {
    pdf_injected <- function(x) rlang::inject(pdf_fun(x, !!!args))
    cdf_to_qf(pdf_to_cdf(pdf_injected))
  } else if (!is.null(survival_fun)) {
    surv_injected <- function(x) rlang::inject(survival_fun(x, !!!args))
    cdf_to_qf(survival_to_cdf(surv_injected))
  } else if (!is.null(hf_fun)) {
    hf_injected <- function(x) rlang::inject(hf_fun(x, !!!args))
    cdf_to_qf(hf_to_cdf(hf_injected, lower = hf_lower))
  } else {
    function(p) rlang::inject(fun(p, !!!args))
  }
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

  data.frame(sample = x, p = stats::ppoints(n, a = a), n = n)
}

#' Convert a hazard function to a cumulative hazard function via numerical integration
#'
#' Computes the cumulative hazard H(x) = integral of h(t) from lower to x,
#' @noRd
hf_to_chf <- function(hf_fun, lower = -Inf) {
  if (!is.numeric(lower) || length(lower) != 1L || is.na(lower) || lower == Inf) {
    cli::cli_abort("{.arg lower} must be a finite number or {.code -Inf}.")
  }
  function(x) {
    vapply(x, function(xi) {
      if (is.finite(lower) && xi <= lower) {
        return(0)
      }
      res <- try(
        stats::integrate(hf_fun, lower = lower, upper = xi, stop.on.error = FALSE),
        silent = TRUE
      )
      if (inherits(res, "try-error")) NA_real_ else res$value
    }, numeric(1))
  }
}

#' Convert a hazard function to a CDF function via numerical integration
#'
#' Computes the cumulative hazard H(x) = integral of h(t) from lower to x,
#' then returns F(x) = 1 - exp(-H(x)).
#' @noRd
hf_to_cdf <- function(hf_fun, lower = -Inf) {
  chf_fun <- hf_to_chf(hf_fun, lower = lower)
  function(x) 1 - exp(-chf_fun(x))
}

#' Convert a hazard function to a PDF function
#'
#' Uses the relationship f(x) = h(x) * exp(-H(x)) where H(x) is the
#' cumulative hazard. More accurate than hf_to_cdf + cdf_to_pdf since
#' it avoids nested numerical differentiation.
#' @noRd
hf_to_pdf <- function(hf_fun, lower = -Inf) {
  chf_fun <- hf_to_chf(hf_fun, lower = lower)
  function(x) {
    vapply(x, function(xi) {
      if (is.finite(lower) && xi < lower) {
        return(0)
      }
      h_x <- hf_fun(xi)
      H_x <- chf_fun(xi)
      if (is.na(H_x)) return(NA_real_)
      h_x * exp(-H_x)
    }, numeric(1))
  }
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
                         "status", "prob"))
