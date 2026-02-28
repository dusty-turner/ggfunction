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
check_pdf_normalization <- function(f, lower, upper, tol = 1e-3) {
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
check_pmf_normalization <- function(f, support, tol = 1e-3) {
  vals <- try(f(support), silent = TRUE)
  if (inherits(vals, "try-error")) {
    stop("Error evaluating the PMF over the provided support. Please check your function definition.")
  }
  total <- sum(vals)
  if (abs(total - 1) > tol) {
    cli::cli_alert(sprintf("The provided function sums to %.4f over the support [%g, %g], which is not equal to 1 within a tolerance of %.3f.",
                           total, min(support), max(support), tol))
  }
  invisible(total)
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
utils::globalVariables(c("x", "y", "z", "p", "level", "GeomLine", "pdf_fun", "cdf_fun",
                         "pmf_fun", "survival_fun", "qf_fun", "ymin", "ymax"))
