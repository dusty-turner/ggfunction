#' Validate a one-dimensional computational support interval.
#' @noRd
validate_support_1d <- function(support = c(-Inf, Inf), arg = "support") {
  if (is.null(support)) support <- c(-Inf, Inf)
  if (!is.numeric(support) || length(support) != 2L ||
      any(is.na(support)) || support[1] >= support[2]) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector of length 2 with increasing endpoints.")
  }
  support
}

#' Validate a named argument list used for distribution functions.
#' @noRd
validate_named_args <- function(args = list(), arg = "args") {
  args <- args %||% list()
  if (!is.list(args)) {
    cli::cli_abort("{.arg {arg}} must be a list.")
  }
  if (length(args) > 0L) {
    nm <- names(args)
    if (is.null(nm) || any(!nzchar(nm))) {
      cli::cli_abort("{.arg {arg}} must be a named list.")
    }
  }
  args
}

#' Attach route metadata to a derived distribution function.
#' @noRd
tag_distribution_route <- function(f, route, approximate = FALSE) {
  attr(f, "ggfunction_route") <- route
  attr(f, "ggfunction_approximate") <- isTRUE(approximate)
  f
}

#' Wrap a user function with a named argument list.
#' @noRd
distribution_fun <- function(fun, args = list(), arg = "fun") {
  if (!is.function(fun)) {
    cli::cli_abort("{.arg {arg}} must be a function.")
  }
  args <- validate_named_args(args)
  force(fun)
  force(args)
  function(x) do.call(fun, c(list(x), args))
}

#' Validate mutually exclusive distribution source arguments.
#' @noRd
validate_distribution_sources <- function(sources, allowed_bundles = list(),
                                          what = "distribution") {
  provided <- names(sources)[!vapply(sources, is.null, logical(1))]

  if (length(provided) == 0L) {
    cli::cli_abort("One of {.arg {names(sources)}} must be provided.")
  }

  if (length(provided) == 1L) {
    return(provided)
  }

  for (bundle in allowed_bundles) {
    if (setequal(provided, bundle)) {
      return(provided)
    }
  }

  cli::cli_abort(c(
    "Conflicting {what} sources: {.arg {provided}}.",
    "i" = "Supply exactly one source, except for documented source bundles such as {.arg pdf_fun} + {.arg cdf_fun} for hazards."
  ))
}

#' Resolve the lower endpoint for hazard integrations.
#' @noRd
resolve_hf_lower <- function(hf_lower = -Inf, support = c(-Inf, Inf)) {
  support <- validate_support_1d(support)
  if (!is.numeric(hf_lower) || length(hf_lower) != 1L ||
      is.na(hf_lower) || identical(hf_lower, Inf)) {
    cli::cli_abort("{.arg hf_lower} must be a single finite number or {.code -Inf}.")
  }
  if (identical(hf_lower, -Inf) && is.finite(support[1])) {
    support[1]
  } else {
    hf_lower
  }
}

#' Convert a PDF function to a CDF function by numerical integration.
#' @noRd
pdf_to_cdf <- function(pdf_fun, lower = -Inf, support = NULL) {
  support <- validate_support_1d(support %||% c(lower, Inf))
  lower <- support[1]
  upper_support <- support[2]
  warned <- FALSE

  out <- function(x) {
    vapply(x, function(xi) {
      if (is.na(xi)) return(NA_real_)
      if (xi <= lower) return(0)
      if (xi >= upper_support) return(1)

      res <- try(
        stats::integrate(
          pdf_fun,
          lower = lower,
          upper = xi,
          stop.on.error = FALSE
        ),
        silent = TRUE
      )

      failed <- inherits(res, "try-error") ||
        !is.null(res$message) && !identical(res$message, "OK")
      if (failed) {
        if (!warned) {
          warned <<- TRUE
          cli::cli_warn(c(
            "Numerical integration failed while deriving a CDF from a PDF.",
            "i" = "Check the supplied density and {.arg support}; returning {.code NA} at failed points."
          ))
        }
        NA_real_
      } else {
        res$value
      }
    }, numeric(1))
  }

  tag_distribution_route(out, "pdf->cdf", approximate = TRUE)
}

#' Convert a CDF function to a PDF function by adaptive finite differences.
#' @noRd
cdf_to_pdf <- function(cdf_fun, h = 1e-5, support = c(-Inf, Inf),
                       negative_tol = 1e-8) {
  support <- validate_support_1d(support)

  out <- function(x) {
    vapply(x, function(xi) {
      if (is.na(xi) || !is.finite(xi)) return(NA_real_)
      if (xi < support[1] || xi > support[2]) return(0)

      step <- h * max(1, abs(xi))
      lo <- xi - step
      hi <- xi + step

      if (is.finite(support[1]) && lo < support[1]) {
        lo <- xi
        hi <- min(support[2], xi + step)
      } else if (is.finite(support[2]) && hi > support[2]) {
        lo <- max(support[1], xi - step)
        hi <- xi
      }

      if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(NA_real_)
      val <- (cdf_fun(hi) - cdf_fun(lo)) / (hi - lo)
      if (is.finite(val) && val < 0 && abs(val) <= negative_tol) val <- 0
      val
    }, numeric(1))
  }

  tag_distribution_route(out, "cdf->pdf", approximate = TRUE)
}

#' Convert a CDF function to a quantile function by root-finding.
#' @noRd
cdf_to_qf <- function(cdf_fun, support = c(-Inf, Inf),
                      search_lower = NULL, search_upper = NULL,
                      max_expand = 60) {
  if (!is.null(search_lower) || !is.null(search_upper)) {
    support <- c(search_lower %||% support[1], search_upper %||% support[2])
  }
  support <- validate_support_1d(support)

  bracket_one <- function(p) {
    lo <- support[1]
    hi <- support[2]

    if (!is.finite(lo)) lo <- search_lower %||% -1
    if (!is.finite(hi)) hi <- search_upper %||% 1
    if (lo >= hi) {
      lo <- -1
      hi <- 1
    }

    for (i in seq_len(max_expand)) {
      f_lo <- suppressWarnings(cdf_fun(lo))
      if (is.finite(f_lo) && f_lo <= p) break
      if (is.finite(support[1])) return(NULL)
      lo <- if (lo < 0) lo * 2 else -max(1, abs(lo) * 2)
    }

    for (i in seq_len(max_expand)) {
      f_hi <- suppressWarnings(cdf_fun(hi))
      if (is.finite(f_hi) && f_hi >= p) break
      if (is.finite(support[2])) return(NULL)
      hi <- if (hi > 0) hi * 2 else max(1, abs(hi) * 2)
    }

    f_lo <- suppressWarnings(cdf_fun(lo))
    f_hi <- suppressWarnings(cdf_fun(hi))
    if (!is.finite(f_lo) || !is.finite(f_hi) || f_lo > p || f_hi < p) {
      return(NULL)
    }
    c(lo, hi)
  }

  out <- function(p) {
    vapply(p, function(pi) {
      if (is.na(pi)) return(NA_real_)
      if (pi < 0 || pi > 1) {
        cli::cli_abort("{.arg p} values must be between 0 and 1.")
      }
      if (pi == 0) return(support[1])
      if (pi == 1) return(support[2])

      br <- bracket_one(pi)
      if (is.null(br)) {
        cli::cli_abort(c(
          "Could not bracket quantile probability {.val {pi}}.",
          "i" = "Provide finite {.arg support} bounds for this distribution."
        ))
      }

      res <- try(
        stats::uniroot(
          function(xx) cdf_fun(xx) - pi,
          lower = br[1],
          upper = br[2],
          tol = sqrt(.Machine$double.eps)
        ),
        silent = TRUE
      )
      if (inherits(res, "try-error")) {
        cli::cli_abort("Root-finding failed while deriving a quantile function from a CDF.")
      }
      res$root
    }, numeric(1))
  }

  tag_distribution_route(out, "cdf->qf", approximate = TRUE)
}

#' Convert a survival function to a CDF function.
#' @noRd
survival_to_cdf <- function(survival_fun) {
  out <- function(x) 1 - survival_fun(x)
  tag_distribution_route(out, "survival->cdf", approximate = FALSE)
}

#' Convert a CDF function to a survival function.
#' @noRd
cdf_to_survival <- function(cdf_fun) {
  out <- function(x) pmax(0, 1 - cdf_fun(x))
  tag_distribution_route(out, "cdf->survival", approximate = FALSE)
}

#' Convert a quantile function to a CDF function by monotone interpolation.
#' @noRd
qf_to_cdf <- function(qf_fun, n = 10000, support = c(-Inf, Inf)) {
  support <- validate_support_1d(support)
  k <- seq_len(n)
  p_grid <- sort((1 - cos((2 * k - 1) * pi / (2 * n))) / 2)
  x_grid <- qf_fun(p_grid)

  ok <- is.finite(x_grid) & is.finite(p_grid)
  x_grid <- x_grid[ok]
  p_grid <- p_grid[ok]
  ord <- order(x_grid, p_grid)
  x_grid <- x_grid[ord]
  p_grid <- p_grid[ord]

  keep <- !duplicated(x_grid)
  x_grid <- x_grid[keep]
  p_grid <- p_grid[keep]

  if (length(x_grid) < 2L || any(diff(x_grid) <= 0)) {
    cli::cli_abort("The supplied quantile function is not strictly increasing on the interpolation grid.")
  }

  interpolator <- stats::approxfun(x_grid, p_grid, rule = 2, ties = "ordered")
  out <- function(x) {
    vals <- interpolator(x)
    if (is.finite(support[1])) vals[x <= support[1]] <- 0
    if (is.finite(support[2])) vals[x >= support[2]] <- 1
    pmin(1, pmax(0, vals))
  }

  tag_distribution_route(out, "qf->cdf", approximate = TRUE)
}

#' Convert a hazard function to a cumulative hazard function.
#'
#' The effective hazard origin is `max(support[1], hf_lower)` (via
#' `resolve_hf_lower()`). Exact endpoint values are returned without
#' attempting numerical integration: `H(x) = 0` at/below the
#' origin and `H(x) = Inf` at/above the upper support endpoint, where the
#' cumulative hazard of a distribution reaching the end of its support
#' necessarily diverges.
#' @noRd
hf_to_chf <- function(hf_fun, lower = -Inf, support = NULL) {
  support <- validate_support_1d(support %||% c(lower, Inf))
  origin <- resolve_hf_lower(lower, support)
  if (origin >= support[2]) {
    cli::cli_abort("The hazard origin ({.arg hf_lower}) must lie below the upper support endpoint.")
  }
  warned <- FALSE

  out <- function(x) {
    vapply(x, function(xi) {
      if (is.na(xi)) return(NA_real_)
      if (xi <= origin) return(0)
      if (xi >= support[2]) return(Inf)

      res <- try(
        stats::integrate(
          hf_fun,
          lower = origin,
          upper = xi,
          stop.on.error = FALSE
        ),
        silent = TRUE
      )
      failed <- inherits(res, "try-error") ||
        !is.null(res$message) && !identical(res$message, "OK")
      if (failed) {
        if (!warned) {
          warned <<- TRUE
          cli::cli_warn(c(
            "Numerical integration failed while deriving a cumulative hazard.",
            "i" = "Provide finite {.arg hf_lower} or {.arg support} when the hazard origin is ambiguous."
          ))
        }
        NA_real_
      } else {
        res$value
      }
    }, numeric(1))
  }

  tag_distribution_route(out, "hf->chf", approximate = TRUE)
}

#' Convert a hazard function to a CDF function via `F(x) = -expm1(-H(x))`,
#' which is exact for small cumulative hazards and returns exactly 1 when
#' `H` is infinite.
#' @noRd
hf_to_cdf <- function(hf_fun, lower = -Inf, support = NULL) {
  chf_fun <- hf_to_chf(hf_fun, lower = lower, support = support)
  out <- function(x) -expm1(-chf_fun(x))
  tag_distribution_route(out, "hf->cdf", approximate = TRUE)
}

#' Convert a hazard function to a PDF function. Where the cumulative hazard
#' is infinite (at/beyond the upper support endpoint) the density is exactly
#' zero, not `NA`.
#' @noRd
hf_to_pdf <- function(hf_fun, lower = -Inf, support = NULL) {
  support <- validate_support_1d(support %||% c(lower, Inf))
  origin <- resolve_hf_lower(lower, support)
  chf_fun <- hf_to_chf(hf_fun, lower = origin, support = support)

  out <- function(x) {
    vapply(x, function(xi) {
      if (is.na(xi)) return(NA_real_)
      if (xi < origin) return(0)
      if (xi > support[2]) return(0)
      H_x <- chf_fun(xi)
      if (identical(H_x, Inf)) return(0)
      if (!is.finite(H_x)) return(NA_real_)
      hf_fun(xi) * exp(-H_x)
    }, numeric(1))
  }

  tag_distribution_route(out, "hf->pdf", approximate = TRUE)
}

#' Convert a PDF to a survival function by upper-tail integration,
#' `S(x) = integral of the density from x to the upper support endpoint`.
#' Numerically stable deep in the upper tail, where `1 - F(x)` suffers
#' catastrophic cancellation.
#' @noRd
pdf_to_survival <- function(pdf_fun, support = c(-Inf, Inf)) {
  support <- validate_support_1d(support)
  warned <- FALSE

  out <- function(x) {
    vapply(x, function(xi) {
      if (is.na(xi)) return(NA_real_)
      if (xi <= support[1]) return(1)
      if (xi >= support[2]) return(0)

      res <- try(
        stats::integrate(
          pdf_fun,
          lower = xi,
          upper = support[2],
          stop.on.error = FALSE
        ),
        silent = TRUE
      )
      failed <- inherits(res, "try-error") ||
        !is.null(res$message) && !identical(res$message, "OK")
      if (failed) {
        if (!warned) {
          warned <<- TRUE
          cli::cli_warn(c(
            "Numerical upper-tail integration failed while deriving a survival function from a PDF.",
            "i" = "Check the supplied density and {.arg support}; returning {.code NA} at failed points."
          ))
        }
        NA_real_
      } else {
        min(1, max(0, res$value))
      }
    }, numeric(1))
  }

  tag_distribution_route(out, "pdf->survival", approximate = TRUE)
}

#' Convert a survival function to a hazard via the logarithmic derivative
#' `h(x) = -d/dx log S(x)`, which stays exact deep in the tail where the
#' `f/S` ratio of reconstructed quantities collapses. Central
#' differences, one-sided at finite support boundaries.
#' @noRd
survival_to_hf <- function(survival_fun, support = c(-Inf, Inf), h = 1e-5,
                           negative_tol = 1e-8) {
  support <- validate_support_1d(support)

  out <- function(x) {
    vapply(x, function(xi) {
      if (is.na(xi) || !is.finite(xi)) return(NA_real_)
      if (xi < support[1]) return(0)
      if (xi > support[2]) return(NaN)

      step <- h * max(1, abs(xi))
      lo <- xi - step
      hi <- xi + step
      if (is.finite(support[1]) && lo < support[1]) {
        lo <- xi
        hi <- min(support[2], xi + step)
      } else if (is.finite(support[2]) && hi > support[2]) {
        lo <- max(support[1], xi - step)
        hi <- xi
      }
      if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(NaN)

      s_lo <- survival_fun(lo)
      s_hi <- survival_fun(hi)
      if (!is.finite(s_lo) || !is.finite(s_hi) || s_lo <= 0 || s_hi <= 0) {
        return(NaN)
      }
      val <- -(log(s_hi) - log(s_lo)) / (hi - lo)
      if (is.finite(val) && val < 0 && abs(val) <= negative_tol) val <- 0
      val
    }, numeric(1))
  }

  tag_distribution_route(out, "survival->hf", approximate = TRUE)
}

#' Convert a survival function to a cumulative hazard.
#' @noRd
survival_to_chf <- function(survival_fun) {
  warned <- FALSE
  out <- function(x) {
    s <- survival_fun(x)
    bad <- is.finite(s) & s > 1
    if (any(bad, na.rm = TRUE) && !warned) {
      warned <<- TRUE
      cli::cli_warn("Survival values greater than 1 were produced while deriving cumulative hazard.")
    }
    ifelse(s <= 0, Inf, -log(s))
  }
  tag_distribution_route(out, "survival->chf", approximate = FALSE)
}

#' Convert a CDF function to a cumulative hazard.
#' @noRd
cdf_to_chf <- function(cdf_fun) {
  out <- function(x) {
    p <- cdf_fun(x)
    ifelse(p >= 1, Inf, -log1p(-p))
  }
  tag_distribution_route(out, "cdf->chf", approximate = FALSE)
}

#' Convert a cumulative hazard function to survival.
#' @noRd
chf_to_survival <- function(chf_fun) {
  out <- function(x) exp(-chf_fun(x))
  tag_distribution_route(out, "chf->survival", approximate = FALSE)
}

#' Convert any supported source to a PDF.
#' @noRd
as_pdf_1d <- function(fun = NULL, cdf_fun = NULL, survival_fun = NULL,
                      qf_fun = NULL, hf_fun = NULL, args = list(),
                      support = c(-Inf, Inf), hf_lower = -Inf) {
  support <- validate_support_1d(support)
  args <- validate_named_args(args)
  validate_distribution_sources(
    list(fun = fun, cdf_fun = cdf_fun, survival_fun = survival_fun,
         qf_fun = qf_fun, hf_fun = hf_fun),
    what = "PDF"
  )

  if (!is.null(fun)) {
    return(tag_distribution_route(distribution_fun(fun, args), "pdf", FALSE))
  }
  if (!is.null(cdf_fun)) {
    return(cdf_to_pdf(distribution_fun(cdf_fun, args, "cdf_fun"), support = support))
  }
  if (!is.null(survival_fun)) {
    cdf <- survival_to_cdf(distribution_fun(survival_fun, args, "survival_fun"))
    return(cdf_to_pdf(cdf, support = support))
  }
  if (!is.null(qf_fun)) {
    cdf <- qf_to_cdf(distribution_fun(qf_fun, args, "qf_fun"), support = support)
    return(cdf_to_pdf(cdf, support = support))
  }

  hf <- distribution_fun(hf_fun, args, "hf_fun")
  hf_to_pdf(hf, lower = hf_lower, support = support)
}

#' Convert any supported source to a CDF.
#' @noRd
as_cdf_1d <- function(fun = NULL, pdf_fun = NULL, survival_fun = NULL,
                      qf_fun = NULL, hf_fun = NULL, args = list(),
                      support = c(-Inf, Inf), hf_lower = -Inf) {
  support <- validate_support_1d(support)
  args <- validate_named_args(args)
  validate_distribution_sources(
    list(fun = fun, pdf_fun = pdf_fun, survival_fun = survival_fun,
         qf_fun = qf_fun, hf_fun = hf_fun),
    what = "CDF"
  )

  if (!is.null(fun)) {
    return(tag_distribution_route(distribution_fun(fun, args), "cdf", FALSE))
  }
  if (!is.null(pdf_fun)) {
    return(pdf_to_cdf(distribution_fun(pdf_fun, args, "pdf_fun"), support = support))
  }
  if (!is.null(survival_fun)) {
    return(survival_to_cdf(distribution_fun(survival_fun, args, "survival_fun")))
  }
  if (!is.null(qf_fun)) {
    return(qf_to_cdf(distribution_fun(qf_fun, args, "qf_fun"), support = support))
  }

  hf <- distribution_fun(hf_fun, args, "hf_fun")
  hf_to_cdf(hf, lower = hf_lower, support = support)
}

#' Convert any supported source to a quantile function.
#' @noRd
as_qf_1d <- function(fun = NULL, cdf_fun = NULL, pdf_fun = NULL,
                     survival_fun = NULL, hf_fun = NULL, args = list(),
                     support = c(-Inf, Inf), hf_lower = -Inf) {
  support <- validate_support_1d(support)
  args <- validate_named_args(args)
  validate_distribution_sources(
    list(fun = fun, cdf_fun = cdf_fun, pdf_fun = pdf_fun,
         survival_fun = survival_fun, hf_fun = hf_fun),
    what = "quantile"
  )

  if (!is.null(fun)) {
    return(tag_distribution_route(distribution_fun(fun, args), "qf", FALSE))
  }
  cdf <- as_cdf_1d(
    fun = cdf_fun, pdf_fun = pdf_fun, survival_fun = survival_fun,
    qf_fun = NULL, hf_fun = hf_fun, args = args, support = support,
    hf_lower = hf_lower
  )
  cdf_to_qf(cdf, support = support)
}

#' Convert any supported source to a survival function.
#' @noRd
as_survival_1d <- function(fun = NULL, cdf_fun = NULL, pdf_fun = NULL,
                           qf_fun = NULL, hf_fun = NULL, args = list(),
                           support = c(-Inf, Inf), hf_lower = -Inf) {
  support <- validate_support_1d(support)
  args <- validate_named_args(args)
  validate_distribution_sources(
    list(fun = fun, cdf_fun = cdf_fun, pdf_fun = pdf_fun,
         qf_fun = qf_fun, hf_fun = hf_fun),
    what = "survival"
  )

  if (!is.null(fun)) {
    return(tag_distribution_route(distribution_fun(fun, args), "survival", FALSE))
  }
  cdf <- as_cdf_1d(
    fun = cdf_fun, pdf_fun = pdf_fun, survival_fun = NULL,
    qf_fun = qf_fun, hf_fun = hf_fun, args = args, support = support,
    hf_lower = hf_lower
  )
  cdf_to_survival(cdf)
}

#' Convert any supported source to a hazard function.
#' @noRd
as_hf_1d <- function(fun = NULL, pdf_fun = NULL, cdf_fun = NULL,
                     survival_fun = NULL, qf_fun = NULL, args = list(),
                     pdf_args = NULL, cdf_args = NULL,
                     support = c(-Inf, Inf), hf_lower = -Inf) {
  support <- validate_support_1d(support)
  args <- validate_named_args(args)
  pdf_args <- validate_named_args(pdf_args %||% list(), "pdf_args")
  cdf_args <- validate_named_args(cdf_args %||% list(), "cdf_args")

  validate_distribution_sources(
    list(fun = fun, pdf_fun = pdf_fun, cdf_fun = cdf_fun,
         survival_fun = survival_fun, qf_fun = qf_fun),
    allowed_bundles = list(c("pdf_fun", "cdf_fun")),
    what = "hazard"
  )

  if (!is.null(fun)) {
    return(tag_distribution_route(distribution_fun(fun, args), "hf", FALSE))
  }

  if (!is.null(survival_fun)) {
    # Logarithmic derivative: h(x) = -d/dx log S(x). Exact deep in the tail,
    # where reconstructing f and S separately cancels to zero.
    S <- distribution_fun(survival_fun, args, "survival_fun")
    return(survival_to_hf(S, support = support))
  }

  if (!is.null(pdf_fun) && !is.null(cdf_fun)) {
    f <- distribution_fun(pdf_fun, utils::modifyList(args, pdf_args), "pdf_fun")
    F <- distribution_fun(cdf_fun, utils::modifyList(args, cdf_args), "cdf_fun")
    # Use 1 - F while it is well-conditioned; fall back to stable upper-tail
    # integration of the density once cancellation sets in.
    S_tail <- pdf_to_survival(f, support = support)
    out <- function(x) {
      vapply(x, function(xi) {
        if (is.na(xi)) return(NA_real_)
        s <- 1 - F(xi)
        if (is.na(s)) return(NA_real_)
        if (s > 1e-10) return(f(xi) / s)
        s_stable <- S_tail(xi)
        if (!is.finite(s_stable) || s_stable <= 0) return(NaN)
        f(xi) / s_stable
      }, numeric(1))
    }
    return(tag_distribution_route(out, "pdf,cdf->hf", TRUE))
  }

  if (!is.null(pdf_fun)) {
    f <- distribution_fun(pdf_fun, utils::modifyList(args, pdf_args), "pdf_fun")
    S_tail <- pdf_to_survival(f, support = support)
    out <- function(x) {
      vapply(x, function(xi) {
        if (is.na(xi)) return(NA_real_)
        s <- S_tail(xi)
        if (is.na(s)) return(NA_real_)
        if (s <= 0) return(NaN)
        f(xi) / s
      }, numeric(1))
    }
    return(tag_distribution_route(out, "pdf->hf", TRUE))
  }

  if (!is.null(cdf_fun)) {
    # CDF-only route: the tail is unrecoverable once F(x) rounds to 1 in
    # double precision. Warn about the saturation rather than silently
    # returning zero.
    F <- distribution_fun(cdf_fun, utils::modifyList(args, cdf_args), "cdf_fun")
    f <- cdf_to_pdf(F, support = support)
    warned <- FALSE
    out <- function(x) {
      vapply(x, function(xi) {
        if (is.na(xi)) return(NA_real_)
        s <- 1 - F(xi)
        if (is.na(s)) return(NA_real_)
        if (s <= 0) {
          if (!warned) {
            warned <<- TRUE
            cli::cli_warn(c(
              "The CDF has rounded to 1, so the upper tail of the hazard is unrecoverable from a CDF alone (saturated survival).",
              "i" = "Supply {.arg pdf_fun} or {.arg survival_fun} for tail-stable hazards."
            ))
          }
          return(NA_real_)
        }
        f(xi) / s
      }, numeric(1))
    }
    return(tag_distribution_route(out, "cdf->hf", TRUE))
  }

  F <- qf_to_cdf(distribution_fun(qf_fun, args, "qf_fun"), support = support)
  f <- cdf_to_pdf(F, support = support)
  out <- function(x) {
    vapply(x, function(xi) {
      if (is.na(xi)) return(NA_real_)
      s <- 1 - F(xi)
      if (is.na(s) || s <= 0) return(NaN)
      f(xi) / s
    }, numeric(1))
  }
  tag_distribution_route(out, "qf->hf", TRUE)
}

#' Convert any supported source to a cumulative hazard function.
#' @noRd
as_chf_1d <- function(fun = NULL, hf_fun = NULL, cdf_fun = NULL,
                      pdf_fun = NULL, survival_fun = NULL, qf_fun = NULL,
                      args = list(), support = c(-Inf, Inf),
                      hf_lower = -Inf) {
  support <- validate_support_1d(support)
  args <- validate_named_args(args)
  validate_distribution_sources(
    list(fun = fun, hf_fun = hf_fun, cdf_fun = cdf_fun, pdf_fun = pdf_fun,
         survival_fun = survival_fun, qf_fun = qf_fun),
    what = "cumulative hazard"
  )

  if (!is.null(fun)) {
    return(tag_distribution_route(distribution_fun(fun, args), "chf", FALSE))
  }
  if (!is.null(hf_fun)) {
    hf <- distribution_fun(hf_fun, args, "hf_fun")
    return(hf_to_chf(hf, lower = hf_lower, support = support))
  }
  if (!is.null(survival_fun)) {
    return(survival_to_chf(distribution_fun(survival_fun, args, "survival_fun")))
  }

  cdf <- as_cdf_1d(
    fun = cdf_fun, pdf_fun = pdf_fun, survival_fun = NULL,
    qf_fun = qf_fun, args = args, support = support
  )
  cdf_to_chf(cdf)
}
