# R/validators.R
#
# Shared structural validators (spec 5.3, B-05). Structural invalidity always
# aborts and is never disabled by options(ggfunction.check = FALSE); that
# option only governs soft normalization/endpoint diagnostics.

#' @noRd
is_scalar_prob <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 && x < 1
}

#' Shared probability-argument validation for shading requests (B-05).
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
