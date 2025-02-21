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
  tibble::tibble(..., .name_repair = "minimal")
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
utils::globalVariables(c("x", "y"))
