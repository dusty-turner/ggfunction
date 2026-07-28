#' @importFrom rlang inject
NULL

#' Deterministic attach behavior.
#'
#' Never touches the RNG: attaching ggfunction must not create or change
#' `.Random.seed`. The citation reminder is shown on every interactive attach
#' and can be silenced with `suppressPackageStartupMessages()` or
#' `options(ggfunction.quiet = TRUE)`.
#' @noRd
on_attach_impl <- function(interactive = FALSE) {
  if (!interactive) return(invisible(NULL))
  if (isTRUE(getOption("ggfunction.quiet", FALSE))) return(invisible(NULL))
  packageStartupMessage('  Please cite ggfunction! See citation("ggfunction") for details.')
  invisible(NULL)
}

.onAttach <- function(...) {
  on_attach_impl(interactive = interactive())
}
