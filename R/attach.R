.onAttach <- function(...) {
  if(!interactive() || stats::runif(1) > 0.1) return()
  packageStartupMessage('  Please cite ggfunction! See citation("ggfunction") for details.')
}
