# Manual performance benchmarks for ggfunction.
#
# This script benchmarks ggplot_build() for representative layers. It is not
# run by R CMD check. To save a CSV in inst/benchmarks/results/, run with
# GGFUNCTION_WRITE_BENCHMARKS=true.

if (!requireNamespace("bench", quietly = TRUE)) {
  stop("Package 'bench' is required to run this benchmark script.", call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required to run this benchmark script.", call. = FALSE)
}
if (!requireNamespace("ggfunction", quietly = TRUE)) {
  stop("Package 'ggfunction' must be installed before running benchmarks.", call. = FALSE)
}

dbvn <- function(v, mu = c(0, 0), Sigma = diag(2)) {
  x <- matrix(v - mu, ncol = 1)
  Sinv <- solve(Sigma)
  1 / (2 * pi * sqrt(det(Sigma))) *
    exp(-0.5 * as.numeric(t(x) %*% Sinv %*% x))
}

f_sc <- function(v) sin(v[1]) * cos(v[2])

script_name <- "inst/benchmarks/benchmark-ggfunction.R"
benchmark_iterations <- 10L

pkg_version <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    as.character(utils::packageVersion(pkg))
  } else {
    NA_character_
  }
}

benchmark_context <- function() {
  run_time <- Sys.time()
  info <- Sys.info()
  data.frame(
    script = script_name,
    run_date = format(run_time, "%Y-%m-%d"),
    run_time = format(run_time, "%Y-%m-%d %H:%M:%S %Z"),
    operation = "ggplot_build()",
    r_version = R.version.string,
    platform = paste(info[["sysname"]], info[["release"]], info[["machine"]]),
    os = info[["sysname"]],
    os_release = info[["release"]],
    machine = info[["machine"]],
    ggfunction_version = pkg_version("ggfunction"),
    ggplot2_version = pkg_version("ggplot2"),
    ggdensity_version = pkg_version("ggdensity"),
    ggvfields_version = pkg_version("ggvfields"),
    bench_version = pkg_version("bench"),
    benchmark_iterations = benchmark_iterations,
    timing_operation = "ggplot_build()",
    stringsAsFactors = FALSE
  )
}

benchmark_meta <- data.frame(
  expression = c(
    "pdf_native",
    "pdf_from_cdf",
    "cdf_from_pdf",
    "scalar_field",
    "pdf_2d_hdr"
  ),
  case = c(
    "geom_pdf(fun = dnorm)",
    "geom_pdf(cdf_fun = pnorm)",
    "geom_cdf(pdf_fun = dnorm)",
    "geom_function_2d_1d()",
    "geom_pdf_2d(), ggdensity HDR"
  ),
  grid_domain = c(
    "101 x values on [-4, 4]",
    "101 x values on [-4, 4]",
    "101 x values on [-4, 4]",
    "80 x 80 grid on [-pi, pi]^2",
    "60 x 60 grid on [-3, 3]^2"
  ),
  stringsAsFactors = FALSE
)

results <- bench::mark(
  pdf_native = ggplot2::ggplot_build(
    ggplot2::ggplot() +
      ggfunction::geom_pdf(fun = stats::dnorm, xlim = c(-4, 4))
  ),
  pdf_from_cdf = ggplot2::ggplot_build(
    ggplot2::ggplot() +
      ggfunction::geom_pdf(cdf_fun = stats::pnorm, xlim = c(-4, 4))
  ),
  cdf_from_pdf = ggplot2::ggplot_build(
    ggplot2::ggplot() +
      ggfunction::geom_cdf(pdf_fun = stats::dnorm, xlim = c(-4, 4))
  ),
  scalar_field = ggplot2::ggplot_build(
    ggplot2::ggplot() +
      ggfunction::geom_function_2d_1d(
        fun = f_sc,
        xlim = c(-pi, pi),
        ylim = c(-pi, pi),
        n = 80
      )
  ),
  pdf_2d_hdr = ggplot2::ggplot_build(
    ggplot2::ggplot() +
      ggfunction::geom_pdf_2d(
        fun = dbvn,
        xlim = c(-3, 3),
        ylim = c(-3, 3),
        n = 60
      )
  ),
  iterations = benchmark_iterations,
  memory = FALSE,
  filter_gc = FALSE,
  check = FALSE
)

print(results)

if (identical(Sys.getenv("GGFUNCTION_WRITE_BENCHMARKS"), "true")) {
  out_dir <- file.path("inst", "benchmarks", "results")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  bench_results <- data.frame(
    expression = as.character(results$expression),
    median = as.character(results$median),
    min = as.character(results$min),
    itr_sec = results[["itr/sec"]],
    n_itr = results$n_itr,
    n_gc = results$n_gc,
    total_time = as.character(results$total_time),
    stringsAsFactors = FALSE
  )
  bench_results <- merge(
    benchmark_meta,
    bench_results,
    by = "expression",
    all.x = TRUE,
    sort = FALSE
  )
  context <- benchmark_context()
  timestamp <- gsub("[^0-9]", "", context$run_time[1L])
  out_file <- file.path(out_dir, paste0("benchmark-", timestamp, ".csv"))
  bench_results <- cbind(
    context[rep(1L, nrow(bench_results)), , drop = FALSE],
    bench_results
  )
  utils::write.csv(bench_results, out_file, row.names = FALSE)
  message("Wrote benchmark results to ", out_file)
}
