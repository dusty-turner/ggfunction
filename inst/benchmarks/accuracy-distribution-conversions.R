# Manual numerical accuracy checks for ggfunction distribution conversions.
#
# This script is intended for manuscript support, not for R CMD check. It
# prints a compact data frame of fixed-grid errors and optionally writes a CSV
# when GGFUNCTION_WRITE_BENCHMARKS=true.

if (!requireNamespace("ggfunction", quietly = TRUE)) {
  stop("Package 'ggfunction' must be installed before running accuracy checks.", call. = FALSE)
}

script_name <- "inst/benchmarks/accuracy-distribution-conversions.R"

error_row <- function(route, distribution, observed, expected, grid_domain,
                      notes = "") {
  err <- abs(observed - expected)
  data.frame(
    route = route,
    distribution = distribution,
    n_grid = length(err),
    grid_domain = grid_domain,
    max_abs_error = max(err, na.rm = TRUE),
    median_abs_error = stats::median(err, na.rm = TRUE),
    notes = notes,
    stringsAsFactors = FALSE
  )
}

run_context <- function() {
  run_time <- Sys.time()
  pkg_version <- function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      as.character(utils::packageVersion(pkg))
    } else {
      NA_character_
    }
  }
  info <- Sys.info()
  data.frame(
    script = script_name,
    run_date = format(run_time, "%Y-%m-%d"),
    run_time = format(run_time, "%Y-%m-%d %H:%M:%S %Z"),
    operation = "fixed-grid function comparison",
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
    benchmark_iterations = NA_integer_,
    timing_operation = NA_character_,
    stringsAsFactors = FALSE
  )
}

pmf_to_cdf <- function(x_support, mass) {
  force(x_support)
  force(mass)
  function(x) {
    vapply(x, function(xi) sum(mass[x_support <= xi]), numeric(1))
  }
}

pmf_to_qf <- function(x_support, mass) {
  force(x_support)
  force(mass)
  cdf <- cumsum(mass)
  function(p) {
    vapply(p, function(pi) x_support[which(cdf >= pi)[1L]], numeric(1))
  }
}

x_norm <- seq(-4, 4, length.out = 401)
p_grid <- seq(0.01, 0.99, length.out = 199)
x_exp <- seq(0.001, 8, length.out = 401)
x_beta <- seq(0.001, 0.999, length.out = 401)

cdf_from_pdf_norm <- ggfunction:::as_cdf_1d(pdf_fun = stats::dnorm)
pdf_from_cdf_norm <- ggfunction:::as_pdf_1d(cdf_fun = stats::pnorm)
qf_from_cdf_norm <- ggfunction:::as_qf_1d(cdf_fun = stats::pnorm)

h_exp <- function(x, rate = 1) ifelse(x >= 0, rate, 0)
pdf_from_hf_exp <- ggfunction:::as_pdf_1d(
  hf_fun = h_exp,
  args = list(rate = 1),
  support = c(0, Inf)
)

h_weibull <- function(x, shape, scale) {
  ifelse(x >= 0, (shape / scale) * (x / scale)^(shape - 1), 0)
}
pdf_from_hf_weibull <- ggfunction:::as_pdf_1d(
  hf_fun = h_weibull,
  args = list(shape = 1.7, scale = 2),
  support = c(0, Inf)
)

cdf_from_pdf_beta <- ggfunction:::as_cdf_1d(
  pdf_fun = stats::dbeta,
  args = list(shape1 = 2, shape2 = 5),
  support = c(0, 1)
)

x_binom <- 0:20
m_binom <- stats::dbinom(x_binom, size = 20, prob = 0.35)
cdf_binom <- pmf_to_cdf(x_binom, m_binom)
qf_binom <- pmf_to_qf(x_binom, m_binom)

results <- rbind(
  error_row("pdf->cdf", "normal", cdf_from_pdf_norm(x_norm), stats::pnorm(x_norm),
            "401 x values on [-4, 4]"),
  error_row("cdf->pdf", "normal", pdf_from_cdf_norm(x_norm), stats::dnorm(x_norm),
            "401 x values on [-4, 4]",
            "finite differences"),
  error_row("cdf->qf", "normal", qf_from_cdf_norm(p_grid), stats::qnorm(p_grid),
            "199 probabilities on [0.01, 0.99]",
            "adaptive uniroot"),
  error_row("hf->pdf", "exponential", pdf_from_hf_exp(x_exp), stats::dexp(x_exp),
            "401 x values on [0.001, 8]",
            "support lower endpoint 0"),
  error_row("hf->pdf", "weibull", pdf_from_hf_weibull(x_exp),
            stats::dweibull(x_exp, shape = 1.7, scale = 2),
            "401 x values on [0.001, 8]",
            "support lower endpoint 0"),
  error_row("pdf->cdf", "beta(2,5)", cdf_from_pdf_beta(x_beta),
            stats::pbeta(x_beta, 2, 5),
            "401 x values on [0.001, 0.999]", "finite support"),
  error_row("pmf->cdf", "binomial(20,0.35)", cdf_binom(x_binom),
            stats::pbinom(x_binom, size = 20, prob = 0.35),
            "support 0:20"),
  error_row("pmf->qf", "binomial(20,0.35)", qf_binom(p_grid),
            stats::qbinom(p_grid, size = 20, prob = 0.35),
            "199 probabilities on [0.01, 0.99]")
)

print(results)

if (identical(Sys.getenv("GGFUNCTION_WRITE_BENCHMARKS"), "true")) {
  out_dir <- file.path("inst", "benchmarks", "results")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  context <- run_context()
  timestamp <- gsub("[^0-9]", "", context$run_time[1L])
  out_file <- file.path(out_dir, paste0("accuracy-", timestamp, ".csv"))
  results_out <- cbind(context[rep(1L, nrow(results)), , drop = FALSE], results)
  utils::write.csv(results_out, out_file, row.names = FALSE)
  message("Wrote accuracy results to ", out_file)
}
