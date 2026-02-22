# ── .tabulate_empirical ────────────────────────────────────────────────────────

test_that(".tabulate_empirical returns correct structure", {
  tab <- ggfunction:::.tabulate_empirical(c(1, 2, 2, 3), na.rm = FALSE)
  expect_equal(tab$x, c(1, 2, 3))
  expect_equal(tab$pmf, c(0.25, 0.5, 0.25))
  expect_equal(tab$cdf, c(0.25, 0.75, 1.0))
  expect_equal(tab$n, c(4L, 4L, 4L))
})

test_that(".tabulate_empirical handles NAs with na.rm", {
  tab <- ggfunction:::.tabulate_empirical(c(1, NA, 2), na.rm = TRUE)
  expect_equal(tab$x, c(1, 2))
  expect_equal(tab$n, c(2L, 2L))
})

test_that(".tabulate_empirical returns empty data.frame for empty input", {
  tab <- ggfunction:::.tabulate_empirical(numeric(0), na.rm = FALSE)
  expect_equal(nrow(tab), 0)
})

test_that(".tabulate_empirical removes non-finite values", {
  tab <- ggfunction:::.tabulate_empirical(c(1, Inf, -Inf, NaN, 2), na.rm = TRUE)
  expect_equal(tab$x, c(1, 2))
})

# ── .expand_step_ribbon ──────────────────────────────────────────────────────

test_that(".expand_step_ribbon returns input when <= 1 row", {
  df <- data.frame(x = 1, ymin = 0, ymax = 1)
  expect_equal(ggfunction:::.expand_step_ribbon(df), df)
})

test_that(".expand_step_ribbon expands correctly", {
  df <- data.frame(x = c(1, 2, 3), ymin = c(0, 0.2, 0.5), ymax = c(0.3, 0.6, 1))
  result <- ggfunction:::.expand_step_ribbon(df)
  expect_equal(nrow(result), 5)
  expect_equal(result$x, c(1, 2, 2, 3, 3))
})

# ── geom_ecdf ────────────────────────────────────────────────────────────────

test_that("geom_ecdf builds without error", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_ecdf()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf builds without conf_int", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_ecdf(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf builds with custom level and conf_alpha", {
  df <- data.frame(x = rnorm(30))
  p <- ggplot(df, aes(x = x)) + geom_ecdf(level = 0.99, conf_alpha = 0.15)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf builds with grouped data", {
  df <- data.frame(
    x = c(rnorm(20), rnorm(20, mean = 2)),
    group = rep(c("A", "B"), each = 20)
  )
  p <- ggplot(df, aes(x = x, colour = group)) + geom_ecdf()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf auto-suppresses points/vert for large n", {
  df <- data.frame(x = rnorm(100))
  p <- ggplot(df, aes(x = x)) + geom_ecdf(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf with show_points=FALSE and show_vert=FALSE", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) +
    geom_ecdf(show_points = FALSE, show_vert = FALSE, conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf with na.rm = TRUE removes NAs", {
  df <- data.frame(x = c(1, NA, 2, 3))
  p <- ggplot(df, aes(x = x)) + geom_ecdf(na.rm = TRUE, conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatECDF computes correct values", {
  result <- StatECDF$compute_group(
    data = data.frame(x = c(1, 2, 2, 3)),
    scales = list(),
    na.rm = FALSE
  )
  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$y, c(0.25, 0.75, 1.0))
})

test_that("StatECDFBand computes band correctly", {
  result <- StatECDFBand$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_true(nrow(result) > 0)
  expect_true(all(result$ymin >= 0))
  expect_true(all(result$ymax <= 1))
  expect_true(all(result$ymin <= result$ymax))
})

test_that("StatECDFBand returns empty for empty data", {
  result <- StatECDFBand$compute_group(
    data = data.frame(x = numeric(0)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_equal(nrow(result), 0)
})

# ── geom_eqf ────────────────────────────────────────────────────────────────

test_that("geom_eqf builds without error", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_eqf()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_eqf builds without conf_int", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_eqf(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_eqf builds with grouped data", {
  df <- data.frame(
    x = c(rnorm(20), rnorm(20, mean = 2)),
    group = rep(c("A", "B"), each = 20)
  )
  p <- ggplot(df, aes(x = x, colour = group)) + geom_eqf()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_eqf auto-suppresses points/vert for large n", {
  df <- data.frame(x = rnorm(100))
  p <- ggplot(df, aes(x = x)) + geom_eqf(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_eqf with show_points=FALSE and show_vert=FALSE", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) +
    geom_eqf(show_points = FALSE, show_vert = FALSE, conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatEQF computes correct values", {
  result <- StatEQF$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5)),
    scales = list(),
    na.rm = FALSE
  )
  expect_equal(result$y, c(1, 2, 3, 4, 5))
  expect_equal(result$x, c(0.2, 0.4, 0.6, 0.8, 1.0))
})

test_that("StatEQFBand computes band correctly", {
  result <- StatEQFBand$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_true(nrow(result) > 0)
  expect_true(all(result$ymin <= result$ymax))
})

test_that("StatEQFBand returns empty for empty data", {
  result <- StatEQFBand$compute_group(
    data = data.frame(x = numeric(0)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_equal(nrow(result), 0)
})

# ── geom_epmf ───────────────────────────────────────────────────────────────

test_that("geom_epmf builds without error", {
  df <- data.frame(x = c(1, 1, 2, 3, 3, 3))
  p <- ggplot(df, aes(x = x)) + geom_epmf()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_epmf builds with grouped data", {
  df <- data.frame(
    x = c(1, 1, 2, 3, 3, 4),
    group = rep(c("A", "B"), each = 3)
  )
  p <- ggplot(df, aes(x = x, colour = group)) + geom_epmf()
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_epmf with custom lollipop params", {
  df <- data.frame(x = c(1, 2, 2, 3))
  p <- ggplot(df, aes(x = x)) +
    geom_epmf(point_size = 3, stick_linewidth = 0.5, stick_linetype = "solid")
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("StatEPMF computes correct values", {
  result <- StatEPMF$compute_group(
    data = data.frame(x = c(1, 1, 2, 3)),
    scales = list(),
    na.rm = FALSE
  )
  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$y, c(0.5, 0.25, 0.25))
})

test_that("geom_epmf with na.rm removes NAs", {
  df <- data.frame(x = c(1, NA, 2, 3))
  p <- ggplot(df, aes(x = x)) + geom_epmf(na.rm = TRUE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_ecdf with custom mapping", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_ecdf(mapping = aes(linetype = "solid"), conf_int = FALSE)
  expect_s3_class(p, "gg")
})

test_that("geom_eqf with custom mapping", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_eqf(mapping = aes(linetype = "solid"), conf_int = FALSE)
  expect_s3_class(p, "gg")
})

test_that("geom_epmf with custom mapping", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_epmf(mapping = aes(size = after_stat(y)))
  expect_s3_class(p, "gg")
})
