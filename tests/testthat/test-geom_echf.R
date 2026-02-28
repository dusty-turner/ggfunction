# ── StatECHF ──────────────────────────────────────────────────────────────────

test_that("StatECHF computes correct values", {
  result <- StatECHF$compute_group(
    data = data.frame(x = c(1, 2, 3, 4)),
    scales = list(),
    na.rm = FALSE
  )
  # cdf = (0.25, 0.5, 0.75, 1.0)
  # H   = (-log(0.75), -log(0.5), -log(0.25), Inf)
  # Last row (cdf=1 -> H=Inf) is dropped
  expect_equal(nrow(result), 3)
  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$y, -log(1 - c(0.25, 0.5, 0.75)))
})

test_that("StatECHF drops Inf boundary", {
  result <- StatECHF$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5)),
    scales = list(),
    na.rm = FALSE
  )
  # 5 distinct values, last one (cdf=1) is dropped

  expect_equal(nrow(result), 4)
  expect_true(all(is.finite(result$y)))
})

test_that("StatECHF returns empty for empty data", {
  result <- StatECHF$compute_group(
    data = data.frame(x = numeric(0)),
    scales = list(),
    na.rm = FALSE
  )
  expect_equal(nrow(result), 0)
})

test_that("StatECHF handles single observation", {
  result <- StatECHF$compute_group(
    data = data.frame(x = 5),
    scales = list(),
    na.rm = FALSE
  )
  # n=1: cdf=1, H=Inf -> all filtered -> empty
  expect_equal(nrow(result), 0)
})

test_that("StatECHF handles ties", {
  result <- StatECHF$compute_group(
    data = data.frame(x = c(1, 1, 2, 2, 3)),
    scales = list(),
    na.rm = FALSE
  )
  # 3 distinct values: cdf = (0.4, 0.8, 1.0)
  # Last (cdf=1) is dropped -> 2 rows
  expect_equal(nrow(result), 2)
  expect_equal(result$x, c(1, 2))
  expect_equal(result$y, -log(1 - c(0.4, 0.8)))
})

# ── StatECHFBand ──────────────────────────────────────────────────────────────

test_that("StatECHFBand structure is valid", {
  result <- suppressMessages(StatECHFBand$compute_group(
    data = data.frame(x = c(1, 2, 3, 4, 5)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  ))
  expect_true(nrow(result) > 0)
  expect_true(all(result$ymin >= 0))
  expect_true(all(result$ymin <= result$ymax))
  expect_true(all(is.finite(result$ymin)))
  expect_true(all(is.finite(result$ymax)))
})

test_that("StatECHFBand clips upper bound at log(2n)", {
  n <- 50
  result <- suppressMessages(StatECHFBand$compute_group(
    data = data.frame(x = seq_len(n)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  ))
  expect_true(max(result$ymax) <= log(2 * n) + 0.01)
})

test_that("StatECHFBand emits message when clipping", {
  expect_message(
    StatECHFBand$compute_group(
      data = data.frame(x = c(1, 2, 3, 4, 5)),
      scales = list(),
      na.rm = FALSE,
      level = 0.95
    ),
    "Upper confidence band clipped"
  )
})

test_that("StatECHFBand returns empty for empty data", {
  result <- StatECHFBand$compute_group(
    data = data.frame(x = numeric(0)),
    scales = list(),
    na.rm = FALSE,
    level = 0.95
  )
  expect_equal(nrow(result), 0)
})

# ── geom_echf smoke tests ────────────────────────────────────────────────────

test_that("geom_echf builds without error", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_echf()
  expect_s3_class(p, "gg")
  suppressMessages(expect_no_warning(ggplot_build(p)))
})

test_that("geom_echf builds without conf_int", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_echf(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_echf builds with custom level and conf_alpha", {
  df <- data.frame(x = rexp(30))
  p <- ggplot(df, aes(x = x)) + geom_echf(level = 0.99, conf_alpha = 0.15)
  expect_s3_class(p, "gg")
  suppressMessages(expect_no_warning(ggplot_build(p)))
})

test_that("geom_echf builds with grouped data", {
  df <- data.frame(
    x = c(rexp(20), rexp(20, rate = 0.5)),
    group = rep(c("A", "B"), each = 20)
  )
  p <- ggplot(df, aes(x = x, colour = group)) + geom_echf()
  expect_s3_class(p, "gg")
  suppressMessages(expect_no_warning(ggplot_build(p)))
})

test_that("geom_echf auto-suppresses points/vert for large n", {
  df <- data.frame(x = rexp(100))
  p <- ggplot(df, aes(x = x)) + geom_echf(conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_echf with show_points=FALSE and show_vert=FALSE", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) +
    geom_echf(show_points = FALSE, show_vert = FALSE, conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_echf with na.rm = TRUE removes NAs", {
  df <- data.frame(x = c(1, NA, 2, 3))
  p <- ggplot(df, aes(x = x)) + geom_echf(na.rm = TRUE, conf_int = FALSE)
  expect_s3_class(p, "gg")
  expect_silent(ggplot_build(p))
})

test_that("geom_echf with custom mapping", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x)) + geom_echf(mapping = aes(linetype = "solid"), conf_int = FALSE)
  expect_s3_class(p, "gg")
})
