# A-02: constructor-local mappings propagate to every auxiliary layer of a
# composite geom. A-03: censoring status never creates statistical groups.

test_that("geom_ecdf works with a constructor-local mapping (A-02)", {
  d <- data.frame(z = 1:5)
  expect_no_error(
    b <- ggplot_build(ggplot(d) + geom_ecdf(aes(x = z)))
  )
  expect_true(all(vapply(b$data, nrow, integer(1)) > 0))
})

test_that("geom_ecdf_km works with a constructor-local mapping (A-02)", {
  d <- data.frame(
    time = 1:5,
    status = c(1L, 1L, 0L, 1L, 0L)
  )
  expect_no_error(
    b <- ggplot_build(
      ggplot(d) +
        geom_ecdf_km(aes(x = time, status = status))
    )
  )
  expect_true(all(vapply(b$data, nrow, integer(1)) > 0))
})

test_that("geom_echf_na works with a constructor-local mapping (A-02)", {
  d <- data.frame(
    time = 1:5,
    status = c(1L, 1L, 0L, 1L, 0L)
  )
  expect_no_error(
    b <- ggplot_build(
      ggplot(d) +
        geom_echf_na(aes(x = time, status = status))
    )
  )
  expect_true(all(vapply(b$data, nrow, integer(1)) > 0))
})

test_that("geom_ppplot works with a constructor-local mapping (A-02)", {
  set.seed(1)
  d <- data.frame(z = rnorm(20))
  expect_no_error(
    b <- ggplot_build(ggplot(d) + geom_ppplot(aes(x = z), fun = pnorm))
  )
  expect_true(all(vapply(b$data, nrow, integer(1)) > 0))
})

test_that("local and global mappings produce identical statistics (A-02)", {
  d <- data.frame(
    z = c(rnorm(15, 0, 1), rnorm(15, 2, 1)),
    g = rep(c("a", "b"), each = 15)
  )

  b_local <- ggplot_build(ggplot(d) + geom_ecdf(aes(x = z, colour = g)))
  b_global <- ggplot_build(ggplot(d, aes(x = z, colour = g)) + geom_ecdf())

  for (i in seq_along(b_local$data)) {
    num <- vapply(b_local$data[[i]], is.numeric, logical(1))
    expect_equal(
      b_local$data[[i]][num],
      b_global$data[[i]][num],
      ignore_attr = TRUE
    )
  }
  expect_equal(length(unique(b_local$data[[1]]$group)), 2)
})

test_that("point-only calculated aesthetics do not leak into bands (A-02)", {
  d <- data.frame(z = qnorm(seq(0.05, 0.95, length.out = 20)))
  b <- ggplot_build(
    ggplot(d) +
      geom_ppplot(
        aes(x = z, alpha = after_stat(p)),
        fun = pnorm,
        conf_alpha = 0.4
      )
  )

  point_i <- layers_with_stat(b, "StatPPPlot")
  band_i <- layers_with_stat(b, "StatPPPlotBand")

  expect_length(point_i, 1)
  expect_length(band_i, 1)
  expect_gt(length(unique(b$data[[point_i]]$alpha)), 1)
  expect_true(all(b$data[[band_i]]$alpha == 0.4))
})

# --- A-03: status normalization before implicit grouping ---

test_that("logical and integer status produce identical KM output (A-03)", {
  d_logical <- data.frame(
    time = 1:5,
    status = c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  d_integer <- transform(d_logical, status = as.integer(status))

  b_logical <- ggplot_build(
    ggplot(d_logical, aes(time, status = status)) +
      geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  )$data[[1]]
  b_integer <- ggplot_build(
    ggplot(d_integer, aes(time, status = status)) +
      geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  )$data[[1]]

  expect_equal(b_logical[c("x", "y")], b_integer[c("x", "y")])
  expect_equal(b_logical$y, c(0.8, 0.6, 0.3))
})

test_that("logical/integer equivalence holds for bands, censor marks, and NA (A-03)", {
  d_logical <- data.frame(
    time = 1:5,
    status = c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  d_integer <- transform(d_logical, status = as.integer(status))

  for (build_fn in list(
    function(d) ggplot_build(ggplot(d, aes(time, status = status)) + geom_ecdf_km()),
    function(d) ggplot_build(ggplot(d, aes(time, status = status)) + geom_echf_na())
  )) {
    b_log <- build_fn(d_logical)
    b_int <- build_fn(d_integer)
    expect_equal(length(b_log$data), length(b_int$data))
    for (i in seq_along(b_log$data)) {
      expect_equal(b_log$data[[i]], b_int$data[[i]], ignore_attr = TRUE)
    }
  }
})

test_that("treatment grouping survives logical status (A-03)", {
  d <- data.frame(
    treatment = rep(c("a", "b"), each = 5),
    time = rep(1:5, 2),
    status = c(TRUE, TRUE, FALSE, TRUE, FALSE,
               TRUE, FALSE, TRUE, TRUE, FALSE)
  )

  b <- ggplot_build(
    ggplot(
      d,
      aes(time, status = status, colour = treatment, group = treatment)
    ) +
      geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  )$data[[1]]
  expect_equal(length(unique(b$group)), 2)

  b_implicit <- ggplot_build(
    ggplot(d, aes(time, status = status, colour = treatment)) +
      geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
  )$data[[1]]
  expect_equal(length(unique(b_implicit$group)), 2)
})

test_that("factor status is rejected before integer coercion (A-03)", {
  expect_error(
    .tabulate_km(
      1:4,
      factor(c(1, 1, 0, 0), levels = c(0, 1)),
      na.rm = FALSE
    ),
    "factor"
  )
  expect_error(
    .censoring_times(1:4, factor(c(1, 1, 0, 0)), na.rm = FALSE),
    "factor"
  )
  d <- data.frame(time = 1:4, status = factor(c(1, 1, 0, 0)))
  expect_error(
    ggplot_build(
      ggplot(d, aes(time, status = status)) +
        geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
    ),
    "factor"
  )
})

test_that("numeric status other than exact 0/1 is rejected (A-03)", {
  expect_error(normalize_status(c(0, 1, 2)), "0/1")
  expect_error(normalize_status(c(0.5, 1)), "0/1")
  expect_identical(normalize_status(c(TRUE, FALSE, NA)), c(1L, 0L, NA_integer_))
  expect_identical(normalize_status(c(0, 1, NA)), c(0L, 1L, NA_integer_))
})

test_that("missing status values follow the na.rm policy quietly (A-03)", {
  d <- data.frame(
    time = 1:5,
    status = c(TRUE, FALSE, NA, TRUE, FALSE)
  )
  expect_no_warning(
    ggplot_build(
      ggplot(d, aes(time, status = status)) +
        geom_ecdf_km(
          na.rm = TRUE,
          conf_int = FALSE,
          censor_marks = FALSE
        )
    )
  )
  expect_warning(
    ggplot_build(
      ggplot(d, aes(time, status = status)) +
        geom_ecdf_km(conf_int = FALSE, censor_marks = FALSE)
    ),
    "Removed"
  )
})
