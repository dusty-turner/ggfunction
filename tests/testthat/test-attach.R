test_that("attach does not consume RNG state", {
  withr::local_seed(42)
  seed <- get(".Random.seed", envir = globalenv())
  suppressPackageStartupMessages(on_attach_impl(interactive = TRUE))
  expect_identical(get(".Random.seed", envir = globalenv()), seed)
})

test_that("attach does not create .Random.seed", {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  suppressPackageStartupMessages(on_attach_impl(interactive = TRUE))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("attach messages are deterministic", {
  msgs1 <- capture_messages(on_attach_impl(interactive = TRUE))
  msgs2 <- capture_messages(on_attach_impl(interactive = TRUE))
  expect_identical(msgs1, msgs2)
  expect_length(msgs1, 1)
  expect_match(msgs1, "cite ggfunction", fixed = TRUE)
})

test_that("attach message respects interactivity and the quiet option", {
  expect_no_message(on_attach_impl(interactive = FALSE))
  withr::local_options(ggfunction.quiet = TRUE)
  expect_no_message(on_attach_impl(interactive = TRUE))
})
