has_exiftool <- nzchar(Sys.which("exiftool"))
fixture_dir <- testthat::test_path("fixtures", "embedded")
fixture <- fs::path(fixture_dir, "dated.jpg")

test_that("observe_embedded observes a file", {
  skip_if_not(has_exiftool)
  x <- observe_embedded(fixture, progress = FALSE)
  md <- x$metadata[[1]]

  # Check structure, status and representative metadata.
  expect_named(x, c("file", "observed", "metadata", "error"))
  expect_equal(x$file, fixture)
  expect_true(x$observed)
  expect_true(is.na(x$error))
  expect_equal(md[["ExifIFD:CreateDate"]], "2026:07:10 14:32:16")
})

test_that("observe_embedded records provenance", {
  skip_if_not(has_exiftool)
  x <- observe_embedded(fixture, progress = FALSE)
  observation <- attr(x, "observation")

  # Check the observation activity recorded by the function.
  expect_named(observation, c(
    "activity_id", "observation_started", "observation_finished", "observer",
    "observer_version", "n_files", "elapsed_seconds", "average_seconds"
  ))
  expect_equal(observation$observer, "exiftool")
  expect_equal(observation$n_files, 1L)
  expect_true(nzchar(observation$observer_version))
  expect_true(observation$observation_finished >= observation$observation_started)
  expect_true(observation$elapsed_seconds >= 0)
  expect_true(observation$average_seconds >= 0)
})

test_that("observe_embedded filters extensions case insensitively", {
  skip_if_not(has_exiftool)
  files <- c(fixture, fs::path(fixture_dir, "observable.png"))
  x <- observe_embedded(files, extensions = "JPG", progress = FALSE)

  # Only the matching extension is passed to ExifTool.
  expect_equal(nrow(x), 1L)
  expect_equal(fs::path_file(x$file), "dated.jpg")
})

test_that("observe_embedded handles empty selections", {
  x <- observe_embedded(fixture, extensions = "png", progress = FALSE)

  # Filtering to no files returns the documented empty result.
  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 0L)
})

test_that("observe_embedded records errors when requested", {
  x <- observe_embedded(
    fixture,
    engine = "definitely-not-exiftool",
    ignore_errors = TRUE, progress = FALSE
  )

  # Processing errors are retained rather than stopping observation.
  expect_false(x$observed)
  expect_true(nzchar(x$error))
  expect_equal(length(x$metadata[[1]]), 0L)
})

test_that("observe_embedded propagates errors when requested", {
  # Processing errors stop observation when ignore_errors is FALSE.
  expect_error(observe_embedded(
    fixture,
    engine = "definitely-not-exiftool",
    ignore_errors = FALSE, progress = FALSE
  ))
})

test_that("observe_embedded validates files", {
  # File paths must be supplied as a character vector.
  expect_error(observe_embedded(1:3, progress = FALSE))
})
