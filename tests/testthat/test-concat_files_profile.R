test_that("concat_files_profile returns supported profiles", {
  expect_type(
    concat_files_profile("r_package"),
    "list"
  )

  expect_type(
    concat_files_profile("quarto"),
    "list"
  )

  expect_type(
    concat_files_profile("testthat"),
    "list"
  )

  expect_type(
    concat_files_profile("hugo"),
    "list"
  )
})

test_that("concat_files_profile resolves R package aliases", {
  expected <- concat_files_profile("r_package")

  expect_equal(
    concat_files_profile("r"),
    expected
  )

  expect_equal(
    concat_files_profile("R"),
    expected
  )

  expect_equal(
    concat_files_profile("r-package"),
    expected
  )

  expect_equal(
    concat_files_profile("R package"),
    expected
  )
})

test_that("concat_files_profile resolves Quarto aliases", {
  expected <- concat_files_profile("quarto")

  expect_equal(
    concat_files_profile("Quarto"),
    expected
  )

  expect_equal(
    concat_files_profile("quarto"),
    expected
  )
})

test_that("concat_files_profile rejects unknown profiles", {
  expect_error(
    concat_files_profile("foobar"),
    "Unknown profile"
  )
})

test_that("concat_files_profile validates input", {
  expect_error(
    concat_files_profile(1),
    "single character value"
  )

  expect_error(
    concat_files_profile(c("r", "quarto")),
    "single character value"
  )

  expect_error(
    concat_files_profile(NA_character_),
    "single character value"
  )
})
