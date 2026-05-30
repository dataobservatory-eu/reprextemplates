test_that("creates package review bundle from fixture package", {
  pkg_dir <- test_path(
    "testdata",
    "review_bundles",
    "package1"
  )

  output_file <- tempfile(fileext = ".txt")

  expect_invisible(
    concat_ordered_text_files(
      source_dir = pkg_dir,
      groups = concat_files_profile("R package"),
      profile = "R package",
      output_file = output_file
    )
  )

  expect_true(file.exists(output_file))

  output <- readLines(output_file)

  expect_true(any(grepl("source: DESCRIPTION", output, fixed = TRUE)))
  expect_true(any(grepl("source: NAMESPACE", output, fixed = TRUE)))
})

## concat_ordered_text_files() ----------------------------------------

test_that("creates bundle from explicit files", {
  pkg_dir <- tempfile()
  dir.create(pkg_dir)

  writeLines(
    "Package: testpkg",
    file.path(pkg_dir, "DESCRIPTION")
  )

  writeLines(
    "# Test package",
    file.path(pkg_dir, "README.md")
  )

  output_file <- tempfile(fileext = ".txt")

  groups <- list(
    list(
      files = c(
        "DESCRIPTION",
        "README.md"
      )
    )
  )

  expect_invisible(
    concat_ordered_text_files(
      source_dir = pkg_dir,
      groups = groups,
      output_file = output_file
    )
  )

  expect_true(file.exists(output_file))

  content <- readLines(output_file)

  expect_true(
    any(grepl("Package: testpkg", content))
  )

  expect_true(
    any(grepl("Test package", content))
  )
})
