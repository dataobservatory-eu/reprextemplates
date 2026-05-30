test_that("concat_yml_text_files works on example project", {
  example_dir <- system.file("quarto-example", package = "reprextemplates")
  yml_file <- file.path(example_dir, "_quarto.yml")

  out <- tempfile(fileext = ".txt")

  res <- concat_yml_text_files(
    yml_file = yml_file,
    output_file = out
  )

  expect_true(file.exists(out))

  lines <- readLines(out)

  expect_true(any(grepl("Test 1", lines)))
  expect_true(any(grepl("Test 2", lines)))
  expect_true(any(grepl("smith2020", lines)))
})


test_that("concat_yml_text_files warns on missing files but still works", {
  # --- locate example ---
  example_dir <- system.file("quarto-example", package = "reprextemplates")

  if (example_dir == "") {
    example_dir <- testthat::test_path("../inst/quarto-example")
  }

  # --- YAML with missing files ---
  yml_file <- file.path(example_dir, "_quarto2.yml")

  out <- tempfile(fileext = ".txt") # ← MISSING LINE FIXED

  # --- expect warning but successful execution ---
  expect_warning(
    concat_yml_text_files(yml_file, out),
    "files listed in YAML were not found"
  )

  expect_true(file.exists(out))

  lines <- readLines(out)

  # existing content still present
  expect_true(any(grepl("Test 1", lines)))

  # missing file not included
  expect_false(any(grepl("test_missing", lines)))
})
