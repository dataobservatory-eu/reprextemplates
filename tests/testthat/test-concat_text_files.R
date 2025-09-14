test_that("concat_text_files combines multiple files correctly", {
  tmp_root <- tempdir()
  tmp <- file.path(tmp_root, "concat_test")
  dir.create(tmp, showWarnings = FALSE)

  # Create some sample text files
  file_1 <- file.path(tmp, "a1.txt")
  file_2 <- file.path(tmp, "a2.txt")
  file_3 <- file.path(tmp, "a3.txt")

  writeLines(c("alpha", "beta"), file_1)
  writeLines(c("gamma", "delta"), file_2)
  writeLines(c("epsilon"), file_3)

  out <- file.path(tmp, "combined.txt")
  if (file.exists(out)) file.remove(out)

  result <- concat_text_files(tmp, extension = "txt", output_file = out)

  expect_true(file.exists(out))
  expect_equal(result, out)

  lines <- readLines(out)
  expect_equal(lines, c("alpha", "beta", "gamma", "delta", "epsilon"))
})


test_that("concat_text_files errors if no matching files", {
  tmp <- tempdir()
  out <- file.path(tmp, "none.txt")

  expect_error(
    concat_text_files(tmp, extension = "zzz", output_file = out),
    "No files found"
  )
})
