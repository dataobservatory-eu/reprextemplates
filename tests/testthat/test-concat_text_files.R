test_that("concat_text_files combines multiple files correctly", {
  tmp <- tempdir()

  # Create some sample text files
  f1 <- file.path(tmp, "a1.txt")
  f2 <- file.path(tmp, "a2.txt")
  f3 <- file.path(tmp, "a3.txt")

  writeLines(c("alpha", "beta"), f1)
  writeLines(c("gamma", "delta"), f2)
  writeLines(c("epsilon"), f3)

  out <- file.path(tmp, "combined.txt")

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
