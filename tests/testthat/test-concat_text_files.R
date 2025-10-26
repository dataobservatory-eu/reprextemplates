test_that("concat_text_files combines multiple files correctly", {
  tmp_root <- tempdir()
  tmp <- file.path(tmp_root, "concat_test")
  dir.create(tmp, showWarnings = FALSE)

  # Create sample text files
  file_1 <- file.path(tmp, "a1.txt")
  file_2 <- file.path(tmp, "a2.txt")
  file_3 <- file.path(tmp, "a3.txt")
  writeLines(c("alpha", "beta"), file_1)
  writeLines(c("gamma", "delta"), file_2)
  writeLines("epsilon", file_3)

  out <- file.path(tmp, "combined.txt")
  if (file.exists(out)) file.remove(out)

  result <- concat_text_files(tmp, "txt", out)

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

test_that("concat_text_files inserts source comments when requested", {
  tmp_root <- tempdir()
  tmp <- file.path(tmp_root, "concat_comments")
  dir.create(tmp, showWarnings = FALSE)

  # Create test files
  files <- file.path(tmp, paste0("f", 1:2, ".txt"))
  writeLines("one", files[1])
  writeLines("two", files[2])

  out <- file.path(tmp, "combined_comments.txt")
  if (file.exists(out)) file.remove(out)

  concat_text_files(
    source_dir = tmp,
    extension = "txt",
    output_file = out,
    add_source_comment = TRUE,
    comment_start = "<!---",
    comment_end = "--->"
  )

  expect_true(file.exists(out))
  lines <- readLines(out)

  # Expect to see both comment delimiters
  expect_true(any(grepl("<!---", lines)))
  expect_true(any(grepl("--->", lines)))

  # Expect file basenames appear in comment lines
  expect_true(any(grepl("f1.txt", lines)))
  expect_true(any(grepl("f2.txt", lines)))

  # Ensure content order is preserved (ignore blank and comment lines)
  content_lines <- lines[!grepl("<!|--->", lines)]
  content_lines <- content_lines[nzchar(content_lines)]
  expect_equal(content_lines, c("one", "two"))
})

test_that("concat_text_files supports BibLaTeX style comments", {
  tmp_root <- tempdir()
  tmp <- file.path(tmp_root, "concat_bib")
  dir.create(tmp, showWarnings = FALSE)

  # Create two small .bib files
  writeLines("@article{a, title={A}}", file.path(tmp, "a.bib"))
  writeLines("@article{b, title={B}}", file.path(tmp, "b.bib"))

  out <- file.path(tmp, "combined.bib")
  if (file.exists(out)) file.remove(out)

  concat_text_files(
    source_dir = tmp,
    extension = "bib",
    output_file = out,
    add_source_comment = TRUE,
    comment_start = "@Comment{",
    comment_end = "}"
  )

  lines <- readLines(out)
  # Should contain @Comment markers and both entries
  expect_true(any(grepl("^@Comment", lines)))
  expect_true(any(grepl("a.bib", lines)))
  expect_true(any(grepl("@article\\{a", lines)))
  expect_true(any(grepl("@article\\{b", lines)))
})

test_that("concat_text_files behaves identically when comments disabled", {
  tmp_root <- tempdir()
  src <- file.path(tmp_root, "concat_no_comments_src")
  dir.create(src, showWarnings = FALSE)

  # Create test files
  writeLines("aaa", file.path(src, "x1.txt"))
  writeLines("bbb", file.path(src, "x2.txt"))

  # Outputs go to a separate folder to avoid self-inclusion
  out_dir <- file.path(tmp_root, "concat_no_comments_out")
  dir.create(out_dir, showWarnings = FALSE)

  out1 <- file.path(out_dir, "no_comments.txt")
  out2 <- file.path(out_dir, "with_comments.txt")

  concat_text_files(src, "txt", out1, add_source_comment = FALSE)
  concat_text_files(src, "txt", out2, add_source_comment = TRUE)

  lines_no <- readLines(out1)
  lines_yes <- readLines(out2)
  # Remove comment lines from 'with_comments'
  lines_yes_clean <- lines_yes[!grepl("^(@Comment|<!---|--->)", lines_yes)]
  lines_yes_clean <- lines_yes_clean[nzchar(lines_yes_clean)]

  expect_equal(lines_no, lines_yes_clean)
})
