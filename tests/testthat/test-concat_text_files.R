library(testthat)

# Core behavior (no comments) -------------------------------------------

test_that("concat_text_files combines multiple files without comments", {
  tmp <- file.path(tempdir(), "concat_basic")
  dir.create(tmp, showWarnings = FALSE)

  writeLines(c("alpha", "beta"), file.path(tmp, "a1.txt"))
  writeLines(c("gamma", "delta"), file.path(tmp, "a2.txt"))
  writeLines("epsilon", file.path(tmp, "a3.txt"))

  out <- file.path(tmp, "combined.txt")

  result <- concat_text_files(
    source_dir = tmp,
    extension = "txt",
    output_file = out,
    add_source_comment = FALSE
  )

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

# Annotation behavior (default TRUE) ----------------------------

test_that("concat_text_files adds source comments by default", {
  tmp <- file.path(tempdir(), "concat_comments_default")
  dir.create(tmp, showWarnings = FALSE)

  writeLines("one", file.path(tmp, "f1.txt"))
  writeLines("two", file.path(tmp, "f2.txt"))

  out <- file.path(tmp, "combined.txt")

  concat_text_files(
    source_dir = tmp,
    extension = "txt",
    output_file = out
  )

  lines <- readLines(out)

  expect_true(any(grepl("source:", lines)))
  expect_true(any(grepl("f1.txt", lines)))
  expect_true(any(grepl("f2.txt", lines)))
})

test_that("concat_text_files can disable comments explicitly", {
  tmp <- file.path(tempdir(), "concat_no_comments")
  dir.create(tmp, showWarnings = FALSE)

  writeLines("aaa", file.path(tmp, "x1.txt"))
  writeLines("bbb", file.path(tmp, "x2.txt"))

  out <- file.path(tmp, "combined.txt")

  concat_text_files(
    source_dir = tmp,
    extension = "txt",
    output_file = out,
    add_source_comment = FALSE
  )

  lines <- readLines(out)
  expect_false(any(grepl("source:", lines)))
})

test_that("concat_text_files supports custom comment style", {
  tmp <- file.path(tempdir(), "concat_bib")
  dir.create(tmp, showWarnings = FALSE)

  writeLines("@article{a}", file.path(tmp, "a.bib"))
  writeLines("@article{b}", file.path(tmp, "b.bib"))

  out <- file.path(tmp, "combined.bib")

  concat_text_files(
    source_dir = tmp,
    extension = "bib",
    output_file = out,
    comment_start = "@Comment{",
    comment_end = "}"
  )

  lines <- readLines(out)

  expect_true(any(grepl("^@Comment", lines)))
  expect_true(any(grepl("@article\\{a", lines)))
  expect_true(any(grepl("@article\\{b", lines)))
})

# Structural behavior -------------------------------------------------


test_that("concat_text_files processes files in deterministic order", {
  tmp <- file.path(tempdir(), "concat_order")
  dir.create(tmp, showWarnings = FALSE)

  writeLines("second", file.path(tmp, "b.txt"))
  writeLines("first", file.path(tmp, "a.txt"))

  out <- file.path(tmp, "combined.txt")

  concat_text_files(
    tmp, "txt", out,
    add_source_comment = FALSE
  )

  lines <- readLines(out)
  expect_equal(lines, c("first", "second"))
})

test_that("concat_text_files supports recursive search", {
  base <- file.path(tempdir(), "concat_recursive")
  sub <- file.path(base, "sub")
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)

  writeLines("root", file.path(base, "a.txt"))
  writeLines("child", file.path(sub, "b.txt"))

  out <- file.path(tempdir(), "recursive_out.txt")

  concat_text_files(
    source_dir = base,
    extension = "txt",
    output_file = out,
    recursive = TRUE,
    add_source_comment = FALSE
  )

  lines <- readLines(out)
  expect_equal(sort(lines), sort(c("root", "child")))
})

test_that("concat_text_files supports multiple extensions", {
  tmp <- file.path(tempdir(), "concat_multi_ext")
  dir.create(tmp, showWarnings = FALSE)

  writeLines("rfile", file.path(tmp, "a.R"))
  writeLines("bakfile", file.path(tmp, "b.bak"))

  out <- file.path(tmp, "combined.txt")

  concat_text_files(
    source_dir = tmp,
    extension = c("R", "bak"),
    output_file = out,
    add_source_comment = FALSE
  )

  lines <- readLines(out)
  expect_equal(sort(lines), sort(c("rfile", "bakfile")))
})

test_that("concat_text_files does not include its own output file", {
  tmp <- file.path(tempdir(), "concat_self_exclude")
  dir.create(tmp, showWarnings = FALSE)

  writeLines("one", file.path(tmp, "a.txt"))

  out <- file.path(tmp, "combined.txt")

  concat_text_files(tmp, "txt", out, add_source_comment = FALSE)
  concat_text_files(tmp, "txt", out, add_source_comment = FALSE)

  lines <- readLines(out)
  expect_equal(lines, "one")
})
