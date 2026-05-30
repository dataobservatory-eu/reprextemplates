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

## concat_files_profile() ---------------------------------------------

test_that("R package profile returns a list", {
  profile <- concat_files_profile("R package")

  expect_type(profile, "list")
  expect_gt(length(profile), 0)

  expect_true(
    all(vapply(profile, is.list, logical(1)))
  )
})

test_that("unknown profile errors", {
  expect_error(
    concat_files_profile("banana"),
    "Unknown profile"
  )
})

test_that("invalid profile errors", {
  expect_error(
    concat_files_profile(NULL),
    "single character value"
  )
})


test_that("Quarto profile creates review bundle", {
  book_dir <- test_path(
    "testdata",
    "review_bundles",
    "book-directory"
  )

  output_file <- tempfile(fileext = ".txt")

  expect_invisible(
    concat_ordered_text_files(
      source_dir = book_dir,
      groups = concat_files_profile("Quarto book"),
      profile = "Quarto book",
      output_file = output_file
    )
  )

  expect_true(file.exists(output_file))

  output <- readLines(output_file)

  expect_true(
    any(grepl("source: _quarto.yml", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("source: index.qmd", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("source: intro.qmd", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("source: references.qmd", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("source: summary.qmd", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("source: bib/references.bib", output, fixed = TRUE))
  )

  expect_false(
    any(grepl("_book/", output, fixed = TRUE))
  )
})

test_that("hugo profile creates review bundle", {
  hugo_dir <- test_path(
    "testdata", "review_bundles", "hugo-directory"
  )

  output_file <- tempfile(fileext = ".txt")

  expect_invisible(
    concat_ordered_text_files(
      source_dir = hugo_dir,
      groups = concat_files_profile("hugo"),
      profile = "hugo",
      output_file = output_file
    )
  )

  expect_true(file.exists(output_file))

  output <- readLines(output_file)

  expect_true(
    any(grepl("source: config.yaml", output, fixed = TRUE))
  )

  expect_true(
    any(
      grepl(
        "source: content/page/about.md",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "source: content/post/2026-05-13-budapest-nagyobb.md",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "source: content/post/2026-05-17-a-lucfenyo-menthetetlen-magyarorszag-mar-nem.md",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(grepl("source: netlify.toml", output, fixed = TRUE))
  )

  expect_false(
    any(grepl("source: 01.jpg", output, fixed = TRUE))
  )

  expect_false(
    any(grepl("source: index.bak", output, fixed = TRUE))
  )
})

test_that("only R packages receive review bundles", {
  fixture <- test_path(
    "testdata",
    "review_bundles"
  )

  root_dir <- file.path(
    tempdir(),
    "review_bundles"
  )

  unlink(root_dir, recursive = TRUE)

  copy_fixture(
    fixture,
    root_dir
  )

  create_package_review_bundles(root_dir)

  expect_true(
    file.exists(
      file.path(
        root_dir,
        "package1",
        "package_review-package1.txt"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        root_dir,
        "package2",
        "package_review-package2.txt"
      )
    )
  )

  expect_false(
    file.exists(
      file.path(
        root_dir,
        "book-directory",
        "package_review-book-directory.txt"
      )
    )
  )

  expect_false(
    file.exists(
      file.path(
        root_dir,
        "hugo-directory",
        "package_review-hugo-directory.txt"
      )
    )
  )

  expect_false(
    file.exists(
      file.path(
        root_dir,
        "text-directory",
        "package_review-text-directory.txt"
      )
    )
  )
})

dir(test_path(
  "testdata", "review_bundles"
), recursive = T)
