test_that("create_review_bundles detects profiles correctly", {
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

  create_review_bundles(root_dir,
    exclude_patterns = NULL
  )

  expect_true(
    file.exists(
      file.path(
        root_dir,
        "package1",
        "review-package1.txt"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        root_dir,
        "package2",
        "review-package2.txt"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        root_dir,
        "book-directory",
        "review-book-directory.txt"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        root_dir,
        "hugo-directory",
        "review-hugo-directory.txt"
      )
    )
  )

  expect_false(
    file.exists(
      file.path(
        root_dir,
        "text-directory",
        "review-text-directory.txt"
      )
    )
  )
})
