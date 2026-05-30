test_that("detect_review_profile identifies supported project types", {
  root <- test_path(
    "testdata",
    "review_bundles"
  )

  expect_equal(
    detect_review_profile(
      file.path(root, "package1")
    ),
    "r_package"
  )

  expect_equal(
    detect_review_profile(
      file.path(root, "package2")
    ),
    "r_package"
  )

  expect_equal(
    detect_review_profile(
      file.path(root, "book-directory")
    ),
    "quarto"
  )

  expect_equal(
    detect_review_profile(
      file.path(root, "hugo-directory")
    ),
    "hugo"
  )

  expect_null(
    detect_review_profile(
      file.path(root, "text-directory")
    )
  )
})


test_that("detect_review_profile identifies copied R packages", {
  expect_equal(
    detect_review_profile(
      "D:/packages/wbdataset.Rcheck/wbdataset"
    ),
    "r_package"
  )
})
