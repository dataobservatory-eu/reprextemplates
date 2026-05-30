test_that("is_r_package_dir detects valid package", {
  pkg <- file.path(tempdir(), "pkg")

  unlink(pkg, recursive = TRUE)
  dir.create(pkg)

  file.create(file.path(pkg, "DESCRIPTION"))
  file.create(file.path(pkg, "NAMESPACE"))

  expect_true(
    is_r_package_dir(pkg)
  )
})

test_that("is_r_package_dir rejects incomplete package", {
  pkg <- file.path(tempdir(), "pkg")

  unlink(pkg, recursive = TRUE)
  dir.create(pkg)

  file.create(file.path(pkg, "DESCRIPTION"))

  expect_false(
    is_r_package_dir(pkg)
  )
})


test_that("append_ignore_entry creates new ignore file", {
  ignore_file <- file.path(
    tempdir(),
    "test.gitignore"
  )

  unlink(ignore_file)

  append_ignore_entry(
    ignore_file,
    "review-test.txt"
  )

  expect_true(
    file.exists(ignore_file)
  )

  expect_equal(
    readLines(ignore_file),
    "review-test.txt"
  )
})
