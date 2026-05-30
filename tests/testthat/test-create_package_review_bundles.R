# review_bundles/
# ├── text-directory/
# │   ├── notes.txt
# │   └── draft.md
# │
# ├── package1/
# │   ├── DESCRIPTION
# │   ├── NAMESPACE
# │   └── R/
# │       └── helloworld.R
# │
# └── package2/
# ├── DESCRIPTION
# ├── NAMESPACE
# └── R/
# ├── hellovilag.R
# └── hellowelt.R


test_that("create_package_review_bundles processes packages", {
  fixture <- test_path("testdata", "review_bundles")

  tmp <- file.path(tempdir(), "review_bundles")
  unlink(tmp, recursive = TRUE)

  dir.create(tmp, recursive = TRUE)

  files <- list.files(
    fixture,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )

  for (f in files) {
    rel <- substring(f, nchar(fixture) + 2)
    target <- file.path(tmp, rel)

    if (dir.exists(f)) {
      dir.create(target, recursive = TRUE)
    } else {
      dir.create(dirname(target),
        recursive = TRUE,
        showWarnings = FALSE
      )
      file.copy(f, target)
    }
  }

  create_package_review_bundles(tmp)

  expect_false(
    file.exists(
      file.path(
        tmp,
        "text-directory",
        "package_review-text-directory.txt"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        tmp,
        "package1",
        "package_review-package1.txt"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        tmp,
        "package2",
        "package_review-package2.txt"
      )
    )
  )
})

test_that("create_package_review_bundles is idempotent", {
  fixture <- test_path(
    "testdata",
    "review_bundles"
  )

  root_dir <- file.path(
    tempdir(),
    "review_bundles"
  )

  unlink(root_dir, recursive = TRUE)

  copy_fixture(fixture, root_dir)

  create_package_review_bundles(root_dir)
  create_package_review_bundles(root_dir)
  create_package_review_bundles(root_dir)

  gitignore <- readLines(
    file.path(
      root_dir,
      "package1",
      ".gitignore"
    )
  )

  expect_equal(
    sum(gitignore == "package_review-package1.txt"),
    1
  )

  rbuildignore <- readLines(
    file.path(
      root_dir,
      "package1",
      ".Rbuildignore"
    )
  )

  expect_equal(
    sum(
      rbuildignore ==
        "^package_review-package1.txt$"
    ),
    1
  )

  gitignore <- readLines(
    file.path(
      root_dir,
      "package2",
      ".gitignore"
    )
  )

  expect_equal(
    sum(gitignore == "package_review-package2.txt"),
    1
  )

  rbuildignore <- readLines(
    file.path(
      root_dir,
      "package2",
      ".Rbuildignore"
    )
  )

  expect_equal(
    sum(
      rbuildignore ==
        "^package_review-package2.txt$"
    ),
    1
  )
})
