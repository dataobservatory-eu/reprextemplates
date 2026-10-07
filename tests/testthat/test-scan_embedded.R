fixture_dir <- testthat::test_path("fixtures", "embedded")
fixture_files <- fs::dir_ls(fixture_dir, recurse = TRUE, type = "file")
embedded <- readRDS(testthat::test_path("fixtures", "embedded_snapshot.rds"))

test_that("scan_embedded creates the expected embedded snapshot", {
  expect_named(embedded, c("file", "extension", "field", "value"))
  expect_equal(anyDuplicated(embedded[c("file", "field")]), 0L)
  expect_true(all(embedded$extension == tolower(embedded$extension)))
  expect_setequal(
    unique(fs::path_file(embedded$file)),
    fs::path_file(fixture_files)
  )

  # Check representative fields, namespaces and values.
  dated <- embedded[fs::path_file(embedded$file) == "dated.jpg", ]
  png <- embedded[fs::path_file(embedded$file) == "observable.png", ]
  unicode <- embedded[fs::path_file(embedded$file) == "unicode.jpg", ]

  expect_true(all(c(
    "IFD0:Make", "IFD0:ModifyDate", "ExifIFD:CreateDate"
  ) %in% dated$field))
  expect_equal(dated$value[dated$field == "IFD0:Make"], "Fixture Camera")
  expect_equal(
    dated$value[dated$field == "ExifIFD:CreateDate"],
    "2026:07:10 14:32:16"
  )
  expect_equal(
    png$value[png$field == "PNG:Description"],
    "Synthetic embedded metadata test"
  )
  expect_true(all(c(
    "XMP-dc:Description", "IPTC:Caption-Abstract"
  ) %in% unicode$field))
  expect_true(all(c(
    "ExifIFD:CreateDate", "System:FileCreateDate", "XMP-xmp:CreateDate"
  ) %in% embedded$field))
})

test_that("scan_embedded records observation provenance", {
  observation <- attr(embedded, "observation")

  expect_equal(observation$observer, "exiftool")
  expect_true(nzchar(observation$version))
  expect_equal(observation$n_files, length(fixture_files))
  expect_true(observation$elapsed >= 0 && observation$average >= 0)
})

test_that("scan_embedded validates inputs without scanning files", {
  expect_error(
    scan_embedded(fixture_files, tool = "unsupported", progress = FALSE),
    "Unsupported metadata tool"
  )
  expect_error(scan_embedded(list("photo.jpg"), progress = FALSE))
})

test_that("scan_embedded handles empty input", {
  result <- scan_embedded(character(), progress = FALSE)

  expect_named(result, c("file", "extension", "field", "value"))
  expect_equal(nrow(result), 0L)
})
