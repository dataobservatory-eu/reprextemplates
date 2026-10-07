fixture_dir <- testthat::test_path("fixtures", "embedded")
embedded_file <- testthat::test_path("fixtures", "embedded_snapshot.rds")

snapshot_fixtures <- fscontext::scan_storage(root = fixture_dir)
embedded_fixtures <- readRDS(embedded_file)

test_that("enrich_snapshot projects selected metadata", {
  fields <- c(
    "ExifIFD:CreateDate", "XMP-xmp:CreateDate", "PNG:Description",
    "XMP-dc:Description", "IPTC:Caption-Abstract"
  )
  result <- enrich_snapshot(snapshot_fixtures, embedded_fixtures, fields)

  expect_equal(nrow(result), nrow(snapshot_fixtures))
  expect_true(all(c(
    "exififd_createdate", "xmp_xmp_createdate", "png_description",
    "xmp_dc_description", "iptc_caption_abstract"
  ) %in% names(result)))

  dated <- result[result$filename == "dated.jpg", ]
  png <- result[result$filename == "observable.png", ]
  unicode <- result[result$filename == "unicode.jpg", ]

  expect_equal(dated$exififd_createdate, "2026:07:10 14:32:16")
  expect_true(is.na(dated$xmp_xmp_createdate))
  expect_equal(png$png_description, "Synthetic embedded metadata test")

  expected <- "Árvíztűrő tükörfúrógép. Deliņi – Rīga – Tartu"
  expect_equal(unicode$xmp_dc_description, expected)
  expect_equal(unicode$iptc_caption_abstract, expected)
  expect_false(is.na(unicode$xmp_xmp_createdate))
  expect_true(is.na(unicode$exififd_createdate))
})

test_that("enrich_snapshot leaves absent metadata as NA", {
  result <- enrich_snapshot(
    snapshot_fixtures, embedded_fixtures, "PNG:Description"
  )
  expect_true(all(is.na(
    result$png_description[result$filename != "observable.png"]
  )))
})

test_that("enrich_snapshot preserves snapshot metadata", {
  result <- enrich_snapshot(
    snapshot_fixtures, embedded_fixtures, "ExifIFD:CreateDate"
  )
  metadata <- setdiff(
    names(attributes(snapshot_fixtures)), c("names", "row.names", "class")
  )
  expect_equal(
    attributes(result)[metadata],
    attributes(snapshot_fixtures)[metadata]
  )
})

test_that("enrich_snapshot rejects unknown fields", {
  expect_error(
    enrich_snapshot(
      snapshot_fixtures, embedded_fixtures, "XMP:DefinitelyNotAField"
    ),
    "not found"
  )
})

test_that("enrich_snapshot validates its inputs", {
  expect_error(
    enrich_snapshot(
      data.frame(filename = "dated.jpg"), embedded_fixtures,
      "ExifIFD:CreateDate"
    ),
    "full_path"
  )
  expect_error(
    enrich_snapshot(
      snapshot_fixtures, data.frame(file = "dated.jpg"),
      "ExifIFD:CreateDate"
    ),
    "file.*field.*value"
  )
  expect_error(
    enrich_snapshot(snapshot_fixtures, embedded_fixtures, character()),
    "non-empty character vector"
  )
})

test_that("enrich_snapshot rejects duplicate observations", {
  duplicate <- rbind(
    embedded_fixtures, embedded_fixtures[1, , drop = FALSE]
  )
  expect_error(
    enrich_snapshot(
      snapshot_fixtures, duplicate, duplicate$field[[1]]
    ),
    "duplicate file-field"
  )
})
