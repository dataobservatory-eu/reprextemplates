test_that("multiplication works", {
  expect_equal(
    normalise_embedded_name("IPTC:Caption-Abstract"),
    "iptc_caption_abstract"
  )

  expect_equal(
    normalise_embedded_name("XMP-dc:Description"),
    "xmp_dc_description"
  )

  expect_equal(
    normalise_embedded_name("ExifIFD:CreateDate"),
    "exififd_createdate"
  )
})
