test_that("find_embedded_fields finds and counts matching fields", {
  x <- data.frame(
    file = c("a.jpg", "a.jpg", "b.jpg", "b.jpg", "c.jpg"),
    field = c(
      "System:FileCreateDate",
      "ExifIFD:CreateDate",
      "System:FileCreateDate",
      "XMP-dc:Title",
      "XMP-xmp:CreateDate"
    )
  )

  out <- find_embedded_fields(x, "CreateDate")

  expect_equal(
    out$field,
    c(
      "ExifIFD:CreateDate",
      "System:FileCreateDate",
      "XMP-xmp:CreateDate"
    )
  )
  expect_equal(out$n, c(1L, 2L, 1L))
})


test_that("find_embedded_fields ignores case by default", {
  x <- data.frame(
    file = c("a.jpg", "b.jpg"),
    field = c("ExifIFD:CreateDate", "XMP-dc:Title")
  )

  out <- find_embedded_fields(x, "createdate")

  expect_equal(out$field, "ExifIFD:CreateDate")
  expect_equal(out$n, 1L)
})


test_that("find_embedded_fields counts each file once", {
  x <- data.frame(
    file = c("a.jpg", "a.jpg", "b.jpg"),
    field = rep("ExifIFD:CreateDate", 3)
  )

  out <- find_embedded_fields(x, "CreateDate")

  expect_equal(out$n, 2L)
})


test_that("find_embedded_fields returns empty results for no matches", {
  x <- data.frame(
    file = "a.jpg",
    field = "XMP-dc:Title"
  )

  out <- find_embedded_fields(x, "CreateDate")

  expect_equal(names(out), c("field", "n"))
  expect_equal(nrow(out), 0L)
})


test_that("find_embedded_fields validates its input", {
  expect_error(
    find_embedded_fields("not a data frame", "CreateDate"),
    "`x` must contain a `field` column.",
    fixed = TRUE
  )

  expect_error(
    find_embedded_fields(data.frame(file = "a.jpg"), "CreateDate"),
    "`x` must contain a `field` column.",
    fixed = TRUE
  )

  x <- data.frame(file = "a.jpg", field = "ExifIFD:CreateDate")

  expect_error(
    find_embedded_fields(x, c("CreateDate", "ModifyDate")),
    "`pattern` must be a single character string.",
    fixed = TRUE
  )

  expect_error(
    find_embedded_fields(x, 1),
    "`pattern` must be a single character string.",
    fixed = TRUE
  )
})
