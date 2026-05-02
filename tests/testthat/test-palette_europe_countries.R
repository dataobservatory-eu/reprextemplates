test_that("palette_europe_countries has no NULL values", {
  cols <- palette_europe_countries()

  expect_false(any(vapply(cols, is.null, logical(1))))
})
