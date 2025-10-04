test_that("add_greys returns only greys when no palette supplied", {
  cols <- add_greys(grey_levels = c(20, 50, 80))

  expect_type(cols, "character")
  expect_equal(names(cols), c("grey20", "grey50", "grey80"))
  expect_equal(length(cols), 3)

  # All should be valid hex colors
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("add_greys extends an existing palette", {
  base_pal <- c("red", "blue")
  cols <- add_greys(base_pal, grey_levels = c(40, 60))

  expect_true(all(base_pal %in% cols))
  expect_true(all(c("grey40", "grey60") %in% names(cols)))
  expect_equal(length(cols), length(base_pal) + 2)
})

test_that("add_greys errors on invalid input", {
  expect_error(
    add_greys(pal = 123, grey_levels = 50),
    "must be a character vector"
  )
})
