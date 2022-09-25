test_that("add_greys works", {
  expect_equal(length(add_greys(grey_levels = c(50,70))), 2)
  expect_equal(length(add_greys(reprex_palette(), grey_levels = c(50,70))), 14)
})
