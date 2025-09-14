test_that("palette_eu_countries returns a named character vector", {
  cols <- palette_eu_countries()

  expect_type(cols, "character")
  expect_true(!is.null(names(cols)))
  expect_true(all(nchar(cols) > 0))
})

test_that("palette contains expected countries and groups", {
  cols <- palette_eu_countries()

  expected <- c("PL", "HU", "NL", "IT", "EU27_2020", "EU28", "EU15")
  expect_true(all(expected %in% names(cols)))
})

test_that("palette returns valid hex or color names", {
  cols <- palette_eu_countries()

  # Regex for hex color (e.g. #A1B2C3)
  hex_ok <- grepl("^#[0-9A-Fa-f]{6}$", cols)

  # Or known color names (e.g. "black")
  named_ok <- cols %in% grDevices::colors()

  expect_true(all(hex_ok | named_ok))
})

test_that("specific mappings are correct", {
  cols <- palette_eu_countries()

  expect_equal(cols["PL"], c(PL = "#DB001C")) # Poland = red
  expect_equal(unname(cols["NL"]), "#E88500") # Netherlands = orange
})
