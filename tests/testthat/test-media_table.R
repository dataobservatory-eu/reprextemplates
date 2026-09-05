test_that("media() creates a media cell", {
  x <- media("photo.jpg", width = "80%", alt = "A photograph")

  expect_s3_class(x, "media_cell")
  expect_equal(x$src, "photo.jpg")
  expect_equal(x$width, "80%")
  expect_equal(x$alt, "A photograph")
})


test_that("media_table validates column widths", {
  x <- matrix(
    list(
      media("a.jpg"),
      "Some text"
    ),
    nrow = 1
  )

  expect_error(
    media_table(x, widths = c(1, 1, 1)),
    "`widths` must contain one value for each column"
  )
})


test_that("media_table produces HTML media-text layout", {
  testthat::local_mocked_bindings(
    is_html_output = function(...) TRUE,
    is_latex_output = function(...) FALSE,
    .package = "knitr"
  )

  farmhouse <- system.file(
    "testdata",
    "media-table",
    "P7101565_thumbnail.jpg",
    package = "reprextemplates"
  )

  expect_true(nzchar(farmhouse))
  expect_true(file.exists(farmhouse))

  x <- matrix(
    list(
      media(farmhouse, alt = "Farmhouse"),
      paste(
        "We created five images of the",
        "*farmhouse (dzīvojamā māja)* from two directions."
      )
    ),
    nrow = 1
  )

  html <- as.character(media_table(x, widths = c(50, 50)))

  expect_match(html, basename(farmhouse), fixed = TRUE)
  expect_match(html, "data-qmd", fixed = TRUE)
  expect_match(html, "vertical-align:middle", fixed = TRUE)
  expect_false(grepl("<th", html, fixed = TRUE))
})


test_that("media_table produces LaTeX media-text layout", {
  testthat::local_mocked_bindings(
    is_html_output = function(...) FALSE,
    is_latex_output = function(...) TRUE,
    .package = "knitr"
  )

  farmhouse <- system.file(
    "testdata",
    "media-table",
    "P7101565_thumbnail.jpg",
    package = "reprextemplates"
  )

  expect_true(nzchar(farmhouse))
  expect_true(file.exists(farmhouse))

  x <- matrix(
    list(
      media(farmhouse, alt = "Farmhouse"),
      paste(
        "We created five images of the",
        "*farmhouse (dzīvojamā māja)* from two directions."
      )
    ),
    nrow = 1
  )

  tex <- as.character(media_table(x, widths = c(50, 50)))

  expect_match(tex, "\\includegraphics", fixed = TRUE)
  expect_match(tex, "\\QuartoMarkdownBase64", fixed = TRUE)
  expect_match(tex, "m{7.5cm}", fixed = TRUE)
})


test_that("media_table supports a two-image layout", {
  testthat::local_mocked_bindings(
    is_html_output = function(...) TRUE,
    is_latex_output = function(...) FALSE,
    .package = "knitr"
  )

  document <- system.file(
    "testdata",
    "media-table",
    "P7101622_thumbnail.jpg",
    package = "reprextemplates"
  )

  floorplan <- system.file(
    "testdata",
    "media-table",
    "P7101623_thumbnail.jpg",
    package = "reprextemplates"
  )

  expect_true(nzchar(document))
  expect_true(nzchar(floorplan))
  expect_true(file.exists(document))
  expect_true(file.exists(floorplan))

  x <- matrix(
    list(
      media(document, alt = "Document"),
      media(floorplan, alt = "Floor plan")
    ),
    nrow = 1
  )

  html <- as.character(media_table(x, widths = c(50, 50)))

  expect_match(html, basename(document), fixed = TRUE)
  expect_match(html, basename(floorplan), fixed = TRUE)
})


test_that("media_table supports mixed two-by-two layouts", {
  testthat::local_mocked_bindings(
    is_html_output = function(...) TRUE,
    is_latex_output = function(...) FALSE,
    .package = "knitr"
  )

  farmhouse <- system.file(
    "testdata",
    "media-table",
    "P7101565_thumbnail.jpg",
    package = "reprextemplates"
  )

  document <- system.file(
    "testdata",
    "media-table",
    "P7101622_thumbnail.jpg",
    package = "reprextemplates"
  )

  floorplan <- system.file(
    "testdata",
    "media-table",
    "P7101623_thumbnail.jpg",
    package = "reprextemplates"
  )

  paths <- c(farmhouse, document, floorplan)

  expect_true(all(nzchar(paths)))
  expect_true(all(file.exists(paths)))

  x <- matrix(
    list(
      media(document, alt = "Document"),
      media(floorplan, alt = "Floor plan"),
      "Some *formatted* explanatory text.",
      media(farmhouse, alt = "Farmhouse")
    ),
    nrow = 2,
    byrow = TRUE
  )

  html <- as.character(media_table(x, widths = c(50, 50)))

  expect_match(html, basename(document), fixed = TRUE)
  expect_match(html, basename(floorplan), fixed = TRUE)
  expect_match(html, basename(farmhouse), fixed = TRUE)
  expect_match(html, "data-qmd", fixed = TRUE)

  td_positions <- gregexpr("<td", html, fixed = TRUE)[[1]]
  expect_equal(length(td_positions), 4L)
})
