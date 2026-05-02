test_that("rename_ppt_slides works for PNG", {
  tmp <- file.path(tempdir(), "ppt_png")
  dir.create(tmp, showWarnings = FALSE)

  file.create(file.path(tmp, c("Slide1.PNG", "Slide10.PNG", "Slide2.PNG")))

  rename_ppt_slides(tmp, "deck", extension = "PNG")

  out <- sort(list.files(tmp, pattern = "^deck_.*\\.png$"))
  expect_equal(out, c("deck_01.png", "deck_02.png", "deck_10.png"))
})

test_that("rename_ppt_slides warns when no matching files", {
  tmp <- file.path(tempdir(), "ppt_empty")
  dir.create(tmp, showWarnings = FALSE)

  expect_warning(rename_ppt_slides(tmp, "deck", "PNG"))
})


test_that("rename_ppt_slides warns on mixed extensions", {
  tmp <- file.path(tempdir(), "ppt_mixed")
  dir.create(tmp, showWarnings = FALSE)

  file.create(file.path(tmp, c("Slide1.PNG", "Slide2.JPG")))

  expect_warning(
    rename_ppt_slides(tmp, "deck", extension = "PNG"),
    "different extensions"
  )
})
