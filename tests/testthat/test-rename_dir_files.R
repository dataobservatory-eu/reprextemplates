library(testthat)

test_that("rename_dir_files works correctly with image files", {
  temp_in  <- file.path(tempdir(), "input_test")
  temp_out <- file.path(tempdir(), "output_test")
  dir.create(temp_in, showWarnings = FALSE)
  dir.create(temp_out, showWarnings = FALSE)

  # Create dummy files
  file_names <- c("img1.png", "photo02.jpg", "test3.PNG", "skip.txt")
  dummy_data <- raw(10)
  for (name in file_names) {
    writeBin(dummy_data, file.path(temp_in, name))
  }

  expect_message(
    rename_dir_files(path_to_files = temp_in, new_path = temp_out, common_name = "renamed"),
    regexp = "Copied to"
  )

  output_files <- dir(temp_out)

  # Check expected output
  expect_true(any(grepl("^0*1_renamed\\.png$", output_files)))
  expect_true(any(grepl("^0*2_renamed\\.jpg$", output_files)))
  expect_true(any(grepl("^0*3_renamed\\.png$", output_files)))  # test3.PNG is .png

  expect_false(any(grepl("\\.txt$", output_files)))  # skip.txt shouldn't be copied
})

test_that("rename_dir_files throws error for missing directories", {
  bogus_dir <- file.path(tempdir(), "no_such_dir")

  expect_error(
    rename_dir_files(path_to_files = bogus_dir, new_path = tempdir(), common_name = "test"),
    regexp = "does not exist"
  )

  expect_error(
    rename_dir_files(path_to_files = tempdir(), new_path = bogus_dir, common_name = "test"),
    regexp = "does not exist"
  )
})

test_that("rename_dir_files stops if no image files are present", {
  temp_in <- file.path(tempdir(), "no_images")
  temp_out <- file.path(tempdir(), "no_images_out")
  dir.create(temp_in, showWarnings = FALSE)
  dir.create(temp_out, showWarnings = FALSE)

  writeLines("hello", file.path(temp_in, "note.txt"))

  expect_error(
    rename_dir_files(path_to_files = temp_in, new_path = temp_out, common_name = "fail"),
    regexp = "No valid image files found"
  )
})

test_that("rename_dir_files warns on failed copy", {
  skip_on_os("windows")  # Permission manipulation is tricky on Windows

  temp_in  <- file.path(tempdir(), "input_warn")
  temp_out <- file.path(tempdir(), "output_warn")
  dir.create(temp_in, showWarnings = FALSE)
  dir.create(temp_out, showWarnings = FALSE)

  # Create a valid image file
  file_name <- "img1.jpg"
  writeBin(raw(10), file.path(temp_in, file_name))

  # Pre-create target file with no write permission
  dest_file <- file.path(temp_out, "1_renamed.jpg")
  file.create(dest_file)
  Sys.chmod(dest_file, "0444")  # Make it read-only (UNIX)

  expect_warning(
    rename_dir_files(path_to_files = temp_in, new_path = temp_out, common_name = "renamed"),
    regexp = "Problem files"
  )

  Sys.chmod(dest_file, "0644")  # Reset permissions for cleanup
})
