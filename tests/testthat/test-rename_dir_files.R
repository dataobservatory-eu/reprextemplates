test_rename_dir_files <- function() {
  temp_in  <- file.path(tempdir(), "input")
  temp_out <- file.path(tempdir(), "output")
  dir.create(temp_in, showWarnings = FALSE)
  dir.create(temp_out, showWarnings = FALSE)

  # Create dummy image files
  file_names <- c("img1.png", "photo02.jpg", "test3.PNG", "skip.txt")
  dummy_data <- raw(10)

  for (name in file_names) {
    file_path <- file.path(temp_in, name)
    writeBin(dummy_data, file_path)
  }

  rename_dir_files(path_to_files = temp_in, new_path = temp_out, common_name = "renamed")

  output_files <- dir(temp_out)
  cat("Output files:\n")
  print(output_files)

  # Simple checks
  stopifnot(any(grepl("_renamed\\.png$", output_files)))
  stopifnot(any(grepl("_renamed\\.jpg$", output_files)))
  stopifnot(!any(grepl("\\.txt$", output_files)))  # .txt shouldn't be copied

  message("All tests passed.")
}

# Run tests
test_rename_dir_files()
