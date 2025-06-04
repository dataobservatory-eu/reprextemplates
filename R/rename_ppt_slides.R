#' @title Rename PowerPoint Slides
#' @param director
#' @param presentation_name
#' @return The function renames the files

# Main function to rename PowerPoint-exported slide PNGs
rename_ppt_slides <- function(directory = ".",
                              presentation_name = "presentation_name") {
  if (!dir.exists(directory)) {
    stop(paste0(directory, " does not exist."))
  }

  # List matching files
  files <- list.files(path = directory,
                      pattern = "^Slide[0-9]+\\.PNG$",
                      full.names = TRUE)

  if (length(files) == 0) {
    warning("No matching files found.")
    return(invisible(NULL))
  }

  # Extract slide numbers and sort
  slide_numbers <- as.integer(gsub("Slide([0-9]+)\\.PNG", "\\1", basename(files)))
  sorted_idx <- order(slide_numbers)
  files <- files[sorted_idx]
  slide_numbers <- slide_numbers[sorted_idx]

  # Pad slide numbers
  padded_numbers <- pad_numbers(slide_numbers)

  # Rename files
  for (i in seq_along(files)) {
    new_name <- file.path(directory, paste0(presentation_name, "_", padded_numbers[i], ".png"))
    file.rename(files[i], new_name)
  }

  message("Renaming completed successfully.")
}

#' @keywords internal
pad_numbers <- function(numbers) {
  width <- nchar(as.character(max(numbers)))
  sprintf(paste0("%0", width, "d"), numbers)
}
