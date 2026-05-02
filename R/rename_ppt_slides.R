#' @title Rename PowerPoint Slides
#' @param directory Directory containing Slide*.EXT files
#' @param presentation_name Base name for output files
#' @param extension File extension to match (case-sensitive, e.g. "PNG", "JPG")
#' @return Invisibly returns new file names
#' @export
rename_ppt_slides <- function(directory = ".",
                              presentation_name = "presentation",
                              extension = "PNG") {

  if (!dir.exists(directory)) {
    stop(directory, " does not exist.")
  }

  pattern <- paste0("^Slide[0-9]+\\.", extension, "$")

  files <- list.files(
    path = directory,
    pattern = pattern,
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning("No matching files found.")
    return(invisible(NULL))
  }

  # --- guard: check for other Slide files with different extensions ---
  all_slide_files <- list.files(
    path = directory,
    pattern = "^Slide[0-9]+\\.",
    full.names = FALSE
  )

  other_ext <- all_slide_files[!grepl(paste0("\\.", extension, "$"), all_slide_files)]

  if (length(other_ext) > 0) {
    warning(
      "Found Slide files with different extensions: ",
      paste(other_ext, collapse = ", "),
      "\nOnly processing *.", extension
    )
  }

  if (length(files) == 0) {
    warning("No matching files found.")
    return(invisible(NULL))
  }

  # --- extract and sort ---
  slide_numbers <- as.integer(
    sub(paste0("^Slide([0-9]+)\\.", extension, "$"), "\\1", basename(files))
  )

  ord <- order(slide_numbers)
  files <- files[ord]
  slide_numbers <- slide_numbers[ord]

  # --- pad ---
  width <- nchar(max(slide_numbers))
  padded <- sprintf(paste0("%0", width, "d"), slide_numbers)

  # --- rename ---
  new_files <- file.path(
    directory,
    paste0(presentation_name, "_", padded, ".", tolower(extension))
  )

  ok <- file.rename(files, new_files)

  if (!all(ok)) {
    warning("Some files could not be renamed.")
  } else {
    message("Renaming completed successfully.")
  }

  invisible(new_files)
}
