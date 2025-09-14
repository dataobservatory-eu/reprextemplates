#' Rename and copy image files from one directory to another
#' @param path_to_files Source directory containing image files
#' @param new_path Destination directory
#' @param common_name Base name for the renamed files
#' @export
#' @examples
#' # Create temporary input and output directories
#' input_dir <- file.path(tempdir(), "rename_example_in")
#' output_dir <- file.path(tempdir(), "rename_example_out")
#' dir.create(input_dir, showWarnings = FALSE)
#' dir.create(output_dir, showWarnings = FALSE)
#'
#' # Create dummy image files with numbers in names
#' file.create(file.path(input_dir, "img1.png"))
#' file.create(file.path(input_dir, "photo02.jpg"))
#' file.create(file.path(input_dir, "image3.PNG")) # Will be ignored (case sensitive)
#' file.create(file.path(input_dir, "note.txt")) # Will be ignored
#'
#' # Run the function
#' rename_dir_files(
#'   path_to_files = input_dir,
#'   new_path = output_dir,
#'   common_name = "renamed"
#' )
#'
#' # See the renamed files
#' list.files(output_dir)
rename_dir_files <- function(path_to_files, new_path, common_name) {
  if (!dir.exists(path_to_files)) {
    stop(paste0(path_to_files, " does not exist."))
  }

  if (!dir.exists(new_path)) {
    stop(paste0(new_path, " does not exist."))
  }

  filenames <- dir(path_to_files)
  if (length(filenames) == 0) stop("No files in the directory: ", path_to_files)

  file_extensions <- tolower(tools::file_ext(filenames))
  valid_idx <- file_extensions %in% c("png", "jpg")

  if (!any(valid_idx)) stop("No valid image files found.")

  filenames <- filenames[valid_idx]
  file_extensions <- file_extensions[valid_idx]

  file_numbers <- as.numeric(unlist(regmatches(filenames, gregexpr("[[:digit:]]+", filenames))))
  file_numbers_padded <- pad_numbers(file_numbers)

  new_file_names <- paste0(file_numbers_padded, "_", common_name, ".", file_extensions)
  file_copying <- mapply(function(from, to) {
    file.copy(
      from = file.path(path_to_files, from),
      to = file.path(new_path, to),
      overwrite = TRUE
    )
  }, filenames, new_file_names)

  if (!all(file_copying)) {
    warning("Problem files:\n", paste(file.path(new_path, new_file_names)[!file_copying], collapse = "\n"))
  } else {
    message("Copied to ", new_path)
  }
}
