#' @title Rename files in directory
#' @param path_to_files Path to the files to rename
#' @param new_path New path
#' @param common_name Common file name part as a string.
#' @return The renamed files in \code{new_path}
#' @export

#filenames <- paste0("Slide", 1:12, ".PNG")
#path_to_files <- file.path("data-raw", "20241121_D_Antal_IAMIC_lowres")
#common_name <- "Slides_D_Antal_IAMIC_20241121"


rename_dir_files <- function(path_to_files, new_path, common_name) {
  filenames <- dir(path_to_files)
  file_extensions <- tolower(vapply(strsplit(filenames, "\\."), function(x) x[2], character(1)))
  file_numbers <- as.numeric(unlist(regmatches(filenames, gregexpr("[[:digit:]]+", filenames))))
  file_numbers <- ifelse(file_numbers<10, paste0("0", file_numbers), as.character(file_numbers))
  new_file_names <- paste0(file_numbers, "_", common_name, ".", file_extensions)
  file.copy()

  sapply(1:length(filenames), function(x) file.copy(file.path(path_to_files, filenames[x]),
                                           file.path(new_path, new_file_names[x]), overwrite=TRUE))

}



