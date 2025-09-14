#' Concatenate text files into a single file
#'
#' Combine all files of a given extension (e.g. `txt`, `md`, `bib`) from a source
#' directory into one output file. The function preserves the order returned by
#' [base::list.files()] and trims trailing whitespace on each line.
#'
#' @param source_dir A character string giving the directory containing the files.
#' @param extension File extension (without leading dot) to match, e.g. `"txt"`.
#' @param output_file Path to the output file where the concatenated text will be written.
#'
#' @return (Invisibly) the path to the combined output file.
#'
#' @importFrom stringr str_trim
#' @examples
#' \dontrun{
#' # Combine all .bib files into one
#' concat_text_files("bib", extension = "bib", output_file = "references_all.bib")
#' }
#'
#' @seealso [base::list.files()], [base::readLines()], [base::writeLines()]
#'
#' @export
concat_text_files <- function(source_dir, extension = "txt", output_file = "combined.txt") {
  pattern <- paste0("\\.", extension, "$")
  files <- list.files(source_dir, pattern = pattern, full.names = TRUE)

  # Exclude the target output file if it already exists in the same folder
  files <- setdiff(files, normalizePath(output_file, mustWork = FALSE))

  if (length(files) == 0) {
    stop("No files found with extension: ", extension)
  }

  contents <- lapply(files, function(file) readLines(file, warn = FALSE))
  all_text <- unlist(contents)
  all_text <- stringr::str_trim(all_text, side = "right")

  writeLines(all_text, con = output_file)
  message("✅ Combined ", length(files), " .", extension, " files into ", output_file)

  invisible(output_file)
}
