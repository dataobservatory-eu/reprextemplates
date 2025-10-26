#' Concatenate text files into a single file (with optional source comments)
#'
#' Combine all files of a given extension (e.g. `txt`, `md`, `bib`) from a
#' source directory into one output file. Optionally inserts comment blocks
#' identifying the source file for traceability.
#'
#' @param source_dir Directory containing the files.
#' @param extension File extension (without dot) to match, e.g. `"txt"`.
#' @param output_file Path to the output file to be written.
#' @param add_source_comment Logical; if `TRUE`, insert comment lines indicating
#'   the source file between each block.
#' @param comment_start,comment_end Strings marking the start and end of comment
#'   blocks (default HTML style `"<!---"` / `"--->"`).
#' @param source_comment Type of information in the comment; currently
#'   `"filename"` (default). Future options could include `"path"` or
#'   `"basename"`.
#'
#' @return (Invisibly) the path to the combined output file.
#'
#' @importFrom stringr str_trim
#' @export
concat_text_files <- function(source_dir,
                              extension = "txt",
                              output_file = "combined.txt",
                              add_source_comment = FALSE,
                              comment_start = "<!---",
                              comment_end = "--->",
                              source_comment = "filename") {
  pattern <- paste0("\\.", extension, "$")
  files <- list.files(source_dir, pattern = pattern, full.names = TRUE)

  # Exclude any file that matches the output file name
  files <- files[basename(files) != basename(output_file)]

  if (length(files) == 0) {
    stop("No files found with extension: ", extension)
  }

  output_lines <- character()

  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    lines <- stringr::str_trim(lines, side = "right")

    if (add_source_comment) {
      file_id <- switch(source_comment,
        filename = basename(f),
        path = f,
        basename = basename(f),
        f
      )
      comment_block <- paste0(
        comment_start, " source: ",
        file_id, " ", comment_end
      )
      output_lines <- c(output_lines, comment_block, lines)
    } else {
      output_lines <- c(output_lines, lines)
    }
  }

  writeLines(output_lines, con = output_file)
  message(
    "✅ Combined ", length(files), " .", extension,
    " files into ", output_file
  )

  invisible(output_file)
}
