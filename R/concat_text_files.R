#' Concatenate text files into a single file (with optional source comments)
#'
#' Combine all files of a given extension (e.g. `txt`, `md`, `bib`) from a
#' source directory into one output file. Optionally inserts comment blocks
#' identifying the source file for traceability.
#'
#' @param source_dir Directory containing the files.
#' @param extension Character vector of extensions or patterns:
#'   e.g. c("R", "bak"), c("Rmd", "qmd"), or "*md".
#' @param output_file Path to the output file to be written.
#' @param recursive Logical; search subdirectories (default FALSE).
#' @param add_source_comment Logical; insert source comments (default TRUE).
#' @param comment_start,comment_end Comment delimiters.
#' @param source_comment "filename", "path", or "basename".
#'
#' @return (Invisibly) the output file path.
#' @importFrom stringr str_trim
#' @export
concat_text_files <- function(
  source_dir,
  extension = "txt",
  output_file = "combined.txt",
  recursive = FALSE,
  add_source_comment = TRUE,
  comment_start = "<!---",
  comment_end = "--->",
  source_comment = "filename"
) {
  ext_patterns <- vapply(
    extension,
    function(ext) {
      if (grepl("\\*", ext)) {
        paste0(gsub("\\*", ".*", ext), "$")
      } else {
        paste0("\\.", ext, "$")
      }
    }, character(1)
  )

  pattern <- paste(ext_patterns, collapse = "|")

  files <- list.files(
    source_dir,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )

  files <- files[basename(files) != basename(output_file)]

  if (length(files) == 0) {
    stop(
      "No files found for pattern: ",
      paste(extension, collapse = ", "),
      call. = FALSE
    )
  }

  files <- sort(files)
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
      output_lines <- c(
        output_lines,
        paste0(comment_start, " source: ", file_id, " ", comment_end),
        lines
      )
    } else {
      output_lines <- c(output_lines, lines)
    }
  }

  writeLines(output_lines, con = output_file)

  message("Combined ", length(files), " files into ", output_file)

  invisible(output_file)
}
