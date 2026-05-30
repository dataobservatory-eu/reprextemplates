#' Concatenate files referenced in a Quarto YAML into a single file
#'
#' Lightweight parser: extracts lines that start with "-" and end with
#' .qmd or .bib. No YAML dependency required.
#'
#' @param yml_file Path to the YAML file.
#' @param output_file Output file path.
#' @param add_source_comment Logical; include source comments.
#' @param comment_start,comment_end Comment delimiters.
#' @param source_comment "filename", "path", or "basename".
#'
#' @return (Invisibly) the output file path.
#' @importFrom stringr str_trim
#' @export
concat_yml_text_files <- function(
  yml_file,
  output_file = "combined.txt",
  add_source_comment = TRUE,
  comment_start = "<!---",
  comment_end = "--->",
  source_comment = "filename"
) {
  # --- read raw YAML as text ---
  yml_lines <- readLines(yml_file, warn = FALSE)

  # --- extract file references ---
  # pattern: "- something.qmd" or "- something.bib"
  matches <- grep(
    "^\\s*-\\s*[^#]+\\.(qmd|bib)\\s*$",
    yml_lines,
    value = TRUE
  )

  # clean "- " prefix and whitespace
  files <- sub("^\\s*-\\s*", "", matches)
  files <- trimws(files)

  base_dir <- dirname(yml_file)

  # --- normalize paths ---
  files <- file.path(base_dir, files)

  # --- check existence ---
  exists <- file.exists(files)

  missing <- files[!exists]
  files <- files[exists]

  if (length(missing) > 0) {
    warning(
      paste0(
        "The following files listed in YAML were not found:\n",
        paste0(" - ", missing, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  if (length(files) == 0) {
    stop("No valid files found in YAML.", call. = FALSE)
  }

  # --- concatenate ---
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

  # --- reporting ---
  n_total <- length(files) + length(missing)
  n_missing <- length(missing)

  message(
    "Combined ", length(files), "/", n_total,
    " YAML-referenced files into ", output_file,
    if (n_missing > 0) paste0(" (", n_missing, " missing)") else ""
  )

  invisible(output_file)
}
