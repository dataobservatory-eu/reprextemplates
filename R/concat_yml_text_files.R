#' Concatenate files referenced in a Quarto YAML into a single file
#'
#' Reads a Quarto `_quarto.yml` (or similar YAML) and concatenates
#' the referenced text files (chapters, appendices, bibliography).
#'
#' @param yml_file Path to the YAML file.
#' @param output_file Output file path.
#' @param add_source_comment Logical; include source comments.
#' @param comment_start,comment_end Comment delimiters.
#' @param source_comment "filename", "path", or "basename".
#' @param include_sections Which YAML sections to include.
#'
#' @return (Invisibly) the output file path.
#' @importFrom yaml read_yaml
#' @importFrom stringr str_trim
#' @export
concat_yml_text_files <- function(
    yml_file,
    output_file = "combined.txt",
    add_source_comment = TRUE,
    comment_start = "<!---",
    comment_end = "--->",
    source_comment = "filename",
    include_sections = c("chapters", "appendices", "bibliography")
) {

  # --- read YAML ---
  yml <- yaml::read_yaml(yml_file)

  base_dir <- dirname(yml_file)

  files <- character()

  # --- extract chapters ---
  if ("chapters" %in% include_sections &&
      !is.null(yml$book$chapters)) {
    files <- c(files, yml$book$chapters)
  }

  # --- extract appendices ---
  if ("appendices" %in% include_sections &&
      !is.null(yml$book$appendices)) {
    files <- c(files, yml$book$appendices)
  }

  # --- extract bibliography ---
  if ("bibliography" %in% include_sections &&
      !is.null(yml$bibliography)) {
    files <- c(files, yml$bibliography)
  }

  # --- normalize paths ---
  files <- file.path(base_dir, files)

  # --- check existence ---
  exists <- file.exists(files)

  missing <- files[!exists]
  files   <- files[exists]

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

  # --- preserve YAML order (no sorting!) ---
  output_lines <- character()

  for (f in files) {

    lines <- readLines(f, warn = FALSE)
    lines <- stringr::str_trim(lines, side = "right")

    if (add_source_comment) {

      file_id <- switch(
        source_comment,
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
  n_total   <- length(files) + length(missing)
  n_missing <- length(missing)

  message(
    "Combined ", length(files), "/", n_total,
    " YAML-referenced files into ", output_file,
    if (n_missing > 0) paste0(" (", n_missing, " missing)") else ""
  )

  invisible(output_file)
}
