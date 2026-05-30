#' Concatenate ordered text files into a single file
#'
#' Concatenate text files from explicitly ordered groups of files and
#' directories into a single output file. This function is useful for
#' creating review bundles, package summaries, LLM context files,
#' documentation archives, or forensic snapshots.
#'
#' Files are processed in the order defined by `groups`. Within each
#' directory group, files are sorted alphabetically to ensure
#' reproducibility.
#'
#' @param source_dir Directory containing the source files.
#'   For R package projects, `here::here()` is often a convenient way
#'   to refer to the package root directory.
#' @param groups A list defining the concatenation order. Each element
#'   should contain either:
#'   \describe{
#'     \item{files}{Character vector of explicit file names.}
#'     \item{folder}{Directory name relative to `source_dir`.}
#'     \item{extension}{Optional character vector of file extensions
#'       used to filter files within a folder.}
#'   }
#' @param profile Optional name of the profile used to construct
#'   `groups`. When supplied, the profile name is included in the
#'   generated bundle header. See [concat_files_profile()] for
#'   predefined profiles.
#' @param output_file Path to the output file.
#' @param recursive Logical. Should folder searches recurse into
#'   subdirectories?
#' @param add_source_comment Logical. Add source file comments before
#'   each file.
#' @param comment_start Opening comment delimiter.
#' @param comment_end Closing comment delimiter.
#' @param source_comment One of `"relative_path"`, `"filename"`,
#'   or `"absolute_path"`.
#'
#' @return Invisibly returns the path to the created file.
#'
#' @examples
#' example_dir <- tempfile("bundle")
#' dir.create(example_dir)
#'
#' writeLines(
#'   "Package: examplepkg",
#'   file.path(example_dir, "DESCRIPTION")
#' )
#'
#' writeLines(
#'   "# Example package",
#'   file.path(example_dir, "README.md")
#' )
#'
#' output_file <- file.path(
#'   tempdir(),
#'   "package_review.txt"
#' )
#'
#' groups <- list(
#'   list(
#'     files = c(
#'       "DESCRIPTION",
#'       "README.md"
#'     )
#'   )
#' )
#'
#' concat_ordered_text_files(
#'   source_dir = example_dir,
#'   groups = groups,
#'   output_file = output_file
#' )
#'
#' file.exists(output_file)
#' @importFrom stringr str_trim
#' @importFrom here here
#' @export
concat_ordered_text_files <- function(
  source_dir,
  groups,
  output_file = "combined.txt",
  profile = NULL,
  recursive = TRUE,
  add_source_comment = TRUE,
  comment_start = "<!---",
  comment_end = "--->",
  source_comment = c(
    "relative_path",
    "filename",
    "absolute_path"
  )
) {
  if (!is.list(groups)) {
    stop("groups must be a list.", call. = FALSE)
  }

  if (!all(vapply(groups, is.list, logical(1)))) {
    stop(
      "groups must be a list of group definitions.",
      call. = FALSE
    )
  }

  if (!is.null(profile) &&
    (!is.character(profile) ||
      length(profile) != 1 ||
      is.na(profile))) {
    stop(
      "`profile` must be NULL or a single character value.",
      call. = FALSE
    )
  }

  source_comment <- match.arg(source_comment)

  source_dir <- normalizePath(
    source_dir,
    winslash = "/",
    mustWork = TRUE
  )

  selected_files <- character()

  for (group in groups) {
    if (!is.null(group$files)) {
      explicit_files <- file.path(
        source_dir,
        group$files
      )

      explicit_files <- explicit_files[
        file.exists(explicit_files)
      ]

      selected_files <- c(
        selected_files,
        explicit_files
      )
    }

    if (!is.null(group$folder)) {
      folder_path <- file.path(
        source_dir,
        group$folder
      )

      if (!dir.exists(folder_path)) next

      folder_files <- list.files(
        folder_path,
        full.names = TRUE,
        recursive = recursive
      )

      if (!is.null(group$extension)) {
        pattern <- paste0(
          "\\.(",
          paste(group$extension, collapse = "|"),
          ")$"
        )

        folder_files <- folder_files[
          grepl(
            pattern,
            folder_files,
            ignore.case = TRUE
          )
        ]
      }

      selected_files <- c(
        selected_files,
        sort(folder_files)
      )
    }
  }

  selected_files <- unique(selected_files)

  output_file_abs <- normalizePath(
    output_file,
    winslash = "/",
    mustWork = FALSE
  )

  selected_files <- selected_files[
    normalizePath(
      selected_files,
      winslash = "/",
      mustWork = FALSE
    ) != output_file_abs
  ]

  if (length(selected_files) == 0) {
    stop("No files selected.", call. = FALSE)
  }

  included_items <- character()

  for (group in groups) {
    if (!is.null(group$files)) {
      included_items <- c(
        included_items,
        group$files
      )
    }

    if (!is.null(group$folder)) {
      included_items <- c(
        included_items,
        paste0(
          group$folder,
          "/*.",
          paste(group$extension, collapse = ",")
        )
      )
    }
  }

  output_lines <- c(
    "Package review bundle",
    "",
    paste(
      "Generated:",
      format(
        Sys.time(),
        tz = "UTC",
        usetz = TRUE
      )
    ),
    paste("Project root:", basename(source_dir)),
    paste("Source directory:", source_dir)
  )

  if (!is.null(profile)) {
    output_lines <- c(
      output_lines,
      paste("Profile:", profile)
    )
  }

  output_lines <- c(
    output_lines,
    "",
    "Included files:",
    paste0("- ", included_items),
    "",
    "------------------------------------------------------------",
    ""
  )

  for (f in selected_files) {
    lines <- readLines(
      f,
      warn = FALSE,
      encoding = "UTF-8"
    )

    lines <- stringr::str_trim(
      lines,
      side = "right"
    )

    if (add_source_comment) {
      file_id <- switch(source_comment,
        relative_path = sub(
          paste0("^", source_dir, "/?"),
          "",
          normalizePath(f, winslash = "/")
        ),
        filename = basename(f),
        absolute_path = normalizePath(
          f,
          winslash = "/"
        )
      )

      output_lines <- c(
        output_lines,
        paste0(
          comment_start,
          " source: ",
          file_id,
          " ",
          comment_end
        ),
        ""
      )
    }

    output_lines <- c(
      output_lines,
      lines,
      ""
    )
  }

  writeLines(
    output_lines,
    con = output_file,
    useBytes = TRUE
  )

  message(
    "Combined ",
    length(selected_files),
    " files into ",
    output_file
  )

  invisible(output_file)
}
