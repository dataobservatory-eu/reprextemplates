#' Create review bundles for supported project types
#'
#' Recursively scans a directory tree, detects supported project types,
#' and creates review bundles using the corresponding profile from
#' [concat_files_profile()].
#'
#' Currently supported project types are:
#'
#' - `r_package`: directories containing both `DESCRIPTION` and `NAMESPACE`
#' - `quarto`: directories containing `_quarto.yml`
#' - `hugo`: directories containing `config.yaml`, `config.yml`,
#'   or `config.toml`
#'
#' For each detected project, the function:
#'
#' - Creates a review bundle named `review-[project_name].txt`
#' - Uses the matching profile from [concat_files_profile()]
#' - Adds the review bundle to `.gitignore`
#' - Adds the review bundle to `.Rbuildignore` for R packages
#'
#' Directories that do not match a known project profile are ignored.
#'
#' @param root_dir Root directory to search recursively.
#'   Defaults to [here::here()] when `NULL`.
#' @param exclude_patterns Defaults to `c("/tests/testthat/testdata/",
#' "/inst/extdata/")`
#'
#' @return Invisibly returns a character vector containing the
#'   directories for which review bundles were created.
#'
#' @examples
#' \dontrun{
#'
#' create_review_bundles()
#'
#' create_review_bundles(
#'   root_dir = "D:/allcontent"
#' )
#'
#' create_review_bundles(
#'   root_dir = here::here("..")
#' )
#' }
#'
#' @seealso [concat_files_profile()],
#'   [concat_ordered_text_files()]
#'
#' @importFrom here here
#' @export

create_review_bundles <- function(
  root_dir = NULL,
  exclude_patterns = c(
    "/tests/testthat/testdata/",
    "/inst/"
  )
) {
  if (is.null(root_dir)) {
    root_dir <- here::here()
  }

  candidate_dirs <- list.dirs(
    root_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  candidate_dirs <- normalizePath(
    c(root_dir, candidate_dirs), winslash = "/", mustWork = TRUE
    )

  candidate_dirs <- unique(candidate_dirs)

  if (!is.null(exclude_patterns)) {

    candidate_dirs <- candidate_dirs[
      !vapply(
        candidate_dirs,
        function(x) {
          path <- gsub("\\\\", "/", x)
          any(
            vapply(
              exclude_patterns,
              grepl,
              logical(1),
              x = path,
              fixed = TRUE
            )
          )
        },
        logical(1)
      )
    ]
  }

  processed <- character()

  for (dir_path in candidate_dirs) {
    profile <- detect_review_profile(dir_path)

    if (is.null(profile)) {
      next
    }

    review_file <- paste0(
      "review-",
      basename(dir_path),
      ".txt"
    )

    review_path <- file.path(
      dir_path,
      review_file
    )

    concat_ordered_text_files(
      source_dir = dir_path,
      groups = concat_files_profile(profile),
      profile = profile,
      output_file = review_path
    )

    append_ignore_entry(
      file.path(dir_path, ".gitignore"),
      review_file
    )

    if (profile == "r_package") {
      append_ignore_entry(
        file.path(dir_path, ".Rbuildignore"),
        paste0("^", review_file, "$")
      )
    }

    processed <- c(processed, dir_path)
  }

  invisible(processed)
}


#' Detect review profile from project structure
#'
#' @param path Directory to inspect.
#'
#' @return A profile name or `NULL`.
#'
#' @keywords internal
#' @noRd
detect_review_profile <- function(path) {

  if (
    all(
      file.exists(
        file.path(
          path,
          c("DESCRIPTION", "NAMESPACE")
        )
      )
    )
  ) {
    return("r_package")
  }

  if (file.exists(file.path(path, "_quarto.yml"))) {
    return("quarto")
  }

  if (
    file.exists(file.path(path, "config.toml")) ||
    file.exists(file.path(path, "config.yaml")) ||
    file.exists(file.path(path, "config.yml"))
  ) {
    return("hugo")
  }

  NULL
}
