#' Create review bundles for R packages
#'
#' Recursively scan a directory tree for R packages and create
#' package review bundles using [concat_ordered_text_files()].
#'
#' A directory is considered an R package if it contains both
#' `DESCRIPTION` and `NAMESPACE`.
#'
#' For each package found, the function:
#'
#' - Creates a review bundle named
#'   `package_review-[package_name].txt`
#' - Uses the `"R package"` profile from
#'   [concat_files_profile()]
#' - Adds the review bundle filename to `.gitignore`
#' - Adds the review bundle filename pattern to `.Rbuildignore`
#'
#' Existing ignore entries are not duplicated.
#'
#' @param root_dir Root directory to search recursively for
#'   R packages. Defaults to [here::here()].
#'
#' @return Invisibly returns a character vector containing the
#'   package directories that were processed.
#'
#' @examples
#' \dontrun{
#'
#' create_package_review_bundles(
#'   root_dir = "D:/_packages"
#' )
#'
#' create_package_review_bundles(
#'   root_dir = here::here("..")
#' )
#' }
#'
#' @importFrom here here
#' @export
create_package_review_bundles <- function(
  root_dir = here::here()
) {
  candidate_dirs <- list.dirs(
    root_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  package_dirs <- candidate_dirs[
    vapply(
      candidate_dirs,
      is_r_package_dir,
      logical(1)
    )
  ]

  for (pkg_dir in package_dirs) {
    review_file <- paste0(
      "package_review-",
      basename(pkg_dir),
      ".txt"
    )

    review_path <- file.path(pkg_dir, review_file)

    concat_ordered_text_files(
      source_dir = pkg_dir,
      groups = concat_files_profile("R package"),
      profile = "R package",
      output_file = review_path
    )

    # Append .gitignore
    append_ignore_entry(file.path(pkg_dir, ".gitignore"), review_file)

    # Append .Rbuildignore
    append_ignore_entry(
      file.path(pkg_dir, ".Rbuildignore"),
      paste0("^", review_file, "$")
    )
  }

  invisible(package_dirs)
}

#' @keywords internal
is_r_package_dir <- function(path) {
  required_files <- c("DESCRIPTION", "NAMESPACE")
  all(file.exists(file.path(path, required_files)))
}

#' @keywords internal
append_ignore_entry <- function(file, entry) {
  if (!file.exists(file)) {
    writeLines(entry, file)
    return(invisible(FALSE))
  }

  existing <- readLines(file, warn = FALSE)

  if (!entry %in% existing) {
    write(entry, file = file, append = TRUE)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}
