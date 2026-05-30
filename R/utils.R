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
