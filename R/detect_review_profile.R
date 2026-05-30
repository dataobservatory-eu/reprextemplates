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
