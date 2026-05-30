#' Return predefined file selection profiles
#'
#' Internal helper returning predefined file selection profiles for use
#' with [concat_ordered_text_files()].
#'
#' @param profile Name of a predefined profile.
#'
#' @return A list suitable for the `groups` argument of
#'   [concat_ordered_text_files()].
#'
#' @export
concat_files_profile <- function(profile = "R package") {
  if (!is.character(profile) ||
    length(profile) != 1 ||
    is.na(profile)) {
    stop(
      "`profile` must be a single character value.",
      call. = FALSE
    )
  }

  if (profile %in% c("r", "R", "r-package", "r_package", "R package")) {
    profile <- "r_package"
  }

  if (grepl("Quarto|quarto", profile)) {
    profile <- "quarto"
  }

  if (tolower(profile) == "hugo") {
    profile <- "hugo"
  }


  profiles <- list(
    "r_package" = list(
      list(
        files = c("DESCRIPTION", "README.md", "NEWS.md", "NAMESPACE")
      ),
      list(folder = "R", extension = "R"),
      list(
        folder = "vignettes",
        extension = c("Rmd", "qmd")
      )
    ),
    "quarto" = list(
      list(
        files = "_quarto.yml"
      ),
      list(
        folder = ".",
        extension = "qmd"
      )
    ),
    "testthat" = list(
      list(
        folder = ".",
        extension = "R"
      )
    ),
    "hugo" = list(
      list(
        folder = "content",
        extension = "md"
      ),
      list(
        folder = ".",
        extension = c(
          "yml",
          "yaml",
          "toml"
        )
      )
    )
  )

  available_profiles <- names(profiles)

  if (!profile %in% available_profiles) {
    stop(
      "Unknown profile '", profile, "'. Available profiles: ",
      paste(available_profiles, collapse = ", "),
      call. = FALSE
    )
  }

  profiles[[profile]]
}
