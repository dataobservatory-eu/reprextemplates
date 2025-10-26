#' Load datasets safely to environment
#'
#' @description
#' A robustutility that retrieves datasets by name, accepting
#' either a quoted string (`"ind_ava"`) or a bare object name (`ind_ava`).
#' It uses `utils::data()` to load objects into a temporary environment and
#' returns the requested dataset invisibly. If the dataset is not found,
#' a clear error message is raised (without warnings from `utils::data()`).
#'
#' @param x Unquoted or quoted dataset name (character or symbol).
#' @param package_name The name of the package that contains the dataset.
#' @return The dataset as a data frame or tibble.
#' @export
#' @examples
#' \dontrun{
#' getdata(ind_ava)
#' getdata("prd_use", "iotables")
#' }
getdata <- function(x, package_name = NULL) {
  # Handle both bare and quoted dataset names
  name <- tryCatch(
    {
      nm <- deparse(substitute(x))
      if (is.character(x) && length(x) == 1L) nm <- x
      nm
    },
    error = function(e) {
      stop("Invalid argument for 'x': must be a dataset name or string.", call. = FALSE)
    }
  )

  # Prepare an isolated environment
  e <- new.env(parent = emptyenv())

  # Try loading dataset quietly
  success <- tryCatch(
    suppressMessages(
      suppressWarnings(
        utils::data(list = name, envir = e, package = package_name)
      )
    ),
    warning = function(w) { # suppress "data set not found"
      invokeRestart("muffleWarning")
    },
    error = function(err) {
      stop(
        "Dataset '", name, "' not found in iotables package data.",
        "\nOriginal message: ", conditionMessage(err),
        call. = FALSE
      )
    }
  )

  # Return dataset if found
  if (!exists(name, envir = e)) {
    stop("Dataset '", name, "' not found in iotables package data.", call. = FALSE)
  }

  e[[name]]
}
