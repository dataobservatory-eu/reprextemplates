#' Observe embedded metadata in files
#'
#' Extracts embedded metadata from files using ExifTool and returns one row
#' per file. The complete metadata returned by ExifTool is preserved as a
#' list-column, allowing heterogeneous file types and metadata schemas to be
#' observed without imposing a common metadata model.
#'
#' Metadata fields retain their ExifTool group-qualified names, such as
#' `EXIF:DateTimeOriginal`, `XMP-dc:Description`, or `PNG:Title`. This
#' preserves the metadata vocabulary in which a value was observed rather
#' than treating similarly named fields from different standards as
#' equivalent.
#'
#' The function records information about the observation activity, including
#' the ExifTool version, start and finish times, and elapsed processing time,
#' in the `"observation"` attribute of the returned tibble.
#'
#' ExifTool must be installed separately and available on the system path.
#'
#' @param files Character vector of file paths to observe.
#' @param engine Character scalar giving the ExifTool executable. Defaults to
#'   `"exiftool"`. An explicit executable path may also be supplied.
#' @param extensions Optional character vector of file extensions to include,
#'   without leading dots. Matching is case-insensitive. If `NULL`, all files
#'   supplied in `files` are attempted.
#' @param ignore_errors Logical. If `TRUE`, errors for individual files are
#'   recorded in the returned `error` column and observation continues. If
#'   `FALSE`, an error stops processing.
#' @param progress Logical. Display a text progress bar. Defaults to
#'   `interactive()`.
#' @param ... Reserved for future use.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{file}{The supplied file path.}
#'   \item{observed}{Whether metadata observation completed successfully.}
#'   \item{metadata}{A list-column containing group-qualified metadata
#'     returned by ExifTool.}
#'   \item{error}{An ExifTool or processing error, where applicable.}
#' }
#'
#' The tibble has an `"observation"` attribute containing metadata about the
#' observation activity.
#'
#' @seealso [enrich_snapshot()]
#'
#' @examples
#' \dontrun{
#' files <- fs::dir_ls("photos", recurse = TRUE, type = "file")
#'
#' embedded <- observe_embedded(
#'   files,
#'   extensions = c("jpg", "jpeg", "orf")
#' )
#'
#' attr(embedded, "observation")
#' }
#' @importFrom fs path_ext
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
observe_embedded <- function(
  files,
  engine = "exiftool",
  extensions = NULL,
  ignore_errors = TRUE,
  progress = interactive(),
  ...
) {
  stopifnot(is.character(files), length(engine) == 1)

  if (!is.null(extensions)) {
    ext <- tolower(fs::path_ext(files))
    files <- files[ext %in% tolower(extensions)]
  }

  if (length(files) == 0) {
    return(tibble::tibble())
  }

  message("Observing ", length(files), " files.")

  activity_id <- paste0(
    "observe_embedded_", format(Sys.time(), "%Y%m%d%H%M%OS3")
  )
  observation_started <- Sys.time()

  engine_version <- tryCatch(
    system2(engine, "-ver", stdout = TRUE),
    error = function(e) NA_character_
  )

  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  metadata <- vector("list", length(files))
  observed <- logical(length(files))
  error <- rep(NA_character_, length(files))

  for (i in seq_along(files)) {
    observe_one <- function() {
      out <- suppressWarnings(
        system2(
          engine, c("-j", "-G1", shQuote(files[[i]])),
          stdout = TRUE, stderr = TRUE
        )
      )

      x <- jsonlite::fromJSON(
        paste(out, collapse = "\n"),
        simplifyDataFrame = TRUE
      )

      exif_error <- NA_character_
      error_field <- grep("(^|:)Error$", names(x), value = TRUE)

      if (length(error_field) > 0) {
        exif_error <- paste(unlist(x[error_field]), collapse = "; ")
        x[error_field] <- NULL
      }

      list(
        observed = is.na(exif_error),
        metadata = x,
        error = exif_error
      )
    }

    if (ignore_errors) {
      res <- tryCatch(
        observe_one(),
        error = function(e) {
          list(
            observed = FALSE,
            metadata = tibble::tibble(),
            error = conditionMessage(e)
          )
        }
      )
    } else {
      res <- observe_one()
    }

    observed[i] <- res$observed
    metadata[[i]] <- res$metadata
    error[i] <- res$error

    if (progress) utils::setTxtProgressBar(pb, i)
  }

  observation_finished <- Sys.time()
  elapsed_seconds <- as.numeric(difftime(
    observation_finished, observation_started,
    units = "secs"
  ))
  average_seconds <- round(elapsed_seconds / length(files), 2)

  message(round(elapsed_seconds, 2), " seconds")
  message("Average time per file: ", average_seconds, " seconds")

  embedded_df <- tibble::tibble(
    file = files,
    observed = observed,
    metadata = metadata,
    error = error
  )

  attr(embedded_df, "observation") <- list(
    activity_id = activity_id,
    observation_started = observation_started,
    observation_finished = observation_finished,
    observer = engine,
    observer_version = engine_version,
    n_files = length(files),
    elapsed_seconds = elapsed_seconds,
    average_seconds = average_seconds
  )

  embedded_df
}
