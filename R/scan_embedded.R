#' Scan embedded file metadata
#'
#' Scans files for embedded metadata and returns a reusable long-form metadata
#' snapshot. Native group-qualified field names reported by the selected tool
#' are preserved.
#'
#' Embedded metadata scanning can be substantially slower than filesystem
#' observation. The returned snapshot can therefore be saved with [saveRDS()]
#' and reused for field discovery and snapshot enrichment without rescanning
#' the files.
#'
#' @param files Character vector of file paths to scan, typically the
#'   `full_path` column of a filesystem snapshot returned by
#'   [fscontext::scan_storage()].
#' @param tool Metadata extraction tool. Currently only `"exiftool"` is
#'   supported.
#' @param extensions Optional character vector of file extensions to include.
#' @param ignore_errors Logical. If `TRUE`, files that cannot be observed are
#'   skipped rather than stopping the scan.
#' @param progress Logical. Show a progress bar while scanning.
#'
#' @return A data frame with one row per observed file-field combination and
#'   columns `file`, `extension`, `field`, and `value`. The `"observation"`
#'   attribute records provenance for the metadata scan.
#'
#' @examples
#' \dontrun{
#' snapshot <- fscontext::scan_storage(root = "photos")
#'
#' embedded <- scan_embedded(
#'   snapshot$full_path,
#'   tool = "exiftool"
#' )
#'
#' saveRDS(embedded, "snapshot_embedded.rds")
#' }
#'
#' @seealso [find_embedded_fields()], [enrich_snapshot()]
#' @importFrom fs path_ext
#' @export
scan_embedded <- function(
  files,
  tool = "exiftool",
  extensions = NULL,
  ignore_errors = TRUE,
  progress = interactive()
) {
  stopifnot(is.character(files), length(tool) == 1)

  if (!identical(tool, "exiftool")) {
    stop(
      "Unsupported metadata tool: `", tool, "`.",
      call. = FALSE
    )
  }

  if (!is.null(extensions)) {
    extensions <- tolower(sub("^\\.", "", extensions))
    files <- files[
      tolower(fs::path_ext(files)) %in% extensions
    ]
  }

  if (length(files) == 0) {
    return(tibble::tibble(
      file = character(),
      extension = character(),
      field = character(),
      value = character()
    ))
  }

  engine <- Sys.which(tool)

  if (!nzchar(engine)) {
    stop(
      "`", tool, "` was not found on the system PATH.",
      call. = FALSE
    )
  }

  activity_id <- paste0(
    "scan_embedded_",
    format(Sys.time(), "%Y%m%dT%H%M%S")
  )
  observation_start <- Sys.time()

  tool_version <- tryCatch(
    system2(engine, "-ver", stdout = TRUE),
    error = function(e) NA_character_
  )

  message(
    "Scanning embedded metadata in ",
    length(files),
    " file",
    if (length(files) == 1) "" else "s",
    " using ",
    tool,
    "."
  )

  if (progress) {
    pb <- utils::txtProgressBar(
      min = 0,
      max = length(files),
      style = 3
    )
    on.exit(close(pb), add = TRUE)
  }

  result <- vector("list", length(files))

  for (i in seq_along(files)) {
    scan_one <- function() {
      out <- suppressWarnings(
        system2(
          engine,
          c("-j", "-G1", shQuote(files[[i]])),
          stdout = TRUE,
          stderr = TRUE
        )
      )

      x <- jsonlite::fromJSON(
        paste(out, collapse = "\n"),
        simplifyDataFrame = TRUE
      )

      error_field <- grep("(^|:)Error$", names(x), value = TRUE)

      if (length(error_field) > 0) {
        stop(
          paste(unlist(x[error_field]), collapse = "; "),
          call. = FALSE
        )
      }

      fields <- names(x)

      data.frame(
        file = rep(as.character(files[[i]]), length(fields)),
        extension = rep(
          tolower(fs::path_ext(files[[i]])),
          length(fields)
        ),
        field = fields,
        value = unlist(x, use.names = FALSE),
        stringsAsFactors = FALSE
      )
    }

    result[[i]] <- if (ignore_errors) {
      tryCatch(
        scan_one(),
        error = function(e) NULL
      )
    } else {
      scan_one()
    }

    if (progress) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  observation_finish <- Sys.time()
  elapsed <- as.numeric(
    difftime(
      observation_finish,
      observation_start,
      units = "secs"
    )
  )

  out <- do.call(rbind, result)

  if (is.null(out)) {
    out <- data.frame(
      file = character(),
      extension = character(),
      field = character(),
      value = character(),
      stringsAsFactors = FALSE
    )
  }

  rownames(out) <- NULL

  attr(out, "observation") <- list(
    activity_id = activity_id,
    start = observation_start,
    finish = observation_finish,
    observer = tool,
    version = tool_version,
    n_files = length(files),
    elapsed = elapsed,
    average = elapsed / length(files)
  )

  out
}
