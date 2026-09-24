#' Controlled recursive copy with fscontext verification
#'
#' Creates a source filesystem snapshot, recursively copies the source
#' universe to a destination using Robocopy, creates a destination snapshot,
#' and records concordances and discrepancies between the observations.
#'
#' @param source_root Source directory.
#' @param destination_root Destination directory.
#' @param source_snapshot_root Directory for source-side fscontext artefacts.
#' @param destination_snapshot_root Directory for destination-side fscontext
#'   artefacts.
#' @param source_storage_id Identifier of the source storage environment.
#' @param destination_storage_id Identifier of the destination storage
#'   environment.
#' @param person_id Identifier of the curator performing the operation.
#'
#' @return Invisibly, a list containing paths to the source and destination
#'   snapshots, concordance files, optional discrepancy files, and the
#'   Robocopy exit status.
#'
#' @importFrom dplyr all_of anti_join bind_rows inner_join mutate select
#' @importFrom fs dir_create dir_exists path path_real
#' @importFrom fscontext snapshot_storage
#' @export
controlled_copy <- function(
    source_root,
    destination_root,
    source_snapshot_root,
    destination_snapshot_root,
    source_storage_id,
    destination_storage_id,
    person_id) {

  operation_start_time <- Sys.time()

  source_root <- fs::path(source_root)
  destination_root <- fs::path(destination_root)

  source_snapshot_root <- fs::path(source_snapshot_root)
  destination_snapshot_root <- fs::path(destination_snapshot_root)

  # ----------------------------------------------------------------
  # Preconditions
  # ----------------------------------------------------------------

  if (!fs::dir_exists(source_root)) {
    stop("Source root does not exist: ", source_root)
  }

  if (!fs::dir_exists(source_snapshot_root)) {
    stop("Source snapshot root does not exist: ", source_snapshot_root)
  }

  if (!fs::dir_exists(destination_snapshot_root)) {
    stop(
      "Destination snapshot root does not exist: ",
      destination_snapshot_root
    )
  }

  if (!fs::dir_exists(destination_root)) {
    fs::dir_create(destination_root)
    message("Created: ", destination_root)
  }

  if (fs::path_real(source_root) == fs::path_real(destination_root)) {
    stop("Source and destination roots must differ.")
  }

  # ----------------------------------------------------------------
  # Source observation
  # ----------------------------------------------------------------

  snapshot_1 <- fscontext::snapshot_storage(
    root = source_root,
    storage_id = source_storage_id,
    person_id = person_id,
    path = source_snapshot_root
  )

  # ----------------------------------------------------------------
  # Controlled recursive copy
  # ----------------------------------------------------------------

  message(
    "Controlled copy:\n",
    "  source:      ", source_root, "\n",
    "  destination: ", destination_root
  )

  copy_result <- system2(
    "robocopy",
    args = c(
      shQuote(source_root),
      shQuote(destination_root),
      "/E",
      "/COPY:DAT",
      "/DCOPY:DAT",
      "/R:1",
      "/W:1"
    )
  )

  if (copy_result >= 8) {
    stop(
      "Controlled copy failed. Robocopy exit code: ",
      copy_result
    )
  }

  # ----------------------------------------------------------------
  # Destination observation
  # ----------------------------------------------------------------

  snapshot_2 <- fscontext::snapshot_storage(
    root = destination_root,
    storage_id = destination_storage_id,
    person_id = person_id,
    path = destination_snapshot_root
  )

  snapshot_1_data <- readRDS(snapshot_1)
  snapshot_2_data <- readRDS(snapshot_2)

  concordance_time <- Sys.time()

  keys <- c("filename", "quick_sig", "size")

  # ----------------------------------------------------------------
  # Concordance
  # ----------------------------------------------------------------

  concordance <- snapshot_1_data |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::inner_join(
      snapshot_2_data |>
        dplyr::select(dplyr::all_of(keys)),
      by = keys,
      relationship = "many-to-many"
    )

  # ----------------------------------------------------------------
  # Discrepancies
  # ----------------------------------------------------------------

  missing_from_destination <- snapshot_1_data |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::anti_join(
      snapshot_2_data |>
        dplyr::select(dplyr::all_of(keys)),
      by = keys
    ) |>
    dplyr::mutate(
      discrepancy = "missing_from_destination"
    )

  unexpected_in_destination <- snapshot_2_data |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::anti_join(
      snapshot_1_data |>
        dplyr::select(dplyr::all_of(keys)),
      by = keys
    ) |>
    dplyr::mutate(
      discrepancy = "unexpected_in_destination"
    )

  discrepancies <- dplyr::bind_rows(
    missing_from_destination,
    unexpected_in_destination
  )

  # ----------------------------------------------------------------
  # Comparison provenance
  # ----------------------------------------------------------------

  comparison_id <- paste0(
    gsub("[:/\\\\]+", "", source_root), "_",
    gsub("[:/\\\\]+", "", destination_root), "_",
    format(concordance_time, "%Y%m%d-%H%M%S")
  )

  concordance_file_name <- paste0(
    "concordance_",
    comparison_id,
    ".rds"
  )

  discrepancy_file_name <- paste0(
    "discrepancies_",
    comparison_id,
    ".rds"
  )

  operation_end_time <- Sys.time()

  elapsed_seconds <- as.numeric(
    difftime(
      operation_end_time,
      operation_start_time,
      units = "secs"
    )
  )

  add_comparison_attributes <- function(x) {

    attr(x, "snapshot") <- c(snapshot_1, snapshot_2)

    attr(x, "storage_id") <- c(
      source_storage_id,
      destination_storage_id
    )

    attr(x, "root") <- c(
      source_root,
      destination_root
    )

    attr(x, "person_id") <- person_id
    attr(x, "start_time") <- operation_start_time
    attr(x, "compared_time") <- concordance_time
    attr(x, "operation_end_time") <- operation_end_time
    attr(x, "elapsed_seconds") <- elapsed_seconds
    attr(x, "comparison_id") <- comparison_id
    attr(x, "copy_exit_status") <- copy_result

    x
  }

  concordance <- add_comparison_attributes(concordance)
  discrepancies <- add_comparison_attributes(discrepancies)

  attr(concordance, "concordance_file") <- concordance_file_name

  # ----------------------------------------------------------------
  # Persist comparison evidence
  # ----------------------------------------------------------------

  concordance_paths <- c(
    fs::path(source_snapshot_root, concordance_file_name),
    fs::path(destination_snapshot_root, concordance_file_name)
  )

  saveRDS(concordance, concordance_paths[[1]])
  saveRDS(concordance, concordance_paths[[2]])

  discrepancy_paths <- character()

  if (nrow(discrepancies) > 0) {

    attr(discrepancies, "discrepancy_file") <- discrepancy_file_name

    discrepancy_paths <- c(
      fs::path(source_snapshot_root, discrepancy_file_name),
      fs::path(destination_snapshot_root, discrepancy_file_name)
    )

    saveRDS(discrepancies, discrepancy_paths[[1]])
    saveRDS(discrepancies, discrepancy_paths[[2]])
  }

  # ----------------------------------------------------------------
  # Report
  # ----------------------------------------------------------------

  message(
    "Operation completed in ",
    sprintf(
      "%02d:%02d",
      floor(elapsed_seconds / 60),
      floor(elapsed_seconds %% 60)
    ),
    "\n",
    "Concordant rows: ",
    nrow(concordance),
    "\n",
    "Discrepancies: ",
    nrow(discrepancies)
  )

  invisible(
    list(
      source_snapshot = snapshot_1,
      destination_snapshot = snapshot_2,
      concordance = concordance_paths,
      discrepancies = discrepancy_paths,
      copy_exit_status = copy_result,
      elapsed_seconds = elapsed_seconds
    )
  )
}
