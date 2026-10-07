#' Enrich a filesystem snapshot with embedded metadata
#'
#' Adds selected embedded metadata observations to a filesystem snapshot.
#' Embedded metadata must first be observed with [scan_embedded()]. This
#' function does not invoke ExifTool or another external metadata tool.
#'
#' Fields are selected by their native group-qualified names, such as
#' `ExifIFD:CreateDate`, `XMP-dc:Description`, or `PNG:Description`.
#' Similarly named fields from different namespaces remain distinct.
#'
#' Selected fields are added as columns using normalised names. For example,
#' `ExifIFD:CreateDate` becomes `exififd_createdate` and
#' `XMP-dc:Description` becomes `xmp_dc_description`. Files without an
#' observation for a selected field receive `NA`.
#'
#' @param snapshot A filesystem snapshot returned by
#'   [fscontext::scan_storage()].
#' @param embedded An embedded metadata snapshot returned by
#'   [scan_embedded()].
#' @param fields Character vector of native group-qualified metadata field
#'   names to add to `snapshot`.
#'
#' @return `snapshot` with one additional column for each selected field.
#'
#' @examples
#' \dontrun{
#' snapshot <- fscontext::scan_storage(root = "photos")
#' embedded <- scan_embedded(snapshot$full_path)
#'
#' find_embedded_fields(embedded, "CreateDate|Description")
#'
#' enriched <- enrich_snapshot(
#'   snapshot,
#'   embedded,
#'   fields = c("ExifIFD:CreateDate", "XMP-dc:Description")
#' )
#' }
#'
#' @seealso [scan_embedded()], [find_embedded_fields()]
#' @importFrom fscontext scan_storage
#' @export
enrich_snapshot <- function(snapshot, embedded, fields) {
  if (!is.data.frame(snapshot) || !"full_path" %in% names(snapshot)) {
    stop(
      "`snapshot` must contain a `full_path` column.",
      call. = FALSE
    )
  }

  required <- c("file", "field", "value")
  if (!is.data.frame(embedded) ||
    !all(required %in% names(embedded))) {
    stop(
      "`embedded` must contain `file`, `field`, and `value` columns.",
      call. = FALSE
    )
  }

  if (!is.character(fields) || length(fields) == 0) {
    stop(
      "`fields` must be a non-empty character vector.",
      call. = FALSE
    )
  }

  missing_fields <- setdiff(fields, embedded$field)
  if (length(missing_fields) > 0) {
    stop(
      "Embedded field",
      if (length(missing_fields) == 1) " " else "s ",
      paste0("`", missing_fields, "`", collapse = ", "),
      " not found.",
      call. = FALSE
    )
  }

  selected <- embedded[
    embedded$field %in% fields,
    c("file", "field", "value"),
    drop = FALSE
  ]

  if (anyDuplicated(selected[c("file", "field")])) {
    stop(
      "`embedded` contains duplicate file-field observations.",
      call. = FALSE
    )
  }

  column_names <- normalise_embedded_name(fields)
  if (anyDuplicated(column_names)) {
    stop(
      "Selected fields produce duplicate normalised column names.",
      call. = FALSE
    )
  }

  snapshot_path <- fs::path_norm(snapshot$full_path)
  embedded_path <- fs::path_norm(selected$file)

  for (i in seq_along(fields)) {
    field <- fields[[i]]
    keep <- selected$field == field

    snapshot[[column_names[[i]]]] <- selected$value[keep][
      match(snapshot_path, embedded_path[keep])
    ]
  }

  snapshot
}
