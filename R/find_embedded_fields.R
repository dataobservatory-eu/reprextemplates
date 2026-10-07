#' Find embedded metadata fields
#'
#' Finds similarly named metadata fields in an embedded metadata snapshot
#' returned by [scan_embedded()]. This supports field discovery before
#' enrichment without treating similarly named fields as semantically
#' equivalent.
#'
#' Matching is performed against complete group-qualified field names such as
#' `System:FileCreateDate`, `ExifIFD:CreateDate`, or `XMP-xmp:CreateDate`.
#'
#' @param x A data frame returned by [scan_embedded()].
#' @param pattern A single character string containing a regular expression
#'   used to match metadata field names.
#' @param ignore_case Logical. Should matching ignore character case?
#'   Defaults to `TRUE`.
#'
#' @return A data frame with columns `field` and `n`, where `n` is the number
#'   of files containing each matching field.
#'
#' @examples
#' \dontrun{
#' snapshot <- fscontext::scan_storage(root = "photos")
#' embedded <- scan_embedded(snapshot$full_path)
#'
#' find_embedded_fields(embedded, "CreateDate")
#' find_embedded_fields(embedded, "Description|Caption")
#' }
#'
#' @seealso [scan_embedded()], [enrich_snapshot()]
#' @export
find_embedded_fields <- function(x, pattern, ignore_case = TRUE) {
  if (!is.data.frame(x) || !"field" %in% names(x)) {
    stop("`x` must contain a `field` column.", call. = FALSE)
  }

  if (!is.character(pattern) || length(pattern) != 1) {
    stop("`pattern` must be a single character string.", call. = FALSE)
  }

  matched <- grepl(
    pattern,
    x$field,
    ignore.case = ignore_case
  )

  out <- x[matched, c("file", "field"), drop = FALSE]
  out <- unique(out)

  if (nrow(out) == 0) {
    return(data.frame(
      field = character(),
      n = integer(),
      stringsAsFactors = FALSE
    ))
  }

  counts <- table(out$field)

  data.frame(
    field = names(counts),
    n = as.integer(counts),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
