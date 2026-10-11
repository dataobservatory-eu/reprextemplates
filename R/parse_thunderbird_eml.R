#' Parse Thunderbird EML message metadata
#'
#' Extracts email headers from ImportExportTools NG EML exports.
#' Handles folded headers and preserves the original header values.
#'
#' @param source_dir Directory containing EML exports.
#' @return A data frame with one row per EML message.
#' @export
parse_thunderbird_eml <- function(source_dir) {
  if (any(!dir.exists(source_dir))) {
    stop("Source directory does not exist.")
  }

  files <- unlist(lapply(source_dir, function(path) {
    list.files(path, pattern = "\\.eml$", recursive = TRUE,
               full.names = TRUE, ignore.case = TRUE)
  }), use.names = FALSE)

  if (!length(files)) stop("No EML files found.")

  parse_one <- function(file) {
    x <- readLines(file, warn = FALSE, encoding = "UTF-8")
    blank <- which(!nzchar(x))[1]
    if (!is.na(blank)) x <- head(x, blank - 1L)

    # Unfold continuation lines (RFC 5322).
    headers <- character()
    for (line in x) {
      if (grepl("^[ \t]", line) && length(headers)) {
        headers[length(headers)] <- paste(
          headers[length(headers)], trimws(line)
        )
      } else {
        headers <- c(headers, line)
      }
    }

    field <- function(name) {
      hit <- grep(paste0("^", name, ":"), headers,
                  value = TRUE, ignore.case = TRUE)
      if (!length(hit)) return(NA_character_)
      trimws(sub("^[^:]+:", "", hit[1]))
    }

    data.frame(
      filename = tools::file_path_sans_ext(basename(file)),
      message_id = field("Message-ID"),
      in_reply_to = field("In-Reply-To"),
      references = field("References"),
      date = field("Date"),
      from = decode_mime_header(field("From")),
      to = decode_mime_header(field("To")),
      cc = decode_mime_header(field("Cc")),
      subject = decode_mime_header(field("Subject")),
      eml_file = normalizePath(file, winslash = "/"),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, lapply(files, parse_one))
}
