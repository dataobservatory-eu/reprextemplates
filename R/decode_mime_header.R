#' Decode MIME-encoded email headers
#'
#' Decodes RFC 2047 Base64 and Q-encoded words in email headers.
#' Ordinary text is returned unchanged.
#'
#' @param x Character vector of header values.
#' @return Character vector with decoded header values.
#' @export
decode_mime_header <- function(x) {
  decode_word <- function(word) {
    parts <- regmatches(
      word,
      regexec("^=\\?([^?]+)\\?([BbQq])\\?([^?]*)\\?=$", word)
    )[[1]]

    if (length(parts) != 4L) return(word)

    charset <- parts[2]
    encoding <- toupper(parts[3])
    value <- parts[4]

    tryCatch({
      if (encoding == "B") {
        bytes <- base64enc::base64decode(value)
        value <- rawToChar(bytes)
      } else {
        value <- gsub("_", " ", value, fixed = TRUE)
        value <- gsub("=([0-9A-Fa-f]{2})", "%\\1", value)
        value <- utils::URLdecode(value)
      }

      decoded <- iconv(value, from = charset, to = "UTF-8")
      if (is.na(decoded)) word else decoded
    }, error = function(e) word)
  }

  vapply(x, function(value) {
    if (is.na(value)) return(NA_character_)

    # Whitespace between adjacent encoded words is not displayed.
    value <- gsub(
      "(?<=\\?=)[ \t]+(?==\\?)", "", value, perl = TRUE
    )

    pattern <- "=\\?[^?]+\\?[BbQq]\\?[^?]*\\?="
    matches <- regmatches(value, gregexpr(pattern, value))[[1]]

    if (!length(matches)) return(value)

    decoded <- vapply(matches, decode_word, character(1))
    regmatches(value, gregexpr(pattern, value)) <- list(decoded)
    value
  }, character(1), USE.NAMES = FALSE)
}
