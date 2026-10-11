#' Create a Quarto correspondence dossier from Thunderbird text exports
#'
#' Creates a Quarto document with linked attachments and a CSV manifest.
#' Messages can be grouped by date, sender, or date and sender.
#'
#' @param source_dir Root of an ImportExportTools NG plain-text export.
#' @param output_file Destination .qmd file.
#' @param copy_attachments Copy matched attachments beside the .qmd.
#' @param title Document title.
#' @param group_by Grouping: none, day, week, month, sender, day_sender,
#'   week_sender, or month_sender.
#'
#' @return Invisibly returns output paths, attachment manifest and count.
#' @export
concat_thunderbird_messages <- function(
    source_dir, output_file = "thunderbird-review.qmd",
    copy_attachments = TRUE, title = "Correspondence archive",
    group_by = c("none", "day", "week", "month", "sender",
                 "day_sender", "week_sender", "month_sender")) {

  # ----- Validate paths -----

  group_by <- match.arg(group_by)

  if (!dir.exists(source_dir)) stop("Source directory does not exist.")

  source_dir <- normalizePath(source_dir, winslash = "/")
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)

  # ----- Read Thunderbird messages -----

  read_message <- function(file) {
    x <- readLines(file, warn = FALSE, encoding = "UTF-8")
    blank <- which(!nzchar(trimws(x)))[1]
    if (is.na(blank)) blank <- length(x) + 1L

    header <- head(x, blank - 1L)
    body <- tail(x, length(x) - blank)

    field <- function(key) {
      value <- grep(paste0("^", key, ":"), header,
                    value = TRUE, ignore.case = TRUE)
      if (!length(value)) return("")
      trimws(sub("^[^:]+:\\s*", "", value[1]))
    }

    # Extract attachment names from the message footer.
    marker <- grep("^----- Attachments -+", body)
    attachments <- character()

    if (length(marker)) {
      pos <- tail(marker, 1)
      attachment_lines <- tail(body, length(body) - pos)
      attachments <- grep("^\\s*-\\s+", attachment_lines, value = TRUE)
      attachments <- trimws(sub("^\\s*-\\s+", "", attachments))
      body <- head(body, pos - 1L)
    }

    date <- field("Date")

    list(
      file = file, subject = field("Subject"),
      from = field("From"), to = field("To"), cc = field("Cc"),
      date = date,
      timestamp = as.POSIXct(date, "%m/%d/%Y %H:%M:%S", tz = "UTC"),
      body = body, attachments = attachments
    )
  }

  files <- list.files(
    source_dir, pattern = "\\.txt$", recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )

  files <- files[grepl("(^|/)messages/", files)]
  if (!length(files)) stop("No Thunderbird messages found.")

  messages <- lapply(files, read_message)

  # ----- Group and sort messages -----

  dates <- as.POSIXct(
    vapply(messages, function(x) as.numeric(x$timestamp), numeric(1)),
    origin = "1970-01-01", tz = "UTC"
  )

  senders <- vapply(messages, function(x) x$from, character(1))
  senders[!nzchar(senders)] <- "Unknown sender"

  period_type <- sub("_sender$", "", group_by)

  period <- switch(
    period_type,
    day = format(dates, "%Y-%m-%d", tz = "UTC"),
    week = format(dates, "%G-W%V", tz = "UTC"),
    month = format(dates, "%Y-%m", tz = "UTC"),
    sender = rep("", length(messages)),
    none = rep("", length(messages))
  )

  period[is.na(period)] <- "Unknown date"

  group1 <- if (group_by == "sender") senders else period
  group2 <- if (grepl("_sender$", group_by)) senders else NULL
  if (group_by == "none") group1 <- NULL

  ordering <- if (!is.null(group2)) {
    order(group1, group2, dates, files, na.last = TRUE)
  } else if (!is.null(group1)) {
    order(group1, dates, files, na.last = TRUE)
  } else {
    order(dates, files, na.last = TRUE)
  }

  messages <- messages[ordering]
  if (!is.null(group1)) group1 <- group1[ordering]
  if (!is.null(group2)) group2 <- group2[ordering]

  # ----- Discover attachments -----

  assets <- list.files(
    source_dir, recursive = TRUE, full.names = TRUE
  )

  assets <- assets[grepl("(^|/)Attachments/", assets)]
  assets <- assets[file.info(assets)$isdir %in% FALSE]

  asset_root <- paste0(
    tools::file_path_sans_ext(basename(output_file)), "_files"
  )

  dest_dir <- file.path(dirname(output_file), asset_root, "attachments")

  if (copy_attachments && length(assets)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ----- Formatting helpers -----

  escape_md <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }

  relative_source <- function(file) {
    substring(file, nchar(source_dir) + 2L)
  }

  relative_path <- function(file, from) {
    file <- strsplit(normalizePath(file, winslash = "/"), "/")[[1]]
    from <- strsplit(normalizePath(from, winslash = "/"), "/")[[1]]

    common <- 0L
    while (common < min(length(file), length(from)) &&
           file[common + 1L] == from[common + 1L]) {
      common <- common + 1L
    }

    if (common == 0L) return(paste(file, collapse = "/"))

    parts <- c(
      rep("..", length(from) - common),
      tail(file, length(file) - common)
    )

    paste(parts, collapse = "/")
  }

  # ----- Initialise Quarto document -----

  lines <- c(
    "---",
    paste0("title: ", encodeString(title, quote = '"')),
    "format:",
    "  html:",
    "    toc: true",
    "  pdf:",
    "    toc: true",
    "  docx:",
    "    toc: true"
  )

  if (copy_attachments) {
    lines <- c(lines, "resources:", paste0("  - ", asset_root, "/**"))
  }

  lines <- c(lines, "---", "")
  manifest <- list()

  # ----- Render messages and attachments -----

  previous_group1 <- NULL
  previous_group2 <- NULL

  heading_level <- if (!is.null(group2)) "###" else "##"

  for (i in seq_along(messages)) {
    msg <- messages[[i]]
    subject <- if (nzchar(msg$subject)) msg$subject else "(No subject)"

    if (!is.null(group1) &&
        !identical(group1[i], previous_group1)) {
      lines <- c(lines, paste0("# ", escape_md(group1[i])), "")
      previous_group1 <- group1[i]
      previous_group2 <- NULL
    }

    if (!is.null(group2) &&
        !identical(group2[i], previous_group2)) {
      lines <- c(lines, paste0("## ", escape_md(group2[i])), "")
      previous_group2 <- group2[i]
    }

    lines <- c(
      lines,
      paste0(heading_level, " ", escape_md(subject),
             " {#email-", i, "}"),
      "",
      paste0("**From:** ", escape_md(msg$from), "  "),
      paste0("**To:** ", escape_md(msg$to), "  "),
      if (nzchar(msg$cc)) paste0("**Cc:** ", escape_md(msg$cc), "  "),
      paste0("**Date:** ", msg$date),
      "",
      paste0("<!-- source: ", relative_source(msg$file), " -->"),
      "",
      escape_md(msg$body),
      ""
    )

    if (length(msg$attachments)) {
      lines <- c(lines, "**Attachments**", "")
    }

    for (name in msg$attachments) {
      candidates <- assets[basename(assets) == name]

      status <- if (length(candidates) == 0L) {
        "missing"
      } else if (length(candidates) > 1L) {
        "ambiguous"
      } else {
        "matched"
      }

      link <- ""

      if (status == "matched") {
        source <- candidates[1]

        if (copy_attachments) {
          filename <- paste0(sprintf("%04d-", i), name)
          destination <- file.path(dest_dir, filename)

          if (file.copy(source, destination, overwrite = TRUE)) {
            link <- paste0(asset_root, "/attachments/", filename)
          } else {
            status <- "copy_failed"
          }
        } else {
          link <- relative_path(source, dirname(output_file))
        }
      }

      if (status == "matched") {
        link <- utils::URLencode(link, reserved = FALSE)
        label <- paste0("- [", escape_md(name), "](", link, ")")
      } else {
        label <- paste0("- ", escape_md(name), " (*", status, "*)")
      }

      lines <- c(lines, label)

      manifest[[length(manifest) + 1L]] <- data.frame(
        message = relative_source(msg$file),
        subject = msg$subject, date = msg$date,
        attachment = name, status = status,
        source_file = if (length(candidates) == 1L) candidates else "",
        link = link
      )
    }

    lines <- c(lines, "", "---", "")
  }

  # ----- Write Quarto document and manifest -----

  writeLines(lines, output_file, useBytes = TRUE)

  columns <- c(
    "message", "subject", "date", "attachment",
    "status", "source_file", "link"
  )

  index <- if (length(manifest)) {
    do.call(rbind, manifest)
  } else {
    as.data.frame(
      setNames(rep(list(character()), length(columns)), columns)
    )
  }

  manifest_file <- paste0(
    tools::file_path_sans_ext(output_file), "-attachments.csv"
  )

  utils::write.csv(
    index, manifest_file, row.names = FALSE, fileEncoding = "UTF-8"
  )

  invisible(list(
    qmd = output_file, manifest_file = manifest_file,
    manifest = index, messages = length(messages)
  ))
}
