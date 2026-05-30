copy_fixture <- function(source, target) {
  unlink(target, recursive = TRUE)

  files <- list.files(
    source,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )

  for (f in files) {
    rel <- substring(
      f,
      nchar(source) + 2
    )

    out <- file.path(
      target,
      rel
    )

    if (dir.exists(f)) {
      dir.create(
        out,
        recursive = TRUE,
        showWarnings = FALSE
      )
    } else {
      dir.create(
        dirname(out),
        recursive = TRUE,
        showWarnings = FALSE
      )

      file.copy(
        f,
        out,
        overwrite = TRUE
      )
    }
  }

  invisible(target)
}
