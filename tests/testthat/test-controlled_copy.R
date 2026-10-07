test_that("controlled_copy recursively copies and verifies files", {
  skip_on_os(c("mac", "linux", "solaris"))
  skip_if(Sys.which("robocopy") == "")

  # Use a short root path to avoid Windows PATH_MAX during R CMD check.
  tmp <- fs::path(
    Sys.getenv("SystemDrive", unset = "C:"),
    paste0("rt-", Sys.getpid())
  )
  fs::dir_create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  source <- fs::path(tmp, "src")
  destination <- fs::path(tmp, "dst")
  source_snapshots <- fs::path(tmp, "ss")
  destination_snapshots <- fs::path(tmp, "ds")

  fs::dir_create(c(
    source, destination, source_snapshots, destination_snapshots
  ))

  fs::dir_create(fs::path(source, "documents", "nested"), recurse = TRUE)
  fs::dir_create(fs::path(source, ".hidden"))
  writeLines("first document", fs::path(source, "first.txt"))
  writeLines(
    "nested document",
    fs::path(source, "documents", "nested", "second.txt")
  )
  writeLines("hidden document", fs::path(source, ".hidden", "third.txt"))

  result <- controlled_copy(
    source_root = source,
    destination_root = destination,
    source_snapshot_root = source_snapshots,
    destination_snapshot_root = destination_snapshots,
    source_storage_id = "test-source",
    destination_storage_id = "test-destination",
    person_id = "test-person"
  )

  # Check recursive and hidden files.
  expect_true(all(fs::file_exists(c(
    fs::path(destination, "first.txt"),
    fs::path(destination, "documents", "nested", "second.txt"),
    fs::path(destination, ".hidden", "third.txt")
  ))))

  # Check snapshots, concordance and copy verification.
  expect_true(fs::file_exists(result$source_snapshot))
  expect_true(fs::file_exists(result$destination_snapshot))
  expect_length(result$concordance, 2)
  expect_true(all(fs::file_exists(result$concordance)))
  expect_length(result$discrepancies, 0)
  expect_lt(result$copy_exit_status, 8)
})
