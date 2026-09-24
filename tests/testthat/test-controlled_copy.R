test_that("controlled_copy recursively copies and verifies files", {

  skip_on_os(c("mac", "linux", "solaris"))
  skip_if(Sys.which("robocopy") == "")

  tmp <- withr::local_tempdir()

  source <- fs::path(tmp, "source")
  destination <- fs::path(tmp, "destination")

  source_snapshots <- fs::path(tmp, "source_snapshots")
  destination_snapshots <- fs::path(tmp, "destination_snapshots")

  fs::dir_create(source)
  fs::dir_create(destination)
  fs::dir_create(source_snapshots)
  fs::dir_create(destination_snapshots)

  # Include nested and hidden directories.
  fs::dir_create(fs::path(source, "documents", "nested"))
  fs::dir_create(fs::path(source, ".hidden"))

  writeLines(
    "first document",
    fs::path(source, "first.txt")
  )

  writeLines(
    "nested document",
    fs::path(source, "documents", "nested", "second.txt")
  )

  writeLines(
    "hidden document",
    fs::path(source, ".hidden", "third.txt")
  )

  result <- controlled_copy(
    source_root = source,
    destination_root = destination,
    source_snapshot_root = source_snapshots,
    destination_snapshot_root = destination_snapshots,
    source_storage_id = "test-source",
    destination_storage_id = "test-destination",
    person_id = "test-person"
  )

  expect_true(
    fs::file_exists(
      fs::path(destination, "first.txt")
    )
  )

  expect_true(
    fs::file_exists(
      fs::path(destination, "documents", "nested", "second.txt")
    )
  )

  expect_true(
    fs::file_exists(
      fs::path(destination, ".hidden", "third.txt")
    )
  )

  expect_true(fs::file_exists(result$source_snapshot))
  expect_true(fs::file_exists(result$destination_snapshot))

  expect_length(result$concordance, 2)
  expect_true(all(fs::file_exists(result$concordance)))

  # A clean destination should produce no discrepancy artefact.
  expect_length(result$discrepancies, 0)

  expect_lt(result$copy_exit_status, 8)
})
