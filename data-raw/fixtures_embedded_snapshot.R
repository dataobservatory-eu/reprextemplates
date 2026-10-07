fixture_dir <- testthat::test_path("fixtures", "embedded")

snapshot <- fscontext::scan_storage(root = fixture_dir)
embedded <- scan_embedded(snapshot$full_path, progress = FALSE)

saveRDS(
  embedded,
  testthat::test_path("fixtures", "embedded_snapshot.rds")
)
