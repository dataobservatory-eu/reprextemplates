concat_ordered_text_files(
  source_dir = ".",
  groups = concat_files_profile("R package"),
  profile = "R package",
  output_file = "package_review-reprextemplates.txt"
)

create_review_bundles()

book_directory <- here::here(
  "tests", "testthat","testdata", "review_bundles", "book-directory"
)

dir(book_directory, recursive = TRUE)


concat_ordered_text_files(
  source_dir = here::here("tests", "testthat"),
  groups = concat_files_profile("testthat"),
  profile = "testthat",
  output_file = "review-tests.txt"
)


create_review_bundles(root_dir = "D:/_markdown")

dir(here::here("D:/_markdown", "GraphNotes"))
