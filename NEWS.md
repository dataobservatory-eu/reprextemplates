# reprextemplates 0.1.18

* Add `concat_ordered_text_files()` to create ordered review bundles from package files and directories.
* Add `concat_files_profile()` to provide reusable file selection profiles for review bundle generation.
* Add `create_package_review_bundles()` to generate package review bundles for all R packages in a directory tree.
* Add package review bundle fixtures under `tests/testthat/testdata` for integration testing.
* Improve integration tests for package review bundle creation and idempotency.

# reprextemplates 0.1.17

* New function `concat_yml_text_files()` to concat only files mentioned in a (
  Quarto-tyle) _yml file.


# reprextemplates 0.1.16

* Improve `concat_text_files()`.
* Small bug fixes in palette creation.
* `rename_ppt_slides()` added back to the package in a more robust version.
