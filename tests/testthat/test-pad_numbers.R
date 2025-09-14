test_that("number padding works", {
  expect_equal(pad_numbers(c(1, 12, 132)), c("001", "012", "132"))
  expect_equal(pad_numbers(c(1, 12, 132, 1000)), c("0001", "0012", "0132", "1000"))
})
