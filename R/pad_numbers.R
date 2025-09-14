#' Pad numbers with leading zeros
#' @param numbers A vector of numbers.
#' @return A vector of strings, padded with leading zeros for equal-length
#' numbers.
#' @examples
#' pad_numbers(c(1, 12, 132))
#' @keywords export
pad_numbers <- function(numbers) {
  width <- nchar(as.character(max(numbers)))
  sprintf(paste0("%0", width, "d"), numbers)
}
