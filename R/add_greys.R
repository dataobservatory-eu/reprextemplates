#' Add grey shades to a palette
#'
#' Extends an existing color palette with shades of grey, or creates a
#' standalone greyscale palette if no palette is supplied.
#'
#' @param pal Optional. A character vector of color hex codes (an existing palette).
#'   If `NULL` (default), the function returns only the grey shades.
#' @param grey_levels A numeric vector of grey levels between 0 and 100,
#'   where `0` is black and `100` is white.
#'
#' @return A named character vector of colors. Names of the added greys are
#'   `"greyXX"` where `XX` is the grey level.
#'
#' @examples
#' # Only grey shades
#' add_greys(grey_levels = c(50, 70))
#'
#' # Extend an existing palette
#' add_greys(c("red", "blue"), grey_levels = c(20, 80))
#'
#' @seealso [grDevices::grey()]
#' @family color functions
#' @export
add_greys <- function(pal = NULL, grey_levels) {
  grey_palette <- grDevices::grey(grey_levels / 100)
  names(grey_palette) <- paste0("grey", grey_levels)

  if (!is.null(pal)) {
    if (inherits(pal, "character")) {
      c(pal, grey_palette)
    } else {
      stop("`pal` must be a character vector of colors or NULL.")
    }
  } else {
    grey_palette
  }
}
