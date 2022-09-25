#' @title Add greys
#' @description Add shades of grey to a palette
#' @param pal A palette, defaults to \code{NULL}.
#' @param grey_levels A vector of grey levels between 0-100.
#' @importFrom grDevices grey
#' @examples
#' add_greys (grey_levels = c(50,70))
#' add_greys ( reprex_palette(), grey_levels = c(50,70))
#' @family color functions
#' @export

add_greys <- function ( pal = NULL, grey_levels ) {
  grey_palette <- grDevices::grey(grey_levels/100)
  names(grey_palette) <- paste0("grey", grey_levels)

  if (! is.null(pal)) {
    if (inherits(pal, "character"))
      c(pal, grey_palette)
  } else {
    grey_palette
  }
}

