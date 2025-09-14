#' @title Return the Reprex palette
#'
#' @description Endre Koronczi Palette (c) 2015
#'
#' @details A palette of \code{#007CBB}, \code{#4EC0E4}, \code{#00843A},
#' \code{#3EA135}, \code{#DB001C}, \code{#5C2320}, \code{#4E115A}, \code{#00348A},
#' \code{#BAC615}, \code{#FAE000}, \code{#E88500}, \code{#E4007F}.
#' @param ext Defaults to FALSE. If true, adds five colors
#' @return A named character vector of colors in a discrete palette.
#' @family color functions
#' @examples
#' reprex_palette(ext = TRUE)
#' @export

reprex_palette <- function(ext = FALSE) {
  my_palette <- c(
    "#007CBB", "#4EC0E4", "#00843A", "#3EA135",
    "#DB001C", "#5C2320", "#4E115A", "#00348A",
    "#BAC615", "#FAE000", "#E88500", "#E4007F"
  )

  names(my_palette) <- c(
    "blue", "lightblue", "darkgreen", "green",
    "red", "brown", "violet", "darkblue",
    "lightgreen", "yellow", "orange", "magenta"
  )

  if (ext == TRUE) {
    my_palette_ext <- c(
      my_palette, "#7f7f7f", "#282828", "#666666",
      "#000000", "#141414"
    )
    names(my_palette_ext)[13:17] <- c("ext1", "ext2", "ext3", "ext4", "ext5")
    return(my_palette_ext)
  } else {
    return(my_palette)
  }
}


sr_palette <- function(ext = FALSE) {
  .Deprecated(new,
    package = NULL,
    msg = "sr_palette(ext=FALSE) is deprecated, use reprex_palette(ext=...) instead.",
    old = "use reprex_palette(ext=...) instead"
  )

  reprex_palette(ext)
}
