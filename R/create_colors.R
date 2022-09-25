#' @title Create colors
#' @param x The color to start with.
#' @param palette_length How many shades to add, including the two starting colors (white and \code{x}).
#' @inheritParams reprex_palette
#' @importFrom grDevices colorRampPalette
#' @importFrom assertthat assert_that
#' @export

create_palette <- function(color1,color2, palette_length = 5, used_colors = NULL) {

  assertthat::assert_that(!is.null(color1) & length(color1)==1,
                          msg = "color1 must be a color name or a hex code for a color.")
  assertthat::assert_that(!is.null(color2) & length(color2)==1,
                          msg = "color1 must be a color name or a hex code for a color.")
  assertthat::assert_that(palette_length>=2 & length(color2)==1,
                          msg = "palette_length must be at least 2 (in which case color1 and color2 are returned without transition.)")

  if ( is.null(used_colors)) used_colors <- seq(1, palette_length)

  shades <- function (color1=color1, color2=color2, palette_length = palette_length) {

    my_shades <- grDevices::colorRampPalette(c(color1, color2), bias=.1,space="rgb")

    my_shades(palette_length)
  }
  new_palette <- shades (color1, color2, palette_length = palette_length )

  if (is.null(attr(color1, "names"))) names(color1) <- color1
  if (is.null(attr(color2, "names"))) names(color2) <- color2
  orig_name_1 <- names(color1)
  orig_name_2 <- names(color2)
  new_color_length <- palette_length-1

  if ( any(as.logical(c(color1=="white", color2=="white")))) {
    suffix <- c("", as.character(round(((1:new_color_length)/new_color_length)*100,0)))

    colors <- c(color1,color2)
    color2 <- colors[which(names(colors)!="white")]
    new_palette <- shades ("white", color2, palette_length = palette_length )
    palette_names <- c("white", paste0(names(color2), suffix)[2:palette_length])
    palette_names <- gsub("100", "", palette_names)
  } else {
    color_2_rate <- c(0, round(((1:new_color_length)/new_color_length)*100,0))
    color_1_rate <- 100-color_2_rate

    first_part <- ifelse(color_1_rate == 0, "", paste0(orig_name_1, color_1_rate, "-"))
    second_part <- ifelse (color_2_rate ==0, "", paste0(orig_name_2, color_2_rate))
    first_part
    second_part

    palette_names <- ifelse ( first_part == paste0(orig_name_1, "100-"),
                              orig_name_1,
                              paste0(first_part, ifelse (second_part == paste0(orig_name_2, "100"),
                                                         orig_name_2, second_part)))

    palette_names
  }

  names(new_palette) <- palette_names
  new_palette[used_colors]
}


#' @title Create shades
#' @description Create a discrete color palette of the shades of a single color (and white.)
#' @details This is a wrapper around
#' \code{\link{create_palette}} with the parameters
#' set to \code{color1=color} and \code{color2='white'}.
#' @param color The color for which you need shades.
#' @inheritParams create_palette
#' @examples
#' create_shades( color = c(violet = "#4E115A"))
#' @export
create_shades <- function(color, palette_length = 11, used_colors = c(11,8)) {

  create_palette(color, "white" , palette_length = palette_length, used_colors = used_colors)

}
