#' Return a color for each EU country from the SR palette.
#'
#' @return A named vector of color hex strings for each country code.
#' @examples
#' palette_eu_countries()
#' @export
#'
palette_eu_countries <- function () {

  my_palette <- c(
    "#007CBB", "#4EC0E4", "#00843A", "#3EA135",
    "#DB001C", "#5C2320", "#4E115A", "#00348A",
    "#BAC615", "#FAE000", "#E88500", "#E4007F")

  blue_shades <- create_shades(reprex_palette()['blue'],
                               palette_length = 11,
                               used_colors = c(11,9,7,5,3))


  names (my_palette) <- c(
    "blue", "lightblue", "darkgreen", "green",
    "red", "brown", "violet", "darkblue",
    "lightgreen", "yellow", "orange", "magenta")

  palette_eu_countries <- c(
    "EU27_2020" = as.character(blue_shades[1]),
    "EU28" = as.character(blue_shades[2]),
    "EU15" = as.character(blue_shades[5]),
    "EU27_2007" = as.character(blue_shades[4]),
    "HU" = my_palette[['green']],
    "PL" = my_palette[['red']],
    'SK' = my_palette[['lightblue']],
    'CZ' = my_palette[['blue']],
    'LV' = my_palette[['red']],
    'LT' =  my_palette[['yellow']],
    'EE' = 'black',
    'BE' = 'black',
    'NL' = my_palette[['orange']],
    'LU' = my_palette[['lightblue']],
    'GB' = my_palette[['darkblue']],
    'GB-G' = my_palette[['darkblue']],
    'GB-N' = my_palette[['lightblue']],
    'IE' = my_palette[['darkgreen']],
    'DE' = 'black',
    'DE-W' = 'black',
    'DE-E' = my_palette[['yellow']],
    'AT' = my_palette[['red']],
    'SI'= my_palette[['blue']],
    'FR'= my_palette[['darkblue']],
    'ES'= my_palette[['yellow']],
    'PT'= my_palette[['red']],
    'IT'= my_palette[['green']],
    'SE'= my_palette[['yellow']],
    'DK'= my_palette[['red']],
    'FI'= my_palette[['darkblue']],
    'HR' = my_palette[['red']],
    'GR'= my_palette[['lightblue']],
    'EL'= my_palette[['lightblue']],
    'RO' = my_palette[['yellow']],
    'BG'= my_palette[['green']],
    'CY'= my_palette[['orange']],
    'MT'= my_palette[['red']]
  )

  palette_eu_countries
}
