#' @title Return a color for each European country from the SR palette.
#'
#' @param europe_countries_4 If TRUE, returns values for 'GB-G', 'GB-N', 'DE-W', 'DE-E'.
#' @keywords palette
#' @examples
#' palette_europe_countries <- palette_europe_countries( europe_countries_4 = TRUE)
#' @export
#'
palette_europe_countries <- function ( europe_countries_4 = FALSE) {

  my_palette <- c("#007CBB", "#4EC0E4", "#00843A", "#3EA135",
                  "#DB001C", "#5C2320", "#4E115A", "#00348A",
                  "#BAC615", "#FAE000", "#E88500", "#E4007F")

  names (my_palette) <- c("blue", "lightblue", "darkgreen", "green",
                          "red", "brown", "violet", "darkblue",
                          "lightgreen", "yellow", "orange", "magenta")

  palette_europe_countries_4 <- c(
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
    'GB-G' = my_palette[['darkblue']],
    'GB-N' = my_palette[['lightblue']],
    'IE' = my_palette[['darkgreen']],
    'DE-W' = 'black',
    'DE-E' = my_palette[['yellow']],
    'AT' = my_palette[['red']],
    'SI'= my_palette[['blue']],
    'FR'= my_palette[['darkblue']],
    'ES'= my_palette[['yellow']],
    'PT'= my_palette[['red']],
    'IT'= my_palette[['green']],
    'IS'= my_palette[['purple']],
    'SE'= my_palette[['yellow']],
    'DK'= my_palette[['red']],
    'FI'= my_palette[['darkblue']],
    'HR' = my_palette[['red']],
    'GR'= my_palette[['lightblue']],
    'EL'= my_palette[['lightblue']],
    'RO' = my_palette[['yellow']],
    'BG'= my_palette[['green']],
    'CY'= my_palette[['orange']],
    'MT'= my_palette[['red']],
    'MK'= my_palette[['yellow']],
    'AL' = my_palette[['red']],
    'ME' = my_palette[['red']],
    'RS' = my_palette[['blue']],
    'MD' = my_palette[['darkblue']],
    'BA' = my_palette[['darkblue']],
    'UA' = my_palette[['yellow']],
    'XK' = 'black')


  if ( europe_countries_4 == TRUE) return ( palette_europe_countries_4 )

  c("HU" = my_palette[['green']],
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
    'IE' = my_palette[['darkgreen']],
    'DE' = 'black',
    'IS' = my_palette[['purple']],
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
    'RO' = my_palette[['yellow']],
    'BG'= my_palette[['green']],
    'CY'= my_palette[['orange']],
    'MT'= my_palette[['red']],
    'MK'= my_palette[['yellow']],
    'AL' = my_palette[['red']],
    'ME' = my_palette[['red']],
    'RS' = my_palette[['blue']],
    'MD' = my_palette[['darkblue']],
    'BA' = my_palette[['darkblue']],
    'XK' = 'black',
    'UA' = my_palette[['yellow']],
  )
}
