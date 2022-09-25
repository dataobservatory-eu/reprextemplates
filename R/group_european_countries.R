#' @title Group European Countries
#' @importFrom tibble tribble
#' @return A tibble with geo codes and suggested groups.
#' @examples
#' group_european_countries()
#' @export
#'
group_european_countries <- function() {

  tibble::tribble(
    ~geo, ~group,
    "EA", 'aggregate',
    "EU27_2020", 'aggregate',
    "EU28", 'aggregate',
    "EU15", 'aggregate',
    "EU27_2007", 'aggregate',
    "HU", "Visegrad",
    "PL", "Visegrad",
    'SK', "Visegrad",
    'CZ', "Visegrad",
    'LV', "Baltic",
    'LT', "Baltic",
    'EE', "Baltic",
    'BE', 'Benelux',
    'NL', 'Benelux',
    'LU', 'Benelux',
    'UK', "West",
    'GB', "West",
    'GB-G', "West",
    'GB-N', "West",
    'IE', "West",
    'DE', 'Central',
    'DE-W', "Central",
    'DE-E', "Central",
    'LI',  "Central",
    'CH',  "Central",
    'AT',  "Central",
    'SI', "Central",
    'FR', "West",
    'ES', "South",
    'PT', "South",
    'IT', "South",
    'MT', "South",
    'SE', "Nordic",
    'DK', "Nordic",
    'FI', "Nordic",
    'IS', "Nordic",
    'NO', "Nordic",
    'HR', "Southeast",
    'GR', "Southeast",
    'EL', "Southeast",
    'RO', "Southeast",
    'BG', "Southeast",
    'RS', "Southeast",
    'ME', "Southeast",
    'BA', "Southeast",
    'XK', "Southeast",
    'MK', "Southeast",
    'AL', "Southeast",
    'SK', "Southeast",
    'MD', "East",
    'TR', "East",
    'UA', "East",
    'RU', "East",
    'AM', "East",
    'CY', "South"
  )
}
