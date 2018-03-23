##%######################################################%##
#                                                          #
####           Get species data function.  #            ####
####  Mirza Cengic | mirzaceng@gmail.com | 23.03.2018.  ####
#                                                          #
##%######################################################%##


#' Get species data
#'
#' Retrieve species data from GBIF repository. You can use species binomial name
#' to query the database. Query is passed to spocc::occ() function (use ... to pass further arguments to this function).
#'
#'
#' @param species_name Binomial species name in format "Genus species".
#'   Character.
#' @param return Object to return. If return = "sf" (default) will return simple features
#'   object, while return = "sp" will return spatial points data frame object.
#' @param return_clean Logical (Default is FALSE). Return data with cleaned observation. Data cleaning is
#'   performed with scrubr package (functions coord_impossible(),
#'   coord_incomplete() and coord_unlikely()). See
#'   https://ropensci.org/tutorials/scrubr_tutorial/ for more details.
#' @param country Crop results with country boundaries. Results might be
#'   influenced by limit argument from spocc::occ() function. Country data is
#'   retrieved from GADM database. See ?raster::getData() for more details. Use
#'   getData('ISO3') to see the available country options.
#' @param ... Additional arguments that can be passed to spocc::occ() function.
#'
#' @return A spatial object (sf or sp class)
#' @export
#'
#' @importFrom magrittr "%>%"
#' @import spocc
#' @import scrubr
#' @import sp
#' @import sf
#' @importFrom raster getData
#'
#' @importFrom magrittr "%>%"
#' @examples sal_atra <- get_species_data("Salamandra atra")
#' plot(sal_atra[1])
#' sal_atra_ita <- get_species_data("Salamandra atra", return_clean = TRUE, country = "ITA", limit = 10000)
#' plot(sal_atra_ita[1])
get_species_data <- function(species_name, return = "sf",
                             return_clean = FALSE, country = "", ...)
{
  # Basic argument checks
  stopifnot(return %in% c("sf", "sp"), is.character(species_name))
  # Get species spatial data from GBIF
  sp_data <- spocc::occ(query = species_name,
                        from = "gbif",
                        has_coords = TRUE, ...)
  # As spatial points dataframe
  sp_data <- sp_data %>%
    spocc::occ2df()

  # Function fork - clean coordinates with scrubr package
  if (return_clean)
  {
    sp_data <- sp_data %>%
      coord_unlikely(lat = "latitude", lon = "longitude", drop = TRUE) %>%
      coord_incomplete(lat = "latitude", lon = "longitude", drop = TRUE) %>%
      coord_impossible(lat = "latitude", lon = "longitude", drop = TRUE) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      as("Spatial")
  } else {
    sp_data <- sp_data %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      as("Spatial")
  }
  if (nchar(country) > 1)
  {
    country_shape <- raster::getData("GADM", country = country, level = 0)
    sp_data <- sp_data[country_shape, ]
  }

  if (return == "sp")
  {

    return(sp_data)
  }

  if (return == "sf")
  {
    sp_data <- sp_data  %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    return(sp_data)
  }
  stop("Input error.")
}
