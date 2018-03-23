#' Get extent of occurence (EOO).
#'
#' @param species_data Output of get_species_data() function. Supports sp and sf objects.
#' @param return Object to return. If return = "sf" (default) will return simple features
#'   object, while return = "sp" will return spatial points data frame object.
#'
#' @return A spatial object (sf or sp class).
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom rgeos gConvexHull
#' @import sf
#' @import sp
#' @importFrom dplyr mutate
#'
#'
#' @examples sal_atra <- get_species_data("Salamandra atra")
#' sal_atra_eoo <- get_eoo(sal_atra, return = "sp")

get_eoo <- function(species_data, return = "sf")
{
  stopifnot(return %in% c("sf", "sp"))

  # Check if input object is simple feature or data frame. Mail EP to check if there is a better way.
  # Check if object contains columns latitude or longitude
  # Fork creates object_type that contains object related string.
  if (inherits(species_data, "data.frame"))
  {
    if (any(stringr::str_detect(names(species_data), paste(c("latitude", "longitude"), collapse = '|'))))
    {
      # Object type - "sf"
      species_chull <- species_data %>%
        as("Spatial") %>%
        rgeos::gConvexHull() %>%
        sf::st_as_sf()

      species_chull <- species_chull %>%
        dplyr::mutate(
          name = unique(species_data$name),
          prov = unique(species_data$prov))

      if (return == "sf")
      {
        return(species_chull)
      } else {
        return(as(species_chull, "Spatial"))
      }

    } else {
      # Object type - "df"
      species_chull <- species_data %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        as("Spatial") %>%
        rgeos::gConvexHull() %>%
        sf::st_as_sf()

      species_chull <- species_chull %>%
        dplyr::mutate(
          name = unique(species_data$name),
          prov = unique(species_data$prov))

      if (return == "sf")
      {
        return(species_chull)
      } else {
        return(as(species_chull, "Spatial"))
      }
    }
  } else if (inherits(species_data, "SpatialPointsDataFrame"))
  {
    # Object type - "sp"
    species_chull <- species_data %>%
      rgeos::gConvexHull() %>%
      sf::st_as_sf()

    species_chull <- species_chull %>%
      mutate(
        name = unique(species_data$name),
        prov = unique(species_data$prov))

    if (return == "sf")
    {
      return(species_chull)
    } else {
      return(as(species_chull, "Spatial"))
    }
    return(species_chull)
  } else {
    stop("Input is not one of sf, sp or df.")
  }
}
