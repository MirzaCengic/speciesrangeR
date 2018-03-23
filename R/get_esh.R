#' Get extent of suitable habitat (ESH).
#' Retrieve extent of suitable habitat according to a single environmental variable.
#' Variable should be continous raster (in future add multiple rasters + categorical) variables.
#' Function removes the species extent of occurence outside of minimum and maximum values of raster variable.
#'
#' @param species_data Output of get_species_data() function. Currently only sp object.
#' @param species_eoo Output of get_eoo() function. Currently only sp object.
#' @param habitat_variable Raster layer. Should be continous variable with same CRS and overlapping extent.
#'
#' @return
#' @export
#'
#' @examples
get_esh <- function(species_data, species_eoo, habitat_variable)
{
  data_cropped <- raster::crop(habitat_variable, species_eoo)
  data_cropped <- raster::mask(data_cropped, species_eoo)
# Get altitude data for species presences. The range will be filtered within current altitude range (according to SRTM 90m DEM).

species_values <- extract(data_cropped, species_data)

data_cropped[data_cropped < min(species_values, na.rm = TRUE)] <- NA
data_cropped[data_cropped > max(species_values, na.rm = TRUE)] <- NA
# All values to 1 for polygon conversion

data_cropped[!is.na(data_cropped)] <- 1

species_range_cropped <- rasterToPolygons(data_cropped, n = 16, na.rm = TRUE, dissolve = TRUE)
return(species_range_cropped)
}