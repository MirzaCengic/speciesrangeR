##%######################################################%##
#                                                          #
####           Initial script to work on GSoC           ####
####     code # Mirza Cengic | mirzaceng@gmail.com      ####
#                                                          #
##%######################################################%##


# Stuff TODO: -------------------------------------------------------------
#
# Easy: write a script to download occurrence data from GBIF for a species and
# display them on a map 

# Medium: Draw a convex hull polygon around the points in the earlier test. 

# Hard: Clip the polygon you generated in medium test to world
# map (Keep only part of the polygon which is on land).

#### 1. Get gbif species point data:
# Tasks: write function that retrieves point data for given species.
# Potential arguments - return_clean (clean data with that Ropensci pkg),
# bounding_box - return for given bounding box, country - return for given country
# 
#### 2. Draw polygons around points
# Tasks: write function that can give convex or concave hulls 

#### 3. Clip to land area
# Tasks: instead for task 1 - move country argument here?

# Bonus: fit bioclim model to filter out suitable habitat within the EOO
pacman::p_load(spocc, sf, mapview, lubridate, dplyr, raster,
               sf, scrubr, stringr, sp, rgeos, dismo)

# Create empty dataframe
species_name <- "Salamandra atra"

#### get species data working version ####
get_species_data <- function(species_name, return = "sf", 
                             return_clean = FALSE, country, ...)
{
  # Basic argument checks
  stopifnot(return %in% c("df", "sf", "sp"), is.character(species_name))
  
  # Get species spatial data from GBIF
  sp_data <- spocc::occ(query = species_name, 
                        from = "gbif",
                        has_coords = TRUE, ...)
  # As data.frame
  sp_data <- sp_data %>% 
    spocc::occ2df() 
  
  # Function fork - clean coordinates with scrubr package
  if (return_clean)
  {
    
    sp_data <- sp_data %>% 
      coord_unlikely(lat = "latitude", lon = "longitude", drop = TRUE) %>% 
      coord_incomplete(lat = "latitude", lon = "longitude", drop = TRUE) %>% 
      coord_impossible(lat = "latitude", lon = "longitude", drop = TRUE)
    
  }
  if (return == "df")
  {
    return(sp_data)
  }
  if (return == "sf")
  {
    sp_data <- sp_data  %>% 
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    return(sp_data) 
  } 
  if (return == "sp") {
    sp_data <- sp_data  %>% 
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
      as("Spatial")
    return(sp_data)
  }
}

#### DEVLON ####

# As data.frame

#### get_species_data developing part ####
## Final build ####
#### get species data working version ####


#' Get species data
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
#'   getData('ISO3') to see the available country options. #' @param ...
#'
#' @return A spatial object (sf or sp class)
#' @export
#'
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
  
  if (length(country) > 1)
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


# Get country bounding box ####

# bbox <- country_shape %>% 
#   st_as_sf() %>% 
#   st_bbox() %>% 
#   as.numeric()

#### Crop with country ####

  
mapview(sp_data)


# climate_tmin <- raster::getData("worldclim", var = "tmin", res = "10")
# climate_tmax <- raster::getData("worldclim", var = "tmax", res = "10")
# climate_prec <- raster::getData("worldclim", var = "prec", res = "10")
# climate_alt <- getData("alt", country = "ITA", mask = TRUE)

# Get srtm data from range ####
# From http://www.gis-blog.com/r-raster-data-acquisition/ 

srtm_tiles <- shapefile("E:/GIS_data/Get_SRTM/srtm/tiles.shp")

#Intersect country geometry with tile grid
intersects <- gIntersects(sp_data_ita, srtm_tiles, byid = TRUE)
tiles      <- srtm_tiles[intersects[,1],]


#Download tiles
srtm_list  <- list()
for (i in 1:length(tiles)) {
  lon <- extent(tiles[i,])[1]  + (extent(tiles[i,])[2] - extent(tiles[i,])[1]) / 2
  lat <- extent(tiles[i,])[3]  + (extent(tiles[i,])[4] - extent(tiles[i,])[3]) / 2
  
  tile <- getData('SRTM', 
                  lon=lon, 
                  lat=lat)
  
  srtm_list[[i]] <- tile
}

#Mosaic
srtm_list$fun <- mean 
srtm_mosaic   <- do.call(mosaic, srtm_list)

#Crop to country borders
srtm_crop     <- mask(srtm_mosaic, country)

#Plot results
p <- levelplot(srtm_crop)
p + layer(sp.lines(country, 
                   lwd=0.8, 
                   col='darkgray'))

####

climate_stack <- raster::stack(climate_tmin, climate_tmax, climate_prec)

my_model <- dismo::bioclim(climate_stack, sp_data_bih)

#### EOO function ####
sal_atra_df
species_data <- sal_atra





create_eoo <- function(species_data, return = "sf")
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
        mutate(
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
        mutate(
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


## Create EOO working version ####
# create_eoo <- function(species_data, return = "sf")
# {
#   stopifnot(return %in% c("sf", "sp"))
#   
#   # Check if input object is simple feature or data frame. Mail EP to check if there is a better way.
#   # Check if object contains columns latitude or longitude
#   # Fork creates object_type that contains object related string.
#   if (inherits(species_data, "data.frame"))
#   {
#     if (any(stringr::str_detect(names(species_data), paste(c("latitude", "longitude"), collapse = '|'))))
#     {
#       # Object type - "sf"
#       species_chull <- species_data %>% 
#         as("Spatial") %>% 
#         rgeos::gConvexHull()
#       return(species_chull)
#        
#     } else {
#       # Object type - "df"
#       species_chull <- species_data %>% 
#         sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#         as("Spatial") %>% 
#         rgeos::gConvexHull()
#       return(species_chull)
#     }
#   } else if (inherits(species_data, "SpatialPointsDataFrame"))
#   {
#     # Object type - "sp"
#     species_chull <- rgeos::gConvexHull(species_data) 
#     return(species_chull)
#   } else {
#     stop("Input is not one of sf, sp or df.")
#   } 
# }

#### Playing with data ####

sal_atra <- get_species_data(species_name = "Salamandra atra", return = "sf", return_clean = TRUE)

sal_atra_sp <- get_species_data(species_name = "Salamandra atra", return = "sp", return_clean = TRUE)

sal_atra_sp <- get_species_data(species_name = "Vipera berus", return = "sp", return_clean = TRUE)

sal_atra_eoo <- create_eoo(sal_atra, return = "sf")
mapview(sal_atra)
#### DEVOFF ###

