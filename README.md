README
================
Mirza Cengic
23 mart 2018

speciesrangeR - species range maps in R
=======================================

This repository hold skeleton functions to retrieve species data from GBIF and create species ranges. Functions will eventually be migrated into a package. This repo serves as a **proof of concept** for [GSoC 2018 proposal](https://github.com/rstats-gsoc/gsoc2018/wiki/Species-range-maps-in-R).

#### Tasks to solve:

-   Easy: write a script to download occurrence data from GBIF for a species and display them on a map.

-   Medium: Draw a convex hull polygon around the points in the earlier test.

-   Hard: Clip the polygon you generated in medium test to world map (Keep only part of the polygon which is on land).

``` r
if(!require(pacman))
{
  install.packages("pacman")
}
```

    ## Loading required package: pacman

``` r
pacman::p_load(spocc, sf, mapview, lubridate, dplyr, raster, tictoc,
               sf, scrubr, stringr, sp, rgeos, dismo, broom, tmap)
```

Working with speciesrangeR
--------------------------

We will load here speciesrangeR package. Temporarily it is a set of scripts that can be sourced, but will be replaced with package that can be installed via devtools. Use get\_species\_data() to query [GBIF](https://www.gbif.org/). Function is a wrapper around occ() function from [spocc](https://ropensci.github.io/spocc/) package.

``` r
source("Y:/Mirza_Cengic/Projects/Other/GSoC/R/get_species_data.R")

sal_atra_ita <- get_species_data("Salamandra atra", return_clean = TRUE, 
                                 return = "sp", country = "ITA")

# Plot maps
# mapview(sal_atra_ita)
sal_atra_map <- mapview(sal_atra_ita)
mapshot(sal_atra_map, file = "map.png")
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
