
<!-- README.md is generated from README.Rmd. Please edit that file -->
speciesrangeR - species range maps in R
=======================================

This repository holds a package with several skeleton functions to retrieve data from online repositories, and to create species range maps. This repo serves as a **proof of concept** for [GSoC 2018 proposal](https://github.com/rstats-gsoc/gsoc2018/wiki/Species-range-maps-in-R).

To install the package run:

``` r
devtools::install_github("mirzacengic/speciesrangeR")
```

#### Tasks to solve:

-   Easy: write a script to download occurrence data from GBIF for a species and display them on a map.
-   Medium: Draw a convex hull polygon around the points in the earlier test.
-   Hard: Clip the polygon you generated in medium test to world map (Keep only part of the polygon which is on land).
-   Bonus: Get extent of suitable habitat within the range.

``` r
if(!require(pacman))
{
  install.packages("pacman")
}
```

    ## Loading required package: pacman

``` r
if(!require(speciesrangeR))
{
  pacman::p_install_gh("mirzacengic/speciesrangeR")
}
```

    ## Loading required package: speciesrangeR

``` r
pacman::p_load(spocc, sf, mapview, lubridate, dplyr, raster, tictoc,
               sf, scrubr, stringr, sp, rgeos, dismo, broom, tmap)
```

Working with speciesrangeR
--------------------------

We will load here speciesrangeR package. Temporarily it is a set of scripts that can be sourced, but will be replaced with package that can be installed via devtools. Use get\_species\_data() to query [GBIF](https://www.gbif.org/). Function is a wrapper around occ() function from [spocc](https://ropensci.github.io/spocc/) package.

``` r
# Load package 
library(speciesrangeR)

# Get gbif data for Salamandra atra for Italy, with cleaned output coordinates.
sal_atra_ita <- get_species_data("Salamandra atra", return_clean = TRUE, 
                                 return = "sp", country = "ITA")

# Plot maps
sal_atra_map <- mapview(sal_atra_ita)
```

Get occurence data from [GBIF](https://www.gbif.org/)
-----------------------------------------------------

Here we will retrieve species occurence data for species Salamandra atra. Function get\_species\_data() can take country argument, so we will query for data for Italy. Default limit of spocc:occ() function is 500 points, however you can pass a numeric value to "limit" argument. Country data is filtered afterwards, therefore the number of points does not have to correspond to limit. See help of the function for more details.

We will inspect the data with some interactive mapping.

Previewing retrieved species data.

``` r
mapview(sal_atra_map)
```

![](map1.png)

One point is in Torino and is clearly wrong datapoint. Most likely the point is assigned to an instution where the specimen is storaged.

``` r
p2 <- mapview(sal_atra_ita[which.min(sal_atra_ita@coords), ])
mapshot(p2, file = "map2.png")
```

``` r
mapview(sal_atra_map)
```

![](map2.png)

Function get\_species\_data() has argument return\_clean which uses [scrubr](https://github.com/ropensci/scrubr) package to clean wrong data. According to the documentation of scrubr package, removing of these cases is on a feature list to be implemented, but now we will do it manually.

``` r
sal_atra_ita_clean <- sal_atra_ita[-which.min(sal_atra_ita@coords), ]

p3 <- mapview(sal_atra_ita_clean)
mapshot(p3, file = "map3.png")
```

![](map3.png)

Extent of occurence
-------------------

Calculates excent of occurence using minimum convex hull from rgeos package. Can return simple feature or spatial points dataframe.

``` r
sal_atra_eoo <- get_eoo(sal_atra_ita_clean, return = "sp")


p4 <- mapview(sal_atra_eoo)
mapshot(p4, file = "map4.png")
```

![](map4.png)

Extent of suitable habitat
--------------------------

In this part, we will extract extent of suitable habitat by filtering extent of occurence (EOO) within min-max values of a raster variable. In future multiple rasters + categorical variables. Add later custom cutoff values and function passing instead of min/max.
