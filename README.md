
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- output: rmarkdown::github_document -->
<!-- output: html_notebook -->
speciesrangeR - species range maps in R
=======================================

This repository holds a package with several (yet to be tested) functions to retrieve data from online repositories, and to create species range maps. This repo is inspired by [GSoC 2018 proposal](https://github.com/rstats-gsoc/gsoc2018/wiki/Species-range-maps-in-R), and it serves as a **proof of concept** for how to solve the given problem:

#### Tasks to solve:

-   Easy: write a script to download occurrence data from GBIF for a species and display them on a map.
-   Medium: Draw a convex hull polygon around the points in the earlier test.
-   Hard: Clip the polygon you generated in medium test to world map (Keep only part of the polygon which is on land).
-   Bonus: Get extent of suitable habitat within the range.

------------------------------------------------------------------------

Working with speciesrangeR
--------------------------

To install the package run:

``` r
devtools::install_github("mirzacengic/speciesrangeR")
```

**Use at your own responsibility.**

Let's load the necessary packages using `p_load` function from `pacman` package.

``` r
library(speciesrangeR)

pacman::p_load(spocc, sf, mapview, dplyr, raster, sf, scrubr, stringr, sp, rgeos)
```

------------------------------------------------------------------------

Get occurence data from GBIF
----------------------------

We will load here speciesrangeR package. Use `get_species_data()` to query data from the [Global Biodiversity Information Facility - GBIF](https://www.gbif.org/). Function is a wrapper around [`occ()`](https://www.rdocumentation.org/packages/spocc/versions/0.7.0/topics/occ) function from [spocc](https://ropensci.github.io/spocc/) package.

``` r
# Get gbif data for Salamandra atra for Italy, with cleaned output coordinates.
sal_atra_ita <- get_species_data("Salamandra atra", return_clean = TRUE, 
                                 return = "sp", country = "ITA")
```

Here we will retrieve species occurence data for species [*Salamandra atra*](https://www.gbif.org/species/2431781) Laurenti, 1768. Function `get_species_data()` can take country argument to return only the data for given country (use `raster::getData("ISO3")` for list of countries), so we will query for data for Italy. Default limit of `spocc::occ()` function is 500 points, however you can pass a numeric value to the `limit` argument. Country data is filtered after the GBIF query, therefore the number of points does not have to correspond to limit. Additional arguments can be passed to the `spocc::occ()` function by using `...`. Use `?get_species_data` and `?spocc::occ`to see the documentation of both functions for more details.

We will preview the retrieved species data.

``` r
# Plot data
mapview(sal_atra_ita)
```

![Retrieved species occurences](README_figures/map1.png)

One point is in Torino and on closer inspection it seems like an erroneous datapoint. Most likely the point is assigned to an instution where the specimen is storaged.

``` r
# Plot the most easterly point 
mapview(sal_atra_ita[which.min(sal_atra_ita@coords), ])
```

![](README_figures/map2.png)

Function `get_species_data()` has argument `return_clean` which uses [`scrubr`](https://github.com/ropensci/scrubr) package to clean wrong data. Currently, `return_clean = TRUE` will clean the coordinates that are missing, unlikely or impossible. According to the documentation of scrubr package, removing of these cases is on a feature list to be implemented, but now we will remove the erroneous point manually.

``` r
# Remove the most easterly point.
sal_atra_ita_clean <- sal_atra_ita[-which.min(sal_atra_ita@coords), ]
```

Let's plot the cleaned dataset.

``` r
mapview(sal_atra_ita_clean)
```

![](README_figures/map3.png)

On the first look it looks better. Now we will calculate the extent of occurence (EOO).

------------------------------------------------------------------------

Extent of occurence
-------------------

Extent of occurence is calculated using minimum convex hull from the `rgeos` package. Function can return simple feature or spatial points dataframe. In the future more methods of calculating bounding polygons should be added (eg. concave hull).
- TODO: Incorporate buffer argument where EOO would include buffers around occurence points specified to user's distance (as proposed by [Graham & Hijmans 2006](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1466-8238.2006.00257.x)).

``` r
# Get EOO
sal_atra_eoo <- get_eoo(sal_atra_ita_clean, return = "sp")
```

Plotting the extent of occurence.

``` r
mapview(sal_atra_eoo)
```

![](README_figures/map4.png)

------------------------------------------------------------------------

Extent of suitable habitat
--------------------------

In this part, we will extract extent of suitable habitat by filtering extent of occurence (EOO) within min-max values of a raster variable. In the future multiple rasters + categorical variables should be added as filtering variable. Add later custom cutoff values and function passing instead of min/max. In the future, calculating the extent of suitable habitat should be also optionally implemented by calculating presence-only envelope model, which would be used to filter the extent of occurence (as suggested by [Graham & Hijmans 2006](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1466-8238.2006.00257.x)).

``` r
# Load SRTM raster for North Italy (resampled to ~2 kilometer resolution for faster code execution)
srtm_italy <- raster("Y:/Mirza_Cengic/Projects/Other/GSoC/srtm_39_03_1.tif")

# Might take time with fine cell rasters
sal_atra_esh <- get_esh(species_data = sal_atra_ita_clean, 
                        species_eoo = sal_atra_eoo,
                        habitat_variable = srtm_italy)
```

Let's plot the extent of suitable habitat with the overlaid points used for creating ESH.

``` r
mapview(sal_atra_esh)
```

Extent of suitable habitat with filtered elevation values. **NOTE** - there are some errors in the overlap of point due to raster aggregation, use better example. ![](README_figures/map5.png)

Right now this repository serves only as a proof of concept and it has not been properly tested yet. All suggestions for improvements, feature requests and bug reports are more than welcomed!
