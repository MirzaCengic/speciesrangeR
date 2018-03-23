# speciesrangeR - species range maps in R

This repository hold skeleton functions to retrieve species data from GBIF and create species ranges. Functions will eventually be migrated into a package.
This repo serves as a **proof of concept** for [GSoC 2018 proposal](https://github.com/rstats-gsoc/gsoc2018/wiki/Species-range-maps-in-R).

#### Tasks to solve:  
* Easy: write a script to download occurrence data from GBIF for a species and
display them on a map.  

* Medium: Draw a convex hull polygon around the points in the earlier test.  

* Hard: Clip the polygon you generated in medium test to world
map (Keep only part of the polygon which is on land).  

