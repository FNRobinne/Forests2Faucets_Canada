## ---------------------------
## Script name: 
##
## Purpose of script:
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2022-01-07
## 
## Version: 1.0
##
## Copyright (c) François-Nicolas Robinne, 2022
## Email: robinne@ualberta.ca
##
## ---------------------------
## Notes:
##   
##
## ---------------------------

## set working directory ---------------------------

setwd("C:/Users/tim/Google Drive/") 

## general options ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries ---------------------------

require(tidyverse)
require(data.table)
# source("functions/packages.R")       # loads up all the packages we need

## load functions ---------------------------

# source("functions/summarise_data.R") 

## load data ---------------------------

library(sf)
library(dplyr)
library(rgeos)
library(maptools) # warning that this package will be retired by the end of 2023
                  # By then, I hope the snap function from sf works better

setwd("C:/Users/frobinne/Documents/Professionel/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/03_ANALYSIS/CODE/RIVERDIST")

pts <- st_read("TEST_DATA/Subset_BA_Centroids_RiverdistDemo.shp") %>%
  st_transform(crs = 3979)
rivs <- st_read("TEST_DATA/Subset_HR_RiverdistDemo.shp") %>%
  st_transform(crs = 3979)
ws <- st_read("TEST_DATA/Subset_BA_RiverdistDemo.shp") %>%
  st_transform(crs = 3979)

# Filter to remove rivers of order one

rivs_two <- rivs %>%
  filter(ORD_STRA > 1) %>% # will be updated with more advanced filter
  group_by(HYBAS_L12) %>% 
  summarise() %>%
  st_cast

# Loop

# Creates empty SF to collect results from snap
snap_pts <- pts %>%
  filter(HYBAS_ID == 1) # Dummy filter to get empty structure

for (pt in pts$HYBAS_ID) {
  for (riv in rivs_two$HYBAS_L12) {
    if (pt == riv) {
      pt_vec <- pts %>%
        filter(HYBAS_ID == pt)
      riv_vec <- rivs_two %>%
        filter(HYBAS_L12 == riv)
      snappy <- snapPointsToLines(as_Spatial(pt_vec), as_Spatial(riv_vec))
      snap_pts <- bind_rows(snap_pts, st_as_sf(snappy))
    }
  }
}

snap_pts_spat_join <- snap_pts %>%
  select(geometry) %>%
  st_join(ws)
  
# Export to geopackage, as sf makes writing shapefile a pain
st_write(snap_pts_spat_join, "TEST_DATA/Subset_BA_Centroids_SnapTestR.gpkg", append = F)
