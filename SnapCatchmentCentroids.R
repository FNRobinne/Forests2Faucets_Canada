## ---------------------------
## Script name: SnapCatchmentCentroids.R
##
## Purpose of script: Snaps the centroid of each HydroBasin catchment to its corresponding HydroRiver segment
##                    It is essentially a snap-by-attribute tool
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2022-01-07
## 
## Version: 1.0
##
## Copyright (c) François-Nicolas Robinne, 2022
## Email: francois.robinne@nrcan-rncan.gc.ca
##
## ---------------------------
## Notes:
##   Starts with the data resulting from 'SimplifyRiverNetwork.R'
##    This is part of the project Forest To Faucets - Canada
## ---------------------------

## set working directory ---------------------------

  setwd("D:/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options ---------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
  memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries ---------------------------
  
  library(tidyverse)
  library(sf)
  library(dplyr)
  library(rgdal)

# source("functions/packages.R")       # loads up all the packages we need

## load functions ---------------------------------------------------------

# source("functions/summarise_data.R") 

## load data ---------------------------------------------------------------

  HYBAS_Fraser <- st_read("02_PROCESSED_DATA/HydroBasin_HUC12_Fraser_Fixed.shp") %>%
    st_transform(crs = 3979) %>%
    select(HYBAS_ID)
  HYRIV_Fraser <- st_read("02_PROCESSED_DATA/HydroRiver_Fraser_Lite.gpkg") # THis is the simplified version of the river network
  
## Data processing ---------------------------------------------------------

  # Create catchment centroids
  HYBAS_centroids <- st_centroid(HYBAS_Fraser)
  # Creates empty SF to collect results from snap
  snap_pts <- HYBAS_centroids %>%
    filter(HYBAS_ID == 1) # Dummy filter to get empty structure
  
  # Loop snapping centroids to closest point on the closest river segment
  # Process seems convoluted, but st_snap does not return the desired output
  # The tool gets the nearest point, casts it, and add the ID of the corresponding HYBAS
  for (pt in HYBAS_centroids$HYBAS_ID) {
    for (riv in HYRIV_Fraser$HYBAS_L12) {
      if (pt == riv) {
        pt_vec <- HYBAS_centroids %>%
          filter(HYBAS_ID == pt)
        riv_vec <- HYRIV_Fraser %>%
          filter(HYBAS_L12 == riv)
        nrst <- st_nearest_points(pt_vec, riv_vec)
        snappy <- st_cast(nrst, "POINT") [2] %>%
          as.data.frame() %>%
          mutate(HYBAS_ID = pt_vec$HYBAS_ID)
        snap_pts <- bind_rows(snap_pts, snappy)
      }
    }
  }

  
  # ESDA (just checking source and snap result)
  ggplot()+
    geom_sf(data = HYBAS_centroids, colour = 'blue') +
    geom_sf(data = snap_pts, colour = 'red')
  
  # Convert the centroids to SP format for use in RiverDist
  snap_pts_ID <- snap_pts %>%
    mutate(ID = row_number())
  snap_pts_SP <- as_Spatial(from = snap_pts_ID)
  
  # At this stage, it is possible to join the other attributes
  # As they were removed when loading the data (i.e., select HYBAS_ID)
  # Or, alternatively, can be done in a subsequent script
  
# Data export -------------------------------------------------------------

  # Export to geopackage, as sf makes writing shapefile a pain
  st_write(HYBAS_centroids, "02_PROCESSED_DATA/HYBAS_Fraser_Centroids.gpkg", append = F )
  st_write(snap_pts, "02_PROCESSED_DATA/HYBAS_Fraser_Centroids_Snap.gpkg", append = F)
  writeOGR(snap_pts_SP, getwd(), "Centroids_Fraser_RD", driver = "ESRI Shapefile") # Version for RiverDist (sp format)
