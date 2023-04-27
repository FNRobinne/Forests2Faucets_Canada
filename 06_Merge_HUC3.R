## ---------------------------
## Script name: 05_CombineBasinsSWiVi.R
##
## Purpose of script: Creates a connectivity index for each HUC12 catchment
##                    within a HUC3 drainage, primarily using upstream distance,
##                    then weighted by water demand. Based principally on the creation
##                    of an origin-destination matrix
##
## Author: Dr. Francois-Nicolas Robinne
## 
## Version: 1.0
##
## Date Created: 2022-09-16
##
## Copyright (c) Francois-Nicolas Robinne, 2022
## Email: francois.robinne@nrcan-rncan.gc.ca
##
## ---------------------------
##
## Notes:
##   

## set working directory -------------------------------------------------------

  setwd("C:/Users/frobinne/OneDrive - NRCan RNCan/Documents/Professionel/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options -------------------------------------------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
  memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries --------------------------------------------------------------

  library(tidyverse)
  library(sf)
  
## load data -------------------------------------------------------------------

  # 'Invalid' HUC12 layer refers to these catchments that were fully disconnected
  huc12_invalid <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC12_RepairedGeometry_Extract_HUC3_WaterSupply_EPSG3979_UpdatedVariables_Invalid.gpkg")
  huc12_valid <- st_read("03_ANALYSIS_RESULTS/huc_12_Inland_SWiVi.gpkg")  
  canada <- st_read("02_PROCESSED_DATA/STATSCAN/Canada_Boundaries_EPSG3979.shp")

## Processing ------------------------------------------------------------------
  
  # Add SWiVi column to the 'Invalid' layer
  huc12_invalid <- huc12_invalid %>%
    mutate(SWiVi = mww_m3_syr)

  # Merge Valid and Invalid
  huc12_SWiVi_merge <- rbind(huc12_valid, huc12_invalid) %>%
    rename(ii_ul_sii = SWiVi)# Rename SWiVi column to its hydroatlas name
  
  # Subset to Canada border
  huc12_SWiVi_can <- huc12_SWiVi_merge %>%
    filter(lengths(st_intersects(.,canada)) > 0)

## Save outputs ----------------------------------------------------------------
  st_write(huc12_SWiVi_Can, "03_ANALYSIS_RESULTS/huc_12_SWiVi_can.gpkg")