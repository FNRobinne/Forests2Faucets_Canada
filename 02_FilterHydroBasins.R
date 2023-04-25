## ---------------------------
## Script name: 02_FilterHydroBASINS.R
##
## Purpose of script: Separate inland from coastal HUC12 catchments, also isolate
##                    catchments without a HydroRIVERS segment
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2022-01-21
##
## Version: 1.0
##
## Project: Forest to Faucets Canada
##
## Copyright (c) François-Nicolas Robinne, 2022
## Email: francois.robinne@nrcan-rncan.gc.ca / robinne@ualberta.ca
##
## 
## Notes: Repaired geometries come from QGIS; it could be done directly in R using
##        {QGIS Geoprocessing}
## 
## set working directory -------------------------------------------------------

# Update as needed
setwd("C:/Users/frobinne/Documents/Professional/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options -------------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries --------------------------------------------------------------

library(tidyverse)
library(sf)

## load data -------------------------------------------------------------------

# All data are in NAD83 Canada Atlas Lambert (EPSG:3979)
hybas_can <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_HUC12_RepairedGeometry_EPSG3979_UpdatedVariables.gpkg")
hyriv_can <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroRivers_HUC3_WaterSupply_RepairedGeometry_EPSG3979.gpkg")

## data processing -------------------------------------------------------------

hybas_inland <- hybas_can %>%
  filter(COAST != 1) 

hybas_inland_valid <- hybas_inland %>%
  filter(lengths(st_intersects(., hyriv_can, sparse = T)) > 0)

hybas_invalid <- hybas_can %>%
  filter(!lengths(st_equals_exact(., hybas_inland_valid, par = 0.001, sparse = T)) > 0)

st_write(hybas_inland_valid,
         "02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC12_RepairedGeometry_Extract_HUC3_WaterSupply_EPSG3979_UpdatedVariables_Valid.gpkg",
         delete_layer = T)
st_write(hybas_invalid, 
         "02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC12_RepairedGeometry_Extract_HUC3_WaterSupply_EPSG3979_UpdatedVariables_Invalid.gpkg",
         delete_layer = T)
