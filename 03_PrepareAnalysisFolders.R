## ---------------------------
## Script name: 03_PrepareAnalysisFolders.R
##
## Purpose of script: Extract all necessary data for network analysis using the
##                    HUC3 drainages and store them in individual folders using
##                    HUC3 HYBAS codes
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2021-10-26
## 
## Version: 1.0
##
## Copyright (c) François-Nicolas Robinne, 2021
## Email: francois.robinne@nrcan-rncan.gc.ca or robinne@ualberta.ca
##
## ---------------------------
## Notes:
##    - Make sure to use watershed and river layers updated in scripts 01 and 02
##    - Update paths as needed
##    


## set working directory -------------------------------------------------------

  setwd("C:/Users/frobinne/Documents/Professional/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options -------------------------------------------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## load libraries --------------------------------------------------------------

  library(sf)
  library(dplyr)
  library(stringr)

## Load data -------------------------------------------------------------------

  HYBAS_HUC3 <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_RepairedGeometry_EPSG3979.gpkg")
  HYBAS_HUC3_NoGeom <- st_drop_geometry(HYBAS_HUC3)
  # Make sure to use HUC12 layer from 01_UpdateHydroAtlas
  HYBAS_HUC12 <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC12_RepairedGeometry_Extract_HUC3_WaterSupply_EPSG3979_UpdatedVariables_Valid.gpkg")
  # HUC12 outlets provided by Bernart Lehner and processed in QGIS
  HYBAS_OUT <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_Outlets_HUC12_RepairedGeometry_EPSG3979.gpkg")
  # HydroRIVERS network (if small PC, it might be necessary to create a simplified version of it, i.e., by prunning it)
  HYRIV <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroRivers_HUC3_WaterSupply_RepairedGeometry_EPSG3979.gpkg")
  # Make sure to use the most up to date version of the water licence file
  WATLIC <- st_read("02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V5_20230221_NonSnap.gpkg")
  dst <- paste(getwd(), "/03_ANALYSIS_RESULTS/Canada_Forest2Faucets", sep = "")

## Data processing -------------------------------------------------------------

  # Creates a loop to parse national-scale updated HydroBASINS HUC12 dataset using HUC3 drainages
  for(i in 1:nrow(HYBAS_HUC3)) {
    pfafID <- as.character(HYBAS_HUC3_NoGeom[i,9])
    sub_huc3 <- HYBAS_HUC3 %>%
      filter(grepl(pfafID, PFAF_ID))
    sub_huc12 <- HYBAS_HUC12 %>%
      #filter(str_detect(as.character(PFAF_ID), paste0("^",pfafID)))
      filter(lengths(st_within(., sub_huc3, sparse = T, prepared = T)) > 0)
    sub_out <- HYBAS_OUT %>%
      filter(lengths(st_within(., sub_huc12, sparse = T, prepared = T)) > 0) %>%
      st_cast("POINT") %>%
      mutate(ID = row_number())
    sub_riv <- HYRIV %>%
      filter(lengths(st_intersects(., sub_huc12, sparse = T)) > 0) %>%
      mutate(ID = row_number())
    sub_lic <- WATLIC %>%
      filter(lengths(st_within(., sub_huc12, sparse = T, prepared = T)) > 0)
    mkfldr <- as.character(HYBAS_HUC3_NoGeom[i,1])
    if(file.exists(file.path(dst,mkfldr)) == F) {
      dir.create(file.path(dst,mkfldr))
    }
    # Save outputs
    st_write(sub_huc3, paste0(dst, "/", mkfldr, "/", "huc3.gpkg"), delete_layer = T)
    st_write(sub_huc12, paste0(dst, "/", mkfldr, "/", "huc12_Valid.gpkg"), delete_layer = T)
    st_write(sub_out, paste0(dst, "/", mkfldr, "/", "outlets_Valid.gpkg"), delete_layer = T)
    st_write(sub_riv, paste0(dst, "/", mkfldr, "/", "hyriv_Valid.gpkg"), delete_layer = T)
    st_write(sub_lic,paste0(dst, "/", mkfldr, "/", "licences.gpkg"), delete_layer = T)
    #writeOGR(sub_out_SP,paste0(dst, "/", mkfldr),"outlet", driver = "ESRI Shapefile",
             #check_exists = T, overwrite_layer = T) # Must be shapefile for Riverdist
    #writeOGR(sub_riv_SP,paste0(dst, "/", mkfldr), "hyriv", driver = "ESRI Shapefile",
             #check_exists = T, overwrite_layer = T) # Must be shapefile for Riverdist
    
    ## Clear memory
    gc(verbose = T, reset = T, full = T)
  }
  