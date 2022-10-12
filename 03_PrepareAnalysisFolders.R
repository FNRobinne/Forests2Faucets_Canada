## ---------------------------
## Script name: 03_PrepareAnalysisFolders.R
##
## Purpose of script: Extract all necessary data for network analysis using the
##                    HUC5 drainages and store them in individual folders using
##                    HUC5 HYBAS codes
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2021-10-26
## 
## Version: 1.0
##
## Copyright (c) François-Nicolas Robinne, 2021
## Email: francois.robinne@nrcan-rncan.gc.ca
##
## ---------------------------
## Notes:
##    - Make sure to use watershed and river layers updated in scripts 01 and 02
##    - Eventually, this script will need to be upgraded with functions from 
##      sf/stars/terra packages, as rgdal will be retired late 2023
##    - With the next update of sfnetworks, the code can run using HUC4 instead 
##      of HUC5
##    


## set working directory -------------------------------------------------------

  setwd("C:/Users/frobinne/OneDrive - NRCan RNCan/Documents/Professionel/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options -------------------------------------------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
  memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries --------------------------------------------------------------

  library(sf)
  library(dplyr)
  library(stringr)
  "rgdal_show_exportToProj4_warnings"="none"
  library(rgdal)

## Load data -------------------------------------------------------------------

  HYBAS_HUC3 <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_RepairedGeometry_EPSG3979.gpkg")
  HYBAS_HUC3_NoGeom <- st_drop_geometry(HYBAS_HUC3)
  # Make sure to use HUC12 layer from 01_UpdateHydroAtlas
  HYBAS_HUC12 <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC12_RepairedGeometry_Extract_HUC3_WaterSupply_EPSG3979_UpdatedVariables_Valid.gpkg")
  HYBAS_OUT <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_Outlets_HUC12_RepairedGeometry_EPSG3979.gpkg")
  # Make sure to use Hydrorivers layer from 02_SimplifyHydroRiverNetwork
  HYRIV <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroRivers_HUC3_WaterSupply_RepairedGeometry_EPSG3979.gpkg")
  # Make sure to use the water licence layer from 00_WaterIntakeAnalysis
  WATLIC <- st_read("02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V4_28102021.gpkg")
  dst <- paste(getwd(), "/03_ANALYSIS_RESULTS", sep = "")

## Data processing -------------------------------------------------------------

  # Creates a loop to parse national-scale datasets using HUC5 drainages
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
      #sub_out_SP <- as_Spatial(from = sub_out)
    sub_riv <- HYRIV %>%
      filter(lengths(st_intersects(., sub_huc12, sparse = T)) > 0) %>%
      mutate(ID = row_number())
      #sub_riv_SP <- as_Spatial(from = sub_riv)
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
    #st_write(sub_lic,paste0(dst, "/", mkfldr, "/", "licences.gpkg"), delete_layer = T)
    #writeOGR(sub_out_SP,paste0(dst, "/", mkfldr),"outlet", driver = "ESRI Shapefile",
             #check_exists = T, overwrite_layer = T) # Must be shapefile for Riverdist
    #writeOGR(sub_riv_SP,paste0(dst, "/", mkfldr), "hyriv", driver = "ESRI Shapefile",
             #check_exists = T, overwrite_layer = T) # Must be shapefile for Riverdist
    
    ## Clear memory
    gc(verbose = T, reset = T, full = T)
  }
