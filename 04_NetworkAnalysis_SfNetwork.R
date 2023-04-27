## ---------------------------
## Script name: 04_NetworkAnalysis_sfnetworks.R
##
## Purpose of script: Creates a connectivity index of upstream importance for each HUC12 catchment
##                    within a HUC3 drainage, primarily using upstream distance,
##                    then weighted by water demand, for current and future water demand.
##
## Author: Dr. Francois-Nicolas Robinne
## 
## Version: 1.0
##
## Date Created: 2022-09-08
##
## Copyright (c) Francois-Nicolas Robinne, 2022
## Email: francois.robinne@nrcan-rncan.gc.ca or robinne@ualberta.ca
##
## ---------------------------
##
## Notes: Based principally on the creation of an origin-destination matrix
##        It uses the HydroRIVERS network. Sfnetworks uses the shortest path.
##   

## Set working directory -------------------------------------------------------
  # setwd("C:/Users/frobinne/Documents/Professionel/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/03_ANALYSIS_RESULTS")
  
  # List the directories within which the network analysis will be applied
  dirs <- list.dirs("C:/Users/frobinne/Documents/Professional/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/03_ANALYSIS_RESULTS/Canada_Forest2Faucets", 
                    recursive = T)
  dirs <- dirs[-1] # Removes the first entry, which is the root of the folder

## Load libraries --------------------------------------------------------------
  library(sfnetworks)
  library(sf)
    sf::sf_use_s2(FALSE)
  library(tidygraph)
  library(tidyverse)
  library(igraph)
  library(dbscan)
  library(units)
  library(tidyr)

## Network analysis loop -------------------------------------------------------

for(i in dirs) {
  setwd(i)

  ## Load data -------------------------------------------------------------------
  hyriv <- st_read("hyriv_Valid.gpkg", as_tibble = T, fid_column_name = "IDS")
  # intakes <- st_read("licences.gpkg")
  outlets <- st_read("outlets_Valid.gpkg")
  hybas <- st_read("huc12_Valid.gpkg")

  ## Data processing -------------------------------------------------------------
  # Reformat HUC12 outlets
  dir_outlets <- st_join(outlets, hybas)%>%
    st_cast(to = "POINT") %>%
    select(HYBAS_ID.x, NEXT_DOWN) %>%
    rename(from = HYBAS_ID.x, to = NEXT_DOWN) %>%
    mutate(name = as.character(from)) 
  # Reformat HydroRIVERS
  hyriv_line <- st_cast(hyriv, to = "LINESTRING")
  # Create a network based on HydroRIVERS
  net <- as_sfnetwork(hyriv_line, directed = T)
  # Update river network with outlets
  new_net <- net %>% # Original river network
    activate("nodes") %>%
    st_network_blend(dir_outlets, tolerance = Inf) %>% # Blend in catchment outlets
    convert(to_spatial_subdivision) %>%
    activate("edges") %>%
    mutate(weight = edge_length()) # Compute distance of each river reach
  # Create origin-destination (OD) matrix (distance-based, here)
  POI_outlets <- st_as_sf(new_net, "nodes") %>%
    filter(is.na(name) == F) # Recreate the outlet layer based on their new position after blending (POIs)
  od_mat <- st_network_cost(new_net, 
                                 from = POI_outlets, to = POI_outlets, # from-to point sf
                                 direction = "in", # inbound 
                                 Inf_as_NaN = T)
  
  od_mat_noUnit <- drop_units(od_mat) # drop [m] unit to simple numerical numbers
  od_mat_km <- od_mat_noUnit/1000 # convert to kilometers
  od_mat_decay <- 0.99^(od_mat_km)# apply inverse distance weighting (decay function)
  od_mat_tibble <- tibble::as_tibble(od_mat_decay, rownames = "id", .name_repair = "minimal") # converts matrix to tibble and add HYBAS_ID as column
  od_mat_tibble <- rename(od_mat_tibble, HYBAS_ID = id) # Rename id field to HYBAS_ID for join
  
  # Compute SWiVi (Equivalent to SWiPi from F2F but with water volumes)
  # Current and future water volumes
  wat_vol <- hybas %>%
    select(HYBAS_ID, mwd_m3_syr) %>%
    mutate(HYBAS_ID = as.character(HYBAS_ID))
  
  # SWiVi for current water demand
  # Column-wise aggregation (downstream looking upstream)
  SWiVi <- inner_join(od_mat_tibble, wat_vol) %>%
    relocate(mwd_m3_syr, .after = HYBAS_ID) %>%
    select(-geom) %>%
    mutate(across(3:ncol(.), ~ .x * mwd_m3_syr)) %>%
    summarise(across(3:ncol(.), ~ sum(.x, na.rm = T))) 
  
  SWiVi_tran <- gather(SWiVi, key = "HYBAS_ID", value = "SWiVi") %>% # Transpose for long format to be joined to HUC12 layer
    mutate(HYBAS_ID = as.numeric(HYBAS_ID))
  
  # SWiVi for future low water demand
  # Column-wise aggregation (downstream looking upstream)
  wat_vol_l60 <- hybas %>%
    select(HYBAS_ID, mwd_m3_syr_lg60) %>%
    mutate(HYBAS_ID = as.character(HYBAS_ID))
  
  SWiVi_l60 <- inner_join(od_mat_tibble, wat_vol_l60) %>%
    relocate(mwd_m3_syr_lg60, .after = HYBAS_ID) %>%
    select(-geom) %>%
    mutate(across(3:ncol(.), ~ .x * mwd_m3_syr_lg60)) %>%
    summarise(across(3:ncol(.), ~ sum(.x, na.rm = T))) 
  
  SWiVi_tran_l60 <- gather(SWiVi_l60, key = "HYBAS_ID", value = "SWiVi_l60") %>% # Transpose for long format to be joined to HUC12 layer
    mutate(HYBAS_ID = as.numeric(HYBAS_ID))
  
  # SWiVi for future high water demand
  # Column-wise aggregation (downstream looking upstream)
  wat_vol_h60 <- hybas %>%
    select(HYBAS_ID, mwd_m3_syr_hg60) %>%
    mutate(HYBAS_ID = as.character(HYBAS_ID))
  
  SWiVi_h60 <- inner_join(od_mat_tibble, wat_vol_h60) %>%
    relocate(mwd_m3_syr_hg60, .after = HYBAS_ID) %>%
    select(-geom) %>%
    mutate(across(3:ncol(.), ~ .x * mwd_m3_syr_hg60)) %>%
    summarise(across(3:ncol(.), ~ sum(.x, na.rm = T))) 
  
  SWiVi_tran_h60 <- gather(SWiVi_h60, key = "HYBAS_ID", value = "SWiVi_h60") %>% # Transpose for long format to be joined to HUC12 layer
    mutate(HYBAS_ID = as.numeric(HYBAS_ID))

  # Create spatial layers
  decay_spatial <- inner_join(hybas, SWiVi_tran) %>%
    inner_join(., SWiVi_tran_l60) %>%
    inner_join(., SWiVi_tran_h60)
  
  ## Save outputs -------------------------------------------------------------
  st_write(decay_spatial, "huc12_Valid_SWiVi.gpkg", delete_layer = T)
  
  ## Clear memory
  gc(verbose = T, reset = T, full = T)
}