## ---------------------------
## Script name: AccumulateUpstreamSupply.R
##
## Purpose of script: Connect downstream water demand to upstream water availability.
##                    It accumulates water demand from multiple downstream catchments.
##                    See paper for more details
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2022-01-10
##
## Copyright (c) François-Nicolas Robinne, 2022
## Email: francois.robinne@nrcan-rncan.gc.ca
##
## ---------------------------
## Notes:
##   Make sure than SnapCatchmentCentroids.R was run before
##    This is part of the project Forest To Faucets - Canada
## ---------------------------

## set working directory ---------------------------

  # Update as needed
  setwd("D:/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/02_PROCESSED_DATA")

## general options ---------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
  memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries ---------------------------

  library(riverdist)
  library(sf)
  library(dplyr, warn.conflicts = F)
  library(ggplot2)

# source("functions/packages.R")       # loads up all the packages we need

## load functions ---------------------------

# source("functions/summarise_data.R") 

## load data ---------------------------
  

## Data processing ---------------------------------------------------------

#   I'm keeping this code in case of, but the use of Pfafstetter hierarchy is more promising.
#   # Riverdist sandbox
#   # Import river network
#   HR <- line2network(path=".", layer = "HydroRiver_Fraser_RD")
#   # Densify the river network to avoid long snaps in lake segments
#   #HR_dense <- addverts(HR, mindist = 500)
#   # plot(x=HR, segmentnum=F)
#   #topologydots(rivers=HR)
#   
#   
#   # Import 
#   segvert_Pop <- pointshp2segvert(path=".",layer="Centroids_Fraser_RD", rivers=HR_dense)
#   #hist(segvert_Pop$snapdist, main = "snapping distance (m)")
#   
#   # Distance computation
#   # First, let's find the end segment (i.e., furthest downstream)
#   # At the moment, that's manually iterative (i.e., change seg values with in functions below)
#   # Segment 1 will not necessarily always be the outlet segment
#   showends(seg=1, rivers =HR_dense) # Here, segment 1 is the end one
#   zoomtoseg(seg=c(1,4), rivers=HR_dense)
#   # Second, set mouth of the network
#   HR1 <- setmouth(seg=1, vert=1, rivers = HR)
#   HR1 <- buildsegroutes(HR1)
#   dmat <- upstreammat(seg=segvert_Pop$seg, vert=segvert_Pop$vert, 
#                       rivers=HR1, ID = segvert_Pop$HYBAS_ID, 
#                       flowconnected = TRUE, net = TRUE)
#   # Third, get the matrix ready for inverse distance weighting computation
#   dmat_pos <- replace(dmat, dmat < 0, NA) # negative values are not needed
#   dmat_km <- dmat_pos/1000 # convert to km distances
#   dmat_decay <- 0.99^(dmat_km) # Applying inverse distance weighting (decay function)
#   dmat_tibble <- as_tibble(dmat_decay, rownames = "id") # Converts matrix to tibble and add HYBAS_ID as column
#   dmat_tibble <- rename(dmat_tibble, HYBAS_ID = id) # Rename id field to HYBAS_ID for join
#   # Fourth, prepare water volume data
#   wat_vol <- segvert_Pop %>%
#     select(HYBAS_ID, QUANTITY_s)
#   # Fifth, compute SWiVi (Equivalent to SWiPi from F2F but with water volumes)
#   SWiVi <- right_join(wat_vol, dmat_tibble) %>%
#     mutate(across(2:62, ~ .x * QUANTITY_s, na.rm = T), digits = 5) %>% # Number of columns for across must become dynamic
#     summarise(across(2:62, ~ sum(.x, na.rm = T))) # Number of columns for across must become dynamic
#   
#   SWiVi_tran <- t(SWiVi[2:61])
#   write.csv(SWiVi_tran, "SWiVi.csv", row.names = T, col.names = c("SWiVi"))
# 
# # Attach SWiVi results to catchments
#   HUC <- st_read("Subset_BA_RiverdistDemo.shp") %>% # HUC Catchments
#     mutate(HYBAS_TID = as.character(HYBAS_ID))
#   
#   HUC_SWiVi <- HUC %>%
#     right_join(SWiVi)
#   
#   st_write(HUC_SWiVi, "C:/Users/frobinne/Documents/Professionel/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/03_ANALYSIS/CODE/RIVERDIST/TEST_DATA/SWiVi.shp")
# 
#   
  
  