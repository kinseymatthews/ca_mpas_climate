
library(ncdf4)
library(tidyverse)
library(raster)
library(thredds)
library(rerddap)
library(ggplot2)
library(sf)
library(exactextractr)
library(terra)
###############


###############
#getting block data
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
# block data
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()%>% 
  st_transform( crs = st_crs(3310))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  #getting rid of international, offshore,  and midshore blocks that are too deep
  filter(blck_st!="International" | is.na(blck_st) )%>%
  filter(blck_ty!="Offshore" | is.na(blck_st))%>%
  filter(blck_st!="Oregon" | is.na(blck_st) )%>%
  filter(blck_st!="Washington" | is.na(blck_st) )

# Get the CRS from the blocks
blocks_crs <- crs(all_blocks)  # This returns a CRS object
# Convert sf blocks to terra-compatible format
all_blocks_vect <- vect(all_blocks)  # all_blocks is your sf object
###############



###############
sst_bt_time1<-rast("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/raw/cmems_mod_glo_phy_my_0.083deg_P1M-m_1755106570377.nc")
sst_bt_time2<-rast("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/raw/cmems_mod_glo_phy_myint_0.083deg_P1M-m_1755106870727.nc")

# See available variables
#names(sst_bt_time1)       # Layer names
#varnames(sst_bt_time1)    # Variable metadata
#time(sst_bt_time1)        # Time dimension (if multiple months)


# Extract bottomT layers
bt_time1 <- sst_bt_time1[[grep("^bottomT", names(sst_bt_time1))]]
bt_time2 <- sst_bt_time2[[grep("^bottomT", names(sst_bt_time2))]]

n1 <- nlyr(bt_time1)
n2 <- nlyr(bt_time2)

# Rename bt_time2 so numbering continues from bt_time1
names(bt_time2) <- paste0("bottomT_", (n1 + 1):(n1 + n2))
bt <- c(bt_time1, bt_time2)


# Extract thetao (surface temp) layers
sst_time1 <- sst_bt_time1[[grep("^thetao", names(sst_bt_time1))]]
sst_time2 <- sst_bt_time2[[grep("^thetao", names(sst_bt_time2))]]

n1 <- nlyr(sst_time1)
n2 <- nlyr(sst_time2)
names(sst_time2) 

# Rename bt_time2 so numbering continues from bt_time1
names(sst_time2) <- paste0("thetao_depth=0.49402499_", (n1 + 1):(n1 + n2))
sst <- c(sst_time1, sst_time2)

# Reproject SST and Bottom Temp to match blocks
sst <- project(sst, crs(all_blocks))
bt<- project(bt, crs(all_blocks))

# Extract using nearest raster cell instead of mean
sst_filled <- focal(sst, w = matrix(1,3,3), fun = mean, na.rm = TRUE, na.policy="only")
bt_filled <- focal(bt, w = matrix(1,3,3), fun = mean, na.rm = TRUE, na.policy="only")
writeRaster(sst_filled, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_sst_GLORYS_raster.tif")
writeRaster(bt_filled, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_bt_GLORYS_raster.tif")


sst_means <- terra::extract(sst_filled, all_blocks_vect, fun = mean, na.rm = TRUE)
bt_means <- terra::extract(bt_filled, all_blocks_vect, fun = mean, na.rm = TRUE)
time_vals <- terra::time(sst_filled )

# Add block IDs from original data
block_ids <- all_blocks_vect$block_id

# For SST
sst_long <- sst_means %>%
  mutate(block_id = block_ids) %>%
  dplyr::select(-ID) %>%
  pivot_longer(
    cols = starts_with("thetao_depth=0.49402499_"),  # or however your columns are named (e.g. layer.1)
    names_to = "layer",
    values_to = "SST"
  ) %>%
  mutate(
    layer_num = as.integer(gsub("thetao_depth=0.49402499_", "", layer)),
    date = time_vals[layer_num]
  ) %>%
  dplyr::select(block_id, date, SST)

time_vals <- terra::time(bt_filled)
# For BT
bt_long <- bt_means %>%
  mutate(block_id = block_ids) %>%
  dplyr::select(-ID) %>%
  pivot_longer(
    cols = starts_with("bottomT_"),  # or however your columns are named (e.g. layer.1)
    names_to = "layer",
    values_to = "BT"
  ) %>%
  mutate(
    layer_num = as.integer(gsub("bottomT_", "", layer)),
    date = time_vals[layer_num]
  ) %>%
  dplyr::select(block_id, date, BT)

#ad them together
sst_bt <- left_join(bt_long, sst_long, by = c("block_id", "date"))%>%
  filter(!is.na(SST))

saveRDS(sst_bt, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_sst_bt_GLORYS.rds")
###############
