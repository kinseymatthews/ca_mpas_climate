
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
#Getting SST Data
# Read in NetCDF file
sst <- rast("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/raw/SST_subset.nc")
bt1 <- rast("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/raw/bottomtemp_2000_2021.nc")
bt2 <- rast("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/raw/bottomtemp_2021_2025.nc")
#combine two bottom temperature netcdfs

# Rename layers in bt2 so they continue from bt1
names(bt2) <- paste0("bottomT_", (nlyr(bt1) + 1):(nlyr(bt1) + nlyr(bt2)))
bt <- c(bt1, bt2)

#set CRS
# Reproject SST and Bottom Temp to match blocks
sst <- project(sst, crs(all_blocks))
bt<- project(bt, crs(all_blocks))

###############



###############
#checking
# First layer (Jan 2000)
sst_jan2000 <- sst[[1]]

# Extract mean SST by block
vals <- extract(sst_jan2000, all_blocks_vect, fun = mean, na.rm = TRUE)

# Combine with block ID
result <- data.frame(
  block_id = all_blocks$block_id,
  date = as.Date("2000-01-01"),
  SST = vals[,2]  # Second column is the extracted mean
)

head(result)
###############

###############
#all now
sst_means <- terra::extract(sst, all_blocks_vect, fun = mean, na.rm = TRUE)
bt_means <- terra::extract(bt, all_blocks_vect, fun = mean, na.rm = TRUE)
time_vals <- terra::time(sst)

# Add block IDs from original data
block_ids <- all_blocks_vect$block_id

# For SST
sst_long <- sst_means %>%
  mutate(block_id = block_ids) %>%
  dplyr::select(-ID) %>%
  pivot_longer(
    cols = starts_with("sst_"),  # or however your columns are named (e.g. layer.1)
    names_to = "layer",
    values_to = "SST"
  ) %>%
  mutate(
    layer_num = as.integer(gsub("sst_", "", layer)),
    date = time_vals[layer_num]
  ) %>%
  dplyr::select(block_id, date, SST)

time_vals <- terra::time(bt)
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
sst_bt <- left_join(bt_long, sst_long, by = c("block_id", "date"))

saveRDS(sst_bt, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_sst_bt.rds")
###############



