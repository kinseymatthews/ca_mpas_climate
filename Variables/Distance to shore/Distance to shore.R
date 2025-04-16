library(sf)
library(sp)
library(tidyverse)

############
#upload block data, transofrm to crs 3310
all_blocks<-st_read(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  #find middle point of each block
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")%>%
  st_transform( crs = st_crs(3310))
#upload shoreline data, transform to crs 3310

mainland<-st_read(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to shore/data/raw data/shoreline.shp"))%>%
  st_transform(crs = 3310)

############



############
#calculate distance of each block to shore
dist_matrix <- st_distance(all_blocks, mainland)

# Get the minimum distance from each block to the shoreline
min_distances <- apply(dist_matrix, 1, min)
# Create a dataframe with block number and distance
block_distances <- all_blocks%>%
  st_drop_geometry() %>%
  mutate(distance_to_shore_km = as.numeric(min_distances) / 1000)  # Convert from meters to km if CRS is in meters

saveRDS(block_distances, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to shore/data/processed/block_shore_distances_complete.rds")
############
