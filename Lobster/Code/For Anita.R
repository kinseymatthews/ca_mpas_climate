
library(tidyverse)
library(sf)



#################
###### read in lobster data
Lobster<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/lobster_logbooks_edited.rds")%>%
  rename(port.complex1=port_complex1, port.complex2=port_complex2)

###### read in block data
all_blocks<-st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

##### read in bathymetry data with blocks
bathy.data<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Raw Data/fishingblockstatistics.csv")%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

##### merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#################




#################
# Data Manipulation

#Want to remove fishing blocks that occur outside the range of the fishery (which goes up to Monterey Bay for a northern range)
#are too deep for the fishery (minimum block depth >300 feet)
#or where the blocks are too large for analyses - international and offshore blocks.

#Figured out that all midshore blocks besides 897 are too deep for the fishery, and 
#blocks 108,135,256,257,265,419,456,457,465, are too north for the fishery

#Data Manipulation
#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)

#lots of incorrect blocks within the logbook data, filter out potential bad blocks before merging the various block id columns and removing block IDs north of monterey bay where fishing shouldnt occur
block.to.remove<-c(108,135,256,257,265,419,456,457,465,0,2,3, 10, 18, 19, 20, 21, 25, 40, 49, 50, 52, 54, 56, 60, 61, 62, 63, 64, 65, 66, 68, 70, 71, 72, 73, 75, 79, 80, 81, 83, 85, 86, 87, 100, 152, 153, 156, 157, 160, 180, 181, 190, 306, 307, 326, 342, 345, 351, 352, 353, 381, 383, 385, 387, 389, 500, 575, 576, 588, 600, 698, 699, 700, 778, 780, 785, 786, 787, 788, 789, 800, 901, 904, 919, 921, 922, 953, 954, 955, 957, 960, 980, 982, 983, 984, 985, 988, 1019, 1028, 3718, 3798, 3860, 3900, 5814, 6001, 6353, 6354, 6355, 6389, 6520, 6532, 6533, 6543, 6654, 6665, 6866, 6885, 6889, 6893, 6894, 6953, 7009, 7102, 7110, 7203, 7230, 7387, 7455, 7520, 7536, 7564, 7569, 7574, 7757, 8136, 8432, 8442, 8460, 8520, 8600, 8605, 8630, 8650, 8660, 8693, 8707, 8760, 8777, 8821, 8822, 8850, 8860, 8922, 8942, 8960)

explor<- Lobster%>%
  mutate(agree = case_when(
    is.na(port.complex1) | is.na(port.complex2) ~ NA,  # Set to NA if either is NA
    port.complex1 == port.complex2 ~ TRUE,                        # Agree
    TRUE ~ FALSE                                # Disagree
  ))%>%
  mutate(port.complex = coalesce(port.complex1, port.complex2))%>%
  left_join(dplyr::select(mpa,port.complex,mpayear),by=("port.complex"))%>%
  rename(block_id_logbook=block_id)%>%
  mutate(block_id = coalesce(block_id_logbook,block_id_gps, block_id_landings1,block_id_landings2))%>% #prefers logbook block id to gps block id
  left_join(all_blocks_df, by="block_id")%>%
  #getting rid of international, offshore,  all midshore besides 897, and all blocks that are too deep
  filter(blck_st!="International" | is.na(blck_st) )%>%
  filter(blck_ty!="Offshore" | is.na(blck_st))%>%
  filter(!(blck_ty == 'Midshore' & block_id != 897))%>%
  filter(Min_Depthm>= -110 | is.na(Min_Depthm))%>%
  #adding column for reported depth in meters
  mutate(Depth_m=depth_ft*(-0.3048))%>%
  filter(!block_id %in% block.to.remove)%>%
  #creating trap nights and filtering out bad data
  mutate(trap_nights=n_nights*n_traps_pulled)%>%
  filter(n_nights<=9)%>%
  filter(n_traps_pulled<=600)%>%
  filter(n_traps_pulled>0)%>%
  filter(n_nights>0)%>%
  #getting rid of duplicated data
  distinct()

saveRDS(explor,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/Lobster_filtered_data_foranita.rds")
