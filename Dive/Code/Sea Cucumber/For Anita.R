#libraries

library(tidyverse)
library(sf)


############
###### read in Dive data
cuc<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seacucumberfishery_matched.rds")
urc<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/redseaurchinfishery_matched.rds")
###### read in block data
all_blocks<-st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

##### read in bathymetry data with blocks
bathy.data<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Raw Data/fishingblockstatistics.csv")%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

##### merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#port codes
port<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Raw Data/Port Codes.csv")
############ 


############ 
explor<-cuc%>%
  #add spatial block data
  left_join(all_blocks_df, by=("block_id"))%>%
  #getting rid of international, offshore,  all midshore besides 897
  filter(blck_st!="International" | is.na(blck_st))%>%
  filter(blck_ty!="Offshore" | is.na(blck_ty))%>%
  filter(!(blck_ty == 'Midshore' & block_id != 897))%>%
  #recorded depth
  mutate(Min_depth_m_recorded=depth_min_ft*(-0.3048))%>%
  mutate(Max_depth_m_recorded=depth_max_ft*(-0.3048))%>%
  mutate(Med_depth_m_recorded=(Min_depth_m_recorded+Max_depth_m_recorded)/2)%>%
  #getting rid of blocks too deep
  filter(Min_Depthm>=-50)%>% #using the block information instead of the recorded depth in case there was a mistake in the recorded depth
  #rename port complex so we can reuse code
  rename(port.complex=port_most_reliable)%>%
  #getting rid of bad or unrealistic dive hours
  filter(hours<10 &  hours>0.10)%>%
  filter(!is.na(block_id))
############ 

############ 
explor2<-urc%>%
  #add spatial block data
  left_join(all_blocks_df, by=("block_id"))%>%
  #getting rid of international, offshore,  all midshore besides 897
  filter(blck_st!="International" | is.na(blck_st))%>%
  filter(blck_ty!="Offshore" | is.na(blck_ty))%>%
  filter(!(blck_ty == 'Midshore' & block_id != 897))%>% #only midshore block within the data is bloick 897 anyways
  #recorded depth
  mutate(Min_depth_m_recorded=depth_min_ft*(-0.3048))%>%
  mutate(Max_depth_m_recorded=depth_max_ft*(-0.3048))%>%
  mutate(Med_depth_m_recorded=(Min_depth_m_recorded+Max_depth_m_recorded)/2)%>%
  #getting rid of blocks too deep
  filter(Min_Depthm>=-50)%>% #using the block information instead of the recorded depth in case there was a mistake in the recorded depth
  #rename port complex so we can reuse code
  rename(port.complex=port_most_reliable)%>%
  #getting rid of bad or unrealistic dive hours
  filter(hours<10 &  hours>0.10)%>%
  filter(!is.na(block_id))
############ 

############ 
#combining into one and then just keeping warty sea cucumber and red sea urchin
foranita<-rbind(explor,explor2)%>%
  filter(comm_name=="Warty sea cucumber" | comm_name=="Red sea urchin")%>%
  dplyr::select(-matching_logbook_complexes)

saveRDS(foranita,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/Cucumber_urchin_filtered_data_foranita.rds")

cucumber<-foranita%>%
  filter(comm_name=="Warty sea cucumber" )

saveRDS(cucumber,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/Cucumber_filtered_data_foranita.rds")

urchin<-foranita%>%
  filter(comm_name=="Red sea urchin" )

saveRDS(urchin,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/Urchin_filtered_data_foranita.rds")

############ 