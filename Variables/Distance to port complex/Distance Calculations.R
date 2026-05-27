library(tidyverse)
library(sf)



############
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

all_ports<- st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/all ports.shp")%>%
  rename(port_complex=MajorPort, port=PortName)%>%
  mutate(port = str_to_title(port),port_complex = str_to_title(port_complex))%>%
  select(-PortCode, -MajorPortC)%>%
  st_as_sf()
all_ports<- st_transform(all_ports, crs = 3310)

#upload block information
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  #find middle point of each block
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")%>%
st_transform( crs = st_crs(3310))
############


############
#create distance matrix with all ports and blocks 

#extract names from each dataframe
port_names <- all_ports$port
block_ids <- all_blocks$block_id  

# Create a dataframe of all combinations of ports and blocks
allports_blocks <- expand.grid(port = port_names, block_id = block_ids)

#run distance matrix
distmat <- st_distance(all_ports,all_blocks,by_element = FALSE)

# Flatten the distance matrix and add it as a column
allports_blocks$distance_m <- as.vector(distmat)
############


############
#distance to port complex by average distance of ports within that port complex
distances_portcomplex<-allports_blocks%>%
  #add port complex through left join
left_join(dplyr::select(all_ports,port, port_complex),by=("port"))%>%
  group_by(port_complex, block_id)%>%
  summarize(average.distance.m=mean(distance_m))%>%
  mutate(average.distance.km=average.distance.m/1000)
saveRDS(distances_portcomplex, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")
############
