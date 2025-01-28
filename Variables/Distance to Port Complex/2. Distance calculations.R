library(tidyverse)
library(sf)



############
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#upload port complex data for each fishery - decided against this
#cog_ports<- st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/COG_complex_fishery.shp")%>%
  #add "_COG" to ends of names to represent the COG for fisheries and port complexes without specific ports
 # mutate(port_complex = port_compl) %>%
  #  rename(port=port_compl, long=cog_longit , lat=cog_latitu)%>%
  #mutate(port = paste0(port, "_COG"))%>%
  #mutate(port = str_to_title(port),port_complex = str_to_title(port_complex))%>%
  #mutate(port=ifelse(is.na(port)| is.na(fishery), NA,paste(port, fishery, sep="_" )))%>%
  #st_as_sf()
#cog_ports<- st_transform(cog_ports, crs = 4326)


all_ports<- st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/all ports.shp")%>%
rename(port_complex=MajorPort, port=PortName)%>%
  mutate(port = str_to_title(port),port_complex = str_to_title(port_complex))%>%
  select(-PortCode, -MajorPortC)%>%
  st_as_sf()
all_ports<- st_transform(all_ports, crs = 4326)


# Define the fisheries
fisheries <- c("CPFV","Cucumber dive", "Cucumber trawl", "Drift gillnet", "Halibut trawl", "Lobster trap", "Pink shrimp trawl", "Ridgeback prawn trawl", "Set gillnet", "Spot prawn trawl", "Squid seine","Urchin dive")

# Repeat the dataframe for each fishery
#all_ports_expanded <- expand_grid(all_ports, fishery = fisheries)

#add all ports together
#ports.final<-rbind(all_ports_expanded,cog_ports)
#ports.final <- st_as_sf(ports.final)
  
# Then transform
#ports.final <- st_transform(ports.final, crs = 4326)

#upload block information
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  #find middle point of each block
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")

############


############
#create distance matrix with all ports (not COG) and blocks 

#extract names from each dataframe
port_names <- all_ports$port
block_ids <- all_blocks$block_id  

# Create a dataframe of all combinations of ports and blocks
allports_blocks <- expand.grid(port = port_names, block_id = block_ids)

#run distance matrix
distmat <- st_distance(all_ports,all_blocks,by_element = FALSE)

# Flatten the distance matrix and add it as a column
allports_blocks $distance_m <- as.vector(distmat)

# Define the fisheries
fisheries <- c("CPFV","Cucumber dive", "Cucumber trawl", "Drift gillnet", "Halibut trawl", "Lobster trap", "Pink shrimp trawl", "Ridgeback prawn trawl", "Set gillnet", "Spot prawn trawl", "Squid seine","Urchin dive")

# Repeat the dataframe for each fishery
 allports_blocks_expanded <- expand_grid(allports_blocks, fishery = fisheries)
############


 ############
 #create distance matrix with COG ports and blocks 
 
 #extract names from each dataframe
 port_names <- cog_ports$port
 block_ids <- all_blocks$block_id  

 
 # Create a dataframe of all combinations of ports and blocks
 cogports_blocks <- expand.grid(port = port_names, block_id = block_ids)
 
 #run distance matrix
 distmat <- st_distance(cog_ports,all_blocks,by_element = FALSE)
 
 # Flatten the distance matrix and add it as a column
 cogports_blocks$distance_m <- as.vector(distmat)
 
 #put fishery in seperate column
 cogports_blocks2<- cogports_blocks%>% 
   mutate(fishery = sub(".*_", "", port))%>%
   mutate(port = sub("_[^_]+$", "", port))
  
 ############

 
 ############
 #add both togther
 
port_block_distances_complete <-rbind( allports_blocks_expanded , cogports_blocks2)

saveRDS(port_block_distances_complete , "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")
 
 
 ############