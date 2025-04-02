library(tidyverse)
library(sf)
#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
cucumber_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/'

#upload raw data
cucumber_logbook<-readRDS(paste0(cucumber_data, "Data/Raw Data/1982_2020_cucumber_trawl_logbooks.Rds"))
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))
port_info<-read.csv(paste0(general_data,'Port Codes.csv'))%>%
  rename(port_id=port_code)
port_info$port_id<-as.double(port_info$port_id)

#upload block data to be used later
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(-c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id_gps=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  filter(blck_st!="International")%>%
  st_make_valid()%>%
  dplyr::select(block_id_gps)
#################

#################
#create new columns with vesselID_date for logbook and landings 

cucumber.logbook2 <- cucumber_logbook %>%
  rename(port_id=port_code)%>%
  left_join(dplyr::select(port_info, port_id, port.complex), by="port_id")%>%
  rename(date=date_landing, port_complex_logbook=port.complex)%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  rename(date.logbooks=date)

cucumber.logbook2$month <- month(cucumber.logbook2$date.logbooks)

cucumber.logbook2<-cucumber.logbook2%>%
  mutate(vesselID_year_month=ifelse(is.na(vessel_id)| is.na(year)| is.na(month), NA,paste(vessel_id, year, month, sep="_" )))


landings2<-landings  %>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  rename(date.landings=date)%>%
  left_join(dplyr::select(port_info, port_id, port.complex), by="port_id")%>%
  rename(block_id_landings=block_id)

landings2$month <- month(landings2$date.landings)

landings2<-landings2%>%
  mutate(vesselID_year_month=ifelse(is.na(vessel_id)| is.na(year)| is.na(month), NA,paste(vessel_id, year, month, sep="_" )))%>%
  mutate(vesselID_year=ifelse(is.na(vessel_id)| is.na(year), NA,paste(vessel_id, year, sep="_" )))
#################

#################
#STEP 1
#One, if a vessel was recorded at only one port complex throughout the entire landings dataset, we assigned that port complex to the logbook data
no.ports<-cucumber.logbook2%>%
  filter(is.na(port_complex_logbook))

ports<-cucumber.logbook2%>%
  filter(!is.na(port_complex_logbook))%>%
  mutate(port_complex_landings=NA)

#list of vessel ids that only had one port associated with it
result <- landings2 %>%
  group_by(vessel_id)%>%
  filter(n_distinct(port.complex) == 1) %>%
  summarize( port.complex=first(port.complex))

result$vessel_id<-as.character(result$vessel_id)

#match with the remaining vessel ids with no ports 
no.ports<-no.ports%>%
  left_join(result, by="vessel_id")%>%
  rename(port_complex_landings=port.complex)

cucumber.logbook3<-rbind(no.ports,ports)
#################



#################
#STEP 2
#Second, we used the combination of vessel id and date to match the datasets. For a day in which there were multiple port complexes 
#listed for a vessel within the landings data, we assigned the port complex that appeared most frequently to the corresponding logbook entry.
no.ports<-cucumber.logbook3%>%
  filter(is.na(port_complex_logbook) & is.na(port_complex_landings))

ports<-cucumber.logbook3%>%
filter(!is.na(port_complex_logbook) | !is.na(port_complex_landings))

df <- landings2 %>%
  group_by(vesselID_date) %>%
  filter(n() > 1) %>%
  ungroup()

# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(vesselID_date) %>%
  summarise(same_B_values = n_distinct(port.complex) == 1)

df1<-result%>%
  filter(same_B_values=="FALSE")

# Find common names
common_names <- intersect(df1$vesselID_date, no.ports$vesselID_date)  #17 combos with different ports now with the same vessel_ID and date.

#separate again....
no.dupes<- no.ports[!(no.ports$vesselID_date %in% common_names), ] #these should be able to match directly with ports
dupes<- no.ports[(no.ports$vesselID_date %in% common_names), ] #these have more than one port listed

# can merge the no duplicates with the landings data
landings.tomerge<- landings2%>%
  distinct(vesselID_date, .keep_all = TRUE) #just need the port info

no.dupes<-no.dupes%>%
  dplyr::select( -port_complex_landings)%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,vesselID_date,port_id,port,primary_gear,gear,date.landings,port.complex, block_id_landings), by="vesselID_date", na_matches = "never")%>%
  rename(port_id1=port_id.y,port1=port.y, port_complex_landings=port.complex)

cucumber.logbook4<-bind_rows(no.dupes, ports,dupes)
#################


#################
#STEP 3
#Third, we assigned a port complex from the landings data if a vessel visited the same port 
#complex 90% or more of the time during a specific year and month

no.ports<-cucumber.logbook4%>%
  filter( is.na(port_complex_logbook) & is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-cucumber.logbook4%>%
  filter(!is.na(port_complex_logbook) | !is.na(port_complex_landings))

vessels<-no.ports$vessel_id

df<-landings2%>%
  filter(vessel_id %in% vessels)%>%
  group_by(vessel_id, year, month, port.complex)%>%
  summarize(number=n())%>%
  ungroup()%>%
  group_by(vessel_id,year, month)%>%
  mutate(perc.complex=number/sum(number))


df.tomerge<-df%>%
  filter(perc.complex>=.9)%>%
  dplyr::select(-perc.complex,-number)%>%
  #mutate(year=as.character(year))%>%
  mutate(vessel_id=as.character(vessel_id))


#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year", "month"))%>%
  rename(port_complex_landings=port.complex)

cucumber.logbook5<-rbind(ports,no.ports)
#################

#################
#STEP 5
#Matching entries with port complexes most often seen that day
no.ports<-cucumber.logbook5%>%
  filter( is.na(port_complex_logbook) & is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-cucumber.logbook5%>%
  filter(!is.na(port_complex_logbook) | !is.na(port_complex_landings))

vessel_dates<-na.omit(no.ports$vesselID_date)

landings3<- landings2[(landings2$vesselID_date %in% vessel_dates), ]


df<-landings3%>%
  group_by(vesselID_date, port.complex) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)%>%
  rename(port_complex_landings=port.complex)

#merging with port and gear information
no.ports2<- no.ports%>%
  #attach information from receipts
  left_join(dplyr::select(df,vesselID_date,port_complex_landings), by="vesselID_date", na_matches = "never")

cucumber.logbook6<-rbind(ports,no.ports2)

#################


#################
#STEP 6
#Fifth we added a port complex to the logbook data if a vessel went to the same port complex 90% or more of the time during a specific year.

no.ports<-cucumber.logbook6%>%
  filter( is.na(port_complex_logbook) & is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-cucumber.logbook6%>%
  filter(!is.na(port_complex_logbook) | !is.na(port_complex_landings))

#list of vessel ids and years that only had one port associated with it
result <- landings2 %>%
  group_by(vesselID_year)%>%
  filter(n_distinct(port.complex) == 1) %>%
  summarize(port.complex=first(port.complex))

vessels<-no.ports$vessel_id

df<-landings2%>%
  filter(vessel_id %in% vessels)%>%
  group_by(vessel_id, year, port.complex)%>%
  summarize(number=n())%>%
  ungroup()%>%
  group_by(vessel_id,year)%>%
  mutate(perc.complex=number/sum(number))

df.tomerge<-df%>%
  filter(perc.complex>=.9)%>%
  dplyr::select(-perc.complex,-number)%>%
  #mutate(year=as.character(year))%>%
  mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year"))%>%
  rename(port_complex_landings=port.complex)

cucumber.logbook7<-rbind(ports,no.ports)
#################


#################
#STEP 7
#Six, we added a port complex to the logbook data if a vessel went to a specific port complex in over 90% of the landings data

no.ports<-cucumber.logbook7%>%
  filter( is.na(port_complex_logbook) & is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-cucumber.logbook7%>%
  filter(!is.na(port_complex_logbook) | !is.na(port_complex_landings))

result2 <- landings2 %>%
  group_by(vessel_id, port.complex)%>%
  summarize(count=n())%>%
  ungroup()%>%
  group_by(vessel_id)%>%
  mutate(percentage.port=round((count/sum(count))*100,2))

vessels<-unique(no.ports$vessel_id)

df<- result2 %>%
  filter(vessel_id %in% vessels) %>%
  group_by(vessel_id, port.complex) %>%
  summarize(count.complex = sum(count), .groups = 'drop') %>%
  group_by(vessel_id) %>%
  mutate(percentage.complex = round((count.complex / sum(count.complex)) * 100,2))

vessels.tomerge<-df%>%
  filter(percentage.complex>90)%>%
  dplyr::select(-percentage.complex, -count.complex)

vessels.tomerge$vessel_id<-as.character(vessels.tomerge$vessel_id)

no.ports<-no.ports%>%
  left_join(vessels.tomerge, by=c("vessel_id"))%>%
  rename(port_complex_landings=port.complex)

cucumber.logbook8<-rbind(ports,no.ports)

#################

#################
#add GPS block id
# Filter out rows where coordinates are out of range
gps <- cucumber.logbook8 %>% 
  filter(long_dd_set >= -180 & long_dd_set <= 180, lat_dd_set >= -90 & lat_dd_set <= 90)

no.gps <- cucumber.logbook8 %>% 
  filter(long_dd_set < -180 | long_dd_set > 180 | lat_dd_set < -90 | lat_dd_set > 90 | is.na(lat_dd_set) |
           is.na(long_dd_set))%>%
  mutate(block_id_gps=NA)

# Convert GPS coordinates dataframe to an sf object (assuming df_gps contains lat and long columns)
gps2 <- st_as_sf(gps, coords = c("long_dd_set", "lat_dd_set"), crs = 4326)

# Perform spatial join
gps2_block <- st_join(gps2 , all_blocks)

coords<-st_coordinates(gps2_block)

# Bind the coordinates back to the original data
gps2_block_df <- cbind(gps2_block, coords)%>%
  rename(long_dd_set=X, lat_dd_set=Y)%>%
  st_drop_geometry()

#combine back with the other dataframe
cucumber.logbook9<-rbind(no.gps,gps2_block_df)
#################

#################
cucumber.logbook10<-cucumber.logbook9%>%
  rename(port_id_logbook=port_id.x, port_logbook=port.x)%>%
  mutate(block_id=coalesce(block_id,block_id_landings, block_id_gps))%>%
  mutate(port.complex=coalesce(port_complex_logbook, port_complex_landings))%>%
  mutate(trip.identifier=paste(logbook_id, port.complex, vessel_id, date_depart, date.logbooks, sep="_"))%>%
  mutate(tow.identifier=paste(logbook_id, port.complex, vessel_id, date_tow, block_id, lat_dd_set, long_dd_set, lat_dd_up, long_dd_up, tow_number, depth_fa_set, depth_fa_up, time_set,time_up, duration_min, sep="_"))

#################


#################
saveRDS(cucumber.logbook10, paste0(cucumber_data,"Data/Processed Data/seacucumbertrawlfishery_matched.rds"))
#################