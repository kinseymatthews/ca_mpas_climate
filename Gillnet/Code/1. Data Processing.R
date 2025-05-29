library(tidyverse)
library(sf)
#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
gillnet_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/'

#upload raw data
gillnet_logbook<-readRDS(paste0(gillnet_data, "Data/Raw Data/gillnet_logbooks_2023/processed/CDFW_1981_2020_gillnet_logbook_data.Rds"))
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))
port_info<-read.csv(paste0(general_data,'Port Codes.csv'))%>%
  rename(port_id=port_code)
port_info$port_id<-as.double(port_info$port_id)

#upload block data to be used later
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id_gps=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  filter(blck_st!="International")%>%
  st_make_valid()%>%
  select(block_id_gps)
#################

#################
#create new columns with vesselID_date for logbook and landings 

gillnet.logbook2 <- gillnet_logbook %>%
  rename(vessel_id_donotuse=vessel_id,vessel_id=vessel_id_use)%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  rename(date.logbooks=date)

gillnet.logbook2$month <- month(gillnet.logbook2$date.logbooks)

gillnet.logbook2<-gillnet.logbook2%>%
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

#list of vessel ids that only had one port associated with it
result <- landings2 %>%
  group_by(vessel_id)%>%
  filter(n_distinct(port.complex) == 1) %>%
  summarize( port.complex=first(port.complex))

result$vessel_id<-as.character(result$vessel_id)

#match with the remaining vessel ids with no ports 
gillnet.logbook3<-gillnet.logbook2%>%
  left_join(result, by="vessel_id")%>%
  rename(port_complex_landings=port.complex)

#################

#################
#STEP 2
#Second, we used the combination of vessel id and date to match the datasets. 
no.ports<-gillnet.logbook3%>%
  filter(is.na(port_complex_landings))

ports<-gillnet.logbook3%>%
  filter(!is.na(port_complex_landings) )

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
common_names <- intersect(df1$vesselID_date, no.ports$vesselID_date)  #354 combos with different ports now with the same vessel_ID and date.

#separate again....
no.dupes<- no.ports[!(no.ports$vesselID_date %in% common_names), ] #these should be able to match directly with ports
dupes<- no.ports[(no.ports$vesselID_date %in% common_names), ] #these have more than one port listed

# can merge the no duplicates with the landings data
landings.tomerge<- landings2%>%
  distinct(vesselID_date, .keep_all = TRUE) #just need the port info

no.dupes<-no.dupes%>%
  dplyr::select( -port_complex_landings)%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,vesselID_date,port_id,port,primary_gear,gear,date.landings,port.complex, block_id_landings, price_usd,landings_lbs,value_usd, receipt_id), by="vesselID_date", na_matches = "never")%>%
  rename( port_complex_landings=port.complex)

gillnet.logbook4<-bind_rows(no.dupes, ports,dupes)
#################

#################
#STEP 3
#Third, we assigned a port complex from the landings data if a vessel visited the same port 
#complex 90% or more of the time during a specific year and month

no.ports<-gillnet.logbook4%>%
  filter(is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-gillnet.logbook4%>%
  filter( !is.na(port_complex_landings))

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

gillnet.logbook5<-rbind(ports,no.ports)
#################

#################
#STEP 4
#Matching entries with port complexes most often seen that day
no.ports<-gillnet.logbook5%>%
  filter(is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-gillnet.logbook5%>%
  filter( !is.na(port_complex_landings))

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

gillnet.logbook6<-rbind(ports,no.ports2)
#################

#################
#STEP 6
#Fifth we added a port complex to the logbook data if a vessel went to the same port complex 90% or more of the time during a specific year.

no.ports<-gillnet.logbook6%>%
  filter(is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-gillnet.logbook6%>%
  filter(!is.na(port_complex_landings))

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

gillnet.logbook7<-rbind(ports,no.ports)
#################


#################
#STEP 7
#Six, we added a port complex to the logbook data if a vessel went to a specific port complex in over 90% of the landings data

no.ports<-gillnet.logbook7%>%
  filter( is.na(port_complex_landings))%>%
  dplyr::select( -port_complex_landings)

ports<-gillnet.logbook7%>%
  filter( !is.na(port_complex_landings))

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

gillnet.logbook8<-rbind(ports,no.ports)
#################

#################
gillnet.logbook9<-gillnet.logbook8%>%
  mutate(block_id=as.numeric(block_id))%>%
  mutate(block_id=coalesce(block_id,block_id_landings))%>%
  mutate(trip.identifier=paste(logbook_id, vessel_id,date.logbooks, sep="_"))%>%
  mutate(set.identifier=paste(logbook_id, vessel_id, date.logbooks, block_id, net_type,  depth_fa_num, net_length_fa_num, mesh_size_in, buoy_line_depth_ft, soak_hr,sep="_"))
#################

#################
saveRDS(gillnet.logbook9, paste0(gillnet_data,"Data/Processed Data/gillnetfishery_matched.rds"))
gillnet.logbook9<-readRDS(paste0(gillnet_data,"Data/Processed Data/gillnetfishery_matched.rds"))

#################

set<-gillnet.logbook9%>%
  filter(net_type=="Set")

saveRDS(set, paste0(gillnet_data,"Data/Processed Data/setgillnetfishery_matched.rds"))

drift<-gillnet.logbook9%>%
  filter(net_type=="Drift")

saveRDS(drift, paste0(gillnet_data,"Data/Processed Data/driftgillnetfishery_matched.rds"))

