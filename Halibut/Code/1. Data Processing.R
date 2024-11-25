library(tidyverse)
library(sf)
#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
halibut_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/'

#upload raw data
halibut_logbook<-readRDS(paste0(halibut_data, "Data/Raw Data/halibut_trawl_logbooks_2024/processed/CDFW_1981_2022_halibut_trawl_data.Rds"))
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
#create new columns with vesselID_date for logbook and landings and for receipts. up to two receipt ids.
halibut_logbook2<- separate(halibut_logbook, receipt_ids, into = c("receipt_id1", "receipt_id2"), sep = ",")
halibut_logbook2$receipt_id2 <- trimws(halibut_logbook2$receipt_id2)

halibut_logbook2<- separate(halibut_logbook2, receipt_dates, into = c("receipt_date1", "receipt_date2"), sep = ",")
halibut_logbook2$receipt_date2 <- trimws(halibut_logbook2$receipt_date2)


halibut_logbook2 <- halibut_logbook2 %>%
  mutate(receipt_vesselID_1 = ifelse(is.na(receipt_id1) | is.na(vessel_id), NA, paste(receipt_id1, vessel_id, sep = "_"))) %>%
  mutate(receipt_vesselID_2 = ifelse(is.na(receipt_id2) | is.na(vessel_id), NA, paste(receipt_id2, vessel_id, sep = "_")))%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date_return), NA, paste(vessel_id, date_return, sep = "_")))%>%
  mutate(date_return=as.Date(date_return, format = "%Y-%m-%d"))%>%
  mutate(receipt_date1=as.Date(receipt_date1, format = "%Y-%m-%d"))%>%
  mutate(receipt_date2=as.Date(receipt_date2, format = "%Y-%m-%d"))%>%
  rename(date.logbooks=date_return)

halibut_logbook2$month <- month(halibut_logbook2$date.logbooks)
halibut_logbook2$year <- year(halibut_logbook2$date.logbooks)

halibut_logbook2<-halibut_logbook2%>%
  mutate(vesselID_year_month=ifelse(is.na(vessel_id)| is.na(year)| is.na(month), NA,paste(vessel_id, year, month, sep="_" )))


landings2<-landings  %>%
  mutate(receipt_vesselID_date = ifelse(is.na(receipt_id) | is.na(vessel_id) |is.na(date), NA, paste(receipt_id, vessel_id,date, sep = "_"))) %>%
  mutate(receipt_vesselID = ifelse(is.na(receipt_id) | is.na(vessel_id), NA, paste(receipt_id, vessel_id, sep = "_")))%>%
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
#halibut logbooks with landing receipts. Will see if we can properly match those with the landings data
halibut.receipt<-halibut_logbook2%>%
  filter(!is.na(receipt_id1))

halibut.no.receipt<-halibut_logbook2%>%
  filter(is.na(receipt_id1))

# Check for duplicated values in 'ID' column
duplicated_ids <- duplicated(landings2$receipt_vesselID)
# Show rows with duplicated values
df<-print(landings2[duplicated_ids, ])
# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(receipt_vesselID) %>%
  summarise(same_B_values = n_distinct(port_id) == 1)

result%>%
  filter(same_B_values=="FALSE") #1 with different ports - none of which are in the halibut logbook data 


landings.tomerge<- landings2%>%
  distinct(receipt_vesselID, .keep_all = TRUE)%>%
  rename(receipt_ids=receipt_id)

#match with first landing receipt available
matched_data<-halibut.receipt%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,receipt_vesselID,port_id,port,primary_gear,gear,date.landings,port.complex, block_id_landings), by=c("receipt_vesselID_1" = "receipt_vesselID"), na_matches = "never")%>%
  filter(((date.landings - receipt_date1) <= 7 ) & (date.landings - receipt_date1) >= 0 )

# Identify unmatched entries
unmatched_entries <- halibut.receipt %>%
  anti_join(matched_data, by = c("receipt_vesselID_1", "receipt_date1"))


# Combine matched entries with unmatched entries (without matched data)
halibut.receipt2 <- bind_rows(
  matched_data,
  unmatched_entries 
)

#match with second landing receipt available
matched_data<-halibut.receipt2%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,receipt_vesselID,port_id,port,primary_gear,gear,date.landings, port.complex, block_id_landings), by=c("receipt_vesselID_2" = "receipt_vesselID"), na_matches = "never")%>%
  filter(((date.landings.y - receipt_date2) <= 7 ) & (date.landings.y - receipt_date2) >= 0 )

# Identify unmatched entries
unmatched_entries <- halibut.receipt %>%
  anti_join(matched_data, by = c("receipt_vesselID_2", "receipt_date2"))

# Combine matched entries with unmatched entries (without matched data)

halibut.receipt2 <- bind_rows(
  matched_data,
  unmatched_entries 
)


halibut.receipt2<-halibut.receipt2%>%
  rename(port_complex1=port.complex.x, port_id1=port_id.x, port_id2=port_id.y,gear1=gear.x, port_complex2=port.complex.y,  gear2=gear.y,primary_gear1=primary_gear.x, primary_gear2=primary_gear.y, date_landings1=date.landings.x, date_landings2=date.landings.y, port2=port.y, port1=port.x, block_id_landings1=block_id_landings.x,block_id_landings2=block_id_landings.y)


halibut.no.receipt<-halibut.no.receipt%>%
  mutate(port_complex1=NA,date_landings1=NA, port_id1=NA, port1=NA, primary_gear1=NA, gear1=NA,date_landings2=NA, port_id2=NA, port2=NA, primary_gear2=NA, gear2=NA, port_complex2=NA, block_id_landings1=NA,block_id_landings2=NA)

halibut.logbook3<-rbind(halibut.no.receipt,halibut.receipt2)
#################



#################
#STEP 1.5
#matching receipts that only has one port complex regardless of date
no.ports<-halibut.logbook3%>%
  filter(is.na(port_id1) & is.na(port_id2))

ports<-halibut.logbook3%>%
  filter(!is.na(port_id1) | !is.na(port_id2))


#landings data with more more than one port complex
df <- landings2%>%
  group_by(receipt_id) %>%
  filter(n() > 1) %>%
  ungroup()


# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(receipt_id) %>%
  summarise(same_B_values = n_distinct(port.complex) == 1)

df1<-result%>%
  filter(same_B_values=="FALSE") 
# Find common names
common_names <- intersect(df1$receipt_id, no.ports$receipt_id1) #4 for receipt 1, 0 for receipt 2

no.ports.1<- no.ports[!(no.ports$receipt_id1 %in% common_names), ] #these should be able to match directly with ports
no.ports.2<- no.ports[(no.ports$receipt_id1 %in% common_names), ] #these have more than one port listed

# can merge the no duplicates with the landings data
landings2.tomerge<- landings2%>%
  distinct(receipt_id, .keep_all = TRUE)

#merging with port and gear information
no.ports.1.match<- no.ports.1%>%
  dplyr::select(-port_complex1,-date_landings1, -port_id1, -port1, -primary_gear1, -gear1, -block_id_landings1)%>%
  #attach information from receipts
  left_join(dplyr::select(landings2.tomerge,receipt_id,port_id,port,primary_gear,gear,date.landings, port.complex, block_id_landings), by=c("receipt_id1" = "receipt_id"), na_matches = "never")%>%
  rename(port_complex1=port.complex, port1=port,port_id1=port_id, date_landings1=date.landings,primary_gear1=primary_gear,gear1=gear, block_id_landings1=block_id_landings)

#merge with no.ports2. 
no.ports<-rbind(no.ports.1.match, no.ports.2)

#remove second receipt id information and then can add based on 2nd receipt id
no.ports<-no.ports%>%
  dplyr::select(-port_complex2, -port2, -port_id2, -date_landings2,-primary_gear2,-gear2,-block_id_landings2)%>%
  #attach information from receipts
  left_join(dplyr::select(landings2.tomerge,receipt_id,port_id,port,primary_gear,gear,date.landings, port.complex, block_id_landings), by=c("receipt_id2" = "receipt_id"), na_matches = "never")%>%
  rename(port_complex2=port.complex, port2=port,port_id2=port_id, date_landings2=date.landings,primary_gear2=primary_gear,gear2=gear, block_id_landings2=block_id_landings)


#add with other dataframe
halibut.logbook4<-rbind(no.ports,ports)
#################

#################
#STEP 2
#Second, if a vessel was recorded at only one port complex throughout the entire landings dataset, we assigned that port complex to the logbook data
no.ports<-halibut.logbook4%>%
  filter(is.na(port_complex1)&is.na(port_complex2))

ports<-halibut.logbook4%>%
  filter(!is.na(port_complex1) | !is.na(port_complex2))

#list of vessel ids that only had one port associated with it
result <- landings2 %>%
  group_by(vessel_id)%>%
  filter(n_distinct(port.complex) == 1) %>%
  summarize( port.complex=first(port.complex))

#match with the remaining vessel ids with no ports 
no.ports<-no.ports%>%
  select(-port_complex1)%>%
  left_join(result, by="vessel_id")%>%
  rename(port_complex1=port.complex)

halibut.logbook5<-rbind(no.ports,ports)
#################


#################
#STEP 3
#Third, we used the combination of vessel id and date to match the datasets. For a day in which there were multiple port complexes 
#listed for a vessel within the landings data, we assigned the port complex that appeared most frequently to the corresponding logbook entry.
no.ports<-halibut.logbook5%>%
  filter( is.na(port_complex1) & is.na(port_complex2))

ports<-halibut.logbook5%>%
  filter(!is.na(port_complex1) | !is.na(port_complex2))

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
common_names <- intersect(df1$vesselID_date, no.ports$vesselID_date)  #77 combos with different ports now with the same vessel_ID and date.

#separate again....
no.dupes<- no.ports[!(no.ports$vesselID_date %in% common_names), ] #these should be able to match directly with ports
dupes<- no.ports[(no.ports$vesselID_date %in% common_names), ] #these have more than one port listed

# can merge the no duplicates with the landings data
landings.tomerge<- landings2%>%
  distinct(vesselID_date, .keep_all = TRUE) #just need the port info

no.dupes<-no.dupes%>%
  dplyr::select(-port_id1,-port1,-primary_gear1,-gear1,-date_landings1, -port_complex1,-block_id_landings1)%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,vesselID_date,port_id,port,primary_gear,gear,date.landings,port.complex, block_id_landings), by="vesselID_date", na_matches = "never")%>%
  rename(port_complex1=port.complex, port1=port,port_id1=port_id, date_landings1=date.landings,primary_gear1=primary_gear,gear1=gear, block_id_landings1=block_id_landings)

halibut.logbook6<-rbind(ports, no.dupes,dupes)
#################

#################
#STEP 4
#Fourth, we assigned a port complex from the landings data if a vessel visited the same port 
#complex 90% or more of the time during a specific year and month

no.ports<-halibut.logbook6%>%
  filter( is.na(port_complex1) & is.na(port_complex2))%>%
  dplyr::select( -port_complex1)

ports<-halibut.logbook6%>%
  filter(!is.na(port_complex1) | !is.na(port_complex2))

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
  dplyr::select(-perc.complex,-number)
  #mutate(year=as.character(year))%>%
  #mutate(vessel_id=as.character(vessel_id))


#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year", "month"))%>%
  rename(port_complex1=port.complex)

halibut.logbook7<-rbind(ports,no.ports)
#################

#################
#STEP 3.5
#Matching entries with port complexes most often seen that day

no.ports<-halibut.logbook7%>%
  filter( is.na(port_complex1) & is.na(port_complex2))%>%
  dplyr::select( -port_complex1)

ports<-halibut.logbook7%>%
  filter(!is.na(port_complex1) | !is.na(port_complex2))

vessel_dates<-no.ports$vesselID_date

landings3<- landings2[(landings2$vesselID_date %in% vessel_dates), ]


df<-landings3%>%
  group_by(vesselID_date, port.complex) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)%>%
  rename(port_complex1=port.complex)

#merging with port and gear information
no.ports2<- no.ports%>%
  #attach information from receipts
  left_join(dplyr::select(df,vesselID_date,port_complex1), by="vesselID_date", na_matches = "never")

halibut.logbook8<-rbind(ports,no.ports2)
#################

#################
#STEP 5
#Fifth we added a port complex to the logbook data if a vessel went to the same port complex 90% or more of the time during a specific year.

no.ports<-halibut.logbook8%>%
  filter( is.na(port_complex1) & is.na(port_complex2))%>%
  dplyr::select( -port_complex1)

ports<-halibut.logbook8%>%
  filter(!is.na(port_complex1) | !is.na(port_complex2))

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
  dplyr::select(-perc.complex,-number)#%>%
  #  mutate(year=as.character(year))%>%
 # mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year"))%>%
  rename(port_complex1=port.complex)

halibut.logbook9<-rbind(ports,no.ports)
#################

#################
#STEP 6
#Six, we added a port complex to the logbook data if a vessel went to a specific port complex in over 90% of the landings data

no.ports<-halibut.logbook9%>%
  filter( is.na(port_complex1) & is.na(port_complex2))%>%
  dplyr::select( -port_complex1)

ports<-halibut.logbook9%>%
  filter(!is.na(port_complex1) | !is.na(port_complex2))

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
  select(-percentage.complex, -count.complex)

#vessels.tomerge$vessel_id<-as.character(vessels.tomerge$vessel_id)

no.ports2<-no.ports%>%
  left_join(vessels.tomerge, by=c("vessel_id"))%>%
  rename(port_complex1=port.complex)

halibut.logbooks10<-rbind(ports,no.ports2)
#################


#################
#add GPS block id
# Filter out rows where coordinates are out of range
gps <- halibut.logbooks10 %>% 
  filter(long_dd_set >= -180 & long_dd_set <= 180, lat_dd_set >= -90 & lat_dd_set <= 90)

no.gps <- halibut.logbooks10%>% 
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
halibut.logbook11<-rbind(no.gps,gps2_block_df)
#################

#################
halibut.logbook12<-halibut.logbook11%>%
  left_join(dplyr::select(port_info, port_id, port.complex), by=c("port_code_depart" = "port_id"))%>%
  rename(port_id_logbook=port_code_depart, port_complex_logbook=port.complex)%>%
  mutate(port.complex=coalesce(port_complex_logbook, port_complex1, port_complex2))%>%
  mutate(block_id=coalesce(block_id,block_id_landings1,block_id_landings2,block_id_gps))%>%
  mutate(trip.identifier=paste(logbook_id, vessel_id, date_depart, date.logbooks, sep="_"))%>%
  mutate(tow.identifier=paste(logbook_id, vessel_id, date_depart, date.logbooks, date_set, block_id, lat_dd_set, long_dd_set, lat_dd_up, long_dd_up, tow_number, depth_fa_avg, time_set_num,time_up_num, duration_hrs, sep="_"))

#################



#################
#SAVE DATA
saveRDS(halibut.logbook12, paste0(halibut_data, "Data/Processed Data/halibut_logbooks_edited.rds"))
#################
