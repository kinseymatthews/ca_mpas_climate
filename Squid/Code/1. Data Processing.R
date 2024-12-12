library(dplyr)
library(sf)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(data.table)
library(lubridate)


#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
squid_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/'

#upload raw data
squid_logbook<-readRDS(paste0(squid_data, "Data/Raw Data/CDFW_1999_2022_squid_logbook_data.rds"))
squid_lightboat_logbook<-readRDS(paste0(squid_data, "Data/Raw Data/CDFW_2000_2022_squid_lightboat_logbook_data.rds"))
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
#Logbook Data Manipulation
#Note that there is a port column in the logbook data but it might not add up to the landings data and/or the logbook says "Unknown Major Port" 
#when there is a corresponding landing receipt with an accurate port

squid_logbook2<- separate(squid_logbook, receipt_ids, into = c("receipt_id1", "receipt_id2"), sep = ",")
squid_logbook2$receipt_id2 <- trimws(squid_logbook2$receipt_id2)

squid_logbook2<- squid_logbook2%>%
  mutate(receipt_vesselID_1 = ifelse(is.na(receipt_id1) | is.na(vessel_id), NA, paste(receipt_id1, vessel_id, sep = "_"))) %>%
  mutate(receipt_vesselID_2 = ifelse(is.na(receipt_id2) | is.na(vessel_id), NA, paste(receipt_id2, vessel_id, sep = "_")))%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  mutate(month = month(date))%>%
  mutate(year = year(date))%>%
  rename(date.logbooks=date) #%>%
  #mutate(vessel_id = str_replace(vessel_id, "^0+", ""))
#################

#################
#Landing Data Manipulation
landings$month <- month(landings$date)

landings2<-landings  %>%
  mutate(receipt_vesselID_date = ifelse(is.na(receipt_id) | is.na(vessel_id) |is.na(date), NA, paste(receipt_id, vessel_id,date, sep = "_"))) %>%
  mutate(receipt_vesselID = ifelse(is.na(receipt_id) | is.na(vessel_id), NA, paste(receipt_id, vessel_id, sep = "_")))%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  rename(date.landings=date)%>%
  left_join(dplyr::select(port_info, port_id, port.complex), by="port_id")%>%
  mutate(vesselID_year_month=ifelse(is.na(vessel_id)| is.na(year)| is.na(month), NA,paste(vessel_id, year, month, sep="_" )))%>%
  mutate(vesselID_year=ifelse(is.na(vessel_id)| is.na(year), NA,paste(vessel_id, year,sep="_" )))%>%
  rename(block_id_landings=block_id)
#################

#################
#STEP 1
#squid logbooks with landing receipts. Will see if we can properply match those with the landings data

#separate into squid logs with receipts and those without receipts.Will first match with receipt id and vessel id
squid.receipts<-squid_logbook2%>%
  filter(!is.na(receipt_id1))

squid.no.receipts<-squid_logbook2%>%
  filter(is.na(receipt_id1))


#edit the landings data to see if there is only one or more port complexes per receipt_vesselId
df <- landings2 %>%
  group_by(receipt_vesselID) %>%
  filter(n() > 1) %>%
  ungroup()
# Group by column A (receipt_vesselID)and check if values in column B (port.complex) are all the same
result <- df %>% 
  group_by(receipt_vesselID) %>%
  summarise(same_B_values = n_distinct(port.complex) == 1)

result%>%
  filter(same_B_values=="FALSE") #4 with different ports - which are not in the squid data

landings.tomerge<- landings2%>%
  distinct(receipt_vesselID, .keep_all = TRUE)%>%
  rename(receipt_ids=receipt_id)


#Matching with first receipt and vessel ID
matched_data<-squid.receipts%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,receipt_vesselID,port_id,port,primary_gear,gear,date.landings, port.complex, block_id_landings), by=c("receipt_vesselID_1" = "receipt_vesselID"), na_matches = "never")%>%
  filter(((date.landings - date.logbooks) <= 7 ) & (date.landings - date.logbooks) >= 0 )

# Identify unmatched entries
unmatched_entries <- squid.receipts %>%
  anti_join(matched_data, by = c("receipt_vesselID_1", "date.logbooks"))


# Combine matched entries with unmatched entries (without matched data)
df <- bind_rows(matched_data, unmatched_entries)%>%
  rename(port_id_landings1=port_id, port_landings1=port,date_landings1=date.landings, gear_landings1=gear,primary_gear_landings1=primary_gear,port_complex_landings1=port.complex,block_id_landings1=block_id_landings)


#matching with second landing receipt
matched_data<-df%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,receipt_vesselID,port_id,port,primary_gear,gear,date.landings, port.complex, block_id_landings), by=c("receipt_vesselID_2" = "receipt_vesselID"), na_matches = "never")%>%
  filter(((date.landings - date.logbooks) <= 7 ) & (date.landings - date.logbooks) >= 0 )


# Identify unmatched entries
unmatched_entries <- df %>%
  anti_join(matched_data, by = c("receipt_vesselID_2", "date.logbooks"))

# Combine matched entries with unmatched entries (without matched data)

df2 <- bind_rows( matched_data, unmatched_entries )

squid.receipts2<-df2%>%
  rename( port_id_landings2=port_id,gear_landings2=gear,primary_gear_landings2=primary_gear, date_landings2=date.landings, port_landings2=port,port_complex_landings2=port.complex,block_id_landings2=block_id_landings)



squid.no.receipts<-squid.no.receipts%>%
  mutate(port_complex_landings1=NA,date_landings1=NA, port_id_landings1=NA, port_landings1=NA, primary_gear_landings1=NA, gear_landings1=NA, block_id_landings1=NA, date_landings2=NA, port_id_landings2=NA, port_landings2=NA, primary_gear_landings2=NA, gear_landings2=NA, port_complex_landings2=NA, block_id_landings2=NA)

squid_logbook3<-rbind(squid.receipts2,squid.no.receipts)
#################

#################
#STEP 1.5
#matching receipts that only has one port complex regardless of date and vessel
no.ports<-squid_logbook3%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))

ports<-squid_logbook3%>%
  filter(!is.na(port_complex_landings1) | !is.na(port_complex_landings2))

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
common_names <- intersect(df1$receipt_id, no.ports$receipt_id1) #none
common_names <- intersect(df1$receipt_id, no.ports$receipt_id2) #none
#no common names - can match directly

# can merge the no duplicates with the landings data
landings2.tomerge<- landings2%>%
  distinct(receipt_id, .keep_all = TRUE)

#merging with port and gear information - based on receipt 1
no.ports.match<- no.ports%>%
  dplyr::select(-port_complex_landings1,-date_landings1, -port_id_landings1, -port_landings1, -primary_gear_landings1, -gear_landings1,-block_id_landings1)%>%
  #attach information from receipts
  left_join(dplyr::select(landings2.tomerge,receipt_id,port_id,port,primary_gear,gear,date.landings, port.complex,block_id_landings), by=c("receipt_id1" = "receipt_id"), na_matches = "never")%>%
  rename(port_id_landings1=port_id, port_landings1=port,date_landings1=date.landings, gear_landings1=gear,primary_gear_landings1=primary_gear,port_complex_landings1=port.complex, block_id_landings1=block_id_landings)

#merging with port and gear information - based on receipt 2
no.ports.match2<- no.ports.match%>%
  dplyr::select(-port_complex_landings2,-date_landings2, -port_id_landings2, -port_landings2, -primary_gear_landings2, -gear_landings2,-block_id_landings2)%>%
  #attach information from receipts
  left_join(dplyr::select(landings2.tomerge,receipt_id,port_id,port,primary_gear,gear,date.landings, port.complex,block_id_landings), by=c("receipt_id2" = "receipt_id"), na_matches = "never")%>%
  rename(port_id_landings2=port_id,gear_landings2=gear,primary_gear_landings2=primary_gear, date_landings2=date.landings, port_landings2=port,port_complex_landings2=port.complex,block_id_landings2=block_id_landings)

squid_logbook4<-rbind(no.ports.match2,ports)
#################

#################
#STEP 2
#Second, if a vessel was recorded at only one port complex throughout the entire landings dataset, we assigned that port complex to the logbook data
no.ports<-squid_logbook4%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))

ports<-squid_logbook4%>%
  filter(!is.na(port_complex_landings1) | !is.na(port_complex_landings2))

#list of vessel ids that only had one port associated with it
result <- landings2 %>%
  group_by(vessel_id)%>%
  filter(n_distinct(port.complex) == 1) %>%
  summarize( port.complex=first(port.complex))

result$vessel_id<-as.character(result$vessel_id)

#match with the remaining vessel ids with no ports 
no.ports<-no.ports%>%
  dplyr::select(-port_complex_landings1)%>%
  left_join(result, by="vessel_id")%>%
  rename(port_complex_landings1=port.complex)

squid_logbook5<-rbind(no.ports,ports)
#################

#################
#STEP 3
#Third, we used the combination of vessel id and date to match the datasets. For a day in which there were multiple port complexes 
#listed for a vessel within the landings data, we assigned the port complex that appeared most frequently to the corresponding logbook entry.
no.ports<-squid_logbook5%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))

ports<-squid_logbook5%>%
  filter(!is.na(port_complex_landings1) | !is.na(port_complex_landings2))

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
common_names <- intersect(df1$vesselID_date, no.ports$vesselID_date)  #three commone names - have to seperate

#separate again....
no.dupes<- no.ports[!(no.ports$vesselID_date %in% common_names), ] #these should be able to match directly with ports
dupes<- no.ports[(no.ports$vesselID_date %in% common_names), ] #these have more than one port listed



# can merge the no duplicates with the landings data
landings.tomerge<- landings2%>%
  distinct(vesselID_date, .keep_all = TRUE) #just need the port info

no.dupes<-no.dupes%>%
  dplyr::select(-port_complex_landings1,-date_landings1, -port_id_landings1, -port_landings1, -primary_gear_landings1, -gear_landings1,-block_id_landings1)%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,vesselID_date,port_id,port,primary_gear,gear,date.landings,port.complex, block_id_landings), by="vesselID_date", na_matches = "never")%>%
  rename(port_id_landings1=port_id, port_landings1=port,date_landings1=date.landings, gear_landings1=gear,primary_gear_landings1=primary_gear,port_complex_landings1=port.complex, block_id_landings1=block_id_landings)

squid_logbook6<-rbind(ports,no.dupes,dupes)
#################

#################
#STEP 4
#Fourth, we assigned a port complex from the landings data if a vessel visited the same port 
#complex 90% or more of the time during a specific year and month

no.ports<-squid_logbook6%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-squid_logbook6%>%
  filter(!is.na(port_complex_landings1) | !is.na(port_complex_landings2))

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
  mutate(month=as.double(month))%>%
  mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year", "month"))%>%
  rename(port_complex_landings1=port.complex)

squid_logbook7<-rbind(ports,no.ports)
#################

#################
#STEP 5
#Matching entries with port complexes most often seen that day
no.ports<-squid_logbook7%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-squid_logbook7%>%
  filter(!is.na(port_complex_landings1) | !is.na(port_complex_landings2))

vessel_dates<-na.omit(no.ports$vesselID_date)

landings3<- landings2[(landings2$vesselID_date %in% vessel_dates), ]


df<-landings3%>%
  group_by(vesselID_date, port.complex) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)%>%
  rename(port_complex_landings1=port.complex)

#merging with port and gear information
no.ports2<- no.ports%>%
  #attach information from receipts
  left_join(dplyr::select(df,vesselID_date,port_complex_landings1), by="vesselID_date", na_matches = "never")

squid_logbook8<-rbind(ports,no.ports2)
#################

#################
#STEP 6
#Fifth we added a port complex to the logbook data if a vessel went to the same port complex 90% or more of the time during a specific year.

no.ports<-squid_logbook8%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-squid_logbook8%>%
  filter(!is.na(port_complex_landings1) | !is.na(port_complex_landings2))

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
 # mutate(year=as.character(year))%>%
  mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year"))%>%
  rename(port_complex_landings1=port.complex)

squid_logbook9<-rbind(ports,no.ports)
#################

#################
#STEP 7
#Six, we added a port complex to the logbook data if a vessel went to a specific port complex in over 90% of the landings data

no.ports<-squid_logbook9%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-squid_logbook9%>%
  filter(!is.na(port_complex_landings1) | !is.na(port_complex_landings2))

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
  rename(port_complex_landings1=port.complex)

squid_logbook10<-rbind(ports,no.ports)
#################

#################
#add GPS block id
# Filter out rows where coordinates are out of range
gps <- squid_logbook10 %>% 
  filter(long_dd >= -180 & long_dd <= 180, lat_dd >= -90 & lat_dd <= 90)

no.gps <- squid_logbook10%>% 
  filter(long_dd < -180 | long_dd > 180 | lat_dd < -90 | lat_dd> 90 | is.na(lat_dd) |
           is.na(long_dd))%>%
  mutate(block_id_gps=NA)

# Convert GPS coordinates dataframe to an sf object (assuming df_gps contains lat and long columns)
gps2 <- st_as_sf(gps, coords = c("long_dd", "lat_dd"), crs = 4326)

# Perform spatial join
gps2_block <- st_join(gps2 , all_blocks)

coords<-st_coordinates(gps2_block)

# Bind the coordinates back to the original data
gps2_block_df <- cbind(gps2_block, coords)%>%
  rename(long_dd=X, lat_dd=Y)%>%
  st_drop_geometry()

#combine back with the other dataframe
squid_logbook11<-rbind(no.gps,gps2_block_df)
#################

#################
squid_logbook12<-squid_logbook11%>%
  mutate(port.complex=coalesce( port_complex_landings1, port_complex_landings2))%>%
  mutate(block_id=coalesce(block_id_gps,block_id_landings1,block_id_landings2))%>%
  mutate(gear=coalesce(gear_landings1,gear_landings2))%>%
  mutate(trip.identifier=paste(logbook_id, vessel_id, date.logbooks, sep="_"))%>%
  mutate(tow.identifier=paste(logbook_id, vessel_id, date.logbooks, block_id, lat_dd, long_dd,  set_number, depth_fa, time_start,time_end, duration_min, sep="_"))

#################



#################
#SAVE DATA
saveRDS(squid_logbook12, paste0(squid_data, "Data/Processed Data/squid_logbooks_edited.rds"))
#################