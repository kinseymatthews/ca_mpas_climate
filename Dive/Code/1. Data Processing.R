library(sf)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(data.table)
library(tidyverse)

#################
#upload raw data
dive_logbook<-readRDS("Dive/Data/Raw Data/CDFW_1980_2023_dive_logbooks.rds")
landings<-readRDS("Dive/Data/Raw Data/1980_2022_landings_receipts.rds")
port_info<-read.csv('Dive/Data/Raw Data/Port Codes.csv')%>%
  rename(port_id=port_code)
port_info$port_id<-as.double(port_info$port_id)
#################


#################
#Logbook Data Manipulation
#Note that there is a port column in the logbook data but it might not add up to the landings data and/or the logbook says "Unknown Major Port" 
#when there is a corresponding landing receipt with an accurate port

dive_logbook2<- separate(dive_logbook, receipt_id, into = c("receipt_id1", "receipt_id2"), sep = ",")
dive_logbook2$receipt_id2 <- trimws(dive_logbook2$receipt_id2)

dive_logbook2<- dive_logbook2%>%
  mutate(receipt_vesselID_1 = ifelse(is.na(receipt_id1) | is.na(vessel_id), NA, paste(receipt_id1, vessel_id, sep = "_"))) %>%
  mutate(receipt_vesselID_2 = ifelse(is.na(receipt_id2) | is.na(vessel_id), NA, paste(receipt_id2, vessel_id, sep = "_")))%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  mutate(month = month(date))%>%
  rename(date.logbooks=date, port_complex_logbook=port_complex, port_id_logbook=port_id, port_logbook=port)
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
  mutate(vessel_id=if_else(vessel_id==6990, "06990", as.character(vessel_id))) #noticed this vessel_id was inputted incorrectly
#################



#################
#STEP 1
#Dive logbooks with landing receipts. Will see if we can properply match those with the landings data

#separate into dive logs with receipts and those without receipts.Will first match with receipt id and vessel id
dive.receipts<-dive_logbook2%>%
  filter(!is.na(receipt_id1))

dive.no.receipts<-dive_logbook2%>%
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
          filter(same_B_values=="FALSE")
#4 with different ports - which are not in the dive data

landings.tomerge<- landings2%>%
  distinct(receipt_vesselID, .keep_all = TRUE)%>%
  rename(receipt_ids=receipt_id)


#Matching with first receipt and vessel ID
matched_data<-dive.receipts%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,receipt_vesselID,port_id,port,primary_gear,gear,date.landings, port.complex), by=c("receipt_vesselID_1" = "receipt_vesselID"), na_matches = "never")%>%
  filter((abs(date.landings - date.logbooks) <= 15 | is.na(date.landings)))

# Identify unmatched entries
unmatched_entries <- dive.receipts %>%
  anti_join(matched_data, by = c("receipt_vesselID_1", "date.logbooks"))


# Combine matched entries with unmatched entries (without matched data)
df <- bind_rows(matched_data, unmatched_entries)%>%
  rename(port_id_landings1=port_id, port_landings1=port,date_landings1=date.landings, gear_landings1=gear,primary_gear_landings1=primary_gear,port_complex_landings1=port.complex)


#matching with second landing receipt
matched_data<-df%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,receipt_vesselID,port_id,port,primary_gear,gear,date.landings, port.complex), by=c("receipt_vesselID_2" = "receipt_vesselID"), na_matches = "never")%>%
  filter((abs(date.landings - date.logbooks) <= 15 | is.na(date.landings)))


# Identify unmatched entries
unmatched_entries <- df %>%
  anti_join(matched_data, by = c("receipt_vesselID_2", "date.logbooks"))

# Combine matched entries with unmatched entries (without matched data)

df2 <- bind_rows( matched_data, unmatched_entries )

dive.receipts2<-df2%>%
  rename( port_id_landings2=port_id,gear_landings2=gear,primary_gear_landings2=primary_gear, date_landings2=date.landings, port_landings2=port,port_complex_landings2=port.complex)



dive.no.receipts<-dive.no.receipts%>%
  mutate(port_complex_landings1=NA,date_landings1=NA, port_id_landings1=NA, port_landings1=NA, primary_gear_landings1=NA, gear_landings1=NA,date_landings2=NA, port_id_landings2=NA, port_landings2=NA, primary_gear_landings2=NA, gear_landings2=NA, port_complex_landings2=NA)

dive_logbook3<-rbind(dive.receipts2,dive.no.receipts)
#################


#################
#STEP 1.5
#matching receipts that only has one port complex regardless of date and vessel
no.ports<-dive_logbook3%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))

ports<-dive_logbook3%>%
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
  dplyr::select(-port_complex_landings1,-date_landings1, -port_id_landings1, -port_landings1, -primary_gear_landings1, -gear_landings1)%>%
  #attach information from receipts
  left_join(dplyr::select(landings2.tomerge,receipt_id,port_id,port,primary_gear,gear,date.landings, port.complex), by=c("receipt_id1" = "receipt_id"), na_matches = "never")%>%
  rename(port_id_landings1=port_id, port_landings1=port,date_landings1=date.landings, gear_landings1=gear,primary_gear_landings1=primary_gear,port_complex_landings1=port.complex)

#merging with port and gear information - based on receipt 2
no.ports.match2<- no.ports.match%>%
  dplyr::select(-port_complex_landings2,-date_landings2, -port_id_landings2, -port_landings2, -primary_gear_landings2, -gear_landings2)%>%
  #attach information from receipts
  left_join(dplyr::select(landings2.tomerge,receipt_id,port_id,port,primary_gear,gear,date.landings, port.complex), by=c("receipt_id2" = "receipt_id"), na_matches = "never")%>%
  rename(port_id_landings2=port_id,gear_landings2=gear,primary_gear_landings2=primary_gear, date_landings2=date.landings, port_landings2=port,port_complex_landings2=port.complex)

dive_logbook4<-rbind(no.ports.match2,ports)
#################


#################
#STEP 2
#Second, if a vessel was recorded at only one port complex throughout the entire landings dataset, we assigned that port complex to the logbook data
no.ports<-dive_logbook4%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))

ports<-dive_logbook4%>%
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

dive_logbook5<-rbind(no.ports,ports)
#################



#################
#STEP 3
#Third, we used the combination of vessel id and date to match the datasets. For a day in which there were multiple port complexes 
#listed for a vessel within the landings data, we assigned the port complex that appeared most frequently to the corresponding logbook entry.
no.ports<-dive_logbook5%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))

ports<-dive_logbook5%>%
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
common_names <- intersect(df1$vesselID_date, no.ports$vesselID_date)  #no common names - dont have to seperate

# can merge the no duplicates with the landings data
landings.tomerge<- landings2%>%
  distinct(vesselID_date, .keep_all = TRUE) #just need the port info

no.ports.match<-no.ports%>%
  dplyr::select(-port_complex_landings1,-date_landings1, -port_id_landings1, -port_landings1, -primary_gear_landings1, -gear_landings1)%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,vesselID_date,port_id,port,primary_gear,gear,date.landings,port.complex), by="vesselID_date", na_matches = "never")%>%
  rename(port_id_landings1=port_id, port_landings1=port,date_landings1=date.landings, gear_landings1=gear,primary_gear_landings1=primary_gear,port_complex_landings1=port.complex)

dive_logbook6<-rbind(ports, no.ports.match)
#################


#################
#STEP 3.5
#Matching entries with port complexes most often seen that day
no.ports<-dive_logbook6%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-dive_logbook6%>%
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

dive_logbook7<-rbind(ports,no.ports2)

#################


#################
#STEP 4
#Fourth, we assigned a port complex from the landings data if a vessel visited the same port 
#complex 90% or more of the time during a specific year and month

no.ports<-dive_logbook7%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-dive_logbook7%>%
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
  mutate(year=as.character(year))%>%
  mutate(month=as.double(month))%>%
  mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year", "month"))%>%
  rename(port_complex_landings1=port.complex)

dive_logbook8<-rbind(ports,no.ports)
#################


#################
#STEP 5
#Fifth we added a port complex to the logbook data if a vessel went to the same port complex 90% or more of the time during a specific year.

no.ports<-dive_logbook8%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-dive_logbook8%>%
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
  mutate(year=as.character(year))%>%
  mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year"))%>%
  rename(port_complex_landings1=port.complex)

dive_logbook9<-rbind(ports,no.ports)
#################


#################
#STEP 6
#Six, we added a port complex to the logbook data if a vessel went to a specific port complex in over 90% of the landings data

no.ports<-dive_logbook9%>%
  filter(is.na(port_complex_landings1) & is.na(port_complex_landings2))%>%
  dplyr::select( -port_complex_landings1)

ports<-dive_logbook9%>%
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

dive_logbook10<-rbind(ports,no.ports)
#################


#################
#Assigning a dive as a sea cucumber or urchin fishery
dive_logbook10<-dive_logbook10%>%
  mutate(dive.identifier=paste(date.logbooks,port_id_logbook,vessel_id,block_id,depth_min_ft, depth_max_ft, hours, dive_number, sep="_"))

df<-dive_logbook10%>%
  group_by(dive.identifier)%>%
  summarize(n_species= n_distinct(comm_name))%>%
  filter(n_species >= 2) %>%
  pull(dive.identifier)

#group by the identifier, common name, and then retain the one with more catch_lbs, if 
target<-dive_logbook10%>%
  filter(dive.identifier %in% df)%>%
  filter(comm_name=="Red sea urchin" | comm_name=="Warty sea cucumber")%>%
  group_by(dive.identifier, comm_name)%>%
  summarise(total_catch = sum(catch_lbs, na.rm=TRUE), .groups = 'drop') %>%  # Summarize total catch
  group_by(dive.identifier) %>%
  mutate(Target_species = comm_name[which.max(total_catch)]) %>%  # Identify comm_name with greatest total catch
  ungroup() %>%# Ungroup for final output
  dplyr::select(-comm_name, - total_catch)%>%
  distinct()

df1<-dive_logbook10 %>%
  filter(dive.identifier %in% df)%>%
  left_join(target, by="dive.identifier")


df2<- dive_logbook10 %>%
  filter(!dive.identifier %in% df)%>%
  mutate(Target_species=comm_name)

dive_logbook11<-rbind(df1,df2)
#################




#################
#looking at  trip identifiers and target species and seeing how it differs from the dive target species
dive_logbook11<-dive_logbook11%>%
  mutate(trip.identifier=paste(date.logbooks,port_id_logbook,vessel_id, sep="_"))

df<-dive_logbook11%>%
  group_by(trip.identifier)%>%
  summarize(n_species= n_distinct(comm_name))%>%
  filter(n_species >= 2) %>%
  pull(trip.identifier)

#group by the identifier, common name, and then retain the one with more catch_lbs, if 
target<-dive_logbook11%>%
  filter(trip.identifier %in% df)%>%
  filter(comm_name=="Red sea urchin" | comm_name=="Warty sea cucumber")%>%
  group_by(trip.identifier, comm_name)%>%
  summarise(total_catch = sum(catch_lbs, na.rm=TRUE), .groups = 'drop') %>%  # Summarize total catch
  group_by(trip.identifier) %>%
  mutate(Target_species_Trip = comm_name[which.max(total_catch)]) %>%  # Identify comm_name with greatest total catch
  ungroup() %>%# Ungroup for final output
  dplyr::select(-comm_name, - total_catch)%>%
  distinct()

#adding info and editing
df1.1<-dive_logbook11 %>%
  filter(trip.identifier %in% df)%>%
  left_join(target, by="trip.identifier")

df2<- dive_logbook11 %>%
  filter(!trip.identifier %in% df)%>%
  mutate(Target_species_Trip=comm_name)

dive_logbook12<-rbind(df1.1,df2)

# Extract rows where column1 does not match column2
non_matching_rows <-  dive_logbook12[dive_logbook12$Target_species_Trip !=  dive_logbook12$Target_species, ]
#################


#################
#dive wise
#group by the identifier, common name, and then retain the one with more catch_lbs, if 
target<-dive_logbook10%>%
  group_by(dive.identifier, comm_name)%>%
  summarise(total_catch_species = sum(catch_lbs, na.rm=TRUE))%>%
  ungroup()%>%
  group_by(dive.identifier)%>%
  mutate(total_catch_dive=sum(total_catch_species))%>%
  group_by(dive.identifier, comm_name)%>%
  mutate(percent_catch=(total_catch_species/total_catch_dive)*100)%>%
  mutate(percent_catch_log=log(percent_catch+1))%>%
  filter(total_catch_species>0)%>%
  filter(comm_name=="Red sea urchin" | comm_name=="Warty sea cucumber")



ggplot() + 
  geom_density(data=target,aes(x=percent_catch,fill=comm_name),alpha = 0.5)+
  xlab("Percent of Catch")+
  ylab("Density") + 
  # theme
  theme(axis.text=element_text(size=8),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


#trip wise

#group by the identifier, common name, and then retain the one with more catch_lbs, if 
target<-dive_logbook12%>%
  group_by(trip.identifier, comm_name)%>%
  summarise(total_catch_species = sum(catch_lbs, na.rm=TRUE))%>%
  ungroup()%>%
  group_by(trip.identifier)%>%
  mutate(total_catch_dive=sum(total_catch_species))%>%
  group_by(trip.identifier, comm_name)%>%
  mutate(percent_catch=(total_catch_species/total_catch_dive)*100)%>%
  mutate(percent_catch_log=log(percent_catch+1))%>%
  filter(total_catch_species>0)%>%
 filter(comm_name=="Red sea urchin" | comm_name=="Warty sea cucumber")
 
 
ggplot() + 
  geom_density(data=target,aes(x=percent_catch,fill=comm_name),alpha = 0.5)+
  xlab("Percent of Catch")+
  ylab("Density") + 
  # theme
  theme(axis.text=element_text(size=8),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
