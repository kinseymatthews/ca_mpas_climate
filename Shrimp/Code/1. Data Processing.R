library(dplyr)
library(sf)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(data.table)
library(lubridate)


#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
shrimp_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/'

#upload raw data
shrimp_logbook<-readRDS(paste0(shrimp_data, "Data/Raw Data/CDFW_1982_2023_shrimp_prawn_logbook_data.rds"))
landings<-readRDS(paste0(shrimp_data,"Data/Raw Data/1980_2022_landings_receipts.rds"))
port_info<-read.csv(paste0(general_data,'Port Codes.csv'))
port_info$port_code<-as.double(port_info$port_code)
#################


#################
#create new columns with vesselID_date for logbook and landings and for receipts. up to two receipt ids.
shrimp_logbook<- shrimp_logbook%>%
  left_join(dplyr::select(port_info, port_code, port.complex), by="port_code")%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date_tow), NA, paste(vessel_id, date_tow,sep = "_"))) %>%
  mutate(vesselID_date_species = ifelse(is.na(vessel_id) | is.na(date_tow)| is.na(species), NA, paste(vessel_id, date_tow, species,sep = "_"))) %>%
  mutate(vesselID_year = ifelse(is.na(vessel_id) | is.na(year), NA, paste(vessel_id, year,sep = "_"))) %>%
  mutate(month = month(date_tow)) %>%
  mutate(vesselID_year_month = ifelse(is.na(vessel_id) | is.na(year)| is.na(month), NA, paste(vessel_id, year,month,sep = "_"))) %>%
  rename(date.tow.logbooks=date_tow, port_complex_logbook=port.complex, port_code_logbook=port_code, port_logbook=port)
#################


#################
landings<-landings%>%
  rename(port_code=port_id)%>%
  left_join(dplyr::select(port_info, port_code, port.complex), by="port_code")%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date,sep = "_"))) %>%
  mutate(vesselID_date_species = ifelse(is.na(vessel_id) | is.na(date)| is.na(comm_name), NA, paste(vessel_id, date, comm_name,sep = "_"))) %>%
  mutate(vesselID_year = ifelse(is.na(vessel_id) | is.na(year), NA, paste(vessel_id, year,sep = "_"))) %>%
  mutate(month = month(date)) %>%
  mutate(vesselID_year_month = ifelse(is.na(vessel_id) | is.na(year)| is.na(month), NA, paste(vessel_id, year,month,sep = "_"))) %>%
  rename(date.landings=date, port_complex_landings=port.complex, port_code_landings=port_code, port_landings=port, block_id_landings=block_id)
#################



#################
#list of vessel ids that only had one port associated with it
result <- landings %>%
  group_by(vessel_id)%>%
  filter(n_distinct(port_complex_landings) == 1) %>%
  summarize(port_complex_landings=first(port_complex_landings))

result$vessel_id<-as.character(result$vessel_id)


#match with the remaining vessel ids with no ports 
shrimp_logbook2<-shrimp_logbook%>%
  left_join(result, by="vessel_id")
#################


#################

no.ports<-shrimp_logbook2%>%
  filter(is.na(port_complex_landings) & is.na(port_complex_logbook))

ports<-shrimp_logbook2%>%
  filter(!is.na(port_complex_landings) | !is.na(port_complex_logbook))


df <- landings %>%
  group_by(vesselID_date) %>%
  filter(n() > 1) %>%
  ungroup()

# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(vesselID_date) %>%
  summarise(same_B_values = n_distinct(port_complex_landings) == 1)

df1<-result%>%
  filter(same_B_values=="FALSE") #59 common

# Find common names
common_names <- intersect(df1$vesselID_date, no.ports$vesselID_date) #none

#seperate into the ones that intersect with the duplicattes and those that dont
no.ports.no.duplicates<- no.ports[!(no.ports$vesselID_date %in% common_names), ] #these should be able to match directly with port complexes using vessel_id and date
no.ports.duplicates<- no.ports[(no.ports$vesselID_date %in% common_names), ] #these have more than one port complex listed for a single vessel_id and date



# can merge the no duplicates with the landings data
landings.tomerge<- landings%>%
  distinct(vesselID_date, .keep_all = TRUE) #just need the port info

#merging with port and gear information
no.ports.no.duplicates2<-no.ports.no.duplicates%>%
  select(-port_complex_landings)%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,vesselID_date,port_code_landings,port_landings,primary_gear,gear,date.landings, port_complex_landings, block_id_landings), by="vesselID_date", na_matches = "never")

#merge together with the eother
shrimp_logbook3<-bind_rows(no.ports.no.duplicates2,no.ports.duplicates, ports)

shrimp_logbook3%>%
  filter(is.na(port_complex_landings) & is.na(port_complex_logbook))
#################


#################
#STEP 4
#Fourth, we assigned a port complex from the landings data if a vessel visited the same port 
#complex 90% or more of the time during a specific year and month

no.ports<-shrimp_logbook3%>%
  filter(is.na(port_complex_landings) & is.na(port_complex_logbook))%>%
  dplyr::select( -port_complex_landings)

ports<-shrimp_logbook3%>%
  filter(!is.na(port_complex_landings) | !is.na(port_complex_logbook))

vessels<-no.ports$vessel_id

df<-landings%>%
  filter(vessel_id %in% vessels)%>%
  group_by(vessel_id, year, month, port_complex_landings)%>%
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
  left_join(df.tomerge, by=c("vessel_id" , "year", "month"))

shrimp_logbook4<-rbind(ports,no.ports)
#################

#################
#STEP 3.5
#Matching entries with port complexes most often seen that day

no.ports<-shrimp_logbook4%>%
  filter(is.na(port_complex_landings) & is.na(port_complex_logbook))%>%
  dplyr::select( -port_complex_landings)

ports<-shrimp_logbook4%>%
  filter(!is.na(port_complex_landings) | !is.na(port_complex_logbook))

vessel_dates<-no.ports$vesselID_date

landings2<- landings[(landings$vesselID_date %in% vessel_dates), ]


df<-landings2%>%
  group_by(vesselID_date, port_complex_landings) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)

#merging with port and gear information
no.ports2<- no.ports%>%
  #attach information from receipts
  left_join(dplyr::select(df,vesselID_date,port_complex_landings), by="vesselID_date", na_matches = "never")

shrimp_logbook5<-rbind(ports,no.ports2)
#################

#################
#STEP 5
#Fifth we added a port complex to the logbook data if a vessel went to the same port complex 90% or more of the time during a specific year.

no.ports<-shrimp_logbook5%>%
  filter(is.na(port_complex_landings) & is.na(port_complex_logbook))%>%
  dplyr::select( -port_complex_landings)

ports<-shrimp_logbook5%>%
  filter(!is.na(port_complex_landings) | !is.na(port_complex_logbook))

#list of vessel ids and years that only had one port associated with it
result <- landings %>%
  group_by(vesselID_year)%>%
  filter(n_distinct(port_complex_landings) == 1) %>%
  summarize(port_complex_landings=first(port_complex_landings))

vessels<-no.ports$vessel_id

df<-landings%>%
  filter(vessel_id %in% vessels)%>%
  group_by(vessel_id, year, port_complex_landings)%>%
  summarize(number=n())%>%
  ungroup()%>%
  group_by(vessel_id,year)%>%
  mutate(perc.complex=number/sum(number))

df.tomerge<-df%>%
  filter(perc.complex>=.9)%>%
  dplyr::select(-perc.complex,-number)%>%
  #  mutate(year=as.character(year))%>%
  mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-no.ports%>%
  left_join(df.tomerge, by=c("vessel_id" , "year"))

shrimp_logbook6<-rbind(ports,no.ports)
#################


#################
#STEP 6
#Six, we added a port complex to the logbook data if a vessel went to a specific port complex in over 90% of the landings data

no.ports<-shrimp_logbook6%>%
  filter(is.na(port_complex_landings) & is.na(port_complex_logbook))%>%
  dplyr::select( -port_complex_landings)

ports<-shrimp_logbook6%>%
  filter(!is.na(port_complex_landings) | !is.na(port_complex_logbook))

result2 <- landings %>%
  group_by(vessel_id, port_complex_landings)%>%
  summarize(count=n())%>%
  ungroup()%>%
  group_by(vessel_id)%>%
  mutate(percentage.port=round((count/sum(count))*100,2))

vessels<-unique(no.ports$vessel_id)


df<- result2 %>%
  filter(vessel_id %in% vessels) %>%
  group_by(vessel_id, port_complex_landings) %>%
  summarize(count.complex = sum(count), .groups = 'drop') %>%
  group_by(vessel_id) %>%
  mutate(percentage.complex = round((count.complex / sum(count.complex)) * 100,2))

vessels.tomerge<-df%>%
  filter(percentage.complex>90)%>%
  select(-percentage.complex, -count.complex)

vessels.tomerge$vessel_id<-as.character(vessels.tomerge$vessel_id)

no.ports2<-no.ports%>%
  left_join(vessels.tomerge, by=c("vessel_id"))

shrimp_logbook7<-rbind(ports,no.ports2)
#################



#################
#SAVE DATA
saveRDS(shrimp_logbook7 ,paste0(shrimp_data, "Data/Processed Data/shrimp_logbooks_edited.rds"))
shrimp_logbook7<-readRDS(paste0(shrimp_data, "Data/Processed Data/shrimp_logbooks_edited.rds"))
#################

#################
#Firuing out the trip target and the tow target
shrimp_logbook8<-shrimp_logbook7%>%
  mutate(trip.identifier=paste(logbook_id, vessel_id, date.tow.logbooks, sep="_"))%>%
  mutate(tow.identifier=paste(logbook_id, vessel_id, date.tow.logbooks, block_id, lat_dd_set, long_dd_set, lat_dd_up, long_dd_up, tow_number, depth_fa_set, depth_fa_up, time_set,time_up, duration_min, sep="_"))

target<-shrimp_logbook8%>%
  group_by(tow.identifier, species)%>%
  summarise(total_catch_species = sum(catch_lbs, na.rm=TRUE))%>%
  ungroup()%>%
  group_by(tow.identifier)%>%
  mutate(total_catch_dive=sum(total_catch_species))%>%
  group_by(tow.identifier, species)%>%
  mutate(percent_catch=(total_catch_species/total_catch_dive)*100)%>%
  mutate(percent_catch_log=log(percent_catch+1))%>%
  filter(total_catch_species>0)%>%
  filter(species=="Ridgeback prawn" | species=="Spot prawn"| species=="Pacific pink shrimp")

ggplot() + 
  geom_density(data=target,aes(x=percent_catch,fill=species),alpha = 0.5)+
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

#trip 

#group by the identifier, species, and then retain the one with more catch_lbs, if 
target<-shrimp_logbook8%>%
  group_by(trip.identifier, species)%>%
  summarise(total_catch_species = sum(catch_lbs, na.rm=TRUE))%>%
  ungroup()%>%
  group_by(trip.identifier)%>%
  mutate(total_catch_dive=sum(total_catch_species))%>%
  group_by(trip.identifier, species)%>%
  mutate(percent_catch=(total_catch_species/total_catch_dive)*100)%>%
  mutate(percent_catch_log=log(percent_catch+1))%>%
  filter(total_catch_species>0)%>%
  filter(species=="Ridgeback prawn" | species=="Spot prawn"| species=="Pacific pink shrimp")


ggplot() + 
  geom_density(data=target,aes(x=percent_catch,fill=species),alpha = 0.5)+
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
#################


#################
#Assigning a dive as a sea cucumber or urchin fishery

df<-shrimp_logbook8%>%
  group_by(tow.identifier)%>%
  summarize(n_species= n_distinct(species))%>%
  filter(n_species >= 2) %>%
  pull(tow.identifier)

#group by the identifier, common name, and then retain the one with more catch_lbs, if 
target<-shrimp_logbook8%>%
  filter(tow.identifier%in% df)%>%
  filter(species=="Ridgeback prawn" | species=="Spot prawn"| species=="Pacific pink shrimp")%>%
  group_by(tow.identifier, species)%>%
  summarise(total_catch = sum(catch_lbs, na.rm=TRUE), .groups = 'drop') %>%  # Summarize total catch
  group_by(tow.identifier) %>%
  mutate(Target_species_tow = species[which.max(total_catch)]) %>%  # Identify species with greatest total catch
  ungroup() %>%# Ungroup for final output
  dplyr::select(-species, - total_catch)%>%
  distinct()

df1<-shrimp_logbook8 %>%
  filter(tow.identifier %in% df)%>%
  left_join(target, by="tow.identifier")


df2<- shrimp_logbook8%>%
  filter(!tow.identifier %in% df)%>%
  mutate(Target_species_tow=species)

shrimp_logbook9<-rbind(df1,df2)
#################



#################
#Assigning a trip as a sea cucumber or urchin fishery

df<-shrimp_logbook9%>%
  group_by(trip.identifier)%>%
  summarize(n_species= n_distinct(species))%>%
  filter(n_species >= 2) %>%
  pull(trip.identifier)

#group by the identifier, common name, and then retain the one with more catch_lbs, if 
target<-shrimp_logbook9%>%
  filter(trip.identifier %in% df)%>%
  filter(species=="Ridgeback prawn" | species=="Spot prawn"| species=="Pacific pink shrimp")%>%
  group_by(trip.identifier, species)%>%
  summarise(total_catch = sum(catch_lbs, na.rm=TRUE), .groups = 'drop') %>%  # Summarize total catch
  group_by(trip.identifier) %>%
  mutate(Target_species_Trip = species[which.max(total_catch)]) %>%  # Identify species with greatest total catch
  ungroup() %>%# Ungroup for final output
  dplyr::select(-species, - total_catch)%>%
  distinct()

#adding info and editing
df1.1<-shrimp_logbook9 %>%
  filter(trip.identifier %in% df)%>%
  left_join(target, by="trip.identifier")

df2<- shrimp_logbook9%>%
  filter(!trip.identifier %in% df)%>%
  mutate(Target_species_Trip=species)

shrimp_logbook10<-rbind(df1.1,df2)
#################


#################
#ridgepack prawn, spot prawn, pink shrimp
rp<-shrimp_logbook10 %>%
  filter(Target_species_Trip=="Ridgeback prawn")
saveRDS(rp ,paste0(shrimp_data, "Data/Processed Data/ridgeback_logbooks_edited.rds"))

sp<-shrimp_logbook10 %>%
  filter(Target_species_Trip=="Spot prawn")
saveRDS(sp ,paste0(shrimp_data, "Data/Processed Data/spot_logbooks_edited.rds"))

ps<-shrimp_logbook10 %>%
  filter(Target_species_Trip=="Pacific pink shrimp")
saveRDS(ps ,paste0(shrimp_data, "Data/Processed Data/pink_logbooks_edited.rds"))

