# load libraries ----
library(shiny)
library(tidyverse)
library(shinyWidgets)

#.......................MPA implementation.......................
#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)

#Port Codes
port<-read.csv(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/Port Codes.csv"))
#.......................wrangle data for vessels and trips exploratory.......................

# read in all  data
CPFV<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Raw Data/1980_2022_cpfv_logbooks.Rds"))%>%
  mutate(fishery="CPFV")%>%
  mutate(logbookID_vesselID_date=paste(logbook_id, vessel_id, date, sep="_"))%>%
  rename(trip.identifier=logbookID_vesselID_date)%>%
  #attach information from port codes
  left_join(port, by="port_code") %>%
  #remove ports we wont want to include such as Oregon and San Dieg
  filter(port.complex!= "San Diego" & port.complex!= "Oregon" )
cuc_trawl<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/seacucumbertrawlfishery_matched.rds"))%>%
  mutate(fishery="Cucumber trawl")%>%
  #remove port complexes we dont care about
  filter(port.complex=="Eureka" | port.complex=="Monterey" | port.complex=="Morro Bay" | port.complex=="Santa Barbara"| port.complex=="Los Angeles")

cuc_dive<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seacucumberfishery_matched.rds"))%>%
  mutate(fishery="Cucumber dive")%>%
rename(port.complex=port_most_reliable)%>%
  filter(port.complex=="Santa Barbara" |port.complex=="Los Angeles" |port.complex=="San Diego")


urch_dive<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/redseaurchinfishery_matched.rds"))%>%
  mutate(fishery="Urchin dive")%>%
  rename(port.complex=port_most_reliable)%>%
  filter(!is.na(port.complex)& port.complex=="Fort Bragg"|port.complex=="Santa Barbara"|port.complex=="Los Angeles"|port.complex=="San Diego")
  

set<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/setgillnetfishery_matched.rds"))%>%
  mutate(fishery="Set gillnet")%>%
  rename(port.complex=port_complex_landings)  %>%
  #remove port complexes we dont care about
  filter(port.complex=="Bodega Bay" | port.complex=="San Francisco" | port.complex=="Monterey" | port.complex=="Morro Bay" | port.complex=="Santa Barbara"| port.complex=="Los Angeles"| port.complex=="San Diego")%>%
  filter(!(port.complex == "Bodega Bay" & year >= 1989)) 

hal<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/halibut_logbooks_edited.rds"))%>%
  mutate(fishery="California halibut trawl")%>%
  #remove port complexes we dont care about
  filter(port.complex!= "Sacramento Delta" & port.complex!= "San Diego" & port.complex!= "Unknown Major Port")



lob<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/lobster_logbooks_edited.rds"))%>%
  mutate(fishery="Lobster") %>%
  mutate(trip.identifier = row_number())%>%
  mutate(port.complex = coalesce(port_complex1, port_complex2))%>%
  filter(port.complex=="Santa Barbara" |port.complex=="Los Angeles" | port.complex=="San Diego")



rid<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridgeback_logbooks_edited.rds"))%>%
  mutate(fishery="Ridgeback prawn")%>%
  mutate(port.complex = coalesce(port_complex_logbook , port_complex_landings))%>%
  #remove port complexes we dont care about
  filter(port.complex=="Los Angeles" | port.complex=="Santa Barbara")



pink<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pink_logbooks_edited.rds"))%>%
  mutate(fishery="Pacific pink shrimp")%>%
  mutate(port.complex = coalesce(port_complex_logbook , port_complex_landings))%>%
  #remove port complexes we dont care about
  filter(port.complex=="Eureka" | port.complex=="Monterey" | port.complex=="Morro Bay" | port.complex=="Santa Barbara")


squid<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/squid_logbooks_edited.rds"))%>%
  mutate(fishery="Market Squid")%>%
  #remove port complexes we dont care about
  filter(port.complex== "San Francisco" | port.complex== "Monterey" | port.complex== "Santa Barbara"| port.complex== "Los Angeles")


fishery_list<-list(CPFV, cuc_trawl, cuc_dive, urch_dive,set,hal,lob,rid,pink,squid)


summarize_trips <- function(fishery) {
  fishery%>%
    mutate(year = as.integer(year)) %>%
    group_by(fishery,port.complex, year) %>%
    summarize(
      ntrips = n_distinct(trip.identifier),
      nvessels = n_distinct(vessel_id),
      .groups = "drop"
    )
}

result_list <- lapply(fishery_list, summarize_trips)
result_df <- bind_rows(result_list)

saveRDS(result_df, ("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_vessels_trips.rds"))

#.......................wrangle data for depth exploratory.......................

CPFV<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/depth.rds"))%>%
  mutate(fishery="CPFV")%>%
  rename(effort.identifier=logbookID_vesselID_date)
cuc_trawl<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/depth.rds"))%>%
  mutate(fishery="Cucumber trawl")
cuc_dive<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/cucumber_depth.rds"))%>%
  mutate(fishery="Cucumber dive")%>%
  rename(Depth_m=Med_depth_m_recorded)
urch_dive<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/urchin_depth.rds"))%>%
  mutate(fishery="Urchin dive")%>%
  rename(Depth_m=Med_depth_m_recorded)
set<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/set_depth.rds"))%>%
  mutate(fishery="Set gillnet")
hal<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/depth.rds"))%>%
  mutate(fishery="California halibut trawl")
lob<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/depth.rds"))%>%
  mutate(fishery="Lobster") %>%
  mutate(effort.identifier = row_number())
rid<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridge_depth.rds"))%>%
  mutate(fishery="Ridgeback prawn")
pink<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pink_depth.rds"))%>%
  mutate(fishery="Pacific pink shrimp")
squid<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/depth.rds"))%>%
  mutate(fishery="Market Squid")

fishery_list<-list(CPFV, cuc_trawl, cuc_dive, urch_dive,set,hal,lob,rid,pink,squid)


summarize_depth <- function(fishery) {
  fishery%>%
    rename_with(~ case_when(
      . %in% c("set.identifier", "tow.identifier", "dive.identifier") ~ "effort.identifier",
      TRUE ~ .
    ))%>%
    mutate(year=as.factor(year), effort.identifier=as.character(effort.identifier)) %>%
    select(port.complex, year,Max_Depthm,Min_Depthm,Depth_m, mpayear,fishery, effort.identifier)
      
}

result_list_dp <- lapply(fishery_list, summarize_depth)
result_df_dp <- bind_rows(result_list_dp)

saveRDS(result_df_dp, ("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_depth.rds"))

#.......................wrangle data for KD time series.......................
#volume contours (area)
volume<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/allfisheries_all contour volumes.rds")%>%
  rename(year=Year)%>%
  filter(fishery!="Drift gillnet" & fishery!="Spot prawn trawl")
shore<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/allfisheries_all distance shore.rds")%>%
  filter(fishery!="Drift gillnet"& fishery!="Spot prawn trawl")
mpa<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/allfisheries_effortmpa.rds")%>%
  rename(year=Year)%>%
  filter(fishery!="Drift gillnet"& fishery!="Spot prawn trawl")

joined <- full_join(volume, mpa, by = c("fishery", "port.complex", "year"))
final_df <- full_join(joined, shore, by = c("fishery", "port.complex", "year"))


final_sf<-st_as_sf(final_df, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = 4326)%>%
  mutate(distance_shore_km=cog_dist_shore/1000)
final_sf$year=as.factor(final_sf$year)


final_sf_df<-final_sf%>%
  sf::st_drop_geometry()%>%
  mutate(year=as.numeric(as.character(year)))

final_sf_df$COG_latitude <- st_coordinates(final_sf)[, "Y"]  # Extracts latitude
final_sf_df$COG_longitude <- st_coordinates(final_sf)[, "X"]




saveRDS(final_sf_df, ("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_lat_extent_shore_mpa_area.rds"))

