library(tidyverse)
library(sf)
############
#Upload data for each fishery
cpfv<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/CPFV_filtered.rds")%>%
  mutate(fishery="CPFV",year=as.numeric(year), trip.identifier=logbookID_vesselID_date)%>%
  select(fishery,year,trip.identifier, port,port.complex)
cuc_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/cuctrawl_filtered.rds")%>%
  mutate(fishery="Cucumber trawl",year=as.numeric(year))%>%
  select(fishery,year,trip.identifier, port,port.complex)
cuc_dive<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seacucumber_filtered.rds")%>%
  mutate(fishery="Cucumber dive",year=as.numeric(year))%>%
  rename(port=port_logbook)%>%
  select(fishery,year,trip.identifier, port,port.complex)
urchin<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seaurchin_filtered.rds")%>%
  mutate(fishery="Urchin dive",year=as.numeric(year))%>%
  rename(port=port_logbook)%>%
  select(fishery,year,trip.identifier, port,port.complex)
drift<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/driftgillnet_filtered.rds")%>%
  mutate(fishery="Drift gillnet",year=as.numeric(year))%>%
  select(fishery,year,trip.identifier, port,port.complex)
set<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/setgillnet_filtered.rds")%>%
  mutate(fishery="Set gillnet",year=as.numeric(year))%>%
  select(fishery,year,trip.identifier, port,port.complex)
hal_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/halibuttrawl_filtered.rds")%>%
  mutate(fishery="Halibut trawl",year=as.numeric(year))%>%
  mutate(port=coalesce(port_depart, port1, port2))%>%
  select(fishery,year,trip.identifier, port,port.complex)
lobster<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/lobster_filtered.rds")%>%
  mutate(fishery="Lobster trap",year=as.numeric(year))%>%
  mutate(port=coalesce(port1, port2))%>%
  mutate(trip.identifier=ifelse(is.na(logbook_id)| is.na(date.logbooks)| is.na(vessel_id), NA,paste(logbook_id, date.logbooks, vessel_id, sep="_" )))%>%
  select(fishery,year,trip.identifier, port,port.complex)
pink_shrimp<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pinktrawl_filtered.rds")%>%
  mutate(fishery="Pink shrimp trawl",year=as.numeric(year))%>%
  mutate(port=coalesce(port_logbook, port_landings))%>%
  select(fishery,year,trip.identifier, port,port.complex)
ridge_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridegback_filtered.rds")%>%
  mutate(fishery="Ridgeback prawn trawl",year=as.numeric(year))%>%
  mutate(port=coalesce(port_logbook, port_landings))%>%
  select(fishery,year,trip.identifier, port,port.complex)
spot_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/spot_filtered.rds")%>%
  mutate(fishery="Spot prawn trawl",year=as.numeric(year))%>%
  mutate(port=coalesce(port_logbook, port_landings))%>%
  select(fishery,year,trip.identifier, port,port.complex)
squid<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/squid_filtered.rds")%>%
  mutate(fishery="Squid seine",year=as.numeric(year))%>%
  mutate(port=coalesce(port_landings1, port_landings2))%>%
  select(fishery,year,trip.identifier, port,port.complex)

fisheries<-rbind(cpfv,cuc_trawl,cuc_dive,urchin, drift, set, hal_trawl, lobster, pink_shrimp,spot_prawn, ridge_prawn,squid)%>%
  mutate(port = tolower(port), port.complex=tolower(port.complex))%>%
  distinct()
  

distances_port<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Raw/portcomplexes.csv")%>%
select(Port.Name, Major.Port, long, lat)%>%
  rename(port=Port.Name,port.complex=Major.Port)%>%
  mutate(port = tolower(port), port.complex=tolower(port.complex))

# base theme
base_theme <- theme(axis.text=element_text(size=7),
                    # axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text = element_text(size=10),
                    plot.tag =element_text(size=9),
                    # plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

#################


#################
#join the two together
fisheries2<-fisheries%>%
  left_join(select(distances_port, port, lat,long), by="port")

#remove those without a port - will save later and assign the cog port complex
fisheries_noport<-fisheries2%>%
  filter(is.na(lat) | is.na(long))
 
#keep those with a port. will then calculate the weighted COG by trips
fisheries_port<-fisheries2%>%
  filter(!is.na(lat))%>%
  group_by(fishery,port.complex,port)%>%
  summarize(Ntrips=n())%>%
  left_join(select(distances_port, port, lat,long), by="port")
#################


#################
#COG calculation

COG_fisheries<- fisheries_port %>%
st_as_sf(coords=c("long", "lat"))%>%
  group_by(fishery, port.complex) %>%
  summarize(
    cog_longitude = sum(Ntrips * st_coordinates(geometry)[, "X"]) / sum(Ntrips),
    cog_latitude = sum(Ntrips * st_coordinates(geometry)[, "Y"]) / sum(Ntrips) ) %>%
  sf::st_drop_geometry()


saveRDS(COG_fisheries, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/COG_complex_fishery.rds")
write.csv(COG_fisheries, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/COG_complex_fishery.csv")
#################

#################
#Add to other dataframe that didnt have a specific port
fisheries_noport_matched<-fisheries_noport%>%
  select(-c(lat,long))%>%
  left_join(COG_fisheries, by=c("fishery", "port.complex"))
#################


