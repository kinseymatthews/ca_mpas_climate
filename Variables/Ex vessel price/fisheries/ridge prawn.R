library(tidyverse)


##################
#Read in data
shrimp_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/'
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))


#edited shrimp data from the block script
shrimp<-readRDS(paste0(shrimp_data,"Data/Processed Data/ridegback_filtered.rds"))%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))

#maybe enough data? going to compare both methods of using logbook data and landings data

ridge.price.port.year.logbook<-shrimp%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex== "Santa Barbara" | port.complex== "Los Angeles")

saveRDS(ridge.price.port.year.logbook, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/ridge_logbook_exvessel_port_notadjusted.rds")

##################
##landings data
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))

trawl_gears <- unique(landings$gear[grepl("trawl", landings$gear, ignore.case = TRUE)])
trawl_gears <- setdiff(trawl_gears, c("Midwater trawl", "Pelagic trawl"))
# Define valid species-gear combos

species_gear <- list(
  'Ridgeback prawn' = trawl_gears
)

landings_ridge<- map2_dfr( # loops through each species and its associated gear types to filter rows accordingly.
  names(species_gear),
  species_gear,
  ~ landings %>%
    filter(comm_name == .x, gear %in% .y)
)%>%
  left_join(port, by='port')%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))


ridge.price.port.year.landings<-landings_ridge%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex== "Santa Barbara" | port.complex== "Los Angeles")

saveRDS(ridge.price.port.year.landings, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/ridge_landing_exvessel_port_notadjusted.rds")

##################