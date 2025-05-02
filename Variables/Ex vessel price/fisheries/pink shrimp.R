library(tidyverse)


##################
#Read in data
shrimp_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/shrimp/'
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))


#edited shrimp data from the block script
shrimp<-readRDS(paste0(shrimp_data,"Data/Processed Data/pinktrawl_filtered.rds"))%>%
  mutate(receipt_landings1=  paste(receipt_id, landings_lbs, sep = "_"))

#when looking at the the below dataframe and filter the shrimp for a specific receipt_id_landings, the
#receipt_id_landings (receipt id from the landings data) is more accurate than the receipt_id in the logbook data
shrimp%>%
  group_by(receipt_id_landings) %>%
  summarize(diferent= n_distinct(receipt_id) > 1)%>%
  filter(diferent == "TRUE")

#not enough data so instead going to use the landings data and filter for shrimp and 
##landings data
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))


trawl_gears <- unique(landings$gear[grepl("trawl", landings$gear, ignore.case = TRUE)])
trawl_gears <- setdiff(trawl_gears, c("Midwater trawl", "Pelagic trawl"))
# Define valid species-gear combos

species_gear <- list(
  'Pacific pink shrimp' = trawl_gears
)

landings_pink<- map2_dfr( # loops through each species and its associated gear types to filter rows accordingly.
  names(species_gear),
  species_gear,
  ~ landings %>%
    filter(comm_name == .x, gear %in% .y)
)%>%
  left_join(port, by='port')%>%
  mutate(receipt_landings1=  paste(receipt_id, landings_lbs, sep = "_"))

landings_pink%>%
  group_by(receipt_landings1) %>%
  summarize(diferent= n_distinct(landings_lbs) > 1)%>%
  filter(diferent == "TRUE")

pink.price.port.year<-landings_pink%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))


saveRDS(pink.price.port.year, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/pinkshrimp_exvessel_port_notadjusted.rds")
