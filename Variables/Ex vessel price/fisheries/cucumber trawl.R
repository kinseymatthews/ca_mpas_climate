library(tidyverse)


##################
#Read in data
cucumber_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/cucumber/'
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))

#edited cucumber data from the block script
cucumber<-readRDS(paste0(cucumber_data,"Data/Processed Data/cuctrawl_filtered.rds"))%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))
#20,000 of 86,000 entries had a receipt id - i think it's enough?


cuc.price.port.year.logbook<-cucumber%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))



##landings data
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))

trawl_gears <- unique(landings$gear[grepl("trawl", landings$gear, ignore.case = TRUE)])
trawl_gears <- setdiff(trawl_gears, c("Midwater trawl", "Pelagic trawl"))
# Define valid species-gear combos

species_gear <- list(
  'Giant red sea cucumber' = trawl_gears,
  'Unspecified sea cucumber' = trawl_gears
)


landings_cuc<- map2_dfr( # loops through each species and its associated gear types to filter rows accordingly.
  names(species_gear),
  species_gear,
  ~ landings %>%
    filter(comm_name == .x, gear %in% .y)
)%>%
  mutate(comm_name = case_when(
    comm_name == "Unspecified sea cucumber" ~ "Giant red sea cucumber",
    TRUE ~ comm_name
  ))%>%
  left_join(port, by='port')%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))


cuc.price.port.year.landings<-landings_cuc%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))


saveRDS(cuc.price.port.year.logbook, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cuc_logbook_exvessel_port_notadjusted.rds")


saveRDS(cuc.price.port.year.landings, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cuc_landing_exvessel_port_notadjusted.rds")

