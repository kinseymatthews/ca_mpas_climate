library(tidyverse)


##################
#Read in data
cucumber_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/cucumber/'
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))

#edited cucumber data from the block script
cucumber<-readRDS(paste0(cucumber_data,"Data/Processed Data/cuctrawl_filtered.rds"))%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"),
         month_year=paste(month,year, sep="_"))
#20,000 of 86,000 entries had a receipt id - i think it's enough?


cuc.price.port.year.month.logbook<-cucumber%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))

saveRDS(cuc.price.port.year.month.logbook, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cuc_logbook_exvessel_port_month_year_notadjusted.rds")

##################

##################
##landings data
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))

#code to get all unique gear with the word trawl in it
trawl_gears <- unique(landings$gear[grepl("trawl", landings$gear, ignore.case = TRUE)])

#exclude these trawl gears
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

landings_cuc$month<- month(landings_cuc$date)

landings_cuc<-landings_cuc%>%
  mutate(month_year=paste(month,year, sep="_"))

cuc.price.port.year.month.landings<-landings_cuc%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex %in% c("Eureka","Los Angeles", "Monterey", "Morro Bay","Santa Barbara"))


saveRDS(cuc.price.port.year.month.landings, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cuc_landing_exvessel_port_month_year_notadjusted.rds")
##################
