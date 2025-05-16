library(tidyverse)


##################
#Read in data
halibut_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/'

#edited lobster data from the block script
halibut<-readRDS(paste0(halibut_data,"Data/Processed Data/halibuttrawl_filtered.rds"))%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"))

halibut%>%
  group_by(receipt_id_landings1) %>%
  summarize(diferent= n_distinct(receipt_id1) > 1)%>%
  filter(diferent == "TRUE")


hal_1<-halibut%>%
  dplyr::select(year, port.complex,receipt_landings1, landings_lbs1, value_usd1) 
hal_2 <-halibut %>%
  dplyr::select(year, port.complex, receipt_landings2,landings_lbs2, value_usd2) 

hal.price.port.year.1<-hal_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))


hal.price.port.year.2<-hal_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

hal.price.port.year<-rbind(hal.price.port.year.1,hal.price.port.year.2)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=(all_value/all_landings))


saveRDS(hal.price.port.year, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/halibut_exvessel_port_notadjusted.rds")
############
