library(tidyverse)


##################
#Read in data
lobster_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/'

#edited lobster data from the block script
lobster<-readRDS(paste0(lobster_data,"Data/Processed Data/lobster_filtered.rds"))%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"))

#when looking at the the below dataframe and filter the lobster for a specific receipt_id_landings, the
#receipt_id_landings (receipt id from the landings data) is more accurate than the receipt_id in the logbook data
lobster%>%
  group_by(receipt_id_landings1) %>%
  summarize(diferent= n_distinct(receipt_id1) > 1)%>%
  filter(diferent == "TRUE")

lob_1<-lobster%>%
  select(year, port.complex,receipt_landings1, landings_lbs1, value_usd1) 
lob_2 <-lobster %>%
  select(year, port.complex, receipt_landings2,landings_lbs2, value_usd2) 

lob.price.port.year.1<-lob_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))


lob.price.port.year.2<-lob_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

lob.price.port.year<-rbind(lob.price.port.year.1,lob.price.port.year.2)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=(all_value/all_landings))


saveRDS(lob.price.port.year, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/lobster_exvessel_port_notadjusted.rds")
############


