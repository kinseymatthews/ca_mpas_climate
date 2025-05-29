library(tidyverse)


##################
#Read in data
gillnet_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/'


#edited set gillnet data from the block script
gillnet<-readRDS(paste0(gillnet_data,"Data/Processed Data/setgillnet_filtered.rds"))%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))


gill<-gillnet%>%
  select(year, port.complex,receipt_landings, landings_lbs, value_usd)  %>%
  filter(value_usd>0)


gill.price.port.year<-gill%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex %in% c("Bodega Bay",  "Los Angeles","Monterey", "Morro Bay", "San Diego", "San Francisco", "Santa Barbara"))


saveRDS(gill.price.port.year, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/setgillnet_exvessel_port_notadjusted.rds")
############
