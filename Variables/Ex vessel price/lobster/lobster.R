library(tidyverse)


##################
#Read in data
lobster_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/'

#edited lobster data from the block script
lobster<-readRDS(paste0(lobster_data,"Data/Processed Data/lobster_filtered.rds"))%>%
  filter(price_usd1>2 | is.na(price_usd1) )%>%
  filter( price_usd2>2 | is.na(price_usd2 ))%>%
  mutate(price_usd= rowMeans(cbind(price_usd1, price_usd2), na.rm = TRUE))

price.port.year<-lobster%>%
  group_by(port.complex,year)%>%
  summarize(avg.price.lb=mean(price_usd, na.rm = TRUE))

saveRDS(price.port.year, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/lobster_exvessel_port_notadjusted.rds")
############