
library(tidyverse)


##################
#Read in data
dive_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/'

cucumber<-readRDS(paste0(dive_data,"Data/Processed Data/seacucumber_filtered.rds"))%>%
  filter(price_usd1>0 | is.na(price_usd1) )%>%
  filter( price_usd2>0 | is.na(price_usd2 ))%>%
  mutate(price_usd= rowMeans(cbind(price_usd1, price_usd2), na.rm = TRUE))


urchin<-readRDS(paste0(dive_data, "Data/Processed Data/seaurchin_filtered.rds"))%>%
  filter(price_usd1>0 | is.na(price_usd1) )%>%
  filter( price_usd2>0 | is.na(price_usd2 ))%>%
  mutate(price_usd= rowMeans(cbind(price_usd1, price_usd2), na.rm = TRUE))


#average the price per dive. identfier
cucumber%>%
 group_by(dive.identifier) %>%
  summarize(diferent_usd = n_distinct(price_usd) > 1)%>%
   filter(diferent_usd == "TRUE")

urchin%>%
  group_by(dive.identifier) %>%
  summarize(diferent_usd  = n_distinct(price_usd) > 1)%>%
  filter(diferent_usd == "TRUE")

#yes for cucumber not for urchin


price.port.year<-cucumber%>%
  group_by(port.complex,year)%>%
  summarize(avg.price.lb=mean(price_usd, na.rm = TRUE))

price.port.year<-lobster%>%
  group_by(port.complex,year)%>%
  summarize(avg.price.lb=mean(price_usd, na.rm = TRUE))