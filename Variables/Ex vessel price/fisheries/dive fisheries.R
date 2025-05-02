
library(tidyverse)


##################
#Read in data
dive_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/'
cucumber<-readRDS(paste0(dive_data,"Data/Processed Data/seacucumber_filtered.rds"))
urchin<-readRDS(paste0(dive_data, "Data/Processed Data/seaurchin_filtered.rds"))


#almost true like above but not always. so, create new column of receipt id and landings, and use those unique columns to summarize data

cucumber<-cucumber%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"))

urchin<-urchin%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"))

#when looking at the the below dataframe and filter the cucumber for a specific receipt_id_landings, the
#receipt_id_landings (receipt id from the landings data) is not more accurate than the receipt_id in the logbook data

cucumber%>%
  group_by(receipt_id_landings1) %>%
  summarize(diferent= n_distinct(receipt_id1) > 1)%>%
  filter(diferent == "TRUE")

#same receipt id has the same landings_lbs (did the above for both receipt_id2/landings_lbs2 and receipt_id1/landings_lbs1)

urchin%>%
  group_by(receipt_id_landings2) %>%
  summarize(diferent_usd = n_distinct(landings_lbs2) > 1)%>%
  filter(diferent_usd == "TRUE")


############
cuc_1<-cucumber %>%
  select(year, port.complex,receipt_landings1, landings_lbs1, value_usd1) 

cuc_2 <-cucumber %>%
  select(year, port.complex, receipt_landings2,landings_lbs2, value_usd2) 

cuc.price.port.year.1<-cuc_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))


cuc.price.port.year.2<-cuc_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

cuc.price.port.year<-rbind(cuc.price.port.year.1,cuc.price.port.year.2)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=all_value/all_landings)

saveRDS(cuc.price.port.year, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cucumberdive_exvessel_port_notadjusted.rds")
############


############
urch_1<-urchin %>%
  select(year, port.complex,receipt_landings1, landings_lbs1, value_usd1) 

urch_2 <-urchin %>%
  select(year, port.complex, receipt_landings2,landings_lbs2, value_usd2) 

urch.price.port.year.1<-urch_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))

urch.price.port.year.2<-urch_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

urch.price.port.year<-rbind(urch.price.port.year.1,urch.price.port.year.2)%>%
  group_by(port.complex,year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=all_value/all_landings)

saveRDS(urch.price.port.year, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/urchindive_exvessel_port_notadjusted.rds")
############


