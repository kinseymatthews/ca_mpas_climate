library(tidyverse)


##################
#Read in data
squid_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/'
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'


#edited squid data from the block script
squid<-readRDS(paste0(squid_data,"Data/Processed Data/squid_filtered.rds"))%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"),
         month_year=paste(month,year, sep="_"))

#when looking at the the below dataframe and filter the squid for a specific receipt_id_landings, the
#receipt_id_landings (receipt id from the landings data) is more accurate than the receipt_id in the logbook data
squid%>%
  group_by(receipt_id_landings1) %>%
  summarize(diferent= n_distinct(receipt_id1) > 1)%>%
  filter(diferent == "TRUE")

squid_1<-squid%>%
  select(month_year, port.complex,receipt_landings1, landings_lbs1, value_usd1)  %>%
  filter(value_usd1>0)
squid_2 <-squid %>%
  select(month_year, port.complex, receipt_landings2,landings_lbs2, value_usd2)  %>%
  filter(value_usd2>0)

squid.price.port.year.month.1<-squid_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))


squid.price.port.year.month.2<-squid_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

squid.price.port.year.month<-rbind(squid.price.port.year.month.1,squid.price.port.year.month.2)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=(all_value/all_landings))

saveRDS(squid.price.port.year.month, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/squid_exvessel_port__year_month_notadjusted.rds")
############

##################
# By landings data
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))
#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))

#code to market squid caught by the various boats
squid_landings<-landings%>%
  filter(gear %in% c("Purse seine","Brail/dip net or a-frame", "Drum seine" ))%>%
  filter(comm_name=="Market squid")%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))%>%
  left_join(port, by='port')

squid_landings$month<- month(squid_landings$date)

squid_landings<-squid_landings%>%
  mutate(month_year=paste(month,year, sep="_"))


squid.price.port.year.month.landings<-squid_landings%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex %in% c("Los Angeles", "Monterey", "San Francisco","Santa Barbara"))


saveRDS(squid.price.port.year.month.landings, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/squid_landing_exvessel_port_year_month_notadjusted.rds")
############

