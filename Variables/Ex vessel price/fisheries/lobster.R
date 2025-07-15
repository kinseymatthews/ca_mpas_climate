library(tidyverse)


##################
#Read in data
lobster_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/'
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#edited lobster data from the block script
lobster<-readRDS(paste0(lobster_data,"Data/Processed Data/lobster_filtered.rds"))%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"),
         month_year=paste(month,year, sep="_"))

#when looking at the the below dataframe and filter the lobster for a specific receipt_id_landings, the
#receipt_id_landings (receipt id from the landings data) is more accurate than the receipt_id in the logbook data
lobster%>%
  group_by(receipt_id_landings1) %>%
  summarize(diferent= n_distinct(receipt_id1) > 1)%>%
  filter(diferent == "TRUE")

lob_1<-lobster%>%
  select(month_year, port.complex,receipt_landings1, landings_lbs1, value_usd1)  %>%
  filter(value_usd1>0)
lob_2 <-lobster %>%
  select(month_year, port.complex, receipt_landings2,landings_lbs2, value_usd2)  %>%
  filter(value_usd2>0)

lob.price.port.year.month.1<-lob_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))


lob.price.port.year.month.2<-lob_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

lob.price.port.year.month<-rbind(lob.price.port.year.month.1,lob.price.port.year.month.2)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=(all_value/all_landings))


saveRDS(lob.price.port.year.month, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/lobster_exvessel_port_year_month_notadjusted.rds")
############


############
# By landings data
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))

landings$month<- month(landings$date)

landings<-landings%>%
  mutate(month_year=paste(month,year, sep="_"))
#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))

#code to get lobster data caught by a crab or lobster trap
lobster_landings<-landings%>%
  filter(gear=="Crab or lobster trap")%>%
  filter(comm_name=="California spiny lobster")%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))%>%
  left_join(port, by='port')

lobster.price.port.year.month.landings<-lobster_landings%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex %in% c( "Los Angeles", "San Diego","Santa Barbara"))

saveRDS(lobster.price.port.year.month.landings, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/lobster_landing_exvessel_port_year_month_notadjusted.rds")
############

