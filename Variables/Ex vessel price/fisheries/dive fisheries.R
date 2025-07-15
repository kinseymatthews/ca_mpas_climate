
library(tidyverse)


##################
#Read in data
dive_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/'
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
cucumber<-readRDS(paste0(dive_data,"Data/Processed Data/seacucumber_filtered.rds"))
urchin<-readRDS(paste0(dive_data, "Data/Processed Data/seaurchin_filtered.rds"))


#create new column of receipt id and landings, and use those unique columns to summarize data

cucumber<-cucumber%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"),
         month_year=paste(month,year, sep="_"))

urchin<-urchin%>%
  mutate(receipt_landings1=  paste(receipt_id_landings1, landings_lbs1, sep = "_"),
         receipt_landings2= paste(receipt_id_landings2, landings_lbs2, sep = "_"),
         month_year=paste(month,year, sep="_"))

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
  select(month_year, port.complex,receipt_landings1, landings_lbs1, value_usd1)  %>%
  filter(value_usd1>0)

cuc_2 <-cucumber %>%
  select(month_year, port.complex, receipt_landings2,landings_lbs2, value_usd2)  %>%
  filter(value_usd2>0)

cuc.price.port.year.month.1<-cuc_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))


cuc.price.port.year.month.2<-cuc_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

cuc.price.port.year.month<-rbind(cuc.price.port.year.month.1,cuc.price.port.year.month.2)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=all_value/all_landings)

saveRDS(cuc.price.port.year.month, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cucumberdive_exvessel_port__month_year_notadjusted.rds")
############


############
urch_1<-urchin %>%
  select(month_year, port.complex,receipt_landings1, landings_lbs1, value_usd1)  %>%
  filter(value_usd1>0)

urch_2 <-urchin %>%
  select(month_year, port.complex, receipt_landings2,landings_lbs2, value_usd2)  %>%
  filter(value_usd2>0)

urch.price.port.year.month.1<-urch_1%>%
  distinct(receipt_landings1, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs1), all_value=sum(value_usd1))

urch.price.port.year.month.2<-urch_2%>%
  distinct(receipt_landings2, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs2), all_value=sum(value_usd2))

urch.price.port.year.month<-rbind(urch.price.port.year.month.1,urch.price.port.year.month.2)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(all_landings), all_value=sum(all_value))%>%
  mutate(price_lbs=all_value/all_landings)

saveRDS(urch.price.port.year.month, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/urchindive_exvessel_port_year_month_notadjusted.rds")
############

############
# By landigns data
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))
#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))

#code to get all unique gear with the word dive in it
dive_landings<-landings%>%
  filter(gear %in% c("diving",
                    "Diving /hooks (sea urchins)",
                    "Diving"))%>%
  filter(comm_name %in% c("Red sea urchin", "Unspecified sea cucumber", "Warty sea cucumber"))%>%
  mutate(comm_name = case_when(
    comm_name == "Unspecified sea cucumber" ~ "Warty sea cucumber",
    TRUE ~ comm_name
  ))%>%
  mutate(receipt_landings=  paste(receipt_id, landings_lbs, sep = "_"))%>%
  left_join(port, by='port')

dive_landings$month<- month(dive_landings$date)

dive_landings<-dive_landings%>%
  mutate(month_year=paste(month,year, sep="_"))

urchin_landings<-dive_landings%>%
  filter(comm_name =="Red sea urchin")

cuc_landings<-dive_landings%>%
  filter(comm_name =="Warty sea cucumber")

urch.price.port.year.month.landings<-urchin_landings%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex %in% c("Los Angeles", "Fort Bragg", "San Diego","Santa Barbara"))


cuc.price.port.year.month.landings<-cuc_landings%>%
  filter(value_usd>0)%>%
  distinct(receipt_landings, .keep_all = TRUE)%>%
  group_by(port.complex,month_year)%>%
  summarize(all_landings=sum(landings_lbs), all_value=sum(value_usd))%>%
  mutate(price_lbs=(all_value/all_landings))%>%
  filter(port.complex %in% c("Los Angeles", "San Diego", "Santa Barbara"))


saveRDS(cuc.price.port.year.month.landings, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cuc_dive_landing_exvessel_port_month_year_notadjusted.rds")
saveRDS(urch.price.port.year.month.landings, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/urch_dive_landing_exvessel_port_month_year_notadjusted.rds")


