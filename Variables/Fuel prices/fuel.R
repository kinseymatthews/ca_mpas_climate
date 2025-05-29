library(tidyverse)
library(priceR) #https://cran.r-project.org/web/packages/priceR/priceR.pdf

#Fuel data from Efin
fuel<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Fuel price/data/raw/fuelca.csv")%>%
  rename(port_code=port, port=portname)%>%
  mutate(port = case_when(
    port == "Eureka (incl. Fields Landing)" ~ "Eureka",
    port == "Santa Barb" ~ "Santa Barbara",
    port == "Crescent C" ~ "Crescent City",
    port == "San Franci" ~ "San Francisco",
    port == "Port Huene" ~ "Port Hueneme",
    TRUE ~ port #keep other values unchanged
  ))%>%
  #remove entries where they didnt get a price so they put 0
  filter(pxquoted!="No Price Quoted - See Notes")

#port 
ports<-read.csv(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/Port Codes.csv"))

#merge fuel and port ocmplexes

fuel_ports<-fuel%>%
  left_join(ports, by="port" )%>%
  #some of the rows include prices that are taxed - get rid of
filter(!notes %in% c("INCLUDES TAX", 
                 "INCLUDES TAX-wouldn't give before tax",
                 "Tax included; won't give w/o tax",
                 "INCL TAX",
                 "with tax, doesn't know tax rate",
                 "with taxes",
                "includes tax",
                "	includes tax, didn't know rate",
                "INCLUDES 7.255 TAX",
                "1.555-7.25% tax",
                "1.16-7%",
                "1.115-7.25% tax",
                "$1.512-7.25% tax",
                "$1.477-7% sales tax",
                "$1.34-8.25% sales tax",
                "$1.34-7.5% tax",
                "$1.212-7.25%",
                "$1.13-7.8% tax",
                "includes tax, didn't know rate",
                "INCLUDES 7.25% TAX"))

ggplot(fuel_ports, aes(x=YEAR, y=pricegal, color=port))+
  geom_point()

#something weird going on with south San Francisco
a<-fuel_ports%>%
  filter(DOCKCODE=="SF_3")
ggplot(a, aes(x=YEAR, y=pricegal))+
  geom_point()

fuel_ports<-fuel_ports%>%
  filter(DOCKCODE!="SF_3")
ggplot(fuel_ports, aes(x=YEAR, y=pricegal, color=port))+
  geom_point()

a<-fuel_ports%>%
  filter(port=="Morro Bay")
ggplot(a, aes(x=YEAR, y=pricegal))+
  geom_point()


fuel_ports_sum<-fuel_ports%>%
  group_by(port.complex,YEAR)%>%
  summarize(price.gal.avg=mean(pricegal))

#adjusting price
# Assign these variables once
country <- "United States"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()

fuel_ports_sum$cost_per_gal_adjusted<-adjust_for_inflation(fuel_ports_sum$price.gal.avg,
                                                           fuel_ports_sum$YEAR, 
                                                       country="United States", 
                                                       to_date =  2022,
                                                       inflation_dataframe = inflation_dataframe,
                                                       countries_dataframe = countries_dataframe )

#and save into one dataframr
saveRDS(fuel_ports_sum, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Fuel price/data/processed/allportcomplexes_dieselprice_adjusted.rds")
