library(tidyverse)

############
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
#landings
landings<-readRDS(paste0(general_data,"1980_2022_landings_receipts.rds"))
#port 
port<-read.csv( paste0(general_data,"Port Codes.csv"))
#CPFV edited cpfv data from the block script
cpfv<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/CPFV_filtered.rds"))

############

############
#get species for each year and each port complex from the cpfv data
cpfv_summ<-cpfv%>%
  group_by(year, port.complex, species)%>%
  summarize(total_kept=sum(number_kept))

# Remove commas from Column1
cpfv_summ$species <- gsub(",", "", cpfv_summ$species)

#filter landings data for CPFV gear
landings_filtered<-landings%>%
  filter(gear %in% c("Hook & line",
                     "Vertical hook & line/portuguese longline",
                     "Troll (groundfish or other fish)",
                    "Mooching (salmon)",
                     "Troll long line",
                     "Troll (salmon)",
                     "Harpoon/spear" ,
                     "Troll (albacore)",
                     "Jig/bait (albacore)" ,
                     "Hoop net crab-rings" ,
                     "Crab or lobster trap"
                    )
        )%>%
  #add in port compelx info
  left_join(port, by='port')

# Sum landings and price by year, port_complex, species and get average price per pound
landings_summarized <- landings_filtered %>%
  group_by(year, port.complex, comm_name_orig) %>%
  summarise(
    total_landings = sum(landings_lbs, na.rm = TRUE),
    total_value = sum(value_usd, na.rm = TRUE),
    .groups = "drop"
  )%>%
  rename(species=comm_name_orig)

#match with logbook data
cpfv_landings<-cpfv_summ%>%
  left_join(landings_summarized, by = c("year", "port.complex", "species"))%>%
  filter(!is.na(total_landings))

#okay now get rid of the psecies component as i jsut want one value per year
cpfv_exvessel_price<-cpfv_landings%>%
  group_by(year, port.complex)%>%
  summarise(all_landings=sum(total_landings),
            all_value=sum(total_value))%>%
  mutate(price_lbs=((all_value/all_landings)))

saveRDS(cpfv_exvessel_price, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/cpfv_landing_exvessel_port_notadjusted.rds")
############

