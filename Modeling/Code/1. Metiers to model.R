library(tidyverse)


############
#only modeling fisheries that are operating recently, in the last 10 years
#fishing in 80% of years with more than 1 block per year

#upload data from all fisheries

cpfv<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/CPFV_filtered.rds'))%>%
  mutate(fishery="CPFV",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
cuc_trawl<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/cuctrawl_filtered.rds'))%>%
  mutate(
    fishery = "Cucumber trawl",
    fishery_portcomplex = paste(fishery, port.complex, sep = "_"),
    month = if_else(is.na(month) | month == 0, as.integer(format(date_tow, "%m")), month)
  )
cuc_dive<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seacucumber_filtered.rds'))%>%
  mutate(year=as.numeric(year))%>%
  mutate(fishery="Cucumber dive",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
urch<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seaurchin_filtered.rds'))%>%
  mutate(year=as.numeric(year))%>%
  mutate(fishery="Urchin dive",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
gill<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/setgillnet_filtered.rds'))%>%
  mutate(fishery="Gillnet",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))%>%
  filter(!is.na(date.logbooks))
hal<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/halibuttrawl_filtered.rds'))%>%
  mutate(fishery="Halibut trawl",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
lob<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/lobster_filtered.rds'))%>%
  mutate(fishery="Lobster trap",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
pink<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pinktrawl_filtered.rds'))%>%
  mutate(fishery="Pink shrimp trawl",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
ridge<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridegback_filtered.rds'))%>%
  mutate(fishery="Ridgeback prawn trawl",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
squid<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/squid_filtered.rds'))%>%
  mutate(fishery="Squid seine",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))
############

############
#create function to see if  there are landings in the past 

plot_fun<-function(fishery, fishery_name){
  
  # Get unique port complexes
all_port_complexes <- unique(fishery$port.complex)

results <- map_dfr(all_port_complexes, function(pc) {
  complex <- fishery%>%
    filter(port.complex == pc)
  
all_years <- unique(complex$year)
end<-2020
start<-end-10
years_above_start <- all_years[all_years >= start]


#for year in years_above_start, get how many blocks there are
block_counts_per_year <- complex %>%
  filter(year %in% years_above_start) %>%
  group_by(year) %>%
  summarize(n_unique_blocks = n_distinct(block_id))
  
# Count how many years have more than 1 block
n_years_with_multiple_blocks <- sum(block_counts_per_year$n_unique_blocks > 1)
qualifies_as_operating <- n_years_with_multiple_blocks >= 8

# Return just one row per port complex per fishery
tibble(
  fishery = fishery_name,
  port.complex = pc,
  qualifies_as_operating = qualifies_as_operating
)
})

return(results)
}

fisheries_status <- bind_rows(
  plot_fun(cpfv, "CPFV"),
  plot_fun(cuc_trawl, "Cucumber trawl"),
  plot_fun(cuc_dive, "Cucumber dive"),
  plot_fun(urch, "Urchin dive"),
  plot_fun(gill, "Gillnet"),
  plot_fun(hal, "Halibut trawl"),
  plot_fun(lob, "Lobster trap"),
  plot_fun(pink, "Pink shrimp trawl"),
  plot_fun(ridge, "Ridgeback prawn trawl"),
  plot_fun(squid, "Squid seine")
)%>%
  mutate(fishery_portcomplex=paste(fishery, port.complex, sep = "_"))

# Get valid fishery_portcomplex combinations
valid_fishery_portcomplexes <- fisheries_status %>%
  filter(qualifies_as_operating) %>%
  pull(fishery_portcomplex)

# Filter each fishery to only include qualifying fishery_portcomplex combos
cpfv <- cpfv %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
cuc_trawl <- cuc_trawl %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
cuc_dive <- cuc_dive %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
urch <- urch %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
gill <- gill %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
hal <- hal %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
lob <- lob %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
pink <- pink %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
ridge <- ridge %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
squid <- squid %>% filter(fishery_portcomplex %in% valid_fishery_portcomplexes)
############

saveRDS(cpfv,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/CPFV_filtered_modeling.rds')
saveRDS(cuc_trawl,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/cuctrawl_filtered_modeling.rds')
saveRDS(cuc_dive,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/cucdive_filtered_modeling.rds')
saveRDS(urch,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/urch_filtered_modeling.rds')
saveRDS(gill,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/gill_filtered_modeling.rds')
saveRDS(hal,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/hal_filtered_modeling.rds')
saveRDS(lob,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/lob_filtered_modeling.rds')
saveRDS(pink,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/pink_filtered_modeling.rds')
saveRDS(ridge,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/ridge_filtered_modeling.rds')
saveRDS(squid,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/squid_filtered_modeling.rds')





