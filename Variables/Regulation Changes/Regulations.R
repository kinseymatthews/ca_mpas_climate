library(tidyverse)
library(lubridate)
library(sf)


############
#upload block info
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#upload block information
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(3310))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")
all_blocks <- st_make_valid(all_blocks)%>%
  select(-c(blck_st, blck_ty, blck_sq, block_long_dd, block_lat_dd))

############



############
#upload shapefiles 
reg_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/'

#historical mpas
hist_mpas_sf <- st_read(paste0(reg_data,"MPAs/processed data/historical mpas/historical mpas.shp"))%>%
  select(-NAME)%>%
  rename(name=Namenew)
#current mpas
mpas_sf <- st_read(paste0(reg_data,"MPAs/processed data/current mpas/MPAs.shp"))
DECA_sf <- st_read(paste0(reg_data,"DECA/processed/shapefile/DECA.shp"))
CCA_sf <- st_read(paste0(reg_data,"CCA_YRCA/CCA/processed/shapefile/CCA.shp"))%>%
  rename(name=Name)
EFHCA_sf <- st_read(paste0(reg_data,"EFHCA/processed/shapefile/EFHCA.shp"))%>%
  rename(name=AREA_NAME)
urchin_sf <- st_read(paste0(reg_data,"urchin closure/processed/shapefile/south capsar point.shp"))
trawl_sf <- st_read(paste0(reg_data,"trawl/processed/shapefile/trawl_closed_areas.shp"))
gillnet_sf <- st_read(paste0(reg_data,"gillnet/processed/shapefile/gillnet_closures.shp"))
#RCA
############


############
## upload regulations
#historical mpas
mpas_hist <- read.csv(paste0(reg_data,"MPAs/processed data/old_mpa_regulations.csv"))
#current mpas
mpas_cur <- read.csv(paste0(reg_data,"MPAs/processed data/current_mpa_regulations.csv"))
DECA <- read.csv(paste0(reg_data,"DECA/processed/DECA regulations.csv"))
CCA<- read.csv(paste0(reg_data,"CCA_YRCA/CCA/processed/CCA regulations.csv"))%>%
      rename(name=Name)
EFHCA <-  read.csv(paste0(reg_data,"EFHCA/processed/EFHCA regulations.csv"))%>%
          rename(name=Name)
urchin <- read.csv(paste0(reg_data,"urchin closure/processed/urchin_regulations.csv"))
trawl <- read.csv(paste0(reg_data,"trawl/processed/trawl_regulations.csv"))
gillnet<- read.csv(paste0(reg_data,"gillnet/processed/gillnet_regulations.csv"))
#RCA
############


############
# combine shapefiles with regulations
hist_mpas_sf_comb <- left_join(hist_mpas_sf, mpas_hist, by = "name")
mpas_sf_comb <- left_join(mpas_sf, mpas_cur, by = "name")
deca_sf_comb <- left_join(DECA_sf, DECA, by = "name")
cca_sf_comb <- left_join(CCA_sf, CCA, by = "name")
efhca_sf_comb <- left_join(EFHCA_sf, EFHCA, by = "name")
urchin_sf_comb <- left_join(urchin_sf, urchin, by = "name")
trawl_sf_comb <- left_join(trawl_sf, trawl, by = "name")
gillnet_sf_comb <- left_join(gillnet_sf, gillnet, by = "name")

# Ensure CRS matches for all shapefiles
mpas_sf_comb  <- st_transform(mpas_sf_comb , st_crs(all_blocks))
hist_mpas_sf_comb <- st_set_crs(hist_mpas_sf_comb, st_crs(all_blocks))  
deca_sf_comb <- st_transform(deca_sf_comb, st_crs(all_blocks))  
cca_sf_comb <- st_transform(cca_sf_comb, st_crs(all_blocks))  
efhca_sf_comb <- st_transform(efhca_sf_comb, st_crs(all_blocks))  
urchin_sf_comb <- st_transform(urchin_sf_comb, st_crs(all_blocks))  
trawl_sf_comb <- st_transform(trawl_sf_comb, st_crs(all_blocks))  
gillnet_sf_comb <- st_transform(gillnet_sf_comb, st_crs(all_blocks))  



#Edit and combine management shapefiles
#current mpas
mpas_sf_comb <-mpas_sf_comb %>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)

#historical mpas
hist_mpas_sf_comb <-hist_mpas_sf_comb %>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)

deca_sf_comb <- deca_sf_comb%>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)
cca_sf_comb <-cca_sf_comb  %>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)
efhca_sf_comb <- efhca_sf_comb%>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)%>%
  mutate(name = paste0(name, " EHFCA"))
urchin_sf_comb <- urchin_sf_comb  %>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)%>%
  mutate(name = paste0(name, " Urchin Closure"))
trawl_sf_comb <- trawl_sf_comb%>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)
gillnet_sf_comb <- gillnet_sf_comb%>%
  select(name,Date.implemented, Year.implemented, Date.ended, Year.ended, CPFV, Cucumber.trawl, 
         Cucumber.dive, Urchin.dive,Set.gillnet, Halibut.trawl, Lobster.trap, Pink.shrimp.trawl,Ridgeback.prawn.trawl,
         Squid.seine,geometry)



#combine all together in one regulation file
regulations_sf<-rbind(mpas_sf_comb ,hist_mpas_sf_comb,deca_sf_comb,cca_sf_comb,efhca_sf_comb,urchin_sf_comb, 
                      trawl_sf_comb,gillnet_sf_comb)
regulations_sf <- regulations_sf %>%
  mutate(
    Date.implemented = as.Date(Date.implemented, format = "%m/%d/%Y"),
    Date.ended = as.Date(Date.ended, format = "%m/%d/%Y")
  )

fishery_cols <- c("CPFV", "Cucumber.trawl", "Cucumber.dive", "Urchin.dive", "Set.gillnet",
                  "Halibut.trawl", "Lobster.trap", "Pink.shrimp.trawl", "Ridgeback.prawn.trawl", "Squid.seine")

regulations_sf <- regulations_sf %>%
  mutate(across(all_of(fishery_cols), ~ as.numeric(as.character(.))))

regulations_sf <- st_zm(regulations_sf, drop = TRUE, what = "ZM")

regulations_sf  <- regulations_sf  %>% 
  mutate(geometry = st_make_valid(geometry))
############


############
#great a time grid
dates <- seq(ymd("2000-01-01"), ymd("2023-12-01"), by = "1 month")
time_grid <- tibble(month_year = format(dates, "%Y-%m"))
############

############
calculate_closure_for_month <- function(date_string, fishery, all_blocks, regs_sf) {
  #Converts the string that is year-month into a full date using lubridate::ymd.
  date <- ymd(paste0(date_string, "-01"))
  
  # Subset active MPAs for that date and fishery = 0 (closed)
  mpas_active <- regs_sf %>%
    #finding any closed areas that started before or on the given date
    filter(Date.implemented <= date, 
           #either has no end date or the end date is after the goal date
           (is.na(Date.ended) | Date.ended >= date), 
           #are closed to the fishery. since fishery is a dynamic name column, !!sym(fishery) uses tidy evaluation
           !!sym(fishery) == 0)
  
  # If there are no closures, return each block, a closed percent of 0
  if (nrow(mpas_active) == 0) {
    return(all_blocks %>% 
             mutate(month_year = date_string, 
                    fishery = (fishery), 
                    closed_pct = 0))
  }
  
  
  # Union only the closed areas (safe because they're all closed to the same fishery)
  mpas_closed_union <- mpas_active %>%
    st_union() %>%
    st_sf() %>%
    mutate(geometry = geometry)  # force geometry column
  
  
  # Intersect blocks with closed MPAs, will compute the area of overlap between each block and the closed mpa
  #calculates area of intersection
  block_intersections <- st_intersection(all_blocks,   mpas_closed_union ) %>%
    mutate(intersect_area = st_area(.))
  
  # Calculates total area of each block
  all_blocks <- all_blocks %>%
    mutate(block_area = st_area(.))
  
  # Sum intersected area per block
  closed_area <- block_intersections %>%
    group_by(block_id) %>%
    summarise(closed_area = sum(intersect_area)) %>%
    left_join(all_blocks %>% st_drop_geometry(), by = "block_id") %>%
    mutate(closed_pct = as.numeric(closed_area / block_area)) %>%
    mutate(month_year = date_string, fishery = fishery) %>%
    select(block_id, month_year, fishery, closed_pct)
  
  # Fill in blocks with 0 closure
  all_blocks <- all_blocks %>%
    st_drop_geometry() %>%
    select(block_id) %>%
    left_join(closed_area, by = "block_id") %>%
    mutate(month_year = date_string,
           closed_pct = replace_na(closed_pct, 0))
  
  all_blocks$fishery <- fishery  # assign the string explicitly
  
  return(all_blocks)
}
  
  fisheries <- c("CPFV", "Cucumber.trawl", "Cucumber.dive", "Urchin.dive","Set.gillnet", "Halibut.trawl",
                 "Lobster.trap", "Pink.shrimp.trawl","Ridgeback.prawn.trawl","Squid.seine")
  
  results <- expand.grid(date = time_grid$month_year, fishery = fisheries, stringsAsFactors = FALSE) %>%
    pmap_dfr(~ calculate_closure_for_month(..1, ..2, all_blocks , regulations_sf))
  
  
  results_with_geom <- left_join(all_blocks, results, by = "block_id")
  
saveRDS(results_with_geom, paste0(reg_data,"monthly_percent_closed_blocks.rds"))
        
  a<-  results_with_geom%>%
    filter(fishery=="Lobster.trap" & block_id=="684")
  
  