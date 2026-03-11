library(sp)
library(sf)
library(spatstat)
library(raster)
library(sf)
library(terra)
library(tidyverse)
###############


############### Base spatial extent of grid based on blocks
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#upload bathy data
bathy.data<-read.csv(paste0(general_data, "fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)%>%
  mutate(block_id=as.factor(block_id))


all_blocks<-st_read(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()
all_blocks<- st_transform(all_blocks, crs = st_crs(3310))%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")%>%
  dplyr::select(-Max_Depthm,-Min_Depthm,-Avg_Depthm, -blck_sq,-block_d)%>%
  mutate(block_id=as.factor(as.numeric(block_id)))

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")

all_blocks_df<-(all_blocks)%>%
  st_drop_geometry()


#upload management regulations
regulations<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/monthly_percent_closed_blocks.rds")%>%
  mutate(block_id=as.factor(block_id))%>%
  st_drop_geometry()%>%
  mutate(
    # split "1_2000" into month and year
    year = sub(".*-(\\d+)$", "\\1", month_year),
    month = sub("^(\\d+)-.*", "\\1", month_year),
    # build a proper date (always first day of month)
    date = as.Date(paste(year, month, "01", sep = "-"))
  )%>%
  dplyr::select(-year,-month,-month_year)%>%
  mutate(fishery = recode(fishery, "Halibut.trawl" = "California.halibut.trawl"))
regulations$fishery <- gsub("\\.", " ", regulations$fishery)

#upload SST and BT
temp<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_sst_bt_GLORYS.rds")%>%
  mutate(block_id=as.factor(block_id))

#upload distance to shore
dist_shore<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to shore/data/processed/block_shore_distances_complete.rds")%>%
  mutate(block_id=as.factor(block_id))
#upload distance to port complex
dist_complex<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")%>%
  rename(port.complex=port_complex, distance_complex_km=average.distance.km)%>%
  dplyr::select(-average.distance.m)%>%
  mutate(block_id=as.factor(as.numeric(block_id)))

#upload ex vessel price info
exvessel<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/allfisheries_exvessel_port_year_month_adjusted.rds")%>%
  dplyr::select( port.complex, month_year,fishery, year, cost_per_pound_adjusted)%>%
  mutate(year=as.double(year))%>%
  mutate(fishery=case_when(fishery == "Halibut trawl" ~ "California halibut trawl",
                           TRUE ~ fishery))%>%
  mutate(
    # split "1_2000" into month and year
    year = sub(".*_(\\d+)$", "\\1", month_year),
    month = sub("^(\\d+)_.*", "\\1", month_year),
    # build a proper date (always first day of month)
    date = as.Date(paste(year, month, "01", sep = "-"))
  )%>%
  dplyr::select(-year,-month,-month_year)
#upload diesel price info
fuel<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Fuel price/data/processed/allportcomplexes_dieselprice_monthly_adjusted.rds")%>%
  mutate(
    # split "1_2000" into month and year
    year = sub(".*_(\\d+)$", "\\1", month_year),
    month = sub("^(\\d+)_.*", "\\1", month_year),
    # build a proper date (always first day of month)
    date = as.Date(paste(year, month, "01", sep = "-"))
  )%>%
  dplyr::select(-year,-month,-month_year)



#expand dataframe across fisheries and across dates
#Monthly sequence from Jan 2000 – Dec 2023

#create columns for fisheries
fisheries <- c(
  "CPFV",
  "Cucumber trawl",
  "Cucumber dive",
  "Urchin dive",
  "California halibut trawl",
  "Set gillnet",
  "Squid seine",
  "Lobster trap",
  "Pink shrimp trawl",
  "Ridegback prawn trawl"
)
# Keep only the unique block-level attributes
block_info <- all_blocks_df %>% 
              distinct(block_id, .keep_all = TRUE)

# Build full expansion: block × date × fishery
dates <- seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "month")
port.complexes<-c("Eureka","Fort Bragg","Bodega Bay","San Francisco","Monterey",
                "Morro Bay","Santa Barbara","Los Angeles","San Diego")

prediction_grid <- expand_grid(
  block_id = unique(all_blocks$block_id),
  date = dates,
  fishery = fisheries,
  port.complex=port.complexes
)%>%
  mutate(month_year = format(date, "%m-%Y")) 


# Join back the block attributes
prediction_grid2 <- prediction_grid %>%
  left_join(block_info, by = "block_id")%>%
  left_join(dist_complex, by=c('port.complex','block_id'))%>%
  left_join(temp, by=c('date','block_id'))%>%
  left_join(regulations, by=c('fishery','block_id','date'))%>%
  left_join(dist_shore, by=c('block_id'))%>%
  left_join(fuel, by = c("port.complex", "date"))%>%
  left_join(exvessel, by=c("port.complex", "date","fishery"))%>%
  filter(block_id!="101") %>%
  mutate(metier=paste(port.complex, fishery, sep = "_"))#create metier column

#read in actual data that we will be using
fisheries<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/summarized_fishery_data_ready4model.rds")%>%
  filter(year>1999)%>%
  mutate(metier=paste(port.complex, fishery, sep = "_"))
#collect unique metiers that we are modeling
metiers<-unique(fisheries$metier)

#filter predicition grid for these metiers to remove metiers that we are not looking at 
prediction_grid3<-prediction_grid2%>%
  filter(metier %in% metiers)

saveRDS(prediction_grid3, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/prediction_grid_df.rds")


