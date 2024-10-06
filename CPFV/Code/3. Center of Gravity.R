
library(sp)
library(sf)
library(dplyr)
library(spatstat)
library(raster)
library(sf)
library(terra)
###############


###############
#Upload Data

#distance raster from GIS
distance_raster1<-raster('CPFV/Data/Processed Data/Distance to shore/Dist_shore_rast_clip.tif')
projection(distance_raster1)
plot(distance_raster1)
res(distance_raster1)

# Set values above the threshold to NA
distance_raster1[distance_raster1 > 10000] <- NA

# Set NA values to 0 (or any value you choose)
distance_raster1[is.na(distance_raster1)] <- 0  # Replace NA with 0

#CPFV data that has been filtered already
explor<-readRDS("CPFV/Data/Processed Data/CPFV_filtered.rds")
#block data 

all_blocks<-st_read("CPFV/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv("CPFV/Data/Raw Data/fishingblockstatistics.csv")%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#Distances from shore of each block
block_distances <- read.csv( "CPFV/Data/Raw Data/Block_Distances_shore.csv")%>%
  dplyr:: select(-X)
###############


###############
#spatialize
stats<- explor %>%
  # Step 1: Remove duplicates within each logbookID_vesselID_date and calculate distinct angler_hours
  group_by(logbookID_vesselID_date, angler_hours) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct angler_hours for each logbookID_vesselID_date
  group_by(logbookID_vesselID_date) %>%
  summarize(total_angler_hours = sum(angler_hours), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(logbookID_vesselID_date, year, port.complex, block_id), by = "logbookID_vesselID_date") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, block_id) %>%
  summarize(ntrips = n_distinct(logbookID_vesselID_date),
            total_angler_hours = sum(total_angler_hours, na.rm = TRUE),
            angler_hours_per_trip=(total_angler_hours/ntrips),
            # same as hours_fished but oh well
            .groups = 'drop')%>%
  left_join(all_blocks_df, by = "block_id")%>%
  as.data.frame()%>%
  filter(!is.na("block_long_dd"))
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = 4326)%>% 
  left_join(block_distances, by="block_id")

stats_kd_sf<- st_transform(stats, st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
stats_kd_sf$latitude <- st_coordinates(stats_kd_sf)[, "Y"] 
stats_kd_sf$longitude<- st_coordinates(stats_kd_sf)[, "X"] 
###############