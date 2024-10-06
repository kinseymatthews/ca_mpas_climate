
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
  group_by( year,port.complex, block_id) %>%
  summarize(ntrips = n_distinct(logbookID_vesselID_date),
            total_angler_hours = sum(total_angler_hours, na.rm = TRUE),
            angler_hours_per_trip=(total_angler_hours/ntrips),
            # same as hours_fished but oh well
            .groups = 'drop')%>%
  left_join(all_blocks_df, by = "block_id")%>%
  as.data.frame()%>%
  filter(!is.na(block_long_dd))%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = 4326)%>% 
  left_join(block_distances, by="block_id")

stats_kd_sf<- st_transform(stats, st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
stats_kd_sf$latitude <- st_coordinates(stats_kd_sf)[, "Y"] 
stats_kd_sf$longitude<- st_coordinates(stats_kd_sf)[, "X"] 
###############


###############
#weighted COG with distance from shore
cog_results <- stats_kd_sf %>%
  group_by(port.complex, year) %>%
  summarize(
    cog_longitude = sum(total_angler_hours * st_coordinates(geometry)[, "X"]) / sum(total_angler_hours),
    cog_latitude = sum(total_angler_hours * st_coordinates(geometry)[, "Y"]) / sum(total_angler_hours),
    cog_dist_shore = sum(total_angler_hours * NEAR_DIST) / sum(total_angler_hours)
  ) %>%
  sf::st_drop_geometry()

# Print or use final results as needed
print(cog_results)
saveRDS(cog_results, 'CPFV/Data/Processed Data/COG_TrapNights_distanceway.rds')
cog_results_2<-readRDS('CPFV/Data/Processed Data/COG_TrapNights_distanceway.rds')

#getting longitude from distance to shore
plot_fun<-function(portcomplex){ 
  #filter location
  location<-cog_results_2%>%
    filter(port.complex==portcomplex)
  
  #get unique years in dataset
  unique_years<-unique(location$year)
  
  #create empty dataframe to add to after each year
  datalist=list()
  
  #Loop through each year 
  for(year in unique_years){
    
    #subset data for the current year
    year_data<-location[location$year==year,]
    #get target latitude and distance
    target_lat<-year_data$cog_latitude
    target_distance<-year_data$cog_dist_shore
    target_longitude<-year_data$cog_longitude
    #get bounding box - if coordinate system changes have to change. longitude should cover the entire range
    max_lat=target_lat+15
    min_lat=target_lat-15
    min_long=325027.4
    max_long=1101227
    
    # Create spatial points for the corners of the bounding box
    coords <- matrix(c(min_long, min_lat,
                       max_long, min_lat,
                       max_long, max_lat,
                       min_long, max_lat,
                       min_long, min_lat), 
                     ncol = 2, byrow = TRUE)
    # Create SpatialPolygons object
    bbox <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = "bbox")),
                            proj4string = crs(distance_raster1))
  
    
    # Extract values from the raster at the specified latitude
    values_along_latitude <- extract(distance_raster1, bbox)
    # Convert the list of lists to a numeric vector
    values_along_latitude <- unlist(values_along_latitude)
    
    # Find which raster cell it is (index of closest value)
    closest_index <- which.min(abs(values_along_latitude - target_distance))
    
    # Get the middle longitude of the raster cell
    # Calculate the middle longitude of the raster cell
    #WRONG: middle_longitude <- (distance_raster@extent@xmin + ((closest_index - 1) %% ncol(distance_raster)) * res(distance_raster)[1] + res(distance_raster)[1] / 2)
    cell_cord<-xyFromCell(distance_raster1,closest_index)
    longitude <- cell_cord[1]
    df <- year_data %>%
      mutate(longitude_calculated=longitude)
    
    # Append current year's results to combined_df
    datalist[[year]] <- df
    
  }
  all.data<- dplyr::bind_rows(datalist)
}
a<-plot_fun("Bodega Bay")
b<-plot_fun("Eureka")
c<-plot_fun("Fort Bragg")
d<-plot_fun("San Francisco")
e<-plot_fun("Monterey")
f<-plot_fun("Morro Bay")
g<-plot_fun("Santa Barbara")
h<-plot_fun("Los Angeles")

df<-rbind(a,b,c,d,e,f,g,h)
saveRDS(df,'CPFV/Data/Processed Data/COG_distance_results_traphours_final.rds')
###############