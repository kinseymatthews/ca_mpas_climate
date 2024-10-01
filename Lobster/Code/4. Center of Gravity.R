
library(sp)
library(sf)
library(dplyr)
library(spatstat)
library(raster)
library(sf)
library(terra)

###############
#Data

#distance raster from GIS
distance_raster1<-raster('Lobster/Data/Processed Data/Distance to shore/Dist_shore_rast_clip.tif')
projection(distance_raster1)
plot(distance_raster1)
res(distance_raster1)

# Set values above the threshold to NA
distance_raster1[distance_raster1 > 10000] <- NA

# Set NA values to 0 (or any value you choose)
distance_raster1[is.na(distance_raster1)] <- 0  # Replace NA with 0



#lobster data that has been filtered already
explor<-readRDS("Lobster/Data/Processed Data/lobster_filtered.csv")

#block data 

all_blocks<-st_read("Lobster/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv("Lobster/Data/Raw Data/fishingblockstatistics.csv")%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#Distances from shore of each block

block_distances <- read.csv( "Lobster/Data/Raw Data/Block_Distances_shore.csv")%>%
  dplyr:: select(-X)
###############



###############
#spatialize
stats<- explor %>%
  #summarize by year, port, block
  group_by(year,port.complex,block_id)%>%
  summarize(ntrips=n(),
            nvessels=n_distinct(vessel_id),
            trap_nights=sum(trap_nights))%>%
  ungroup()%>%
  left_join(all_blocks_df, by = "block_id")%>%
  as.data.frame()%>%
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
    cog_longitude = sum(trap_nights * st_coordinates(geometry)[, "X"]) / sum(trap_nights),
    cog_latitude = sum(trap_nights * st_coordinates(geometry)[, "Y"]) / sum(trap_nights),
    cog_dist_shore = sum(trap_nights * NEAR_DIST) / sum(trap_nights)
  ) %>%
  sf::st_drop_geometry()

# Print or use final results as needed
print(cog_results)
write.csv(cog_results, 'Lobster/Data/Processed Data/COG_TrapNights_distanceway.csv')
cog_results_2<-read.csv('Lobster/Data/Processed Data/COG_TrapNights_distanceway.csv')

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
    plot(bbox)
    
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
a<-plot_fun("Santa Barbara")
b<-plot_fun("Los Angeles")
c<-plot_fun("San Diego")

df<-rbind(a,b,c)
write.csv(df,'COG_distance_results_traphours_final.csv')

###############




###############
#plotting COG on their own - not doing a function as I am trying to figure out what is going wrong with San Diego

COG_port<-read.csv('COG_distance_results_traphours_final.csv')%>%
  dplyr:: select(-X)%>%
  filter(port.complex=="San Diego")
 
location_cog<-st_as_sf(COG_port, coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
location_cog<-st_transform(location_cog, st_crs("+proj=longlat  +datum=WGS84 +units=m +no_defs"))
location_cog$year=as.factor(location_cog$year)



#extracting coordinates 
coordinates<-st_coordinates(location_cog)
# Create a point pattern (ppp object)
minx<-(min(coordinates[,1]))
maxx<-(max(coordinates[,1]))
miny<-(min(coordinates[,2]))
maxy<-(max(coordinates[,2]))

p<-ggplot() +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # State waters
  geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
  # points
  geom_sf(data=location_cog, aes(color=(year)),size=.75)+
  # Crop
  coord_sf(xlim = c(minx-.8, maxx+.8), ylim = c(miny-.8, maxy+.8)) +
  # Axes
  scale_x_continuous(breaks=seq(minx-.5, maxx+.5, .5),labels=scales::number_format(accuracy=0.50)) +
  scale_y_continuous(breaks=seq(maxx+.5, maxy+.5, .5),labels=scales::number_format(accuracy=0.50)) + 
  # Theme
  theme_bw() + base_theme +
  #theme(axis.title.x=element_blank())+
  ggtitle(('San Diego Fishing Grounds'), subtitle = 'Lobster Fishery Center of Gravity')+
  scale_color_viridis_d(name = "Year", option = "D") 

###############



###############
#Alternative Distance to shore raster to create in R - this is a combination of random ideas and imcomplete code

#upload shoreline data
shoreline<-st_read("Lobster/Data/Raw Data/Shoreline/shoreline.shp")%>%
  st_transform(crs = 3310) #California Albers projection which is good for distance calculations

st_geometry_type(shoreline)
shoreline_lines <- shoreline[st_geometry_type(shoreline) == "LINESTRING", ]
combined_shoreline <- st_union(shoreline_lines)
combined_shoreline <- st_cast(combined_shoreline, "LINESTRING")


plot(combined_shoreline)
res(D128pix)

v <- terra::vect(combined_shoreline)
r <- rast(v, res = 10)
r <- rasterize(v, r)
d <- distance(r)
plot(d)





# Define the extent (xmin, xmax, ymin, ymax)
extent_values <- c(-126, -116, 32, 42)

# Create a raster template
raster_template <- raster(extent(extent_values), res = 10)  # Set resolution as needed

# Rasterize the shoreline
shoreline_raster <- raster::rasterize(st_geometry(shoreline_sp), raster_template, field = 1, fun = "first", background = NA)

# Calculate distance from shoreline
distance_raster <- distance(shoreline_raster)

###############
