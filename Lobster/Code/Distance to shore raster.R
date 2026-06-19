

library(ggplot2)
library(terra)
library(tidyverse)
library(sp)
library(sf)
library(raster)

#upload shoreline data
shoreline<-st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Raw Data/Shoreline/shoreline.shp")%>%
  st_transform(crs = 3310) #California Albers projection which is good for distance calculations

shoreline_lines <- shoreline[st_geometry_type(shoreline) == "LINESTRING", ]
combined_shoreline <- st_union(shoreline_lines)%>%
  st_cast("LINESTRING")

plot(combined_shoreline)
crs(combined_shoreline)


#vectorize the combined shoreline sf file to a spatvector
v <- terra::vect(combined_shoreline)

# Create a raster with a desired resolution
r <- terra::rast(terra::ext(v), res = 100) # Adjust resolution as needed

# Rasterize the shoreline to create a binary raster (1 where the shoreline is)
shoreline_raster <- terra::rasterize(v, r, field = 1)
plot(shoreline_raster)
# Calculate distance from shoreline
distance_to_shoreline <- terra::distance(shoreline_raster)


r <- rast(res = 500, nrows = 1000, ncols = 1000, ext = ext(v))
r <- rasterize(v, r)
d <- distance(r)
plot(d)+
  combined_shoreline

ggplot() +
  # Land
  geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=combined_shoreline)


distance_to_shoreline<-terra::distance(combined_shoreline)





############START HERE

#distance raster from GIS
distance_raster<-raster('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/Distance to shore 2/Distanc_Shor3.tif')
plot(distance_raster)
# Set values above the threshold to 0
distance_raster[distance_raster== 0] <- NA


# Convert to SpatRaster
spat_raster <- rast(distance_raster)


# Define the target CRS using EPSG:3310
target_crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"

#Project to California projection
reprojected_raster <- projectRaster(distance_raster, crs=target_crs)
plot(reprojected_raster)

# Set values above the threshold to 0
reprojected_raster[reprojected_raster< 1] <- NA

#lobster data that has been filtered already
explor<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/lobster_filtered.rds")

#block data 

all_blocks<-st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(3310))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Raw Data/fishingblockstatistics.csv")%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)
all_blocks$block_id=as.double(all_blocks$block_id)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#Distances from shore of each block
block_distances <- read.csv( "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data//Raw Data/Block_Distances_shore.csv")%>%
  dplyr:: select(-X)

#spatial data for graphing
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs")

# State waters
state_waters <- readRDS( "/Users/kinse/Box/ca mpas climate/Lobster/Data/Raw Data/CA_state_waters_polyline.Rds")

#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)


# base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text = element_text(size=10),
                    plot.tag =element_text(size=9),
                    # plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))
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

stats_kd_sf<- st_transform(stats, (crs="+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs" ))

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

###############


#getting longitude from distance to shore
plot_fun<-function(portcomplex){ 
  #filter location
  location<-cog_results%>%
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
    max_lat=target_lat+200
    min_lat=target_lat-200
    min_long=364693.3
    max_long=1057213
    
    # Create spatial points for the corners of the bounding box
    coords <- matrix(c(min_long, min_lat,
                       max_long, min_lat,
                       max_long, max_lat,
                       min_long, max_lat,
                       min_long, min_lat), 
                     ncol = 2, byrow = TRUE)
    # Create SpatialPolygons object
    bbox <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = "bbox")),
                            proj4string = crs(reprojected_raster))%>%
            st_as_sf()
 
    # Extract values from the raster at the specified latitude
    values_along_latitude <- raster::extract(reprojected_raster, bbox)
    
    # Convert the list of lists to a numeric vector
    values_along_latitude <- unlist(values_along_latitude)
    
    # Find which raster cell it is (index of closest value)
    closest_index <- which.min(abs(values_along_latitude - target_distance))
    
    # Calculate the middle longitude of the raster cell
    cell_cord<-xyFromCell(reprojected_raster,closest_index)
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


location_cog<-st_as_sf(c, coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
location_cog<-st_transform(location_cog, st_crs("+proj=longlat  +datum=WGS84 +units=m +no_defs"))

#extracting coordinates 
coordinates<-st_coordinates(location_cog)
# Create a point pattern (ppp object)
minx<-(min(coordinates[,1]))
maxx<-(max(coordinates[,1]))
miny<-(min(coordinates[,2]))
maxy<-(max(coordinates[,2]))

ggplot() +
  # Land
  geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
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
  scale_color_viridis_c(name = "Year", option = "D") 
