
library(sp)
library(sf)
library(dplyr)
library(spatstat)
library(raster)
library(sf)
library(terra)

###############
#file path
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
shrimp_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/'

#Data
#distance raster from GIS
distance_raster<-raster(paste0(general_data,'Distance to shore 2/Distanc_Shor5.tif')) 

# Define the target CRS using EPSG:3310
target_crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"

#Project to California projection
reprojected_raster <- projectRaster(distance_raster, crs=target_crs)

# Set values above the threshold to 0
reprojected_raster[reprojected_raster< 1] <- NA

#lobster data that has been filtered already
explor<-readRDS(paste0(shrimp_data, "Data/Processed Data/pinktrawl_filtered.rds"))

#block data 
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv(paste0(general_data,"fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#Distances from shore of each block
block_distances <- read.csv( paste0(general_data,"Block_Distances_shore.csv"))%>%
  dplyr:: select(-X)

#spatial data for graphing
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

# State waters
state_waters <- readRDS( paste0(general_data,"CA_state_waters_polyline.Rds"))

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

port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)
###############


###############
#spatialize
stats_kd_sf<- explor %>%
  # Step 1: Remove duplicates within each tow.identifier and calculate distinct angler_duration_min
  group_by(tow.identifier, duration_min) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct dive_duration_min for each tow.identifier
  group_by(tow.identifier) %>%
  summarize(total_min = sum(duration_min), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(tow.identifier, year, port.complex, block_id), by = "tow.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, year, block_id) %>%
  summarize(total_tow_min = sum(total_min, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks, by = "block_id") %>% 
  sf::st_as_sf()%>%
  #remove blocks that dont exist
  st_make_valid()%>%
  #as numeric
  mutate(year=as.numeric(year))%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = 4326)%>% 
  left_join(block_distances, by="block_id")%>%
  st_transform(st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
  
  
stats_kd_sf$latitude <- st_coordinates(stats_kd_sf)[, "Y"] 
stats_kd_sf$longitude<- st_coordinates(stats_kd_sf)[, "X"] 
###############



###############
#weighted COG with distance from shore
cog_results <- stats_kd_sf %>%
  group_by(port.complex, year) %>%
  summarize(
    cog_longitude = sum(total_tow_min * st_coordinates(geometry)[, "X"]) / sum(total_tow_min),
    cog_latitude = sum(total_tow_min * st_coordinates(geometry)[, "Y"]) / sum(total_tow_min),
    cog_dist_shore = sum(total_tow_min * NEAR_DIST) / sum(total_tow_min)
  ) %>%
  sf::st_drop_geometry()

# Print or use final results as needed
saveRDS(cog_results, paste0(shrimp_data,'Data/Processed Data/pinkshrimp_COG_TrapNights_distanceway.rds'))
cog_results_2<-readRDS(paste0(shrimp_data, 'Data/Processed Data/pinkshrimp_COG_TrapNights_distanceway.rds'))

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
    max_lat=target_lat+200
    min_lat=target_lat-200
    min_long=206724.6
    max_long=1106724
    
    # Create spatial points for the corners of the bounding box
    coords <- matrix(c(min_long, min_lat,
                       max_long, min_lat,
                       max_long, max_lat,
                       min_long, max_lat,
                       min_long, min_lat), 
                     ncol = 2, byrow = TRUE)
    # Create SpatialPolygons object
    bbox <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = "bbox")),
                            proj4string = crs(reprojected_raster))
    
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
a<-plot_fun('Eureka')
b<-plot_fun('Monterey')
c<-plot_fun('Morro Bay')
d<-plot_fun('Santa Barbara')

df<-rbind(a,b,c,d)
saveRDS(df, paste0(shrimp_data,'Data/Processed Data/pinkshrimp_COG_distance_results_towhours_final.rds'))
###############


###############
#plotting COG on their own - not doing a function as I am trying to figure out what is going wrong with San Diego
plot_fun<-function(portcomplex){
  COG_port<-readRDS(paste0(shrimp_data,'Data/Processed Data/pinkshrimp_COG_distance_results_towhours_final.rds'))%>%
    filter(port.complex==portcomplex)
  
  location_cog<-st_as_sf(COG_port, coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
  location_cog<-st_transform(location_cog, st_crs("+proj=longlat  +datum=WGS84 +units=m +no_defs"))
  
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
    ggtitle(paste0(portcomplex, ' Fishing Grounds'), subtitle = 'Pink Shrimp Fishery Center of Gravity')+
    scale_color_viridis_c(name = "Year", option = "D") 
  
  filename1 <- paste0(shrimp_data,'Figures/Pink Shrimp/Kernel Density/',portcomplex,'/',portcomplex,'COG.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
  
}

plot_fun('Eureka')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
###############


###############
#Coastwide figure with port complexes and fishing ground shifts
df<-readRDS(paste0(shrimp_data,'Data/Processed Data/pinkshrimp_COG_distance_results_towhours_final.rds'))%>%
  ungroup()%>%
  mutate(year=as.numeric(year))

#filter for first and last year which will be actual points on the plot
COG1<-df%>%
  group_by(port.complex)%>%
  filter(year== min(year) | year== max(year))%>%
  rename(Year=year)

COG_sf<-st_as_sf(COG1, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = "+proj=longlat +datum=WGS84")
COG_sf$Year=as.factor(COG_sf$Year)

COG_sf$port.complex<- factor(COG_sf$port.complex, levels = c( 'Santa Barbara', 'Los Angeles', 'San Diego'))

#get all spatial data for the path
COG2<-st_as_sf(df, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = "+proj=longlat +datum=WGS84")
COG2$Year=as.factor(COG2$year)

# Group by port.complex and combine the geometries
combined_geometries <- COG2 %>%
  group_by(port.complex) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# Cast the combined geometries to LINESTRING
linestring <- st_cast(combined_geometries, "LINESTRING")
linestring $port.complex<- factor(linestring $port.complex, levels = c('Eureka', 'Monterey', 'Morro Bay','Santa Barbara'))
cali<-usa%>%
  filter(name=="California")

p<-ggplot() +
  # Land
  geom_sf(data = cali, fill = "gray", col = "black", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="gray", col="black", linewidth=0.2, inherit.aes = F) +
  # Lines connecting points for the same port complex
  geom_sf(data = linestring, aes(color = port.complex), linewidth = 0.8) +
  # points
  geom_sf(data=COG_sf,aes(shape=Year),size=2)+
  # Crop
  coord_sf(xlim = c(-125, -116), ylim = c(32, 38)) +
  # Axes
  scale_x_continuous(breaks=seq(-125, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 38, 2)) + 
  # Theme
  theme_bw() + base_theme +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text.x = element_blank(),    # Remove x-axis text
    axis.text.y = element_blank(),    # Remove y-axis text
    axis.ticks.x = element_blank(),   # Remove x-axis ticks
    axis.ticks.y = element_blank(),   # Remove y-axis ticks
    panel.border = element_blank(),   # Remove the border around the plotting area
    panel.grid = element_blank(),     # Remove grid lines
    plot.background = element_blank(), # Remove the background color of the plot
    plot.margin = margin(0, 0, 0, 0) # Remove margins around the plot
  ) +
  
  ggtitle(('Pink Shrimp Trawl Fishing Grounds'), subtitle = 'Shifts in Center of Gravity')+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1)

filename1 <- paste0(shrimp_data, 'Figures/Pink Shrimp/Kernel Density/AllPorts_mapped_COG_artsy.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
###############



###############
#Time series analyses

#COG distance from shore
COG3<-df%>%
  mutate(distance_shore_km=cog_dist_shore/1000)%>% #might have to update if the units change with the COG calculation
  left_join(mpa,by="port.complex")

plot_fun<-function(portcomplex){
  
  location<-COG3%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=location, aes(x=year))+
    geom_line(aes(y=distance_shore_km),size=1) +
    labs(y = bquote('Distance from shore'~(km^2))) +
    # Theme
    theme_bw() + 
    base_theme  +
    theme(legend.title = element_blank())+
    annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
    geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "red",size=1)+
    xlab('Year')
  
  filename1 <- paste0(shrimp_data,'Figures/Pink Shrimp/Kernel Density/', portcomplex, '/',portcomplex,'_COG_distanceshore.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Eureka')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
###############

###############
#Time series analyses
#latitude
df1<-st_as_sf(df, coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform( st_crs("+proj=longlat  +datum=WGS84 +units=m +no_defs"))%>% 
  left_join(mpa,by="port.complex")

df1$latitude <- st_coordinates(df1)[, "Y"]  # Extracts latitude
df1$longitude <- st_coordinates(df1)[, "X"]
df1<-df1 %>% sf::st_drop_geometry()

plot_fun<-function(portcomplex){
  
  location<-df1%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=location, aes(x=year))+
    geom_line(aes(y=latitude),size=1)+
    labs(y = ('Latitude °N')) +
    # Theme
    theme_bw() + 
    base_theme  +
    theme(legend.title = element_blank())+
    #adding heatwave information
    annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
    #adding mpa implementation year
    geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "red",size=1)+
    xlab('Year')
  
  
  filename1 <- paste0(shrimp_data,'Figures/Pink Shrimp/Kernel Density/', portcomplex, '/',portcomplex,'_COG_latitude.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Eureka')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
###############


###############
#time series analyses
#graph of all COG latitudes together 

df2<-df1%>%
  mutate(distance_shore_km=cog_dist_shore/1000)
df2$port.complex<- factor(df2$port.complex, levels = c('Eureka', 'Monterey', 'Morro Bay','Santa Barbara'))


p<-ggplot(data=df2, aes(x=year, color=port.complex))+
  geom_line(aes(y=latitude),size=.5)+
  labs(y = ('Latitude °N'), x='Year') +
  scale_color_brewer(palette = 'RdYlBu', direction=-1) +
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank())

filename1 <- paste0(shrimp_data,'Figures/Pink Shrimp/Kernel Density/All Ports_COG_latitude.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
###############

###############
#time series analyses
#graph of all distance to shore together
p<-ggplot(data=df2, aes(x=year, color=port.complex))+
  geom_line(aes(y=distance_shore_km),size=.5)+
  labs(y = ('Distance from shore (km)'), x='Year') +
  scale_color_brewer(palette = 'RdYlBu', direction=-1) +
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank())

filename1 <- paste0(shrimp_data,'Figures/Pink Shrimp/Kernel Density/All Ports_COG_distanceshore.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
###############

