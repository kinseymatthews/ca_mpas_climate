library(tidyverse)
library(sf)

##################
#Read in data
#filepaths
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
cucumber_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/'

#edited lobster data from the block script
explor<-readRDS(paste0(cucumber_data,"Data/Processed Data/seacucumber_filtered.rds"))

#cog data
COG<-readRDS(paste0(cucumber_data,'Data/Processed Data/COG_distance_results_divehours_final_cucumber.rds'))
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = 3310)
COG_sf$year=as.factor(COG_sf$year)

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
usa_mexico_sf_proj<- st_transform(usa_mexico_sf, crs = 3310)

#port codes
port<-read.csv( paste0(general_data,"Port Codes.csv"))

# State waters
state_waters <- readRDS( paste0(general_data,"CA_state_waters_polyline.Rds"))
state_waters <- st_transform(state_waters, crs = 3310)

# block data
###### read in block data
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

##### read in bathymetry data with blocks
bathy.data<-read.csv(paste0(general_data,"fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

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

#adding in mpa implementation data
port.complex=c("Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)
############

############

sf_df <- explor %>%
  mutate(year = as.numeric(as.character(year)))%>%
  group_by(port.complex)%>%
  #filter for  the last  5 years
  filter(year >= max(year, na.rm = TRUE) - 4)%>%
  ungroup()%>%
  group_by(dive.identifier) %>%
  summarize(total_hours = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
  left_join(explor %>% dplyr::select(dive.identifier, year, port.complex, block_id), by = "dive.identifier") %>%
  distinct() %>%
  group_by(port.complex, block_id) %>%
  summarize(total_dive_hours = sum(total_hours, na.rm = TRUE), .groups = 'drop') %>%
  left_join(all_blocks_df, by = "block_id") %>%
  filter(!is.na(block_long_dd)) %>%
  st_as_sf(coords = c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  filter(!st_is_empty(.))

sf_df$port.complex <- factor(sf_df$port.complex, levels = c("Santa Barbara", "Los Angeles", "San Diego"))

sf_df_projected <- st_transform(sf_df, crs = 3310)

unique_complex<-unique(sf_df_projected$port.complex)

#contours for shapefile
all_contours <- list()


#Loop through each year and calculate kernel density
for(this_complex in unique_complex){
  
  location <- sf_df_projected%>%
    filter(port.complex==this_complex)
  
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(location)
  #weights
  weights<- location$total_dive_hours
  
  # Create a point pattern (ppp object)
  minx<-(min(coordinates[,1], na.rm = TRUE))
  maxx<-(max(coordinates[,1], na.rm = TRUE))
  miny<-(min(coordinates[,2], na.rm = TRUE))
  maxy<-(max(coordinates[,2], na.rm = TRUE))
  
  # Define the tolerance
  tolerance <- 0.0000001
  
  if (abs(minx - maxx) < tolerance) {
    minx <- minx - 0.025
    maxx <- maxx + 0.025
  }
  
  
  if (abs( miny - maxy)< tolerance) {
    miny <- miny - 0.025
    maxy <- maxy + 0.025
  }
  
  # Expand bounding box
  minx <- minx - 20000
  maxx <- maxx + 20000
  miny <- miny - 20000
  maxy <- maxy + 20000
  
  points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
  
  # Compute kernel density estimation using raw and logcdata
  sigma<-bw.diggle(points_ppp)
  
  density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE , edge=TRUE)
  
  # Convert density to a raster (assuming a reasonable raster resolution)
  density_raster <- as(raster(as.im(density)), "RasterLayer")
  
  
  # Extend the extent of the density raster by 200 meters
  buffer_distance <- 20000  # 200 meters
  extended_extent <- extend(extent(density_raster), buffer_distance)
  
  # Create a new raster with the extended extent
  extended_raster <- extend(density_raster, extended_extent, value = 0)
  
  # Mask the density raster with the land raster
  density_masked <- mask(extended_raster, usa_mexico_sf_proj, inverse=TRUE)
  
  # Convert density to a data frame
  density_df <- as.data.frame(rasterToPoints(density_masked))
  names(density_df) <- c("x", "y", "value")
  
  # Calculate the cumulative distribution function (CDF)
  density_values <- sort(density_df$value, decreasing = TRUE)
  cdf <- cumsum(density_values) / sum(density_values)
  
  # Find the contour level corresponding to 75% volume
  contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
  
  # Create contour lines from the density raster
  contour_level_75_lines <- rasterToContour(density_masked, levels = c(contour_level_75))
  
  
  # Convert contour lines to sf polygons
  contour_75_sf <- st_as_sf(contour_level_75_lines)
  st_crs(contour_75_sf) <-  3310
  
  #save into dataframe
  contour_75_sf$portcomplex <- this_complex
  all_contours[[this_complex]] <-    contour_75_sf
}  

# ---- Save merged contours to contour directory ----
# Combine all contours
merged_contours <- do.call(rbind, all_contours)


# Save to file
st_write(merged_contours, file.path(cucumber_data, "Data/Processed Data/cucumber_all_portcomplex_contours_75_last5years.gpkg"),
         delete_layer = TRUE, quiet = TRUE)


