library(tidyverse)
library(sf)
library(sp)
library(spatstat)
library(raster)
library(magick)
library(ggforce)
library(RColorBrewer)


#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
gillnet_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/'
#################

#################
#read in set gillnet data
explor<-readRDS(paste0(gillnet_data,"Data/Processed Data/setgillnet_filtered.rds"))

#cog data
COG<-readRDS(paste0(gillnet_data, 'Data/Processed Data/setgillnet_COG_distance_results_towhours_final.rds'))
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = 3310)
COG_sf$year=as.factor(COG_sf$year)

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
usa_mexico_sf_proj<- st_transform(usa_mexico_sf, crs = 3310)
# Union all geometries into one polygon
usa_mexico_union <- st_union(usa_mexico_sf_proj)


#port codes
port<-read.csv( paste0(general_data,"Port Codes.csv"))

# State waters
state_waters <- readRDS( paste0(general_data,"CA_state_waters_polyline.Rds"))
state_waters <- st_transform(state_waters, crs = 3310)

# block data
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
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)
############

############
#Kernel Densities - All years combined
sf_df<- explor %>%
  mutate(year = as.numeric(as.character(year)))%>%
  group_by(port.complex)%>%
  #filter for  the last  5 years
  filter(year >= max(year, na.rm = TRUE) - 4)%>%
  ungroup()%>%
  # Step 1: Remove duplicates within each tow.identifier and calculate distinct angler_duration_min
  group_by(set.identifier, soak_hr_num) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct dive_duration_min for each set.identifier
  group_by(set.identifier) %>%
  summarize(total_hr = sum(soak_hr_num), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(set.identifier, year, port.complex, block_id), by = "set.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex,  block_id) %>%
  summarize(total_set_hr = sum(total_hr, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks, by = "block_id") %>% 
  #remove block IDs of NA
  filter(!is.na(block_long_dd))%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  #remove any bad spatial blocks
  filter(!st_is_empty(.))

sf_df_projected <- st_transform(sf_df, crs = 3310)


unique_complex<-unique(sf_df_projected$port.complex)

#contours for shapefile
all_contours <- list()


#Loop through each year and calculate kernel density
for(portcomplex in unique_complex){
  
  location <- sf_df_projected%>%
    filter(port.complex==portcomplex)
  
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(location)
  #weights
  weights<- location$total_set_hr
  
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
  contour_level_75_lines <- rasterToContour(extended_raster, levels = c(contour_level_75))
  
  
  # Convert contour lines to sf polygons
  contour_75_sf <- st_as_sf(contour_level_75_lines)
  st_crs(contour_75_sf) <-  3310
  
  # Convert the LINESTRING to POLYGON if needed
  # Attempt to cast to POLYGON
  closed_poly <- tryCatch(
    st_cast(contour_75_sf, "POLYGON"),
    error = function(e) {
      message("st_cast failed, using st_polygonize instead")
      st_polygonize(contour_75_sf)
    }
  )
  
  # Ensure valid geometry
  closed_poly <- st_make_valid(closed_poly)
  
  
  
  # Plot the closed polygon to verify the closure
  plot(closed_poly)
  
  # Extract only the geometry of the USA/Mexico shapefile
  usa_geometry <- st_geometry(usa_mexico_union)
  
  # First calculate the intersection (land area)
  intersection <- st_intersection(closed_poly, usa_geometry)
  
  # Check if there is an intersection
  if (nrow(intersection) > 0) {
    # If there is an intersection, manually subtract the land portion from the ocean using st_difference
    closed_poly_no_land <- st_difference(closed_poly, intersection)
    plot(closed_poly_no_land)
    
    # Compute the 75% volume area (land portion removed)
    area_75_m2 <- sum(st_area(closed_poly_no_land))
  } else {
    closed_poly_no_land <- closed_poly
    plot(closed_poly_no_land)
    # If there is no intersection with land, calculate the volume directly from the closed ocean polygon
    area_75_m2 <- sum(st_area(closed_poly))
  }
  # Convert area from square meters to square kilometers
  area_75_km2 <- as.numeric(area_75_m2) / 1e6  # Convert units object to numeric and divide by 1e6
  #compute area of mpas within 75%
  #turn mpas into polygons and calculate interactions between the two
  
  
  closed_poly_no_land <- st_make_valid(closed_poly_no_land)
  
  
  closed_poly_no_land$portcomplex <- portcomplex
  closed_poly_no_land$total_area <- area_75_km2
  
  
  # Save in list
  all_contours[[portcomplex]] <- closed_poly_no_land
  
}  

# ---- Save merged contours to contour directory ----
# Combine all contours
merged_contours <- all_contours |>
  lapply(\(x) dplyr::select(x, level, portcomplex, total_area)) |>
  dplyr::bind_rows()

# Save to file
st_write(merged_contours, file.path(gillnet_data, "Data/Processed Data/set_all_portcomplex_contours_75_last5years.gpkg"),
         delete_layer = TRUE, quiet = TRUE)



