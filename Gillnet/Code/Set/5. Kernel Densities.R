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

plot_fun<-function(portcomplex){ 
  
  #filtering for location
  location<-sf_df_projected  %>%
    filter(port.complex==portcomplex)
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(location)
  weights<-location$total_set_hr
  
  # Create a point pattern (ppp object)
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
  density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE)
  
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
  
  # Normalize density values to the range 0-1
  density_df$value <- scales::rescale(density_df$value)
  
  # Calculate the cumulative distribution function (CDF)
  density_values <- sort(density_df$value, decreasing = TRUE)
  cdf <- cumsum(density_values) / sum(density_values)
  
  # Find the contour level corresponding to 75% volume
  contour_level <- density_values[which.min(abs(cdf - 0.75))]
  
  #map
  p<-ggplot() +
    geom_raster(data = density_df, aes(x=x,y=y,fill = value)) +
    
    scale_fill_gradientn(name = "Soak Time", 
                         colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                         limits = c(0, max(density_df$value)),  # Adjust limits as needed
                         na.value = "white") +  # Set NA values to white
    
    # Add contour lines at 75% volume boundary
    geom_contour(data = density_df, aes(x = x, y = y, z = value), 
                 breaks = contour_level, color = "black", size = 0.75) +
    # Land
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    # Crop
    coord_sf(xlim = c(-500000,300000), ylim = c(-600000, 450000)) +
    # Axes
   # scale_x_continuous(breaks=seq(-125, -117, 2)) +
    #scale_y_continuous(breaks=seq(32, 44, 2)) + 
    # Theme
    theme_bw() + base_theme +
    ggtitle(paste0( portcomplex, ' Fishing Grounds') , subtitle="Set Gillnet Fishery With 75% volume contour")+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  
  name=paste0(gillnet_data,'Figures/Set/Kernel Density/',portcomplex,'/',portcomplex,' Kernel Densiy_All Years_test.jpg')
  ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)
  
}
plot_fun('Bodega Bay')
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
plot_fun('San Diego')
############

############
#Kernel Densities - years separate
sf_df<- explor %>%
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
  group_by( port.complex,  year, block_id) %>%
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

plot_fun<-function(portcomplex){ 
  
  location <- sf_df_projected%>%
    filter(port.complex==portcomplex)%>%
    group_by(year)%>%
    filter(n() > 1) %>% #kernel densities need more than one block in a year to run.
    ungroup()
  
  #get unique years in dataset
  unique_years<-unique(location$year)
  
  #Loop through each year and calculate kernel density
  for(year in unique_years){
    
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(year_data)
    #weights
    weights<-year_data$total_tow_min
    
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
    density_masked <- mask(extended_raster, usa_mexico_sf, inverse=TRUE)
    
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
    
    # Normalize density values to the range 0-1
    density_df$value <- scales::rescale(density_df$value)
    
    a<-COG_sf%>%
      filter(port.complex==portcomplex)%>%
      filter(year %in% unique_years)
    
    #subset data for the current year
    b<-a[a$year==year,]
    #extracting coordinates and weights
    coordinates<-st_coordinates(a)
    # Create bounding block for plot
    minx<-(min(coordinates[,1], na.rm = TRUE))
    maxx<-(max(coordinates[,1], na.rm = TRUE))
    miny<-(min(coordinates[,2], na.rm = TRUE))
    maxy<-(max(coordinates[,2], na.rm = TRUE))
    
    
    p<-ggplot() +
      geom_raster(data = density_df, aes(x = x, y = y, fill = value)) +
      
      scale_fill_gradientn(name = "Soak Hours", 
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                           limits = c(0, max(density_df$value)),  # Adjust limits as needed
                           na.value = "white") +  # Set NA values to white
      # Add contour lines at 75% volume boundary
      geom_sf(data = contour_75_sf, color = "black", linewidth = 1)  +
      
      # Land
      geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      #points COG
      # geom_sf(data=b,size=5, fill="white",color="black",pch=21)+
      # Crop the plot using the bounding box of the contour
      coord_sf(xlim = c(minx-50000, maxx+50000), ylim = c(miny-50000, maxy+50000)) +
      # Axes
      #  scale_x_continuous(breaks=seq(minx-20000, maxx+20000, 20000),labels=scales::number_format(accuracy=20000)) +
      #  scale_y_continuous(breaks=seq(miny-20000, maxy+20000, 20000),labels=scales::number_format(accuracy=20000)) + 
      
      
      ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle=paste0('Set Gillnet Fishery: Year ', year, ', with 75% volume contour' ))+
      theme_bw() + base_theme+ 
      theme(axis.title = element_blank())
    
    
    name=paste0(gillnet_data,'Figures/Set/Kernel Density/',portcomplex,'/',portcomplex, year,'.jpg')
    ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
  }
}
plot_fun('Bodega Bay')
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
plot_fun('San Diego')


#importing yearly images and creating an animation
years<-c(1984:1987)
frames<-paste0(gillnet_data,"Figures/Set/Kernel Density/Bodega Bay/Bodega Bay",years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m, paste0(gillnet_data,"Figures/Set/Kernel Density/Bodega Bay/Bodega Bay Kernel Densities_Animation.gif"))

years<-c(1984:1994,1997,1998,2007,2008)
frames<-paste0(gillnet_data,"Figures/Set/Kernel Density/San Francisco/San Francisco",years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m, paste0(gillnet_data,"Figures/Set/Kernel Density/San Francisco/San Francisco Kernel Densities_Animation.gif"))

years<-c(1984:2002)
frames<-paste0(gillnet_data, "Figures/Set/Kernel Density/Monterey/Monterey", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(gillnet_data,"Figures/Set/Kernel Density/Monterey/Monterey Kernel Densities_Animation.gif"))

years<-c(1984:2007,2017,2018)
frames<-paste0(gillnet_data,"Figures/Set/Kernel Density/Morro Bay/Morro Bay", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(gillnet_data,"Figures/Set/Kernel Density/Morro Bay/Morro Bay Kernel Densities_Animation.gif"))

years<-c(1984:2009,2011,2012,2017:2022)
frames<-paste0(gillnet_data,"Figures/Set/Kernel Density/Santa Barbara/Santa Barbara", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(gillnet_data,"Figures/Set/Kernel Density/Santa Barbara/Santa Barbara Kernel Densities_Animation.gif"))

years<-c(1984:2009,2017:2022)
frames<-paste0(gillnet_data,"Figures/Set/Kernel Density/Los Angeles/Los Angeles", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(gillnet_data,"Figures/Set/Kernel Density/Los Angeles/Los Angeles Kernel Densities_Animation.gif"))


years<-c(1984:2009,2012,2017:2022)
frames<-paste0(gillnet_data,"Figures/Set/Kernel Density/San Diego/San Diego", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(gillnet_data,"Figures/Set/Kernel Density/San Diego/San Diego Kernel Densities_Animation.gif"))
############


############
#time series analyses
#volume contours through time

plot_fun<-function(portcomplex){ 
  
  location <- sf_df_projected%>%
    filter(port.complex==portcomplex)%>%
    group_by(year)%>%
    filter(n() > 1) %>%
    ungroup()
  
  #get unique years in dataset
  unique_years<-unique(location$year)
  
  #empty list
  datalist=list()
  
  #Loop through each year and calculate kernel density
  for(year in unique_years){
    
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(year_data)
    weights<-year_data$total_tow_min
    
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
    
    # Compute kernel density estimation using raw data
    sigma<-bw.diggle(points_ppp) #increasing bandwidth
    density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE, edge=TRUE)
    
    # Convert density to a raster 
    density_raster <- as(raster(as.im(density)), "RasterLayer")
    
    # Extend the extent of the density raster by 200 meters
    buffer_distance <- 20000  # 200 meters
    extended_extent <- extend(extent(density_raster), buffer_distance)
    
    # Create a new raster with the extended extent
    extended_raster <- extend(density_raster, extended_extent, value = 0)
    
    
    # Mask the density raster with the land raster
    density_masked <- mask( extended_raster, usa_mexico_sf_proj, inverse=TRUE)
    
    
    crs(density_masked) <- CRS("+init=EPSG:3310")
    
    # Convert density to a data frame
    density_df <- as.data.frame(rasterToPoints(density_masked))
    names(density_df) <- c("x", "y", "value")
    
    # Calculate the cumulative distribution function (CDF)
    density_values <- sort(density_df$value, decreasing = TRUE)
    cdf <- cumsum(density_values) / sum(density_values)
    
    # Find the contour level corresponding to,50 75, and 95% volume and then save the corresponding volumes
    
    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    
    # Create contour lines from the density raster
    contours <- rasterToContour(extended_raster, levels = c(contour_level_75))
    
    #convert to sf object
    contours_sf <- st_as_sf(contours)
    #set crs
    contours_sf <- st_set_crs(contours_sf , 3310)
    
    # Convert the LINESTRING to POLYGON if needed
    # Attempt to cast to POLYGON
    closed_poly <- tryCatch(
      st_cast(contours_sf, "POLYGON"),
      error = function(e) {
        message("st_cast failed, using st_polygonize instead")
        st_polygonize(contours_sf)
      }
    )
    
    # Ensure valid geometry
    closed_poly <- st_make_valid(closed_poly)
    
    # Ensure valid geometry
    closed_poly <- st_make_valid(closed_poly)
    
    # Plot the closed polygon to verify the closure
    plot(closed_poly)
    
    # Extract only the geometry of the USA/Mexico shapefile
    usa_geometry <- st_geometry(usa_mexico_sf_proj)
    
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
      # If there is no intersection with land, calculate the volume directly from the closed ocean polygon
      area_75_m2 <- sum(st_area(closed_poly))
    }
    
    # Convert area from square meters to square kilometers
    area_75_km2 <- as.numeric(area_75_m2) / 1e6  # Convert units object to numeric and divide by 1e6
    
    
    # Find the most northern and southern latitudes for the 75% volume contour
    filtered_df_75 <- density_df[density_df$value >= contour_level_75, ]
    northernmost_latitude_75 <- max(filtered_df_75$y, na.rm = TRUE)
    southernmost_latitude_75 <- min(filtered_df_75$y, na.rm = TRUE)
    
    # Transform northernmost and southernmost latitudes to WGS 84 (EPSG:4326)
    # Create an sf object with the coordinates in EPSG:3310
    coords_3310 <- data.frame(
      x = rep(0, 2),  # Dummy longitude (not used)
      y = c(northernmost_latitude_75, southernmost_latitude_75)
    )
    sf_coords_3310 <- st_as_sf(coords_3310, coords = c("x", "y"), crs = 3310)
    
    # Transform to WGS 84 (EPSG:4326)
    sf_coords_4326 <- st_transform(sf_coords_3310, crs = 4326)
    
    # Extract transformed coordinates
    transformed_coords <- st_coordinates(sf_coords_4326)
    northernmost_latitude_wgs84 <- transformed_coords[1, "Y"]
    southernmost_latitude_wgs84 <- transformed_coords[2, "Y"]
    
    
    #save in new dataframe with year so it can be in the animations. NEed to save density_df, contour_level, year
    df <- data.frame(port.complex=portcomplex,
                     Year = year,
                     area_75_m2=area_75_m2,
                     area_75_km2= area_75_km2,
                     northernmost_latitude_75=northernmost_latitude_wgs84,
                     southernmost_latitude_75=southernmost_latitude_wgs84 )
    
    
    # Append current year's results to combined_df
    datalist[[year]] <- df
  }
  
  all.data<- dplyr::bind_rows(datalist)
  
}

a<-plot_fun('Bodega Bay')
b<-plot_fun('San Francisco')
c<-plot_fun('Monterey')
d<-plot_fun('Morro Bay')
e<-plot_fun('Santa Barbara')
f<-plot_fun('Los Angeles')
g<-plot_fun('San Diego')

all.volumes<-rbind(a,b,c,d,e,f,g)%>%
  left_join(mpa,by="port.complex")


saveRDS(all.volumes,paste0(gillnet_data,"Data/Processed Data/setgillnet_all contour volumes.rds"))

#order from north to south
all.volumes$port.complex<- factor(all.volumes$port.complex, levels = c( 'Bodega Bay','San Francisco','Monterey', 'Morro Bay','Santa Barbara', 'Los Angeles', 'San Diego'))

p<-ggplot(data=all.volumes, aes(x=Year))+
  facet_wrap(~port.complex, scales="free_y")+
  
  #geom_line(aes(y=vol_50_km ,color="50% contour"),size=1)+
  geom_line(aes(y=area_75_km2 ,color="75% contour"),size=1)+
  #geom_line(aes(y=vol_95_km ,color="95% contour"),size=1)+
  ylab("Area (km^2)")+
  #legend
  scale_color_manual(name = "Volume Contours",
                     values = c(#"50% contour" = RColorBrewer::brewer.pal(n = 11, name = "Spectral")[2],  # Red from RdYlBu palette
                       "75% contour" = RColorBrewer::brewer.pal(n = 11, name = "Spectral")[10])) + # Green from RdYlBu palette
  # "95% contour" = RColorBrewer::brewer.pal(n = 11, name = "Spectral")[10])) +  # Blue from RdYlBu palette
  # Theme
  theme_bw() + 
  base_theme  +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  # Font size and style for facet labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Font size and style for x-axis label
    axis.title.y = element_text(size = 14, face = "bold")
  ) +
  
  annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
  geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "black",size=1)


for(i in 1:n_pages(p)){
  p_save <-  p + 
    facet_wrap_paginate( ~port.complex, ncol=3, nrow=1, page = i, scales="free_y")
  ggsave(plot = p_save, filename = paste0(gillnet_data,'Figures/Set/Kernel Density/Volume Contours_page_', i, '.jpg'),width = 16, height = 8, units = "in")
}
############

############
#time series analyses

#COG and northern and southern latitudes
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
COG_sf<-st_transform(COG_sf,crs = "+proj=longlat +datum=WGS84")

COG_df<-COG_sf%>%
  sf::st_drop_geometry()%>%
  rename(Year=year)%>%
  mutate(Year=as.double(Year))
COG_df$COG_latitude <- st_coordinates(COG_sf)[, "Y"]  # Extracts latitude
COG_df$COG_longitude <- st_coordinates(COG_sf)[, "X"]


COG_vcontours<-COG_df%>%
  left_join(all.volumes, by=c("port.complex","Year"))

plot_fun<-function(portcomplex){
  
  # Define specific colors from an RColorBrewer palette
  colors <- brewer.pal(n = 11, name = "RdYlBu") 
  
  
  location<-COG_vcontours%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=location, aes(x=Year))+
    geom_line(aes(y=COG_latitude, color = "COG"),size=.75)+
    geom_line(aes(y=northernmost_latitude_75, color = "Northern extent"),size=.75)+
    geom_line(aes(y=southernmost_latitude_75, color = "Southern extent"),size=.75)+
    #labs
    labs(y = ('Latitude °N'), x='Year') +
    # Color scale from RColorBrewer
    # Apply specific colors using scale_color_manual
    scale_color_manual(values = c("Northern extent" = colors[10],
                                  "COG"= "black",
                                  "Southern extent" = colors[2]),
                       breaks = c("Northern extent", "COG", "Southern extent")) +  # Control legend order
    # Set y-axis breaks at every 0.5 latitude
    scale_y_continuous(breaks = seq(floor(min(location$COG_latitude)), 
                                    ceiling(max(location$COG_latitude)), 
                                    by = 0.5)) +
    #title
    ggtitle(paste0(portcomplex, ' Fishing Grounds'))+
    # Theme
    theme_bw() + 
    base_theme  +
    theme(legend.title = element_blank())+
    #marine heatwave and mpa implementation
    annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
    geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "red",size=1)
  
  
  filename1 <- paste0(gillnet_data,'Figures/Set/Kernel Density/', portcomplex,'/',portcomplex, '_COG_extents_latitude.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Bodega Bay')
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
plot_fun('San Diego')
############
