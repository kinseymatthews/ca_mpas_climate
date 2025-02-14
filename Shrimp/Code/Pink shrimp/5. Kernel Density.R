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
shrimp_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/'
#################

#edited shrimp data from the block script
explor<-readRDS(paste0(shrimp_data,"Data/Processed Data/pinktrawl_filtered.rds"))

#cog data
COG<-readRDS(paste0(shrimp_data, 'Data/Processed Data/pinkshrimp_COG_distance_results_towhours_final.rds'))
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = 3310)
COG_sf$year=as.factor(COG_sf$year)

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
usa_mexico_sf<- st_transform(usa_mexico_sf, crs = 3310)

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
  group_by(tow.identifier, duration_min) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct dive_duration_min for each tow.identifier
  group_by(tow.identifier) %>%
  summarize(total_min = sum(duration_min), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(tow.identifier, year, port.complex, block_id), by = "tow.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex,  block_id) %>%
  summarize(total_tow_min = sum(total_min, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks, by = "block_id") %>% 
  #remove block IDs of NA
  filter(!is.na(block_long_dd))%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  #remove any bad spatial blocks
  filter(!st_is_empty(.))


# Reproject to Albers Equal Area Conic (EPSG: 3310)
sf_df_projected <- st_transform(sf_df, crs = 3310)


plot_fun<-function(portcomplex){ 
  
  #filtering for location
  location<-sf_df_projected%>%
    filter(port.complex==portcomplex)
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(location)
  weights<-location$total_tow_min
  
  # Create a point pattern (ppp object)
  minx<-(min(coordinates[,1]))
  maxx<-(max(coordinates[,1]))
  miny<-(min(coordinates[,2]))
  maxy<-(max(coordinates[,2]))
  
  points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
  
  # Compute kernel density estimation using raw and logcdata
  sigma<-bw.diggle(points_ppp)
  density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE)
  
  # Convert density to a raster (assuming a reasonable raster resolution)
  density_raster <- as(raster(as.im(density)), "RasterLayer")
  
  # Mask the density raster with the land raster
  density_masked <- mask(density_raster, usa_mexico_sf, inverse=TRUE)
  
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
    
    scale_fill_gradientn(name = "Tow minutes: \nYears 1983 - 2018", 
                         colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                         limits = c(0, max(density_df$value)),  # Adjust limits as needed
                         na.value = "white") +  # Set NA values to white
    
    # Add contour lines at 75% volume boundary
    geom_contour(data = density_df, aes(x = x, y = y, z = value), 
                 breaks = contour_level, color = "black", size = 0.75) +
    # Land
    geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    # Crop
    #coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
    # Axes
    #scale_x_continuous(breaks=seq(-125, -117, 2)) +
    #scale_y_continuous(breaks=seq(32, 44, 2)) + 
    # Theme
    theme_bw() + base_theme +
    ggtitle(paste0( portcomplex, ' Fishing Grounds') , subtitle="Pink shrimp Trawl Fishery With 75% volume contour")+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  
  name=paste0(shrimp_data,'Figures/Pink Shrimp/Kernel Density/',portcomplex,'/',portcomplex,' Kernel Densiy_All Years_test2.jpg')
  ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)
  
}
plot_fun('Eureka')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
############


############
#Kernel Densities - years separate
sf_df<- explor %>%
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
  group_by( port.complex,  year, block_id) %>%
  summarize(total_tow_min = sum(total_min, na.rm = TRUE),
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
    tryCatch({ #used to catch any errors
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(year_data)
    #weights
    weights<-year_data$total_tow_min
    
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
    # Convert meters to degrees
   # lat_expansion <- 20000 / 111320  # ~0.0898 degrees
    #lon_expansion <- 20000 / (111320 * cos(mean(coordinates[,2]) * pi / 180))  # Adjusted for latitude
    
    # Expand bounding box
    minx <- minx - 20000
    maxx <- maxx + 20000
    miny <- miny - 20000
    maxy <- maxy + 20000
    
    points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
    
    # Compute kernel density estimation using raw and logcdata
    sigma<-bw.diggle(points_ppp)
    
    density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE, edge=TRUE)
    
    # Convert density to a raster (assuming a reasonable raster resolution). as.im converts data into a pixel image which is then converted to a raster
    density_raster <- as(raster(as.im(density)), "RasterLayer")
    
    # Mask the density raster with the land raster
   density_masked <- mask(density_raster, usa_mexico_sf, inverse=TRUE)
    
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
    
    # Extract the bounding box of the 75% contour
    contour_bbox <- st_bbox(contour_75_sf)
 
   p<- ggplot() +
      geom_raster(data = density_df, aes(x = x, y = y, fill = value)) +
      
      scale_fill_gradientn(name = "Tow minutes", 
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                           limits = c(0, max(density_df$value)),  # Adjust limits as needed
                           na.value = "white") +  # Set NA values to white
      # Add contour lines at 75% volume boundary
     # geom_contour(data = density_df, aes(x = x, y = y, z = value), 
      #             breaks = contour_level_75, color = "black", size = 0.5) +
      
      geom_sf(data = contour_75_sf, color = "black", linewidth = 1)  +
    
      # Land
      geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      #points COG
      geom_sf(data=b,size=5, fill="white",color="black",pch=21)+
      # Crop
      # Crop the plot using the bounding box of the contour
     coord_sf(xlim = c(minx-20000, maxx+20000), ylim = c(miny-20000, maxy+20000)) +
     # coord_sf(xlim = c(contour_bbox["xmin"]-100000, contour_bbox["xmax"]+100000), 
      #         ylim = c(contour_bbox["ymin"]-100000, contour_bbox["ymax"]+100000),
       #        expand = FALSE) + # No extra space beyond the bounding box
      # Axes
     # scale_x_continuous(breaks=seq(minx-.5, maxx+.5, .5),labels=scales::number_format(accuracy=0.50)) +
      #scale_y_continuous(breaks=seq(miny-.5, maxy+.5, .5),labels=scales::number_format(accuracy=0.50)) + 
      
      ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle=paste0('Pink shrimp Trawl Fishery: Year ', year, ', with 75% volume contour' ))+
      theme_bw() + base_theme+ 
      theme(axis.title = element_blank())
    
    
    name=paste0(shrimp_data,'Figures/Pink Shrimp/Kernel Density/',portcomplex,'/',portcomplex, year,'TEST.jpg')
    ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
 
    }, error = function(e) {
      message("Error in port complex: ", portcomplex, ", Year: ", year, " - ", e$message)
    })
  }
}
plot_fun('Morro Bay')



plot_fun('Eureka')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')

############
#importing yearly images and creating an animation
years<-c(1995:1997,1999:2006,2010,2013:2020)
frames<-paste0(shrimp_data,"Figures/Pink Shrimp/Kernel Density/Eureka/Eureka",years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m, paste0(shrimp_data,"Figures/Pink Shrimp/Kernel Density/Eureka/Eureka Kernel Densities_Animation.gif"))

years<-c(2000,2002,2003)
frames<-paste0(shrimp_data, "Figures/Pink Shrimp/Kernel Density/Monterey/Monterey", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(shrimp_data,"Figures/Pink Shrimp/Kernel Density/Monterey/Monterey Kernel Densities_Animation.gif"))

years<-c(1983,1984,1994:1999,2001,2002, 2004,2005)
frames<-paste0(shrimp_data,"Figures/Pink Shrimp/Kernel Density/Morro Bay/Morro Bay", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(shrimp_data,"Figures/Pink Shrimp/Kernel Density/Morro Bay/Morro Bay Kernel Densities_Animation.gif"))

years<-c(1984:1986,1989:1994,1996:2003,2005, 2007,2014,2017)
frames<-paste0(shrimp_data,"Figures/Pink Shrimp/Kernel Density/Santa Barbara/Santa Barbara", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(shrimp_data,"Figures/Pink Shrimp/Kernel Density/Santa Barbara/Santa Barbara Kernel Densities_Animation.gif"))
############



############
#time series analyses
#volume contours through time
plot_fun<-function(portcomplex){ 
  
  location <- sf_df_projected%>%
    filter(port.complex=="Morro Bay")%>%
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
    year_data<-location[location$year==1996,]
    
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
    
    # Mask the density raster with the land raster
    density_masked <- mask(density_raster, usa_mexico_sf, inverse=TRUE)
    
    # Convert density to a data frame
    density_df <- as.data.frame(rasterToPoints(density_masked))
    names(density_df) <- c("x", "y", "value")
    
    # Calculate the cumulative distribution function (CDF)
    density_values <- sort(density_df$value, decreasing = TRUE)
    cdf <- cumsum(density_values) / sum(density_values)
    
    # Find the contour level corresponding to,50 75, and 95% volume and then save the corresponding volumes

    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    
    # Create contour lines from the density raster
    contours <- rasterToContour(density_masked, levels = c(contour_level_75))
    
    # Convert contours to SpatialPolygons
    #contour_polygons <- rasterToPolygons(density_raster, fun = function(x) x >= contour_level_75, dissolve = TRUE)
    
    # Convert contour line to sf polygons
    contour_sf <- st_as_sf(contours)
    
                    
    # Convert contour lines (LINESTRING) to MULTILINESTRING first
    contour_sf_multiline <- st_cast(contour_sf, "MULTILINESTRING")
    

    # Extract the coordinates
    coords <- st_coordinates(contour_sf_multiline)
    
    # Create a new LINESTRING connecting the end of the first to the start of the second
    new_line <- st_linestring(rbind(coords[1, ], coords[nrow(coords), ]))
    
    # Combine the original MULTILINESTRING with the new LINESTRING
    combined <- st_sfc(st_multilinestring(list(
      matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE),
      matrix(c(1, 1, 2, 0), ncol = 2, byrow = TRUE),
      matrix(c(coords[1, 1], coords[1, 2], coords[nrow(coords), 1], coords[nrow(coords), 2]), ncol = 2, byrow = TRUE)
    )), crs = 3310)
    
    # Plot the result
    plot(combined, col = 'blue', lwd = 2)
    
    
    
    
    
    # Convert to polygons by polygonizing
    contour_sf_75_poly <- st_polygonize(contour_sf_75_multiline)
    
    # Ensure the geometry is valid
    contour_sf_75_poly <- st_make_valid(contour_sf_75_poly)
    

    # Compute area
    volume_75 <- sum(st_area(contour_sf_75_poly))
    


    #Find area
    #volume_50 <- sum(density_df$value[density_df$value >= contour_level_50])
    #volume_75 <- sum(density_df$value[density_df$value >= contour_level_75])
    #volume_95 <- sum(density_df$value[density_df$value >= contour_level_95])
    
    # Find the most northern and southern latitudes for the 75% volume contour
    filtered_df_75 <- density_df[density_df$value >= contour_level_75, ]
    northernmost_latitude_75 <- max(filtered_df_75$y, na.rm = TRUE)
    southernmost_latitude_75 <- min(filtered_df_75$y, na.rm = TRUE)
    
    #save in new dataframe with year so it can be in the animations. NEed to save density_df, contour_level, year
    df <- data.frame(port.complex=portcomplex,
                     Year = year,
                     volume_75=volume_75,
                     northernmost_latitude_75=northernmost_latitude_75,
                     southernmost_latitude_75=southernmost_latitude_75 )
    
    
    # Append current year's results to combined_df
    datalist[[year]] <- df
  }
  
  all.data<- dplyr::bind_rows(datalist)
  
}

a<-plot_fun('Eureka')
b<-plot_fun('Monterey')
c<-plot_fun('Morro Bay')
d<-plot_fun('Santa Barbara')

all.volumes<-rbind(a,b,c,d)

all.volumes<-all.volumes%>%
  mutate(vol_50_km=volume_50/1000)%>%
  mutate(vol_75_km=volume_75/1000)%>%
  mutate(vol_95_km=volume_95/1000)%>%
  left_join(mpa,by="port.complex")%>%
  mutate(Year=as.numeric(Year))


saveRDS(all.volumes,paste0(shrimp_data,"Data/Processed Data/pinkshrimp_all contour volumes.rds"))

#order from north to south
all.volumes$port.complex<- factor(all.volumes$port.complex, levels = c( 'Eureka', 'Monterey', 'Morro Bay', 'Santa Barbara'))


p<-ggplot(data=all.volumes, aes(x=Year))+
  facet_wrap_paginate(~port.complex, ncol=3, nrow=1, scales="free_y")+
  
  geom_line(aes(y=vol_50_km ,color="50% contour"),size=1)+
  geom_line(aes(y=vol_75_km ,color="75% contour"),size=1)+
  geom_line(aes(y=vol_95_km ,color="95% contour"),size=1)+
  ylab("Area (km^2)")+
  #legened
  scale_color_manual(name = "Volume Contours",
                     values = c("50% contour" = RColorBrewer::brewer.pal(n = 11, name = "Spectral")[2],  # Red from RdYlBu palette
                                "75% contour" = RColorBrewer::brewer.pal(n = 11, name = "Spectral")[8],  # Green from RdYlBu palette
                                "95% contour" = RColorBrewer::brewer.pal(n = 11, name = "Spectral")[10])) +  # Blue from RdYlBu palette
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
  ggsave(plot = p_save, filename = paste0(shrimp_data,'Figures/Pink shrimp/Kernel Density/Volume Contours_page_', i, '.jpg'),width = 16, height = 8, units = "in")
}

#can also do the above so its not facetwrap and all separate
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
  
  
  filename1 <- paste0(shrimp_data,'Figures/Pink shrimp/Kernel Density/', portcomplex,'/',portcomplex, '_COG_extents_latitude.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Eureka')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
############
