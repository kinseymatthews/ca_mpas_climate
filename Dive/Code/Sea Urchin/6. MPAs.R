library(tidyverse)
library(sf)
library(sp)
library(spatstat)
library(raster)
library(ggplot2)

##################
#Read in data
#filepaths
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
urchin_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/'

#edited lobster data from the block script
explor<-readRDS(paste0(urchin_data, "Data/Processed Data/seaurchin_filtered.rds"))

#cog data
COG<-readRDS(paste0(urchin_data, 'Data/Processed Data/COG_distance_results_divehours_final_seaurchin.rds'))
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = 3310)
COG_sf$year=as.factor(COG_sf$year)

COG_df<-COG_sf%>%
  sf::st_drop_geometry()
COG_df$COG_latitude <- st_coordinates(COG_sf)[, "Y"]  # Extracts latitude
COG_df$COG_longitude <- st_coordinates(COG_sf)[, "X"]


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
state_waters<- st_transform(state_waters, crs = 3310)

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


all_regs<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/spatialregulations_perfishery.rds")%>%
  filter(Urchin.dive==0) #only getting regs where the lobster is NOT allowed to fish
all_regs <- st_cast(all_regs, "POLYGON")

#just mpas
mpa_blocks <-all_regs%>%filter(type=="MPA")
# Union the MPAs so they are treated as a single shape
mpa_blocks <- st_union(mpa_blocks)
mpa_blocks <- st_make_valid(mpa_blocks)

# Union the MPAs so they are treated as a single shape
all_regs <- st_union(all_regs)
all_regs <- st_make_valid(all_regs)
############


############
#Kernel Densities - years separated 
#yearly data
sf_df<- explor %>%
  # Step 1: Remove duplicates within each dive.identifier and calculate distinct angler_hours
  group_by(dive.identifier, hours) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct dive_hours for each dive.identifier
  group_by(dive.identifier) %>%
  summarize(total_hours = sum(hours), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(dive.identifier, year, port.complex, block_id), by = "dive.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, year,block_id) %>%
  summarize(total_dive_hours = sum(total_hours, na.rm = TRUE),
            .groups = 'drop')%>%
#get block data
left_join(all_blocks, by = "block_id") %>% 
  #remove block IDs of NA
  filter(!is.na(block_long_dd))%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  #add mpa implementation information
  left_join(mpa, by="port.complex")%>%
  #remove any bad spatial blocks
  filter(!st_is_empty(.))

sf_df_projected <- st_transform(sf_df, crs = 3310)
############

############
#plotting mpas with yearly kernel densities and COG
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
    weights<-year_data$total_dive_hours
    
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
      
      scale_fill_gradientn(name = "Kernel Density", 
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                           limits = c(0, max(density_df$value)),  # Adjust limits as needed
                           na.value = "white") +  # Set NA values to white
      # Add contour lines at 75% volume boundary
      geom_sf(data = contour_75_sf, color = "black", linewidth = 1)  +
      
      # Land
      geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      #mpas
      geom_sf(data=all_regs, fill="black", col="white", linewidth=0.2, inherit.aes = F)+
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      #points COG
      #geom_sf(data=b,size=5, fill="white",color="black",pch=21)+
      # Crop
      coord_sf(xlim = c(minx-50000, maxx+50000), ylim = c(miny-50000, maxy+50000)) +
      # Axes
      #  scale_x_continuous(breaks=seq(minx-20000, maxx+20000, 20000),labels=scales::number_format(accuracy=20000)) +
      #  scale_y_continuous(breaks=seq(miny-20000, maxy+20000, 20000),labels=scales::number_format(accuracy=20000)) + 
      
      ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle=paste0('Sea Urchin Dive Fishery: Year ', year, ', with 75% volume contour' ))+
      theme_bw() + base_theme+ 
      theme(axis.title = element_blank())
    
    
    filename1 <- paste0(urchin_data, 'Figures/Urchin/Kernel Density/',portcomplex,'/', portcomplex, year, '_regs_MPAs_cropped.jpg')
    ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
  }
}

plot_fun('Fort Bragg')
plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun('San Diego')
############


############
#importing yearly images and creating an animation
years<-c(1983,1988:1997,2001,2003:2023)
frames<-paste0(urchin_data,"Figures/Urchin/Kernel Density/Fort Bragg/Fort Bragg",years,"_MPAs_cropped.jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m, paste0(urchin_data, "Figures/Urchin/Kernel Density/Fort Bragg/Fort Bragg Kernel Densities_MPA_Animation.gif"))

years<-c(1998,2003:2022)
frames<-paste0(urchin_data,"Figures/Urchin/Kernel Density/Santa Barbara/Santa Barbara",years,"_MPAs_cropped.jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(urchin_data, "Figures/Urchin/Kernel Density/Santa Barbara/Santa Barbara Kernel Densities_MPA_Animation.gif"))

years<-c(2003:2022)
frames<-paste0(urchin_data,"Figures/Urchin/Kernel Density/Los Angeles/Los Angeles", years,"_MPAs_cropped.jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(urchin_data, "Figures/Urchin/Kernel Density/Los Angeles/Los Angeles Kernel Densities_MPA_Animation.gif"))

years<-c(1980,1981, 1983, 1984, 1988:2013,2015:2020,2023)
frames<-paste0(urchin_data,"Figures/Urchin/Kernel Density/San Diego/San Diego", years,"_MPAs_cropped.jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,paste0(urchin_data, "Figures/Urchin/Kernel Density/San Diego/San Diego Kernel Densities_MPA_Animation.gif"))
############




############
#area within an mpa and regulations
plot_fun<-function(portcomplex){
  
  location <- sf_df_projected%>%
    filter(port.complex==portcomplex)%>%
    group_by(year)%>%
    filter(n() > 1) %>%
    ungroup()
  
  #get unique years in dataset
  unique_years<-unique(location$year)
  
  datalist=list()
  
  #Loop through each year and calculate kernel density
  for(year in unique_years){
    
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(year_data)
    #weights
    weights<-year_data$total_dive_hours
    
    # Create a point pattern (ppp object)
    minx<-(min(coordinates[,1]))
    maxx<-(max(coordinates[,1]))
    miny<-(min(coordinates[,2]))
    maxy<-(max(coordinates[,2]))
    
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
    
    density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE, edge=TRUE)
    
    # Convert density to a raster (assuming a reasonable raster resolution)
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
    
    # Find the contour level corresponding to 75% volume
    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    
    # Create a mask for values above the 75% contour
    density_raster_masked <- density_masked
    density_raster_masked[] <- ifelse(density_masked[] >= contour_level_75, density_masked[], NA)
    
    # Mask the density raster with the land raster
    density_raster_masked <- mask(density_raster_masked, usa_mexico_sf_proj, inverse=TRUE)
    
    # Compute total area of 75% KDE - will need to polygonize and calculate area
    
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
    
    
    # Plot the closed polygon to verify the closure
    plot(closed_poly)
    
    # Extract only the geometry of the USA/Mexico shapefile
    usa_geometry <- st_geometry(usa_mexico_sf_proj)
    
    # First calculate the intersection (land area)
    intersection <- st_intersection(closed_poly, usa_geometry)
    
    # Check if there is an intersection
    if (nrow(intersection) > 0) {
      # If there is an intersection, manually subtract the land portion from the ocean using st_difference
      closed_poly_no_land <- st_difference(closed_poly, st_union(usa_geometry))
      plot(closed_poly_no_land)
      
      # Compute the 75% volume area (land portion removed)
      area_75_m2 <- sum(st_area(closed_poly_no_land))
    } else {
      closed_poly_no_land <- closed_poly
      plot(closed_poly_no_land)
      # If there is no intersection with land, calculate the volume directly from the closed ocean polygon
      area_75_m2 <- sum(st_area(closed_poly_no_land))
    }
    # Convert area from square meters to square kilometers
    area_75_km2 <- as.numeric(area_75_m2) / 1e6  # Convert units object to numeric and divide by 1e6
    
    #compute area of mpas within 75%
    #turn mpas into polygons and calculate interactions between the two
    
    # Extract only the geometry of the regs
    block_geometry <- st_geometry(all_regs)
    
    # First calculate the intersection (land area)
    intersection <- st_intersection(closed_poly_no_land, block_geometry)
    
    closed_poly_no_land <- st_make_valid(closed_poly_no_land)
    
    # Check if there is an intersection
    if (nrow(intersection) > 0) {
      # If there is an intersection, manually subtract the mpa portion from the non mpa using st_difference
      closed_poly_no_reg <- st_difference(closed_poly_no_land, st_union(block_geometry))
      plot(closed_poly_no_reg)
      
      # Compute the 75% volume area (land portion removed)
      area_75_m2_noreg <- sum(st_area(closed_poly_no_reg))
    } else {
      # If there is no intersection with land, calculate the volume directly from the closed ocean polygon
      area_75_m2_noreg <- sum(st_area(closed_poly_no_land))
    }
    
    # Convert area from square meters to square kilometers
    area_75_km2_noreg <- as.numeric(area_75_m2_noreg) / 1e6  # Convert units object to numeric and divide by 1e6
    
    # Extract only the geometry of the USA/Mexico shapefile
    block_geometry <- st_geometry(mpa_blocks)
    
    # First calculate the intersection (land area)
    intersection <- st_intersection(closed_poly_no_land, block_geometry)
    
    closed_poly_no_land <- st_make_valid(closed_poly_no_land)
    
    # Check if there is an intersection
    if (nrow(intersection) > 0) {
      # If there is an intersection, manually subtract the mpa portion from the non mpa using st_difference
      closed_poly_no_mpas <- st_difference(closed_poly_no_land, st_union(block_geometry))
      plot(closed_poly_no_mpas)
      
      # Compute the 75% volume area (land portion removed)
      area_75_m2_nompa <- sum(st_area(closed_poly_no_mpas))
    } else {
      # If there is no intersection with land, calculate the volume directly from the closed ocean polygon
      area_75_m2_nompa <- sum(st_area(closed_poly_no_land))
    }
    
    # Convert area from square meters to square kilometers
    area_75_km2_nompa <- as.numeric(area_75_m2_nompa) / 1e6  # Convert units object to numeric and divide by 1e6
    
    df <- data.frame(Portcomplex=portcomplex,
                     Year = year,
                     total_area=area_75_km2,
                     total_area_notinmpa=area_75_km2_nompa,
                     total_area_inmpa=(area_75_km2-area_75_km2_nompa),
                     total_area_notinreg=area_75_km2_noreg ,
                     total_area_inreg=(area_75_km2-area_75_km2_noreg))%>%
      mutate(percent_inmpa = ((total_area_inmpa) / (total_area)) * 100,
             percent_inmpa = ifelse(percent_inmpa < 0.001, 0, percent_inmpa))%>%
      mutate(percent_inreg = ((total_area_inreg) / (total_area)) * 100,
             percent_inreg = ifelse(percent_inreg < 0.001, 0, percent_inreg))
    
    # Append current year's results to combined_df
    datalist[[year]] <- df
  }
  all.data<- dplyr::bind_rows(datalist)
  
}
a<-plot_fun("Fort Bragg")
b<-plot_fun("Santa Barbara")
c<-plot_fun("Los Angeles")
d<-plot_fun("San Diego")


all.volumes<-rbind(a,b,c,d)%>%
  rename(port.complex=Portcomplex)%>%
  left_join(mpa,by="port.complex")%>%
  mutate(Year=as.numeric(Year))

saveRDS(all.volumes,paste0(urchin_data,"Data/Processed Data/Urchin_regs_MPA_area.rds"))

#mpa only plot
plot_fun<-function(portcomplex){
  location <- all.volumes%>%
    filter(port.complex==portcomplex)%>%
    mutate(Year=as.numeric(Year))
  
  
  p<-ggplot(data=location, aes(x=Year))+
    geom_line(aes(y=percent_inmpa), color="black",size=1)+
    labs(y ='Percent weighted effort \n within MPAs') +
    # Theme
    theme_bw() + 
    base_theme  +
    theme(legend.position="none")+
    annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + 
    geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "red",size=1)
  
  filename1 <- paste0(urchin_data, 'Figures/Urchin/Kernel Density/', portcomplex, '_perc.effort within mpa.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 4, units = "in", dpi = 300)
  
  
}
plot_fun('Fort Bragg')
plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun("San Diego")

#all regs plot

plot_fun<-function(portcomplex){
  location <- all.volumes%>%
    filter(port.complex==portcomplex)%>%
    mutate(Year=as.numeric(Year))
  
  
  p<-ggplot(data=location, aes(x=Year))+
    geom_line(aes(y=percent_inreg), color="black",size=1)+
    labs(y ='Percent weighted effort \n within regulation areas') +
    # Theme
    theme_bw() + 
    base_theme  +
    theme(legend.position="none")+
    annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + 
    geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "red",size=1)
  
  filename1 <- paste0(urchin_data, 'Figures/Urchin/Kernel Density/', portcomplex, '_perc.effort within mpa.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 4, units = "in", dpi = 300)
  
  
}
plot_fun('Fort Bragg')
plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun("San Diego")
############


############
all.volumes$port.complex<- factor(all.volumes$port.complex, levels = c('Fort Bragg','Santa Barbara', 'Los Angeles', 'San Diego'))


p<-ggplot(data=all.volumes, aes(x=Year, color=port.complex))+
  # geom_line(aes(y=vol_50_km ,color="50% contour"),size=1)+
  geom_line(aes(y=percent_inmpa),size=.5)+
  #  geom_line(aes(y=vol_95_km ,color="95% contour"),size=1)+
  labs(y = ('Percent weighted effort \n within MPAs'), x='Year') +
  
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank())+
  scale_color_manual(name = "Port complex",
                     values = c("Fort Bragg" = RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")[10],
                                 "Santa Barbara" = RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")[8],           
                                "Los Angeles" = RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")[4],  
                                "San Diego" = RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")[2]))+
  scale_y_continuous(
    limits = c(0, 16),            # Set the limits of the y-axis
    breaks = seq(0, 16, by = 4) # Set the breaks on the y-axis
    
  )

filename1 <- paste0(urchin_data,'Figures/Urchin/Kernel Density/All Ports_weightedefforft_MPAs.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
############

