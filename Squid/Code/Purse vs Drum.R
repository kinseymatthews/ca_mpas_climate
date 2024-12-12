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
squid_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/squid/'
#################

#edited squid data from the block script
explor<-readRDS(paste0(squid_data,"Data/Processed Data/squid_filtered.rds"))

#cog data
#COG<-readRDS(paste0(squid_data, 'Data/Processed Data/pinksquid_COG_distance_results_towhours_final.rds'))
#COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
#  st_transform(crs = 4326)
#COG_sf$year=as.factor(COG_sf$year)

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

#port codes
port<-read.csv( paste0(general_data,"Port Codes.csv"))

# State waters
state_waters <- readRDS( paste0(general_data,"CA_state_waters_polyline.Rds"))

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
#Kernel Densities - years seerate
sf_df<- explor %>%
  # Step 1: Remove duplicates within each tow.identifier and calculate distinct angler_duration_min
  group_by(tow.identifier, duration_min) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct dive_duration_min for each tow.identifier
  group_by(tow.identifier) %>%
  summarize(total_min = sum(duration_min), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(tow.identifier, gear, year, port.complex, block_id), by = "tow.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( gear,port.complex,  year, block_id) %>%
  summarize(total_tow_min = sum(total_min, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks, by = "block_id") %>% 
  #remove block IDs of NA
  filter(!is.na(block_long_dd))%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  #remove any bad spatial blocks
  filter(!st_is_empty(.))%>%
  #filer for purse seine or drum seine
  filter(gear=="Purse seine"|gear=="Drum seine")

plot_fun<-function(portcomplex){ 
  
  location <- sf_df%>%
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
    minx<-(min(coordinates[,1]))
    maxx<-(max(coordinates[,1]))
    miny<-(min(coordinates[,2]))
    maxy<-(max(coordinates[,2]))
    
    points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
    
    # Compute kernel density estimation using raw and logcdata
    sigma<-bw.diggle(points_ppp)
    
    density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE5)
    
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
  
    
    p<-ggplot() +
      geom_raster(data = density_df, aes(x = x, y = y, fill = value)) +
      
      scale_fill_gradientn(name = "Tow minutes", 
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                           limits = c(0, max(density_df$value)),  # Adjust limits as needed
                           na.value = "white") +  # Set NA values to white
      # Add contour lines at 75% volume boundary
      geom_contour(data = density_df, aes(x = x, y = y, z = value), 
                   breaks = contour_level, color = "black", size = 0.5) +
      
      # Land
      geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +

      # Set the limits for the coordinate system
      coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
      # Axes
      scale_x_continuous(breaks=seq(-126, -118, 2)) +
      scale_y_continuous(breaks=seq(32, 42, 2)) + 
      ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle=paste0('Squid Fishery: Year ', year, ', with 75% volume contour' ))+
      theme_bw() + base_theme+ 
      theme(axis.title = element_blank())
    
    
    name=paste0(squid_data,'Figures/Kernel Density/',portcomplex,'/',portcomplex, year,'.jpg')
    ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
  }
}


plot_fun <- function(portcomplex) { 
  location <- sf_df %>%
    filter(port.complex == portcomplex) %>%
    group_by(year, gear) %>%
    filter(n() > 1) %>% # Kernel densities need more than one block in a year to run
    ungroup()
  
  # Get unique years in the dataset
  unique_years <- unique(location$year)
  
  # Loop through each year to calculate kernel density
  for (year in unique_years) {
    
    # Subset data for the current year
    year_data <- location[location$year == year, ]
    
    # Initialize a list to store density data for each gear type
    density_list <- list()
    
    # Loop through gear types and calculate densities
    for (gear_type in unique(year_data$gear)) {
      gear_data <- year_data[year_data$gear == gear_type, ]
      
      # Skip if insufficient data points
     # if (nrow(gear_data) < 2) next
      
      # Extract coordinates and weights
      coordinates <- st_coordinates(gear_data)
      weights <- gear_data$total_tow_min
      
      # Create a point pattern (ppp object)
      minx <- min(coordinates[, 1])
      maxx <- max(coordinates[, 1])
      miny <- min(coordinates[, 2])
      maxy <- max(coordinates[, 2])
      
      points_ppp <- ppp(coordinates[, 1], coordinates[, 2], window = owin(c(minx, maxx), c(miny, maxy)))
      
      # Compute kernel density estimation
      sigma <- bw.diggle(points_ppp)
      density <- density.ppp(points_ppp, sigma = sigma, kernel = "gaussian", weights = weights, diggle = TRUE, scalekernel = TRUE)
      
      # Convert density to a raster
      density_raster <- as(raster(as.im(density)), "RasterLayer")
      
      # Mask the density raster with the land raster
      density_masked <- mask(density_raster, usa_mexico_sf, inverse = TRUE)
      
      # Convert density to a data frame
      density_df <- as.data.frame(rasterToPoints(density_masked))
      names(density_df) <- c("x", "y", "value")
      
      # Normalize density values
      density_df$value <- scales::rescale(density_df$value)
      
      # Add gear type to the data frame
      density_df$gear <- gear_type
      
      # Append to the density list
      density_list[[gear_type]] <- density_df
    }
    
    # Combine all densities into a single data frame
    combined_density <- do.call(rbind, density_list)
    
    # Determine bounding box of the significant density area
    significant_density <- combined_density %>% filter(value > 0.01) # Filter for non-zero density
    bbox <- significant_density %>%
      summarize(
        xmin = min(x),
        xmax = max(x),
        ymin = min(y),
        ymax = max(y)
      )
    
    # Add a buffer to the bounding box
    buffer <- 0.1  # Adjust as needed for desired zoom level
    bbox <- bbox %>%
      mutate(
        xmin = xmin - buffer,
        xmax = xmax + buffer,
        ymin = ymin - buffer,
        ymax = ymax + buffer
      )
    
    # Plot the combined densities
    p <- ggplot() +
     # geom_tile(data = combined_density, aes(x = x, y = y, fill = value)) +
     # scale_fill_gradientn(name = "Tow minutes", 
      #                     colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]), 
       #                    limits = c(0, max(combined_density$value)), 
        #                   na.value = "white") +
      # Add contour lines for each gear type
      geom_contour(data = combined_density, aes(x = x, y = y, z = value, color = gear, linetype = gear),
                   breaks = 0.75, size = 0.5) +
      # Land
      geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = FALSE) +
      # State waters
      geom_sf(data = state_waters, color = "grey30", linewidth = 0.2, inherit.aes = FALSE) +
      # Set zoomed-in limits for the coordinate system
      coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
      # Axes
      scale_x_continuous(breaks = seq(-126, -118, 2)) +
      scale_y_continuous(breaks = seq(32, 42, 2)) +
      ggtitle(paste0(portcomplex, ' Fishing Grounds'), 
              subtitle = paste0('Squid Fishery: Year ', year, ', with 75% volume contour')) +
      theme_bw() + base_theme +
      theme(axis.title = element_blank()) +
      scale_color_manual(values = c("Drum seine" = "blue", "Purse seine" = "red"), name = "Gear Type") +
      scale_linetype_manual(values = c("Drum seine" = "solid", "Purse seine" = "dashed"), name = "Gear Type")
    
    # Save the plot
    name <- paste0(squid_data, 'Figures/Kernel Density/', portcomplex, '/', portcomplex, '_', year, '.jpg')
    ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
  }
}
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
############

# Prepare the data
nmds_data <- explor %>%
  filter(gear %in% c("Drum seine", "Purse seine")) %>%  # Filter for relevant gears
  dplyr::select(depth_fa, duration_min, block_id,time_start,time_end) %>%  # Select relevant numerical variables
  mutate_if(is.character, as.numeric) %>%  # Convert character columns to numeric if needed
  na.omit()  # Remove rows with missing data

# Compute a dissimilarity matrix
dissimilarity_matrix <- dist(nmds_data, method = "euclidean")  # Use Euclidean distance

# Perform NMDS
nmds_result <- metaMDS(dissimilarity_matrix, k = 2, trymax = 100)  # 2D solution

# Add NMDS scores to the data
nmds_scores <- as.data.frame(scores(nmds_result))  # Extract NMDS axis scores
nmds_scores$gear <- explor %>%
  filter(gear %in% c("Drum seine", "Purse seine")) %>%
  na.omit() %>%
  pull(gear)

# Visualize NMDS results
p <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = gear)) +
  geom_point(alpha = 0.7, size = 2) +  # Points with semi-transparency
  labs(
    title = "NMDS of Drum Seine vs. Purse Seine",
    x = "NMDS Axis 1",
    y = "NMDS Axis 2",
    color = "Gear Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot the result
print(p)