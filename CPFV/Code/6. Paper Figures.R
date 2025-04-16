library(tidyverse)
library(sf)
library(sp)
library(spatstat)
library(raster)
library(RColorBrewer)
library(ggpubr)
library(patchwork)

##################
#Read in data
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
CPFV_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/'

#edited cpfv data from the block script
explor<-readRDS(paste0(CPFV_data,"Data/Processed Data/CPFV_filtered.rds"))

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
usa_mexico_sf_proj<- st_transform(usa_mexico_sf, crs = 3310)

# State waters
state_waters <- readRDS( paste0(general_data,"CA_state_waters_polyline.Rds"))
state_waters<- st_transform(state_waters, crs = 3310)

#cog data
COG<-readRDS(paste0(CPFV_data,'Data/Processed Data/COG_distance_results_anglerhours_final.rds'))
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform(crs = 3310)%>%
  mutate(distance_shore_km=cog_dist_shore/1000)
COG_sf$year=as.factor(COG_sf$year)

COG_df<-COG_sf%>%
  sf::st_drop_geometry()
COG_df$COG_latitude <- st_coordinates(COG_sf)[, "Y"]  # Extracts latitude
COG_df$COG_longitude <- st_coordinates(COG_sf)[, "X"]

#get time series data - latitude, extents, distance to shore, mpa, area
filepath<-('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/')

area_extents<- readRDS(paste0(filepath,'CPFV_all contour volumes.rds'))
lat_dist<- readRDS(paste0(filepath, "COG_distance_results_anglerhours_final.rds"))%>%
  rename(Year=year)
mpa<- readRDS(paste0(filepath, "MPA_area.rds"))

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


#mpas
mpa_blocks<-wcfish::mpas_ca
mpa_blocks<- st_transform(mpa_blocks, crs = 3310)%>%
  st_make_valid()
mpa_blocks <- st_cast(mpa_blocks, "POLYGON")
# Union the MPAs so they are treated as a single shape
mpa_blocks <- st_union(mpa_blocks)
mpa_blocks <- st_make_valid(mpa_blocks)

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
############

############
#Kernel Densities - All years combined

#summarizing across years to see what one graph with all of the data should kinda look like
sf_df<- explor %>%
  # Step 1: Remove duplicates within each logbookID_vesselID_date and calculate distinct angler_hours
  group_by(logbookID_vesselID_date, angler_hours) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct angler_hours for each logbookID_vesselID_date
  group_by(logbookID_vesselID_date) %>%
  summarize(total_angler_hours = sum(angler_hours), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(logbookID_vesselID_date, year, port.complex, block_id,hours_fished, vessel_id), by = "logbookID_vesselID_date") %>%
  distinct()%>%
  # Step 4: Group by  port, and block to aggregate results
  group_by(port.complex, block_id) %>%
  summarize(ntrips = n_distinct(logbookID_vesselID_date),
            total_angler_hours = sum(total_angler_hours, na.rm = TRUE),
            angler_hours_per_trip=(total_angler_hours/ntrips),
            nvessels=n_distinct(vessel_id),
            
            # same as hours_fished but oh well
            .groups = 'drop')%>%
  #join with block data
  left_join(all_blocks_df, by="block_id")%>%
  #remove block IDs of NA
  filter(!is.na(block_long_dd))%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  #remove any bad spatial blocks
  filter(!st_is_empty(.))

sf_df_projected <- st_transform(sf_df, crs = 3310)
############

############
#all years with multiple ports 
plot_fun<-function(portcomplex){ 
  
  location<- sf_df_projected%>%
    filter(port.complex==portcomplex)
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(location)
  weights<-location$total_angler_hours
  
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
  contours <- rasterToContour( density_masked, levels = c(contour_level_75))
  
  #convert to sf object
  contours_sf <- st_as_sf(contours)
  #set crs
  contours_sf <- st_set_crs(contours_sf , 3310)
  
  # Normalize density values to the range 0-1
  density_df$value <- scales::rescale(density_df$value)
  
  
  p<-ggplot() +
    geom_raster(data = density_df, aes(x=x,y=y,fill = value)) +
    
    scale_fill_gradientn(name = "Kernel Density", 
                         colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                         limits = c(0, max(density_df$value)),  # Adjust limits as needed
                         na.value = "white") +  # Set NA values to white
    
    # Add contour lines at 75% volume boundary
    geom_sf(data = contours_sf, color = "black", linewidth = 1)  +
    
    # Land
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    # Crop
    coord_sf(xlim = c(-470000, 270000), ylim = c(-600000, 460000)) +
    # Theme
    theme_bw() + base_theme +
    ggtitle(paste0(portcomplex))
  
  p
}

a<-plot_fun("Eureka")
b<-plot_fun('Fort Bragg')
c<-plot_fun('Bodega Bay')
d<-plot_fun('San Francisco')
e<-plot_fun('Monterey')
f<-plot_fun('Morro Bay')
g<-plot_fun('Santa Barbara')
h<-plot_fun('Los Angeles')

combined_plot <- a+b+c+d+e+f+g+h+
  plot_layout(ncol = 4, nrow=2 ) &
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.text.x = element_blank(),  # Remove x-axis text
        axis.title.x = element_blank(),# Remove x-axis title
        axis.ticks.y = element_blank(), # Remove y-axis ticks
        axis.title.y = element_blank(), # Remove y-axis title
        legend.position = "none", #Hide legends by default
        plot.margin=margin(1,1,1,1)
  )      

# Add the y-axis back only for the first plot
combined_plot[[1]] <- combined_plot[[1]] + theme(
  axis.text.y = element_text(size=12),
  axis.ticks.y = element_line()
  
)

combined_plot[[5]] <- combined_plot[[5]] + theme(
  axis.text.y = element_text(size=12),
  axis.ticks.y = element_line(),
  axis.text.x = element_text(size=12),
  axis.ticks.x = element_line()
)

combined_plot[[6]] <- combined_plot[[6]] + theme(
  axis.text.x = element_text(size=12),
  axis.ticks.x = element_line()
)

combined_plot[[7]] <- combined_plot[[7]] + theme(
  axis.text.x = element_text(size=12),
  axis.ticks.x = element_line()
)

combined_plot[[8]] <- combined_plot[[8]] + theme(
  axis.text.x = element_text(size=12),
  axis.ticks.x = element_line()
)


combined_plot[[4]] <- combined_plot[[4]] + theme(legend.position = "right",  # Place the legend to the far right for plot 6
                                                 legend.title = element_text(size = 12),  # Smaller font size for legend title
                                                 legend.text = element_text(size = 10) ) 

combined_plot
filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CPFV_allports_allyears.jpg')
ggsave(filename = filename1, plot =   combined_plot , width = 15, height = 8, units = "in", dpi = 300)

#############

############
#Kernel Densities - years separate

#summarizing yearly
sf_df<- explor %>%
  # Step 1: Remove duplicates within each logbookID_vesselID_date and calculate distinct angler_hours
  group_by(logbookID_vesselID_date, angler_hours) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct angler_hours for each logbookID_vesselID_date
  group_by(logbookID_vesselID_date) %>%
  summarize(total_angler_hours = sum(angler_hours), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(logbookID_vesselID_date, year, port.complex, block_id,hours_fished, vessel_id), by = "logbookID_vesselID_date") %>%
  distinct()%>%
  # Step 4: Group by  port, and block to aggregate results
  group_by(port.complex, year, block_id) %>%
  summarize(ntrips = n_distinct(logbookID_vesselID_date),
            total_angler_hours = sum(total_angler_hours, na.rm = TRUE),
            angler_hours_per_trip=(total_angler_hours/ntrips),
            nvessels=n_distinct(vessel_id),
            .groups = 'drop')%>%
  #join with block data
  left_join(all_blocks_df, by="block_id")%>%
  #remove block IDs of NA
  filter(!is.na(block_long_dd))%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")%>%
  #remove any bad spatial blocks
  filter(!st_is_empty(.))

sf_df_projected <- st_transform(sf_df, crs = 3310)
############


############
# Figure 1 no heat map, first volume contour and last volume contour per port complex. 

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
    weights<-year_data$total_angler_hours
    
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
    contours <- rasterToContour( density_masked, levels = c(contour_level_75))
    
    #convert to sf object
    contours_sf <- st_as_sf(contours)
    #set crs
    contours_sf <- st_set_crs(contours_sf , 3310)
    
    # Add the 'year' column to the contours_sf for later identification
    contours_sf$Year <- year
    contours_sf$port.complex <- portcomplex  # Add the port complex information
    
    # Append current year's contours to the list
    datalist[[year]] <- contours_sf
  }
  
  all.data<- dplyr::bind_rows(datalist)
  
}

a<-plot_fun("Eureka")
b<-plot_fun("Fort Bragg")
c<-plot_fun("Bodega Bay")
d<-plot_fun("San Francisco")
e<-plot_fun("Monterey")
f<-plot_fun("Morro Bay")
g<-plot_fun("Santa Barbara")
h<-plot_fun("Los Angeles")

df<-rbind(a,b,c,d,e,f,g,h)

vc<-df%>%
  group_by(port.complex)%>%
  filter(Year== min(Year) | Year== max(Year))%>%
  mutate(Year_Type = ifelse(Year == min(Year), "First", "Last")) 

vc$port.complex<- factor(vc$port.complex, levels = c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles'))

p<-ggplot() +
  # Land
  geom_sf(data=usa_mexico_sf_proj, fill="gray", col="black", linewidth=0.2, inherit.aes = F) +
  # Lines connecting points for the same port complex
  geom_sf(data = vc, aes(color = port.complex, linetype=Year_Type), linewidth = 0.8)+
  # Crop
  coord_sf(xlim = c(-470000, 270000), ylim = c(-600000, 460000)) +
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1,
                     guide = guide_legend(reverse = FALSE) )+
  base_theme

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CPFV_Allports_FirstLast.jpg')
ggsave(filename = filename1, plot = p, width = 15, height = 8, units = "in", dpi = 300)

############



############
# Figure 2 one port with kernel densities 
location <- sf_df_projected%>%
  filter(port.complex=="Monterey")%>%
  group_by(year)%>%
  filter(n() > 1) %>%
  ungroup()%>%
  filter(year== min(year) | year== max(year))%>%
  mutate(Year_Type = ifelse(year == min(year), "First Year", "Last Year")) 

plot_fun<-function(type){ 
  
  year_data<- location%>%
    filter(Year_Type==type)
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(year_data)
  weights<-year_data$total_angler_hours
  
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
  contours <- rasterToContour( density_masked, levels = c(contour_level_75))
  
  #convert to sf object
  contours_sf <- st_as_sf(contours)
  #set crs
  contours_sf <- st_set_crs(contours_sf , 3310)
  
  # Normalize density values to the range 0-1
  density_df$value <- scales::rescale(density_df$value)
  
  
  p<-ggplot() +
    geom_raster(data = density_df, aes(x=x,y=y,fill = value)) +
    
    scale_fill_gradientn(name = "Kernel Density", 
                         colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                         limits = c(0, max(density_df$value)),  # Adjust limits as needed
                         na.value = "white") +  # Set NA values to white
    
    # Add contour lines at 75% volume boundary
    geom_sf(data = contours_sf, color = "black", linewidth = 1)  +
    
    # Land
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    # Crop
    coord_sf(xlim = c(-350000, 6000), ylim = c(-250000, 20000))+
    # Theme
    theme_bw() + base_theme +
    ggtitle(paste0(type))
  
  p
}

a<-plot_fun("First Year")
b<-plot_fun("Last Year")

combined_plot <- a+b+
  plot_layout(ncol = 2, nrow=1 ) &
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.title.x = element_blank(),# Remove x-axis text
        axis.ticks.y = element_blank(), # Remove y-axis ticks
        axis.title.y = element_blank(), # Remove y-axis title
        legend.position = "none", #Hide legends by default
        plot.margin=margin(0,0,0,0)
  )      

combined_plot[[1]] <- combined_plot[[1]] + theme(
  axis.text.y = element_text(size=12),
  axis.ticks.y = element_line(),
  axis.text.x = element_text(size=12)
)

combined_plot[[2]] <- combined_plot[[2]] + theme(legend.position = "right",  # Place the legend to the far right for plot 6
                                                 legend.title = element_text(size = 12),  # Smaller font size for legend title
                                                 legend.text = element_text(size = 10) ) 


combined_plot
filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CPFV_Monterey_twoyears.jpg')
ggsave(filename = filename1, plot =   combined_plot , width = 15, height = 8, units = "in", dpi = 300)

############

############
# multi-panel figure #1981 and 2021
location <- sf_df_projected%>%
  filter(port.complex=="Eureka")%>%
  group_by(year)%>%
  filter(n() > 1) %>%
  ungroup()%>%
  filter(year== min(year) | year== max(year))

plot_fun<-function(Year){ 
  
  year_data<- location%>%
    filter(year==Year)
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(year_data)
  weights<-year_data$total_angler_hours
  
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
  contours <- rasterToContour( density_masked, levels = c(contour_level_75))
  
  #convert to sf object
  contours_sf <- st_as_sf(contours)
  #set crs
  contours_sf <- st_set_crs(contours_sf , 3310)
  
  # Normalize density values to the range 0-1
  density_df$value <- scales::rescale(density_df$value)
  #center of gravity
  a<-COG_sf%>%
    filter(port.complex=="Eureka")%>%
    filter(year==Year)
  
  
  p<-ggplot() +
    geom_raster(data = density_df, aes(x=x,y=y,fill = value)) +
    
    scale_fill_gradientn(name = "Kernel Density", 
                         colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                         limits = c(0, max(density_df$value)),  # Adjust limits as needed
                         na.value = "white") +  # Set NA values to white
    # Black ticks to color bar
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Add contour lines at 75% volume boundary
    geom_sf(data = contours_sf, color = "black", linewidth = 1)  +
    # Land
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    #mpas
    geom_sf(data=mpa_blocks, fill="black", col="white", linewidth=0.2, inherit.aes = F)+
    #points COG
    geom_sf(data=a,size=4, fill="white",color="black",pch=21)+
    # Crop
    coord_sf(xlim = c(-500000, -200000), ylim = c(190000, 450000),
             crs = 3310,
             label_graticule = "SW",   # Only label axes, not plot
             datum = 4326)+
    #scale
    scale_x_continuous(breaks = seq(-125.5, -122.5, by = 1.0)) +
    # Theme
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    base_theme +
    ggtitle(paste0(Year))
  
}

a<-plot_fun(1980)
b<-plot_fun(2022)

plot(a)


#other parts
combined_eureka<-area_extents%>%
  filter(port.complex=="Eureka")%>%
  left_join(lat_dist, by=c("port.complex","Year"))%>%
  left_join(mpa, by=c("port.complex","Year"))

combined_eureka_years<-combined_eureka%>%
  filter(Year %in% c(1980,2022))

d<-ggplot(data=combined_eureka, aes(x=Year))+
  #add line
  geom_line(aes(y=area_75_km2 ),size=0.5)+
  #label y axis
  labs(y = bquote(atop("Fishing ground area", "(" ~ km^2 ~ ")"))) +  # km^2 in parentheses and centered
  #add point for years
  geom_point(data =combined_eureka_years, 
             aes(x = Year, y = area_75_km2 ), 
             color = "blue", 
             size = 4,
             shape=8) +  
  #theme
  theme_bw() + base_theme  +
  #remove y axis
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #add red line for mpa implementation and gray box for marine heatwave
  annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
  geom_vline(aes(xintercept = mpayear.x), linetype = "dotted", color = "red",size=1)



#COG and northern and southern latitudes
COG_sf <- st_transform(COG_sf, crs = 4326)

COG_df<-COG_sf%>%
  sf::st_drop_geometry()%>%
  mutate(year=as.numeric(as.character(year)))

COG_df$COG_latitude <- st_coordinates(COG_sf)[, "Y"]  # Extracts latitude
COG_df$COG_longitude <- st_coordinates(COG_sf)[, "X"]

COG_vcontours<-combined_eureka%>%
  rename(year=Year)%>%
  mutate(year=as.numeric(as.character(year)))%>%
  left_join(COG_df, by=c("port.complex","year"))

COG_vcontours_years<-COG_vcontours%>%
  filter(year %in% c(1980,2022))

colors <- brewer.pal(n = 11, name = "RdYlBu")

c<-ggplot(data=COG_vcontours, aes(x=year))+
  geom_line(aes(y=COG_latitude, color = "COG"),size=.5)+
  geom_line(aes(y=northernmost_latitude_75, color = "Northern extent"),size=.5)+
  geom_line(aes(y=southernmost_latitude_75, color = "Southern extent"),size=.5)+
  #labs
  labs(y = ('Latitude \n(°N)'), x='Year') +
  # Color scale from RColorBrewer
  # Apply specific colors using scale_color_manual
  scale_color_manual(values = c("Northern extent" = colors[10],
                                "COG"= "black",
                                "Southern extent" =  colors[2]),
                     breaks = c("Northern extent", "COG", "Southern extent")) +  # Control legend order
  geom_point(data =COG_vcontours_years, 
             aes(x = year, y = COG_latitude), 
             color = "blue", 
             size = 4,
             shape=8) +  
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none")+
  #marine heatwave and mpa implementation
  annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
  geom_vline(aes(xintercept = 2012), linetype = "dotted", color = "red",size=1)

e<-ggplot(data=combined_eureka, aes(x=Year))+
  geom_line(aes(y=cog_dist_shore),size=.5)+
  labs(y = ('Distance from shore \n(km)'), x='Year')+
  #point to highlight years
  geom_point(data =combined_eureka_years, 
             aes(x = Year, y = cog_dist_shore), 
             color = "blue", 
             size = 4,
             shape=8) +  
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank())+
  #marine heatwave and mpa implementation
  annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
  geom_vline(aes(xintercept = 2012), linetype = "dotted", color = "red",size=1)

f<-ggplot(data=combined_eureka, aes(x=Year))+
  geom_line(aes(y=percent_inmpa),size=.5)+
  #y label
  labs(y = ('Effort in MPA \n(%)'), x='Year')+
  #point to highlight years
  geom_point(data =combined_eureka_years, 
             aes(x = Year, y = percent_inmpa ), 
             color = "blue", 
             size = 4,
             shape=8) +  
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank())+
  #marine heatwave and mpa implementation
  annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
  geom_vline(aes(xintercept = 2012), linetype = "dotted", color = "red",size=1)

###########

a <- a + theme(
  legend.position = "none",
  axis.text.y=element_text(size=10),
  axis.text.x=element_text(size=10)
)

b <- b + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x=element_text(size=10)
)


top_row <- a + b
bottom_row <- (c + d + e + f)  &
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

combined_plot <- top_row / bottom_row

combined_plot

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CPFV_Eureka_methodsfigure.jpg')
ggsave(filename = filename1, plot =   combined_plot , width = 10, height = 8, units = "in", dpi = 300)
