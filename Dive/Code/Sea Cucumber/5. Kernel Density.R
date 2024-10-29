
library(dplyr)
library(sf)
library(sp)
library(spatstat)
library(raster)
library(ggplot2)
library(magick)
library(ggforce)
library(RColorBrewer)


##################
#Read in data

#edited lobster data from the block script
explor<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seacucumber_filtered.rds")

#cog data
COG<-readRDS('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/COG_distance_results_divehours_final_cucumber.rds')

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

#port codes
port<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Raw Data/Port Codes.csv")

# State waters
state_waters <- readRDS( "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Raw Data/CA_state_waters_polyline.Rds")

# block data
all_blocks<-st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Raw Data/fishingblockstatistics.csv")%>%
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
#Kernel Densities - All years combined

#summarizing across years to see what one graph with all of the data should kinda look like
allports.acrossyears<- explor %>%
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
  group_by( port.complex, block_id) %>%
  summarize(total_dive_hours = sum(total_hours, na.rm = TRUE),
            .groups = 'drop')

allports2<-merge(x = allports.acrossyears, y = all_blocks_df, by = "block_id", all.x = TRUE)

#remove block IDs of NA
allports2<-allports2[!is.na(allports2$block_long_dd),] 

sf_df<- allports2 %>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84 +no_defs")

#order from north to south
sf_df$port.complex<- factor(sf_df$port.complex, levels = c('Santa Barbara', 'Los Angeles', 'San Diego'))

#remove any bad spatial blocks
sf_df<- sf_df%>% filter(!st_is_empty(.))

plot_fun<-function(portcomplex){ 
  
  #filtering for location
  location<-sf_df %>%
    filter(port.complex==portcomplex)
  
  #extracting coordinates and weights
  coordinates<-st_coordinates(location)
  weights<-location$total_dive_hours
  
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
  contour_level2 <- density_values[which.min(abs(cdf - 0.75))]
  contour_level1 <- density_values[which.min(abs(cdf - 0.50))]
  contour_level3 <- density_values[which.min(abs(cdf - 0.95))]
  
  #raw data map
  p<-ggplot() +
    geom_raster(data = density_df, aes(x=x,y=y,fill = value)) +
    
    scale_fill_gradientn(name = "Dive Hours: \nYears 1998 - 2023", 
                         colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                         limits = c(0, max(density_df$value)),  # Adjust limits as needed
                         na.value = "white") +  # Set NA values to white
    
    # Add contour lines at 75% volume boundary
    geom_contour(data = density_df, aes(x = x, y = y, z = value), 
                 breaks = contour_level2, color = "black", size = 0.75) +
    # Land
    geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    # Set the limits for the coordinate system
    coord_sf(xlim = c(-117, -121.5), ylim = c(32, 35)) +
    # Axes
    scale_x_continuous(breaks=seq(-121.5, -117, 1.0),labels=scales::number_format(accuracy=0.01)) +
    scale_y_continuous(breaks=seq(32, 35, 1.0),labels=scales::number_format(accuracy=0.01)) + 
    # Theme
    theme_bw() + base_theme +
    ggtitle(paste0(portcomplex, ' Fishing Grounds') , subtitle=" Sea cucumber Dive Fishery With 75% volume contour")+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  
  name=paste0('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/',portcomplex,'/',portcomplex,' Kernel Densiy_All Years_test.jpg')
  ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)
  
  
}

plot_fun("Santa Barbara")
plot_fun("Los Angeles")
plot_fun("San Diego")
############


############
#Kernel Densities - years separated 

#yearly data
df<- explor %>%
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
            .groups = 'drop')

df<-merge(x =df, y = all_blocks_df, by = "block_id", all.x = TRUE)

#remove block IDs of NA
df<-df[!is.na(df$block_long_dd),] 

df_sf<- df%>%
  as.data.frame()%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = "+proj=longlat +datum=WGS84")

#order from north to south
df_sf$port.complex<- factor(df_sf$port.complex, levels = c('Santa Barbara', 'Los Angeles', 'San Diego'))

df_sf<- df_sf%>% filter(!st_is_empty(.))

#Kernel density figure with COG
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
COG_sf<-st_transform(COG_sf, crs=("+proj=longlat +datum=WGS84"))
COG_sf$year=as.factor(COG_sf$year)

plot_fun<-function(portcomplex){ 
  
  location <- df_sf%>%
    filter(port.complex==portcomplex)%>%
    group_by(year)%>%
    filter(n() > 1) %>% #kernel densities need more than one block in a year to run.
    ungroup()
  #get unique years in dataset
  unique_years<-sort(unique(location$year))
  
  #Loop through each year and calculate kernel density
  for(year in unique_years){
    
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(year_data)
    
    #weights
    weights<-year_data$total_dive_hours
    
    # Create a point pattern (ppp object)
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
    
    # Find the contour level corresponding to volume
    contour_level2 <- density_values[which.min(abs(cdf - 0.75))]
    contour_level1 <- density_values[which.min(abs(cdf - 0.50))]
    contour_level3 <- density_values[which.min(abs(cdf - 0.95))]
    
    a<-COG_sf%>%
      filter(port.complex==portcomplex)
    #subset data for the current year
    b<-a[a$year==year,]
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(a)
    # Create a point pattern (ppp object)
    minx<-(min(coordinates[,1]))
    maxx<-(max(coordinates[,1]))
    miny<-(min(coordinates[,2]))
    maxy<-(max(coordinates[,2]))
    
    p<-ggplot() +
      geom_raster(data = density_df, aes(x = x, y = y, fill = value)) +
      
      scale_fill_gradientn(name = "Dive Hours", 
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                           limits = c(0, max(density_df$value)),  # Adjust limits as needed
                           na.value = "white") +  # Set NA values to white
      
      # Add contour lines at 75% volume boundary
      geom_contour(data = density_df, aes(x = x, y = y, z = value), 
                   breaks = contour_level2, color = "black", size = 0.5, show.legend = TRUE) +
      # Land
      geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      #points COG
      geom_sf(data=b,size=5, fill="white",color="black",pch=21)+
      # Crop
      coord_sf(xlim = c(minx-.5, maxx+.5), ylim = c(miny-.5, maxy+.5)) +
      # Axes
      scale_x_continuous(breaks=seq(minx-.5, maxx+.5, .5),labels=scales::number_format(accuracy=0.50)) +
      scale_y_continuous(breaks=seq(miny-.5, maxy+.5, .5),labels=scales::number_format(accuracy=0.50)) + 
      # Theme
      theme_bw() + base_theme +
      ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle=paste0('Sea Cucumber Dive Fishery: Year ',year, ', with 75% volume contour') )
    
    
    name=paste0('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/',portcomplex,'/',portcomplex,' Kernel Density_',year,'.jpg')
    ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)
    
  }
}
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
plot_fun('San Diego')

#importing yearly images and creating an animation
years<-c(1998,2003:2022)
frames<-paste0("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/Santa Barbara/Santa Barbara Kernel Density_",years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/Santa Barbara/Santa Barbara Kernel Densities_Animation.gif")

years<-c(2003:2022)
frames<-paste0("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/Los Angeles/Los Angeles Kernel Density_", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/Los Angeles/Los Angeles Kernel Densities_Animation.gif")

years<-c(2004,2007:2010,2012,2016, 2018,2019,2021)
frames<-paste0("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/San Diego/San Diego Kernel Density_", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/San Diego/San Diego Kernel Densities_Animation.gif")
############



############
#time series analyses
#volume contours through time

plot_fun<-function(portcomplex){ 
  
  location <- df_sf%>%
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
    weights<-year_data$total_dive_hours
    
    # Create a point pattern (ppp object)
    minx<-(min(coordinates[,1]))
    maxx<-(max(coordinates[,1]))
    miny<-(min(coordinates[,2]))
    maxy<-(max(coordinates[,2]))
    
    points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
    
    # Compute kernel density estimation using raw data
    sigma<-bw.diggle(points_ppp)
    density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE)
    
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
    contour_level_50 <- density_values[which.min(abs(cdf - 0.50))]
    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    contour_level_95 <- density_values[which.min(abs(cdf - 0.95))]
    
    #Find area
    volume_50 <- sum(density_df$value[density_df$value >= contour_level_50])
    volume_75 <- sum(density_df$value[density_df$value >= contour_level_75])
    volume_95 <- sum(density_df$value[density_df$value >= contour_level_95])
    
    # Find the most northern and southern latitudes for the 75% volume contour
    filtered_df_75 <- density_df[density_df$value >= contour_level_75, ]
    northernmost_latitude_75 <- max(filtered_df_75$y, na.rm = TRUE)
    southernmost_latitude_75 <- min(filtered_df_75$y, na.rm = TRUE)
    
    #save in new dataframe with year so it can be in the animations. NEed to save density_df, contour_level, year
    df <- data.frame(port.complex=portcomplex,
                     Year = year,
                     volume_50=volume_50,
                     volume_75=volume_75,
                     volume_95=volume_95,
                     northernmost_latitude_75=northernmost_latitude_75,
                     southernmost_latitude_75=southernmost_latitude_75 )
    
    
    # Append current year's results to combined_df
    datalist[[year]] <- df
  }
  
  all.data<- dplyr::bind_rows(datalist)
  
}

a<-plot_fun("Santa Barbara")
b<-plot_fun("Los Angeles")
c<-plot_fun("San Diego")

all.volumes<-rbind(a,b,c)

all.volumes<-all.volumes%>%
  mutate(vol_50_km=volume_50/1000)%>%
  mutate(vol_75_km=volume_75/1000)%>%
  mutate(vol_95_km=volume_95/1000)%>%
  left_join(mpa,by="port.complex")%>%
  mutate(Year=as.numeric(Year))


saveRDS(all.volumes,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seacucumber_all contour volumes.rds")

#order from north to south
all.volumes$port.complex<- factor(all.volumes$port.complex, levels = c( 'Santa Barbara', 'Los Angeles', 'San Diego'))


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
  ggsave(plot = p_save, filename = paste0('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/Volume Contours_page_', i, '.jpg'),width = 16, height = 8, units = "in")
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
  
  
  filename1 <- paste0('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Figures/Cucumber/Kernel Density/', portcomplex,'/',portcomplex, '_COG_extents_latitude.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun('San Diego')
############
