
library(dplyr)
library(sf)
library(sp)
library(spatstat)
library(raster)
library(ggplot2)

##################
#Read in data

#edited lobster data from the block script
explor2<-readRDS("Lobster/Data/Processed Data/lobster_filtered.rds")

#cog data
COG<-read.csv('COG_distance_results_traphours_final!.csv')%>%
  dplyr::select(-X)

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

#port codes
port<-read.csv("Lobster/Data/Raw Data/Port Codes.csv")

# State waters
state_waters <- readRDS( "Lobster/Data/Raw Data/CA_state_waters_polyline.Rds")

# block data
all_blocks<-st_read("Lobster/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv("Lobster/Data/Raw Data/fishingblockstatistics.csv")%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

############

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
#Kernal Densities - All years combined

#summarizing across years to see what one graph with all of the data should kinda look like
allports.acrossyears<- explor2 %>%
  #summarize by, port, block
  group_by(port.complex,block_id)%>%
  summarize(ntrips=n(),
            nvessels=n_distinct(vessel_id),
            trap_nights=sum(trap_nights))%>%
  ungroup()

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
  weights<-location$trap_nights
  
  # Create a point pattern (ppp object)
  minx<-(min(coordinates[,1]))
  maxx<-(max(coordinates[,1]))
  miny<-(min(coordinates[,2]))
  maxy<-(max(coordinates[,2]))
  
  points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
  
  # Compute kernel density estimation using raw and logcdata
  sigma<-bw.diggle(points_ppp)
  density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE, eps=0.05)
  
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
    
    scale_fill_gradientn(name = "Kernel Density of Trap Nights: \nYears 1980 - 2022", 
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
ggtitle(paste0( portcomplex, ' Fishing Grounds') , subtitle=" Lobster Fishery With 75% volume contour")+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())

  
  name=paste0('Lobster/Figures/Kernel Density/',portcomplex,'/',portcomplex,' Kernel Densiy_All Years_test.jpg')
  ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)
 

}

plot_fun("Santa Barbara")
plot_fun("Los Angeles")
plot_fun("San Diego")

############

############
#Kernel Densities - years separated 

#yearly data
df<- explor2 %>%
  #get rid of certain ports
  
  #summarize by, port, block
  group_by(year,port.complex,block_id)%>%
  summarize(ntrips=n(),
            nvessels=n_distinct(vessel_id),
            trap_nights=sum(trap_nights))%>%
  ungroup()

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
    weights<-year_data$trap_nights
    
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
      
      scale_fill_gradientn(name = "Kernel Density of Trap Nights", 
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
      ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle=paste0('Lobster Fishery: Year ',year, ', with 75% volume contour') )
  
    
    name=paste0('Lobster/Figures/Kernel Density/',portcomplex,'/',portcomplex,' Kernel Density_',year,'.jpg')
    ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)

  }
}
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
plot_fun('San Diego')

#importing yearly images and creating an animation
years<-c(1980:1990,1993:2022)
frames<-paste0("Lobster/Figures/Kernel Density/Santa Barbara/Santa Barbara Kernel Density_",years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,"Lobster/Figures/Kernel Density/Santa Barbara/Santa Barbara Kernel Densities_Animation.gif")

years<-c(1980:1990,1993:2022)
frames<-paste0("Lobster/Figures/Kernel Density/Los Angeles/Los Angeles Kernel Density_", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,"Lobster/Figures/Kernel Density/Los Angeles/Los Angeles Kernel Densities_Animation.gif")

years<-c(1980:1990,1993:2022)
frames<-paste0("Lobster/Figures/Kernel Density/San Diego/San Diego Kernel Density_", years,".jpg")
m<-image_read(frames)
m<-image_animate(m, fps=1)
image_write(m,"Lobster/Figures/Kernel Density/San Diego/San Diego Kernel Densities_Animation.gif")

############


############
#Coastwide figure with port complexes and fishing ground shifts

#filter for first and last year which will be actual points on the plot
COG1<-COG%>%
  filter(year=="1980" | year=="2022")%>%
  rename(Year=year)

COG_sf<-st_as_sf(COG1, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
COG_sf<-st_transform(COG_sf,crs = "+proj=longlat +datum=WGS84")
COG_sf$Year=as.factor(COG_sf$Year)

COG_sf$port.complex<- factor(COG_sf$port.complex, levels = c( 'Santa Barbara', 'Los Angeles', 'San Diego'))

#get all spatial data for the path
COG2<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
COG2<-st_transform(COG2,crs = "+proj=longlat +datum=WGS84")
COG2$Year=as.factor(COG2$year)

# Group by port.complex and combine the geometries
combined_geometries <- COG2 %>%
  group_by(port.complex) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# Cast the combined geometries to LINESTRING
linestring <- st_cast(combined_geometries, "LINESTRING")
linestring $port.complex<- factor(linestring $port.complex, levels = c('Santa Barbara', 'Los Angeles', 'San Diego'))
cali<-usa%>%
  filter(name=="California")

p<-ggplot() +
  # Land
  geom_sf(data = cali, fill = "gray", col = "black", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="gray", col="black", linewidth=0.2, inherit.aes = F) +
  # Lines connecting points for the same port complex
  geom_sf(data = linestring, aes(color = port.complex), linewidth = 0.8) +
  # points
  geom_sf(data=COG_sf,aes(fill="black", shape=Year),size=2)+
  # Crop
  coord_sf(xlim = c(-121, -118), ylim = c(32, 35)) +
  # Axes
  scale_x_continuous(breaks=seq(-121, -118, 2)) +
  scale_y_continuous(breaks=seq(32, 45, 2)) + 
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

  ggtitle(('Lobster Fishing Grounds'), subtitle = 'Shifts in Center of Gravity')+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1)


filename1 <- ('Lobster/Figures/Kernel Density/AllPorts_mapped_COG_artsy.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)

############


############
#time series analyses

#COG distance from shore and latitude 
mpa<-mpa%>%
  rename(Portcomplex=port.complex)

df<-COG%>%
  rename(Portcomplex=port.complex)%>%
  mutate(distance_shore_km=cog_dist_shore/1000)%>% #might have to update if the units change with the COG calculation
  left_join(mpa,by="Portcomplex")


plot_fun<-function(portcomplex){
  
  location<-df%>%
    filter(Portcomplex==portcomplex)
  
  p<-ggplot(data=location, aes(x=year))+
    geom_line(aes(y=distance_shore_km),size=1)+
    labs(y = bquote('Distance from shore'~(km^2))) +
    # Theme
    theme_bw() + 
    base_theme  +
    theme(legend.title = element_blank())+
    annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
    geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "red",size=1)
  
  
  filename1 <- paste0('Lobster/Figures/Kernel Density/', portcomplex, '_COG_distanceshore.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun("San Diego")


#latitude
df1<-st_as_sf(df, coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
df2<-st_transform(df1, st_crs("+proj=longlat  +datum=WGS84 +units=m +no_defs"))

df2$latitude <- st_coordinates(df2)[, "Y"]  # Extracts latitude
df2$longitude <- st_coordinates(df2)[, "X"]
df2<-df2 %>% sf::st_drop_geometry()

plot_fun<-function(portcomplex){
  
  location<-df2%>%
    filter(Portcomplex==portcomplex)
  
  p<-ggplot(data=location, aes(x=year))+
    geom_line(aes(y=latitude),size=1)+
    labs(y = ('Latitude °N')) +
    # Theme
    theme_bw() + 
    base_theme  +
    theme(legend.title = element_blank())+
    #adding heatwave informaiton
    annotate('rect', xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, alpha=.2, fill='black') + # Font size and style for y-axis label
    #adding mpa implemetnation year
    geom_vline(aes(xintercept = mpayear), linetype = "dotted", color = "red",size=1)
  
  
  filename1 <- paste0('Lobster/Figures/Kernel Density/', portcomplex, '_COG_latitude.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun("San Diego")
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
    weights<-year_data$ntrips
    
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
    df <- data.frame(Portcomplex=portcomplex,
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
  all.data
}

i<-plot_fun("Santa Barbara")
j<-plot_fun("Los Angeles")
k<-plot_fun("San Diego")


all.volumes<-rbind(i,j,k)


all.volumes<-all.volumes%>%
  mutate(vol_50_km=volume_50/1000)%>%
  mutate(vol_75_km=volume_75/1000)%>%
  mutate(vol_95_km=volume_95/1000)%>%
  left_join(mpa,by="Portcomplex")


saveRDS(all.volumes,"Lobster/Data/Processed Data/Lobster_all contour volumes.rds")

#order from north to south
all.volumes$Portcomplex<- factor(all.volumes$Portcomplex, levels = c( 'Santa Barbara', 'Los Angeles', 'San Diego'))


p<-ggplot(data=all.volumes, aes(x=Year))+
  facet_wrap_paginate(~Portcomplex, ncol=3, nrow=1, scales="free_y")+
  
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
    facet_wrap_paginate( ~Portcomplex, ncol=3, nrow=1, page = i, scales="free_y")
  ggsave(plot = p_save, filename = paste0('Lobster/Figures/Kernel Density/Volume Contours_page_', i, '.jpg'),width = 16, height = 8, units = "in")
}

#can also do the above so its not facetwrap and all seperate
############





############
#time series analyses

#COG and northern and southern latitudes
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
COG_sf<-st_transform(COG_sf,crs = "+proj=longlat +datum=WGS84")

COG_df<-COG_sf%>%
  sf::st_drop_geometry()
COG_df$COG_latitude <- st_coordinates(COG_sf)[, "Y"]  # Extracts latitude
COG_df$COG_longitude <- st_coordinates(COG_sf)[, "X"]


all.volumes<-readRDS('Lobster/Data/Processed Data/Lobster_all contour volumes.rds')%>%
  rename(port.complex=Portcomplex, year=Year)


COG_vcontours<-COG_df%>%
  left_join(all.volumes, by=c("port.complex","year"))

plot_fun<-function(portcomplex){
  
  # Define specific colors from an RColorBrewer palette
  colors <- brewer.pal(n = 11, name = "RdYlBu") 
  
  
  location<-COG_vcontours%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=location, aes(x=year))+
    geom_line(aes(y=COG_latitude, color = "COG"),size=.75)+
    geom_line(aes(y=northernmost_latitude_75, color = "Northern extent"),size=.75)+
    geom_line(aes(y=southernmost_latitude_75, color = "Southern extent"),size=.75)+
    #labs
    labs(y = ('Latitude °N'), x='Year') +
    # Color scale from RColorBrewer
    # Apply specific colors using scale_color_manual
    scale_color_manual(values = c("Northern extent" = colors[10],
                                  "COG"= "black",
                                  "Southern extent" = colors[2])) +
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

  
  filename1 <- paste0('Lobster/Figures/Kernel Density/', portcomplex, '_COG_extents_latitude.jpg')
  ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
}

plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun('San Diego')

############




############
#time series analyses
#graph of all COG latitudes together and all distance to shore together

#getting the correct lat and long 
df1<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
df2<-st_transform(df1, st_crs("+proj=longlat  +datum=WGS84 +units=m +no_defs"))

df2$latitude <- st_coordinates(df2)[, "Y"]  # Extracts latitude
df2$longitude <- st_coordinates(df2)[, "X"]
df2<-df2 %>% sf::st_drop_geometry()%>%
  mutate(distance_shore_km=cog_dist_shore/1000)
df2$port.complex<- factor(df2$port.complex, levels = c('Santa Barbara', 'Los Angeles', 'San Diego'))


p<-ggplot(data=df2, aes(x=year, color=port.complex))+
  geom_line(aes(y=latitude),size=1)+
  labs(y = ('Latitude °N'), x='Year') +
  scale_color_brewer(palette = 'RdYlBu', direction=-1) +
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank())

filename1 <- ('Lobster/Figures/Kernel Density/All Ports_COG_latitude.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)



p<-ggplot(data=df2, aes(x=year, color=port.complex))+
  geom_line(aes(y=distance_shore_km),size=1)+
  labs(y = bquote('Distance from shore'~(km^2)), x='Year') +
  scale_color_brewer(palette = 'RdYlBu', direction=-1) +
  # Theme
  theme_bw() + 
  base_theme  +
  theme(legend.title = element_blank())

filename1 <- ('Lobster/Figures/Kernel Density/All Ports_COG_distanceshore.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)