library(dplyr)
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

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

#port codes
port<-read.csv(paste0(general_data, "Port Codes.csv"))

# State waters
state_waters <- readRDS( paste0(general_data, "CA_state_waters_polyline.Rds"))

#mpas
mpa_blocks<-wcfish::mpas_ca

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
port.complex=c("Fort Bragg", "Santa Barbara","Los Angeles","San Diego")
values=c(2012, 2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)
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
df_sf$port.complex<- factor(df_sf$port.complex, levels = c('Fort Bragg', 'Santa Barbara', 'Los Angeles', 'San Diego'))

df_sf<- df_sf%>% filter(!st_is_empty(.))
############

############
#cog data manipulation
COG_sf<-st_as_sf(COG, coords = c("longitude_calculated", "cog_latitude"), crs=("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
COG_sf<-st_transform(COG_sf,crs = "+proj=longlat +datum=WGS84")

COG_df<-COG_sf%>%
  sf::st_drop_geometry()
COG_df$COG_latitude <- st_coordinates(COG_sf)[, "Y"]  # Extracts latitude
COG_df$COG_longitude <- st_coordinates(COG_sf)[, "X"]
############

############
#plotting mpas with yearly kernel densities and COG
plot_fun<-function(portcomplex){ 
  
  location <- df_sf%>%
    filter(port.complex== portcomplex)%>%
    group_by(year)%>%
    filter(n() > 1) %>%
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
    
    #graphing
    
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
                   breaks = contour_level, color = "black", size = 0.5) +
      # Land
      geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      #mpas
      geom_sf(data=mpa_blocks, fill="black", col="white", linewidth=0.2, inherit.aes = F)+
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      #points COG
      #points COG
      geom_sf(data=b,size=5, fill="white",color="black",pch=21)+
      # Crop
      coord_sf(xlim = c(minx-.5, maxx+.5), ylim = c(miny-.5, maxy+.5)) +
      # Axes
      scale_x_continuous(breaks=seq(minx-.5, maxx+.5, .5),labels=scales::number_format(accuracy=0.50)) +
      scale_y_continuous(breaks=seq(miny-.5, maxy+.5, .5),labels=scales::number_format(accuracy=0.50)) + 
      
      ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle=paste0('Sea Urchin Dive Fishery: Year ', year, ', with 75% volume contour' ))+
      theme_bw() + base_theme+ 
      theme(axis.title = element_blank())
    
    
    filename1 <- paste0(urchin_data, 'Figures/Urchin/Kernel Density/',portcomplex,'/', portcomplex, year, '_MPAs_cropped.jpg')
    ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
  }
}

plot_fun('Fort Bragg')
plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun('San Diego')
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
#area within an mpa
plot_fun<-function(portcomplex){
  
  location <- df_sf%>%
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
    
    # Calculate the cumulative distribution function (CDF)
    density_values <- sort(density_df$value, decreasing = TRUE)
    cdf <- cumsum(density_values) / sum(density_values)
    
    # Find the contour level corresponding to 75% volume
    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    
    # Create a mask for values above the 75% contour
    density_raster_masked <- density_masked
    density_raster_masked[] <- ifelse(density_masked[] >= contour_level_75, density_masked[], NA)
    
    # Compute total volume of KDE
    total_volume <- cellStats(density_raster_masked, stat = 'sum', na.rm = TRUE)
    
    # Mask the density raster with the MPA raster
    density_within_mpa <- mask(density_raster_masked, mpa_blocks, inverse = FALSE)
    
    # Compute volume of KDE within MPAs
    volume_within_mpa <- cellStats(density_within_mpa, stat = 'sum', na.rm = TRUE)
    
    # Calculate percentage
    percentage_within_mpa <- (volume_within_mpa / total_volume) * 100
    
    df <- data.frame(port.complex=portcomplex,
                     Year = year,
                     volume_within_mpa=volume_within_mpa,
                     percentage_within_mpa =percentage_within_mpa ,
                     total_volume=total_volume)
    
    # Append current year's results to combined_df
    datalist[[year]] <- df
  }
  all.data<- dplyr::bind_rows(datalist)
  all.data
  
}
a<-plot_fun("Fort Bragg")
b<-plot_fun("Santa Barbara")
c<-plot_fun("Los Angeles")
d<-plot_fun("San Diego")


all.volumes<-rbind(a,b,c,d)
all.volumes<-all.volumes%>%
  left_join(mpa,by="port.complex")%>%
  mutate(Year=as.numeric(Year))

saveRDS(all.volumes,paste0(urchin_data,"Data/Processed Data/MPA_area.rds"))


plot_fun<-function(portcomplex){
  location <- all.volumes%>%
    filter(port.complex==portcomplex)%>%
    mutate(Year=as.numeric(Year))
  
  
  p<-ggplot(data=location, aes(x=Year))+
    geom_line(aes(y=percentage_within_mpa), color="black",size=1)+
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
############


############
all.volumes$port.complex<- factor(all.volumes$port.complex, levels = c('Fort Bragg','Santa Barbara', 'Los Angeles', 'San Diego'))


p<-ggplot(data=all.volumes, aes(x=Year, color=port.complex))+
  # geom_line(aes(y=vol_50_km ,color="50% contour"),size=1)+
  geom_line(aes(y=percentage_within_mpa),size=.5)+
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

