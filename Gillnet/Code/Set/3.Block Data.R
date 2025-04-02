
#libraries
library(gganimate)
library(dplyr)
library(sf)
library(sp)


#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
gillnet_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/'
#################

#################
#read in set gillnet data
gillnet<-readRDS(paste0(gillnet_data,"Data/Processed Data/setgillnetfishery_matched.rds"))%>%
  rename(port.complex=port_complex_landings)

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

#port codes
port<-read.csv( paste0(general_data,"Port Codes.csv"))

# State waters
state_waters <- readRDS( paste0(general_data,"CA_state_waters_polyline.Rds"))

#block data 
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

##### merge bathymetry data with block data
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
#################

#################
#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)

explor<- gillnet%>%
  #filtering for only the port complexes that agree
  left_join(dplyr::select(mpa,port.complex,mpayear),by=("port.complex"))%>%
  left_join(all_blocks_df, by="block_id")%>%
  #getting rid of international, offshore,  all midshore besides 897, and all blocks that are too deep
  filter(blck_st!="International" | is.na(blck_st) )%>%
  filter(blck_ty!="Offshore" | is.na(blck_ty) )%>%
  filter(blck_st!="Oregon" | is.na(blck_st))%>%
  #adding column for reported depth in meters
  mutate(Depth_m=depth_fa_num*(-0.3048))%>%
  mutate(depth=depth_fa_num)%>%
  #remove mesh too small
  filter(mesh_size_in_num>3.5)%>%
  #filter for sets that are way too long
  filter(soak_hr_num<217)%>%
#remove port complexes we dont care about
filter(port.complex=="Bodega Bay" | port.complex=="San Francisco" | port.complex=="Monterey" | port.complex=="Morro Bay" | port.complex=="Santa Barbara"| port.complex=="Los Angeles"| port.complex=="San Diego")%>%
  filter(!(port.complex == "Bodega Bay" & year >= 1989)) 

saveRDS(explor, paste0(gillnet_data, "Data/Processed Data/setgillnet_filtered.rds"))
############ 


############ 
#All years combined
#summarizing across years to see what one graph with all of the data should kinda look like
allports.acrossyears<- explor %>%
  # Step 1: Remove duplicates within each tow.identifier and calculate distinct tow hours
  group_by(set.identifier, soak_hr_num) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct tow_hours for each tow.identifier
  group_by(set.identifier) %>%
  summarize(total_hr = sum(soak_hr_num), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(set.identifier, year, port.complex, block_id), by = "set.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, block_id) %>%
  summarize(total_set_hr = sum(total_hr, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  #remove blocks that dont exist
  st_make_valid()

#ordering
allports.acrossyears$port.complex<- factor(allports.acrossyears$port.complex, levels = c("Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego"))

#Graphing
plot_fun<-function(portcomplex){ 
  df<-allports.acrossyears%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=df, mapping=aes(fill=total_set_hr)) +
    # Blocks
    geom_sf(color="grey50", linewidth=0.1)+
    # Land
    geom_sf(data =usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    # Set the limits for the coordinate system
    coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
    # Axes
    scale_x_continuous(breaks=seq(-126, -118, 2)) +
    scale_y_continuous(breaks=seq(32, 42, 2)) + 
    scale_fill_gradientn(name="Soak Hours",  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    # Theme
    theme_bw() + base_theme+
    ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle="Commercial Set Gillnet Fishery" )
  
  name=paste0(gillnet_data,'Figures/Set/Block Data/',portcomplex,'/',portcomplex,' Blocks_All Years.jpg')
  ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
  
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
#Block Data
#years separate
stats_blocks_sf<- explor %>%
  # Step 1: Remove duplicates within each tow.identifier and calculate distinct tow hours
  group_by(set.identifier, soak_hr_num) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct tow_hours for each tow.identifier
  group_by(set.identifier) %>%
  summarize(total_hr = sum(soak_hr_num), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(set.identifier, year, port.complex, block_id), by = "set.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, year,block_id) %>%
  summarize(total_set_hr = sum(total_hr, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  #remove blocks that dont exist
  st_make_valid()%>%
  filter(!st_is_empty(geometry))%>%
  #as numeric
  mutate(year=as.numeric(year))

#ordering
stats_blocks_sf$port.complex<- factor(stats_blocks_sf$port.complex, levels = c( 'Bodega Bay','San Francisco','Monterey', 'Morro Bay','Santa Barbara', 'Los Angeles', 'San Diego'))
############

############
#Graphing
plot_fun<-function(portcomplex){ 
  
  portcomplex_data <- stats_blocks_sf %>%
    filter(port.complex== portcomplex)
  
  p<-ggplot(data=portcomplex_data, mapping=aes(fill=total_set_hr, group=year)) +
    # Blocks
    geom_sf(linewidth=0.1)+
    # Land
    geom_sf(data=usa_mexico_sf , fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
    # State waters
    geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
    # Set the limits for the coordinate system
    coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
    # Axes
    scale_x_continuous(breaks=seq(-126, -118, 2)) +
    scale_y_continuous(breaks=seq(32, 44, 2)) + 
    scale_fill_gradientn(name="Soak Hours" ,  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    # Theme
    theme_bw() + base_theme
  
  p_animation <-  p +
    transition_states(year, transition_length = 1, state_length = 3)+
    labs(subtitle="Set Gillnet Fishery Year: {closest_state}", title=paste(portcomplex, "Port Complex"))
  
  num_years <-max(portcomplex_data$year)-min(portcomplex_data$year) +1
  anim<-gganimate::animate(p_animation, duration=num_years)
  
  name=paste0(gillnet_data, 'Figures/Set/Block Data/',portcomplex,'/',portcomplex,'_Animation_dive hours.gif')
  anim_save(name, anim)
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
#years separately - non animation
plot_fun<-function(portcomplex){ 
  
  location <- stats_blocks_sf %>%
    filter(port.complex== portcomplex)
  
  # Get unique years to loop through
  unique_years <- unique(location$year)
  
  for(year in unique_years){
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    p<-ggplot(data=year_data, mapping=aes(fill=total_set_hr)) +
      # Blocks
      geom_sf(color="grey50", linewidth=0.1)+
      # Land
      geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      # Set the limits for the coordinate system
      coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
      # Axes
      scale_x_continuous(breaks=seq(-126, -118, 2)) +
      scale_y_continuous(breaks=seq(31, 42, 2)) + 
      scale_fill_gradientn(name="Set Hours",  trans="log", 
                           colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                           labels = scales::label_comma(accuracy = 100))+
      
      ggtitle(paste0(portcomplex,' Fishing Grounds'), subtitle=paste0('Gillnet Fishery: Year ', year))+
      # Theme
      theme_bw() + base_theme
    
   
    name=paste0(gillnet_data, 'Figures/Set/Block Data/',portcomplex,'/',portcomplex,'_Blocks', year ,'.jpg')
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
############