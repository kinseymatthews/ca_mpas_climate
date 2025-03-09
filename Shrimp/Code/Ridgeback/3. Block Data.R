#libraries
library(gganimate)
library(tidyverse)
library(sf)
library(sp)


#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
shrimp_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/'
#################

#################
#read in shrimp data
shrimp<-readRDS(paste0(shrimp_data,"Data/Processed Data/ridgeback_logbooks_edited.rds"))

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

#lots of incorrect blocks within the logbook data, filter out potential bad blocks before merging the various block id columns and removing block IDs north of monterey bay where fishing shouldnt occur

explor<- shrimp%>%
  mutate(agree = case_when(
    is.na(port_complex_logbook) | is.na(port_complex_landings) ~ NA,  # Set to NA if either is NA
    port_complex_logbook == port_complex_landings ~ TRUE,                        # Agree
    TRUE ~ FALSE                                # Disagree
  ))%>%
  mutate(port.complex = coalesce(port_complex_logbook , port_complex_landings))%>%
  left_join(dplyr::select(mpa,port.complex,mpayear),by=("port.complex"))%>%
  rename(block_id_logbook=block_id)%>%
  mutate(block_id = coalesce(block_id_logbook,block_id_landings, block_id_gps))%>%  #prefers logbook block id to gps block id
  left_join(all_blocks_df, by="block_id")%>%
  filter(!is.na(block_id))%>%
  filter(!is.na(block_lat_dd))%>%
  #getting rid of international, offshore,  all midshore besides 897, and all blocks that are too deep
  filter(blck_st!="International" | is.na(blck_st) )%>%
  filter(blck_ty!="Offshore" | is.na(blck_st))%>%
  #adding column for reported depth in meters
  mutate(Depth_m=depth_fa_up*(-0.3048))%>%
  mutate(depth=depth_fa_up)%>%
  #Eliminate tows >5 hours (300 min)
  filter(duration_min<300 & duration_min>0)%>%
  #remove port complexes we dont care about
  filter(port.complex=="Los Angeles" | port.complex=="Santa Barbara")


#graph of duration 
ggplot()+
  geom_density(data= explor, aes(x=duration_min))+
  base_theme

saveRDS(explor, paste0(shrimp_data, "Data/Processed Data/ridegback_filtered.rds"))
############ 



############ 
#All years combined
#summarizing across years to see what one graph with all of the data should kinda look like
allports.acrossyears<- explor %>%
  # Step 1: Remove duplicates within each tow.identifier and calculate distinct tow hours
  group_by(tow.identifier, duration_min) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct tow_hours for each tow.identifier
  group_by(tow.identifier) %>%
  summarize(total_min = sum(duration_min), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(tow.identifier, year, port.complex, block_id), by = "tow.identifier") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, block_id) %>%
  summarize(total_tow_min = sum(total_min, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  #remove blocks that dont exist
  st_make_valid()

#ordering
allports.acrossyears$port.complex<- factor(allports.acrossyears$port.complex, levels = c('Santa Barbara','Los Angeles'))

#Graphing
plot_fun<-function(portcomplex){ 
  df<-allports.acrossyears%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=df, mapping=aes(fill=total_tow_min)) +
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
    scale_fill_gradientn(name="Trawl Minutes: \n1983 - 2020",  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    # Theme
    theme_bw() + base_theme+
    ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle="Commercial Ridgeback Prawn Trawl Fishery" )
  
  name=paste0(shrimp_data,'Figures/Ridgeback/Block Data/',portcomplex,'/',portcomplex,' Blocks_All Years.jpg')
  ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
  
}
plot_fun('Los Angeles')
plot_fun('Santa Barbara')
############ 

############ 
#Block Data
#years separate
stats_blocks_sf<- explor %>%
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
  group_by( port.complex, year, block_id) %>%
  summarize(total_tow_min = sum(total_min, na.rm = TRUE),
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  #remove blocks that dont exist
  st_make_valid()%>%
  #as numeric
  mutate(year=as.numeric(year))

#ordering
stats_blocks_sf$port.complex<- factor(stats_blocks_sf$port.complex, levels = c( 'Santa Barbara','Los Angeles'))
############

############
#Graphing
plot_fun<-function(portcomplex){ 
  
  portcomplex_data <- stats_blocks_sf %>%
    filter(port.complex== portcomplex)
  
  p<-ggplot(data=portcomplex_data, mapping=aes(fill=total_tow_min, group=year)) +
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
    scale_fill_gradientn(name="Trawl Minutes" ,  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    # Theme
    theme_bw() + base_theme
  
  p_animation <-  p +
    transition_states(year, transition_length = 1, state_length = 3)+
    labs(subtitle="Ridgeback Prawn Trawl Fishery Year: {closest_state}", title=paste(portcomplex, "Port Complex"))
  
  num_years <-max(portcomplex_data$year)-min(portcomplex_data$year) +1
  anim<-gganimate::animate(p_animation, duration=num_years)
  
  name=paste0(shrimp_data, 'Figures/Ridgeback/Block Data/',portcomplex,'/',portcomplex,'_Animation_dive hours.gif')
  anim_save(name, anim)
}
plot_fun('Los Angeles')
plot_fun('Santa Barbara')

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
    
    p<-ggplot(data=year_data, mapping=aes(fill=total_tow_min)) +
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
      scale_fill_gradientn(name="Trawl Minutes",  trans="log", 
                           colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                           labels = scales::label_comma(accuracy = 100))+
      
      ggtitle(paste0(portcomplex,' Fishing Grounds'), subtitle=paste0('Ridgeback Prawn Trawl Fishery: Year ', year))+
      # Theme
      theme_bw() + base_theme
    
    
    
    name=paste0(shrimp_data, 'Figures/Ridgeback/Block Data/',portcomplex,'/',portcomplex,' Blocks_',year,'.jpg')
    ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
  }
}


plot_fun('Los Angeles')
plot_fun('Santa Barbara')
############