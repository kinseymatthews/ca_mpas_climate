#libraries
library(gganimate)
library(dplyr)
library(sf)
library(sp)


############ 
# read in CPFV data
CPFV<-readRDS("CPFV/Data/Raw Data/1980_2022_cpfv_logbooks.Rds")

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

#port codes
port<-read.csv("CPFV/Data/Raw Data/Port Codes.csv")

# State waters
state_waters <- readRDS( "CPFV/Data/Raw Data/CA_state_waters_polyline.Rds")

# block data
all_blocks<-st_read("CPFV/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()


all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv("CPFV/Data/Raw Data/fishingblockstatistics.csv")%>%
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
############


############
#CPFV Data Manipulation
#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)

explor<- CPFV %>%
  mutate(logbookID_vesselID_date=paste(logbook_id, vessel_id, date, sep="_"))%>%
  #attach information from port codes
  left_join(port, by="port_code")%>%
  #add spatial block data
  left_join(all_blocks_df, by="block_id")%>%
  #remove Oregon, Sacramento Delta, and San Diego port complex
  filter(is.na(port.complex) | port.complex!="Oregon")%>%
  filter(is.na(port.complex) | port.complex!="Sacramento Delta")%>%
  filter(is.na(port.complex) | port.complex!="San Diego")%>%
  #Depth info
  mutate(Depth_m=depth*(-0.3048))%>%
  mutate(Depth_fathom=depth/(-6))%>%
  #add in mpa implementation data
  left_join(mpa,by="port.complex")%>%
  #filter out offshore and international blocks
  filter(is.na(blck_ty) |blck_ty!="Offshore")%>%
  filter(is.na(blck_st) |blck_st!="International")%>%
  #get angler hours as our unit
  mutate(angler_hours=hours_fished*number_of_fishers)%>%
#filter bad data
filter(is.na(hrs_minutes_fished_flag))%>%
  filter(is.na(number_of_fishers_flag))%>%
  filter(number_of_fishers<=120 & number_of_fishers>0)%>%
  filter(hours_fished>0)%>%
  filter(!is.na(port.complex))%>%
  filter(!is.na(block_id))

saveRDS(explor,"CPFV/Data/Processed Data/CPFV_filtered.rds")

############

############
#All years combined
#summarizing across years to see what one graph with all of the data should kinda look like
allports.acrossyears<- explor %>%
  # Step 1: Remove duplicates within each logbookID_vesselID_date and calculate distinct angler_hours
  group_by(logbookID_vesselID_date, angler_hours) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct angler_hours for each logbookID_vesselID_date
  group_by(logbookID_vesselID_date) %>%
  summarize(total_angler_hours = sum(angler_hours), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(logbookID_vesselID_date, year, port.complex, block_id), by = "logbookID_vesselID_date") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, block_id) %>%
  summarize(ntrips = n_distinct(logbookID_vesselID_date),
            total_angler_hours = sum(total_angler_hours, na.rm = TRUE),
            angler_hours_per_trip=(total_angler_hours/ntrips),
            # same as hours_fished but oh well
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  #remove blocks that dont exist
st_make_valid()

#order from north to south
allports.acrossyears$port.complex<- factor(allports.acrossyears$port.complex, levels = c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco','Monterey', 'Morro Bay','Santa Barbara', 'Los Angeles'))
############


############
#Graphing
plot_fun<-function(portcomplex){ 
  df<-allports.acrossyears%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=df, mapping=aes(fill=total_angler_hours)) +
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
    scale_fill_gradientn(name="Angler Hours: \n1980 - 2022",  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    # Theme
    theme_bw() + base_theme+
  ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle="CPFV Fishery" )
  
  
  name=paste0('CPFV/Figures/Block Data/',portcomplex,'/',portcomplex,' Blocks_All Years.jpg')
  ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
  
}
plot_fun('Eureka')
plot_fun('Fort Bragg')
plot_fun('Bodega Bay')
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
############


############
#years separate
stats_blocks_sf<- explor %>%
  # Step 1: Remove duplicates within each logbookID_vesselID_date and calculate distinct angler_hours
  group_by(logbookID_vesselID_date, angler_hours) %>%
  summarize(count = n(), .groups = 'drop') %>%
  # Step 2: Summarize distinct angler_hours for each logbookID_vesselID_date
  group_by(logbookID_vesselID_date) %>%
  summarize(total_angler_hours = sum(angler_hours), .groups = 'drop') %>%
  # Step 3: Join with the original DataFrame to get year, port, and block information
  left_join(explor %>% dplyr::select(logbookID_vesselID_date, year, port.complex, block_id), by = "logbookID_vesselID_date") %>%
  distinct()%>%
  # Step 4: Group by year, port, and block to aggregate results
  group_by( port.complex, year, block_id) %>%
  summarize(ntrips = n_distinct(logbookID_vesselID_date),
            total_angler_hours = sum(total_angler_hours, na.rm = TRUE),
            angler_hours_per_trip=(total_angler_hours/ntrips),
            # same as hours_fished but oh well
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  filter(!st_is_empty(.))%>%
  #remove blocks that dont exist
  st_make_valid()

#order from north to south
stats_blocks_sf$port.complex<- factor(stats_blocks_sf$port.complex, levels = c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco','Monterey', 'Morro Bay','Santa Barbara', 'Los Angeles'))
############

############
#Graphing
plot_fun<-function(portcomplex){ 
  
  portcomplex_data <- stats_blocks_sf %>%
    filter(port.complex== portcomplex)
  
  p<-ggplot(data=portcomplex_data, mapping=aes(fill=total_angler_hours, group=year)) +
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
    scale_fill_gradientn(name="Angler Hours" ,  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    
    # Theme
    theme_bw() + base_theme
  
  p_animation <-  p +
    transition_states(year, transition_length = 1, state_length = 3)+
    labs(subtitle="CPFV Fishery Year: {closest_state}", title=paste(portcomplex, "Port Complex"))
  
  num_years <-max(portcomplex_data$year)-min(portcomplex_data$year) +1
  anim<-gganimate::animate(p_animation, duration=num_years)
  
  name=paste0('CPFV/Figures/Block Data/',portcomplex,'/',portcomplex,'_Animation_angler hours.gif')
  anim_save(name, anim)
  
}
plot_fun('Eureka')
plot_fun('Fort Bragg')
plot_fun('Bodega Bay')
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
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
    
    p<-ggplot(data=year_data, mapping=aes(fill=total_angler_hours)) +
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
      scale_fill_gradientn(name="Angler Hours",  trans="log", 
                           colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                           labels = scales::label_comma(accuracy = 100))+
      
      ggtitle(paste0(portcomplex,' Fishing Grounds'), subtitle=paste0('CPFV Fishery: Year ', year))+
      # Theme
      theme_bw() + base_theme
    
    
    
    name=paste0('CPFV/Figures/Block Data/',portcomplex,'/',portcomplex,' Blocks_',year,'.jpg')
    ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
  }
}

plot_fun('Eureka')
plot_fun('Fort Bragg')
plot_fun('Bodega Bay')
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
############