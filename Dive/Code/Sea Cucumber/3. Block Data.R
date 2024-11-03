
#libraries
library(gganimate)
library(tidyverse)
library(sf)
library(sp)


############ 
#filepaths
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
cucumber_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/'

###### read in Dive data
cuc<-readRDS(paste0(cucumber_data,"Data/Processed Data/seacucumberfishery_matched.rds"))

###### read in block data
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

##### read in bathymetry data with blocks
bathy.data<-read.csv(paste0(general_data,"fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

##### merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")

#port codes
port<-read.csv( paste0(general_data,"Port Codes.csv"))

# State waters
state_waters <- readRDS( paste0(general_data,"CA_state_waters_polyline.Rds"))

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
# Data Manipulation

#Want to remove fishing blocks that occur outside the range of the fishery (which goes up to Monterey Bay for a northern range)
#are too deep for the fishery (maximum block depth <150 feet)
#or where the blocks are too large for analyses - international and offshore blocks.

#Figured out that all midshore blocks besides 897 are too deep for the fishery, and 
#blocks 108,135,256,257,265,419,456,457,465, are too north for the fishery

#Data Manipulation
#adding in mpa implementation data
port.complex=c("Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)

explor<-cuc%>%
  left_join(dplyr::select(mpa,port.complex,mpayear),by=c("port_most_reliable"="port.complex"))%>%
  #add spatial block data
  left_join(all_blocks_df, by=("block_id"))%>%
  #getting rid of international, offshore,  all midshore besides 897
  filter(blck_st!="International" | is.na(blck_st))%>%
  filter(blck_ty!="Offshore" | is.na(blck_ty))%>%
  filter(!(blck_ty == 'Midshore' & block_id != 897))%>%
  #recorded depth
  mutate(Min_depth_m_recorded=depth_min_ft*(-0.3048))%>%
  mutate(Max_depth_m_recorded=depth_max_ft*(-0.3048))%>%
  mutate(Med_depth_m_recorded=(Min_depth_m_recorded+Max_depth_m_recorded)/2)%>%
  #getting rid of blocks too deep
  filter(Min_Depthm>=-50)%>% #using the block information instead of the recorded depth in case there was a mistake in the recorded depth
  #rename port complex so we can reuse code
  rename(port.complex=port_most_reliable)%>%
  #getting rid of bad or unrealistic dive hours
  filter(hours<10 &  hours>0.10)%>%
  filter(!is.na(port.complex))%>%
  filter(!is.na(block_id))

saveRDS(explor, paste0(cucumber_data,"Data/Processed Data/seacucumber_filtered.rds"))
############ 


############ 
#All years combined
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
            .groups = 'drop')%>%
  #get block data
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  #remove blocks that dont exist
  st_make_valid()

#ordering
allports.acrossyears$port.complex<- factor(allports.acrossyears$port.complex, levels = c('Santa Barbara', 'Los Angeles', 'San Diego'))

#Graphing
plot_fun<-function(portcomplex){ 
  df<-allports.acrossyears%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=df, mapping=aes(fill=total_dive_hours)) +
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
    scale_fill_gradientn(name="Dive Hours: \n1998 - 2023",  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    # Theme
    theme_bw() + base_theme+
    ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle="Commercial Sea Cucumber Dive Fishery" )
  
  name=paste0(cucumber_data,'Figures/Cucumber/Block Data/',portcomplex,'/',portcomplex,' Blocks_All Years.jpg')
  ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
  
}
plot_fun("Santa Barbara")
plot_fun("San Diego")
plot_fun("Los Angeles")
############ 


############ 
#Block Data
#years separate
stats_blocks_sf<- explor %>%
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
  group_by( port.complex, year, block_id) %>%
  summarize(total_dive_hours = sum(total_hours, na.rm = TRUE),
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
stats_blocks_sf$port.complex<- factor(stats_blocks_sf$port.complex, levels = c('Santa Barbara', 'Los Angeles', 'San Diego'))
############

############
#Graphing
plot_fun<-function(portcomplex){ 
  
  portcomplex_data <- stats_blocks_sf %>%
    filter(port.complex== portcomplex)
  
  p<-ggplot(data=portcomplex_data, mapping=aes(fill=total_dive_hours, group=year)) +
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
    scale_fill_gradientn(name="Dive Hours" ,  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    # Theme
    theme_bw() + base_theme
  
  p_animation <-  p +
    transition_states(year, transition_length = 1, state_length = 3)+
    labs(subtitle="Sea Cucumber Fishery Year: {closest_state}", title=paste(portcomplex, "Port Complex"))
  
  num_years <-max(portcomplex_data$year)-min(portcomplex_data$year) +1
  anim<-gganimate::animate(p_animation, duration=num_years)
  
  name=paste0(cucumber_data, 'Figures/Cucumber/Block Data/',portcomplex,'/',portcomplex,'_Animation_dive hours.gif')
  anim_save(name, anim)
}
plot_fun("Santa Barbara")
plot_fun("San Diego")
plot_fun("Los Angeles")

############