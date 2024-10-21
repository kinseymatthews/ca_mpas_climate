#libraries
library(gganimate)
library(dplyr)
library(sf)
library(sp)

############ 
#read in data
Lobster<-readRDS("Lobster/Data/Processed Data/lobster_logbooks_edited.rds")%>%
  rename(port.complex1=port_complex1, port.complex2=port_complex2)

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
#adding in mpa implemtnation dates
values=c(2012,2012,2012)
port.complex=c("Santa Barbara","Los Angeles","San Diego")
mpa<-data.frame(port.complex=port.complex,mpayear=values)

#lots of incorrect blocks within the logbook data, filter out potential bad blocks before merging the various block id columns and removing block IDs north of monterey bay where fishing shouldnt occur
block.to.remove<-c(108,135,256,257,265,419,456,457,465,0,2,3, 10, 18, 19, 20, 21, 25, 40, 49, 50, 52, 54, 56, 60, 61, 62, 63, 64, 65, 66, 68, 70, 71, 72, 73, 75, 79, 80, 81, 83, 85, 86, 87, 100, 152, 153, 156, 157, 160, 180, 181, 190, 306, 307, 326, 342, 345, 351, 352, 353, 381, 383, 385, 387, 389, 500, 575, 576, 588, 600, 698, 699, 700, 778, 780, 785, 786, 787, 788, 789, 800, 901, 904, 919, 921, 922, 953, 954, 955, 957, 960, 980, 982, 983, 984, 985, 988, 1019, 1028, 3718, 3798, 3860, 3900, 5814, 6001, 6353, 6354, 6355, 6389, 6520, 6532, 6533, 6543, 6654, 6665, 6866, 6885, 6889, 6893, 6894, 6953, 7009, 7102, 7110, 7203, 7230, 7387, 7455, 7520, 7536, 7564, 7569, 7574, 7757, 8136, 8432, 8442, 8460, 8520, 8600, 8605, 8630, 8650, 8660, 8693, 8707, 8760, 8777, 8821, 8822, 8850, 8860, 8922, 8942, 8960)

Lobster<-Lobster%>%
  filter(!block_id %in% block.to.remove)

explor<- Lobster%>%
  mutate(agree = case_when(
    is.na(port.complex1) | is.na(port.complex2) ~ NA,  # Set to NA if either is NA
    port.complex1 == port.complex2 ~ TRUE,                        # Agree
    TRUE ~ FALSE                                # Disagree
  ))%>%
  mutate(port.complex = coalesce(port.complex1, port.complex2))%>%
  #filtering for only the port complexes that agree
  filter(agree==TRUE | is.na(agree))%>%
  left_join(dplyr::select(mpa,port.complex,mpayear),by=("port.complex"))%>%
  rename(block_id_logbook=block_id)%>%
  mutate(block_id = coalesce(block_id_logbook,block_id_gps, block_id_landings1,block_id_landings2))%>% #prefers logbook block id to gps block id
  left_join(all_blocks_df, by="block_id")%>%
  #getting rid of international, offshore,  all midshore besides 897, and all blocks that are too deep
  filter(blck_st!="International" | is.na(blck_st) )%>%
  filter(blck_ty!="Offshore" | is.na(blck_st))%>%
  filter(!(blck_ty == 'Midshore' & block_id != 897))%>%
  filter(Min_Depthm>= -110 | is.na(Min_Depthm))%>%
  filter(!block_id %in% block.to.remove)%>%
  #creating trap nights and filtering out bad data
  mutate(trap_nights=n_nights*n_traps_pulled)%>%
  filter(n_nights<=9)%>%
  filter(n_traps_pulled<=600)%>%
  filter(n_traps_pulled>0)%>%
  filter(n_nights>0)

saveRDS(explor,"Lobster/Data/Processed Data/lobster_filtered.rds")

#outliers for trap nights
p<-ggplot(explor, aes(x = port.complex, y = trap_nights)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs( x = "Port Complex", y = "Trap Nights") +
  theme_bw() +base_theme

p2<-ggplot() + 
  geom_density(data=explor,aes(x=trap_nights,fill=port.complex),alpha = 0.5)+
  base_theme +
  xlab("Trap Nights")+
  ylab("Density") +base_theme

ggsave(filename = 'Lobster/Figures/Exploratory/Boxplot_outliers_numbertrapnights.jpg', plot = p, width = 10, height = 6, units = "in", dpi = 300)

ggsave(filename = 'Lobster/Figures/Exploratory/Density_outliers_numbertrapnights.jpg', plot = p2, width = 10, height = 6, units = "in", dpi = 300)
############


############ 
#Block Data
#summarizing across years to see what one graph with all of the data should kinda look like
allports.acrossyears<- explor %>%
  #summarize by port, block
  group_by(port.complex,block_id)%>%
  summarize(ntrips=n(),
            nvessels=n_distinct(vessel_id),
            trap_nights=sum(trap_nights))%>%
  ungroup()%>%
  left_join(all_blocks %>% 
              dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>%
  filter(!is.na(block_id))%>%
  filter(!is.na(port.complex))

#ordering
allports.acrossyears$port.complex<- factor(allports.acrossyears$port.complex, levels = c('Santa Barbara', 'Los Angeles', 'San Diego'))

#Graphing
plot_fun<-function(portcomplex){ 
  df<-allports.acrossyears%>%
    filter(port.complex==portcomplex)
  
  p<-ggplot(data=df, mapping=aes(fill=trap_nights)) +
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
    scale_fill_gradientn(name="Number of trap nights: \n1980 - 2022",  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
  
    # Theme
    theme_bw() + base_theme+
  ggtitle(paste0(portcomplex, ' Fishing Grounds'),subtitle="Commercial Lobster Fishery" )
  
  
  name=paste0('Lobster/Figures/Block Data/',portcomplex,'/',portcomplex,' Blocks_All Years.jpg')
  ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
  
}
plot_fun("Santa Barbara")
plot_fun("San Diego")
plot_fun("Los Angeles")

#summarizing within a year
stats_blocks_sf<- explor %>%
  #summarize by year, port, block
  group_by(year,port.complex,block_id)%>%
  summarize(ntrips=n(),
            nvessels=n_distinct(vessel_id),
            trap_nights=sum(trap_nights))%>%
  ungroup()%>%
  filter(!is.na(block_id))%>%
  filter(!is.na(port.complex))%>%
  #spatialize
  left_join(all_blocks %>% dplyr::select(block_id)) %>% 
  sf::st_as_sf()%>% filter(!st_is_empty(.))
st_make_valid(stats_blocks_sf) 

#graphing
plot_fun<-function(portcomplex){ 
  
  portcomplex_data <- stats_blocks_sf %>%
    filter(port.complex== portcomplex)

  p<-ggplot(data=portcomplex_data, mapping=aes(fill=trap_nights, group=year)) +
    #facet_wrap(~year) +
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
    scale_fill_gradientn(name="Number of trap nights" ,  trans="log",
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         labels = scales::label_comma(accuracy = 100))+
    
    # Theme
    theme_bw() + base_theme
  
  p_animation <-  p +
    transition_states(year, transition_length = 1, state_length = 3)+
    labs(subtitle="Year: {closest_state}", title=paste(portcomplex))
  
  num_years <-max(portcomplex_data$year)-min(portcomplex_data$year) +1
  anim<-gganimate::animate(p_animation, duration=num_years)
  
  name=paste0('Lobster/Figures/Block Data/',portcomplex,'/',portcomplex,'_Animation_trap nights.gif')
  anim_save(name, anim)
  
}
plot_fun("Santa Barbara")
plot_fun("Los Angeles")
plot_fun("San Diego")

#years separately
plot_fun<-function(portcomplex){ 
  
  location <- stats_blocks_sf %>%
    filter(port.complex== portcomplex)
  
  # Get unique years to loop through
  unique_years <- unique(location$year)
  
  for(year in unique_years){
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    p<-ggplot(data=year_data, mapping=aes(fill=trap_nights)) +
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
      scale_fill_gradientn(name="Number of trap nights",  trans="log", 
                           colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                           labels = scales::label_comma(accuracy = 100))+
      
      ggtitle(paste0(portcomplex,' Fishing Grounds'), subtitle=paste0('Commercial Lobster Fishery: Year ', year))+
      # Theme
      theme_bw() + base_theme
    
    
    
    name=paste0('Lobster/Figures/Block Data/',portcomplex,'/',portcomplex,' Blocks_',year,'.jpg')
    ggsave(filename = name, plot = p, width = 6, height = 6, units = "in", dpi = 300)
    
  }
}

plot_fun('San Diego')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')
  