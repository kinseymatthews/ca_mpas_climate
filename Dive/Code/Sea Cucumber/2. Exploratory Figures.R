
library(tidyverse)
library(sf)

#################

###### read in Dive data
cuc<-readRDS("Dive/Data/Processed Data/seacucumberfishery_matched.rds")

###### read in block data
all_blocks<-st_read("Dive/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

##### read in bathymetry data with blocks
bathy.data<-read.csv("Dive/Data/Raw Data/fishingblockstatistics.csv")%>%
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
# Data Manipulation

#Want to remove fishing blocks that occur outside the range of the fishery (which goes up to Monterey Bay for a northern range)
#are too deep for the fishery (maximum block depth <150 feet)
#or where the blocks are too large for analyses - international and offshore blocks.

#Figured out that all midshore blocks besides 897 are too deep for the fishery, and 
#blocks 108,135,256,257,265,419,456,457,465, are too north for the fishery

#Data Manipulation
#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)

explor<-cuc%>%
  left_join(dplyr::select(mpa,port.complex,mpayear),by=c("port_most_reliable"="port.complex"))%>%
  #add spatial block data
  left_join(all_blocks_df, by="block_id")%>%
  #getting rid of international, offshore,  all midshore besides 897
  filter(blck_st!="International" | is.na(blck_st))%>%
  filter(blck_ty!="Offshore" | is.na(blck_ty))%>%
 filter(!(blck_ty == 'Midshore' & block_id != 897))%>%
  #recorded depth
  mutate(Min_depth_m_recorded=depth_min_ft*(-0.3048))%>%
  mutate(Max_depth_m_recorded=depth_max_ft*(-0.3048))%>%
  mutate(Med_depth_m_recorded=(Min_depth_m_recorded+Max_depth_m_recorded)/2)%>%
#getting rid of blocks too deep
filter(Min_Depthm>=-50)%>%
  #rename port complex so we can reuse code
  rename(port.complex=port_most_reliable)

#removing block IDs north of monterey bay where fishing shouldnt occur


##### which port complexes to keep?
explor %>%
  group_by(port.complex) %>%
  summarize(ntrips = n_distinct(trip.identifier))%>%
  ungroup()%>%
  mutate(total.trip=sum(ntrips))%>%
  group_by(port.complex) %>%
  mutate(percent.port=(ntrips/total.trip)*100)%>%
  mutate(percent.port = round(percent.port, 4))
# only keep SB, LA, and SD for fishing ground analyses
#################



#################
##Ports
NA.port<-explor[is.na(explor$port.complex),]
#port IDs of NA - 0!
#################

#################
## Number of trips and vessels through time
plot_fun<-function(portcomplex){ 
  #filtering for location
  location<-explor %>%
    filter(port.complex==portcomplex)
  #getting summary informaion for the annual number of trips taken each year and the annual number of vessels from each port
  df<-location%>%
    group_by(year)%>%
    summarize(ntrips=n_distinct(trip.identifier),
              nvessels=n_distinct(vessel_id))
  #Plot of number of trips
  p<- ggplot(data=df, aes(x=year, y=ntrips))+
    geom_point()+
    base_theme+
    ggtitle(paste0(portcomplex," Port Complex"))+
    labs(x="Year", y="Number of Trips")+
    geom_smooth(method="lm")+
    base_theme
  
  #plot of number of vessels
  p2<-ggplot(data=df, aes(x=year, y=nvessels))+
    geom_point()+
    base_theme+
    ggtitle(paste0(portcomplex," Port Complex"))+
    labs(x="Year", y="Number of Vessels")+
    geom_smooth(method="lm")+
    base_theme
  
  filename1 <- paste0('Dive/Figures/Cucumber/Exploratory/', portcomplex, '_NTrips_Year.jpg')
  ggsave(filename = filename1, plot = p, width = 8, height = 6, units = "in", dpi = 300)
  filename2 <- paste0('Dive/Figures/Cucumber/Exploratory/', portcomplex, '_NVessels_Year.jpg')
  ggsave(filename = filename2, plot = p2, width = 8, height = 6, units = "in", dpi = 300)
}

plot_fun('Los Angeles')
plot_fun('Santa Barbara')
plot_fun('San Diego')
#################


#################
## Proportion of Trips Assigned an incorrect block - all port complexes
#entries with block long of NA which signifies that the block does not exist

explor%>%
  filter(is.na(block_long_dd)) # all exist!
#################




#################
##Depth
#Depth being reported through time
#what percentage of trips report depth through time
depth.year<-explor%>%
  filter(!is.na(depth_min_ft))%>%
  group_by(year)%>%
  summarize(ntrips.depth=n_distinct(trip.identifier))

no.depth.year<-explor%>%
  filter(is.na(depth_min_ft))%>%
  group_by(year)%>%
  summarize(ntrips.no.depth=n_distinct(trip.identifier))

#combine
depth.reported <- full_join(depth.year, no.depth.year, by = "year") %>%
  mutate(across(everything(), ~replace_na(., 0)))%>%
  mutate(percent.depth=(ntrips.depth/(ntrips.depth+ntrips.no.depth)*100))

p <-ggplot(data=depth.reported, aes(x=year, y=percent.depth))+
  geom_point()+
  ggtitle("Percent of Sea Cucumber Trips with Reported Depth")+
  labs(x="Year", y="Percentage")+
  # geom_smooth(method="lm")+
  base_theme

ggsave(filename = 'Dive/Figures/Cucumber/Exploratory/Percent_Depth_Reported.jpg', plot = p, width = 8, height = 6, units = "in", dpi = 300)
#################



#################
#Depth by complexes
# number of trips with the reported depth not in the correct bounds
cuc.with.depth<-explor[!is.na(explor$depth_min_ft),]%>%
  dplyr::select(port.complex, year, block_id, Min_depth_m_recorded,Max_depth_m_recorded, Med_depth_m_recorded,Max_Depthm, Min_Depthm, mpayear)

inside.bounds<-cuc.with.depth%>%
  filter(
    (is.na(Max_Depthm) |Max_depth_m_recorded >=  Max_Depthm) & 
      (is.na(Min_Depthm) | Min_depth_m_recorded  <= Min_Depthm) #opposite cause negative numbers
  )
inside.bounds$year<-as.factor(inside.bounds$year)


#boxplots function
plot_fun<-function(portcomplex){ 
  
  df<-inside.bounds%>%
    filter(port.complex==portcomplex)%>%
    dplyr::select( year,block_id, Max_Depthm,Min_Depthm, Min_depth_m_recorded,Max_depth_m_recorded, Med_depth_m_recorded, mpayear)%>%
    mutate(log_depth=log(-1*Med_depth_m_recorded))
  
  # Convert `year` to a factor to ensure separate boxplots per year
  df <- df %>%
    mutate(year = as.factor(year))
  
  mpayear <- as.factor(df %>%
                         distinct(mpayear) %>%
                         pull() ) # Extract the single value directly
  p<-ggplot(data=df, aes(x=year,y=log_depth))+
    geom_rect(
      aes(xmin = "2014", xmax = "2016",
          ymin = -Inf, ymax = Inf),
      fill = "gray80", alpha = 0.1, inherit.aes = FALSE
    ) +
    geom_boxplot()+
    theme_bw() + base_theme +
    ggtitle(paste0(portcomplex,' Port Complex'), subtitle = 'Depth Distributions of Sea Cucumber Dive Trips')+
    # theme(legend.position = "bottom")+
    ylab("Logged Depth (m)")+
    xlab("Year")+
    geom_vline(xintercept = mpayear, linetype = "dotted", color = "red",size=1)

  name=paste0('Dive/Figures/Cucumber/Exploratory/',portcomplex,' Depth Distributions.jpg')
  ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)
}
plot_fun("Santa Barbara")
plot_fun("Los Angeles")
plot_fun("San Diego")

#################