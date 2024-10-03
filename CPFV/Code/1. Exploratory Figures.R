library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)

#################

###### read in CPFV data
CPFV<-readRDS("CPFV/Data/Raw Data/1980_2022_cpfv_logbooks.Rds")

###### read in block data
all_blocks<-st_read("CPFV/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

##### read in bathymetry data with blocks
bathy.data<-read.csv("Lobster/Data/Raw Data/fishingblockstatistics.csv")%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

##### merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()

#Port Codes
port<-read.csv('Lobster/Data/Raw Data/Port Codes.csv')


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
#which ports to include
df<- CPFV %>%
  mutate(logbookID_vesselID_date=paste(logbook_id, vessel_id, date, sep="_"))%>%
  #attach information from port codes
  left_join(port, by="port_code") %>%
  group_by(port.complex) %>%
  summarize(ntrips = n_distinct(logbookID_vesselID_date))%>%
  ungroup()%>%
  mutate(total.trip=sum(ntrips))%>%
  group_by(port.complex) %>%
  mutate(percent.port=(ntrips/total.trip)*100)
#remove  Sacramento Delta and Oregon
#################




#################
# Data Manipulation

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
  #remove Oregon port complex
  filter(is.na(port.complex) | port.complex!="Oregon")%>%
  filter(is.na(port.complex) | port.complex!="Sacramento Delta")%>%
  filter(is.na(port.complex) | port.complex!="San Diego")%>% # removing as most of the blocks are international
  mutate(Depth_m=depth*(-0.3048))%>%
  left_join(mpa,by="port.complex")%>%
  filter(is.na(blck_ty) |blck_ty!="Offshore")%>%
  filter(is.na(blck_st) |blck_st!="International")%>%
  mutate(angler_hours=hours_fished*number_of_fishers)%>%
  mutate(Depth_fathom=depth/(-6))



#################
##Ports
#Across time to see if reporting is getting worse or better
NA.ports.time<- explor %>%
  filter(is.na(port.complex))%>%
  group_by(year)%>%
  summarise(n.trips.na=n_distinct(logbookID_vesselID_date))

not.NA.ports.time< explor %>%
  filter(!is.na(port.complex))%>%
  group_by(year)%>%
  summarise(n.trips.not.na=n_distinct(logbookID_vesselID_date))

df<-merge(NA.ports.time, not.NA.ports.time, by="year", all=TRUE)
df[is.na(df)]<-0

df<- df%>%
  mutate(percent.NA=(n.trips.na/(n.trips.na+n.trips.not.na)*100))

p2 <-ggplot(data=df, aes(x=year, y=percent.NA))+
  geom_point()+
  ggtitle("Percent of Trips without Reported Port Codes ")+
  labs(x="Year", y="Percentage")+
  # geom_smooth(method="lm")+
  base_theme

ggsave(plot = p2, filename = ("CPFV/Figures/Exploratory/Percentage of CPFV Trips without Port Codes.jpg"), width = 8, height = 5, units = "in", dpi = 300)


#vessels and trips per year by prot complex
plot_fun<-function(portcomplex){ 
  
  #filtering for location
 df<-explor %>%
    filter(port.complex==portcomplex)%>%
    group_by(year)%>%
    summarize(ntrips=n_distinct(logbookID_vesselID_date),
              nvessels=n_distinct(vessel_id))
 
  #Plot of number of trips
  p<- ggplot(data=df, aes(x=year, y=ntrips))+
    geom_point()+
    base_theme+
    ggtitle(paste0(portcomplex," Port Complex: Number of CPFV Trips Across Time "))+
    labs(x="Year", y="Number of Trips")+
    geom_smooth(method="lm")+
    base_theme
  
  #plot of number of vessels
  p2<-ggplot(data=df, aes(x=year, y=nvessels))+
    geom_point()+
    base_theme+
    ggtitle(paste0(portcomplex," Port Complex: Number of CPFV Vessels Across Time "))+
    labs(x="Year", y="Number of Vessels")+
    geom_smooth(method="lm")+
    base_theme
  
  
  filename1 <- paste0('CPFV/Figures/Exploratory/', portcomplex, '_NTrips_Year.jpg')
  ggsave(filename = filename1, plot = p, width = 8, height = 6, units = "in", dpi = 300)
  filename2 <- paste0('CPFV/Figures/Exploratory/', portcomplex, '_NVessels_Year.jpg')
  ggsave(filename = filename2, plot = p2, width = 8, height = 6, units = "in", dpi = 300)
}

plot_fun('Eureka')
plot_fun('Fort Bragg')
plot_fun('Bodega Bay')
plot_fun('San Francisco')
plot_fun('Monterey')
plot_fun('Morro Bay')
plot_fun('Santa Barbara')
plot_fun('Los Angeles')


#Proportio of trips that are assigne a block that exists versus one that does not exis
#entries with block long of NA which signifies that the block does not exist
na.blocks.df<-explor[is.na(explor$block_long_dd),]%>%
  group_by(year)%>%
  summarize(ntrips.na=n_distinct(logbookID_vesselID_date))

#entries with block long of not NA which signifies that the block does exist
non.na.blocks.df<-explor[!is.na(explor$block_long_dd),]%>%
  group_by(year)%>%
  summarize(ntrips.non.na=n_distinct(logbookID_vesselID_date))

#combine
na.nonna.blocks<-merge(na.blocks.df,non.na.blocks.df, by="year")%>%
  mutate(percent.na=(ntrips.na/((ntrips.na + ntrips.non.na))*100))

#graph
p <-ggplot(data=na.nonna.blocks, aes(x=year, y=percent.na))+
  geom_point()+
  ggtitle("Percent of Trips Assigned an Incorrect Block")+
  labs(x="Year", y="Percentage")+
  geom_smooth(method="lm")+
  base_theme
p

ggsave(filename = 'CPFV/Figures/Exploratory/Percent_Incorrect_Blocks.jpg', plot = p, width = 8, height = 6, units = "in", dpi = 300)
