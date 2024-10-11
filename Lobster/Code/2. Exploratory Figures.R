
library(tidyverse)
library(sf)



#################

###### read in lobster data
Lobster<-readRDS("Lobster/Data/Processed Data/lobster_logbooks_edited.rds")%>%
  rename(port.complex1=port_complex1, port.complex2=port_complex2)


     
###### read in block data
all_blocks<-st_read("Lobster/Data/Raw Data/Blocks/All_Blocks_Updated_10Jul24.shp")%>%
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
#are too deep for the fishery (minimum block depth >300 feet)
#or where the blocks are too large for analyses - international and offshore blocks.

#Figured out that all midshore blocks besides 897 are too deep for the fishery, and 
#blocks 108,135,256,257,265,419,456,457,465, are too north for the fishery

#Data Manipulation
#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)

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
  mutate(block_id = coalesce(block_id_logbook,block_id_gps))%>% #prefers logbook block id to gps block id
  left_join(all_blocks_df, by="block_id")%>%
  #getting rid of international, offshore,  all midshore besides 897, and all blocks that are too deep
  filter(blck_st!="International" | is.na(blck_st) )%>%
  filter(blck_ty!="Offshore" | is.na(blck_st))%>%
  filter(!(blck_ty == 'Midshore' & block_id != 897))%>%
  filter(Min_Depthm>= -110 | is.na(Min_Depthm))%>%
  #adding column for reported depth in meters
  mutate(Depth_m=depth_ft*(-0.3048))

#removing block IDs north of monterey bay where fishing shouldnt occur
block.to.remove<-c(108,135,256,257,265,419,456,457,465)

explor<-explor%>%
  filter(!block_id %in% block.to.remove)

##### which port complexes to keep?
explor %>%
  group_by(port.complex) %>%
  summarize(ntrips = n())%>%
  ungroup()%>%
  mutate(total.trip=sum(ntrips))%>%
  group_by(port.complex) %>%
  mutate(percent.port=(ntrips/total.trip)*100)%>%
  mutate(percent.port = round(percent.port, 4))
# only keep SB, LA, and SD for fishing ground analyses
#################



#################
##Ports
#port IDs of NA - 56241
NA.port<-explor[is.na(explor$port.complex),]
not.NA.port<-explor[!is.na(explor$port.complex),]

#Across time to see if reporting is getting worse or better
NA.ports.time<- NA.port %>%
  group_by(year)%>%
  summarise(n.trips.na=n())

not.NA.ports.time<- not.NA.port %>%
  group_by(year)%>%
  summarise(n.trips.not.na=n())

NA.ports.time.2<-merge(NA.ports.time, not.NA.ports.time, by="year", all=TRUE)
NA.ports.time.2[is.na( NA.ports.time.2)]<-0
df<- NA.ports.time.2%>%
  mutate(percent.NA=(n.trips.na/(n.trips.na+n.trips.not.na)*100))

p <-ggplot(data=df, aes(x=year, y=percent.NA))+
  geom_point()+
  ggtitle("Percent of Lobster Trips Without Port Codes" )+
  labs(x="Year", y="Percentage")+
  # geom_smooth(method="lm")+
  base_theme

ggsave(plot = p, filename = ("Lobster/Figures/Exploratory/Perentage of Lobster Trips without Port Codes.jpg"), width = 8, height = 5, units = "in", dpi = 300)

#Depth distribution of lobster trips without port codes
#compare distribution of depth attributes to make sure that the missing data is representative of the 
#other data and not biased in some way. look at depth reported of trips with port ids versus those 
#without port codes. If there is a significant difference we could see if here is biases. density distribution 
#of depths. 
#filter data so that only blocks with depth are included
lobster.with.depth<-explor[!is.na(explor$depth_ft),]

#summarize number of trips with port complexes
yes.ports<-lobster.with.depth[!is.na(lobster.with.depth$port.complex),]
#etract relevant columns
yes.ports<-yes.ports%>%
  dplyr::select(depth_ft, Depth_m)%>%
  mutate(Port="Yes")

#summarize number of trips with without ports
no.ports<-lobster.with.depth[is.na(lobster.with.depth$port.complex),]
no.ports<-no.ports%>%
  dplyr::select(depth_ft, Depth_m)%>%
  mutate(Port="No")

df<-rbind(yes.ports,no.ports)%>%
  mutate(Depth_m=Depth_m*-1)%>%
  #filtering depth data as there are some depths that are misreported
  filter(Depth_m>0)%>%
  filter(Depth_m<110)

#graphing
p<- ggplot() + 
  geom_density(data=df,aes(x=Depth_m,fill=Port),alpha = 0.5)+
  base_theme+
  ggtitle("Depth Distributions of Trips with and without Port IDs")+
  xlab("Depth (m)")+
  ylab("Density")

ggsave(filename = 'Lobster/Figures/Exploratory/Port and No Port Depth Density Distributions less 110 m.jpg', plot = p, width = 15, height = 8, units = "in", dpi = 300)
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
    summarize(ntrips=n(),
              nvessels=n_distinct(vessel_id))
  #Plot of nubmer of trips
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
  
  
  filename1 <- paste0('Lobster/Figures/Exploratory/', portcomplex, '_NTrips_Year.jpg')
  ggsave(filename = filename1, plot = p, width = 8, height = 6, units = "in", dpi = 300)
  filename2 <- paste0('Lobster/Figures/Exploratory/', portcomplex, '_NVessels_Year.jpg')
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
  filter(!is.na(depth_ft))%>%
  group_by(year)%>%
  summarize(ntrips.depth=n())

no.depth.year<-explor%>%
  filter(is.na(depth_ft))%>%
  group_by(year)%>%
  summarize(ntrips.no.depth=n())

#combine
depth.reported <- full_join(depth.year, no.depth.year, by = "year") %>%
  mutate(across(everything(), ~replace_na(., 0)))%>%
  mutate(percent.depth=(ntrips.depth/(ntrips.depth+ntrips.no.depth)*100))

p <-ggplot(data=depth.reported, aes(x=year, y=percent.depth))+
  geom_point()+
  ggtitle("Percent of Lobster Trips with Reported Depth")+
  labs(x="Year", y="Percentage")+
  # geom_smooth(method="lm")+
  base_theme

ggsave(filename = 'Lobster/Figures/Exploratory/Percent_Depth_Reported.jpg', plot = p, width = 8, height = 6, units = "in", dpi = 300)
#################



#################
#Depth by complexes
# number of trips with the reported depth not in the correct bounds
lobster.with.depth<-explor[!is.na(explor$depth_ft),]%>%
  dplyr::select(port.complex, year,depth_ft, Depth_m,block_id, block_id_gps, Max_Depthm, Min_Depthm, mpayear)

inside.bounds<-lobster.with.depth%>%
  filter(
    (is.na(Max_Depthm) |Depth_m >=  Max_Depthm) & 
      (is.na(Min_Depthm) | Depth_m <= Min_Depthm) #opposite cause negative numbers
  )
inside.bounds$year<-as.factor(inside.bounds$year)

outside.bounds<-lobster.with.depth%>%
  filter(!(Depth_m >= Max_Depthm & Depth_m <= Min_Depthm)) # 4516 rows out of bounds
inside.bounds$year<-as.factor(inside.bounds$year)
#boxplots function
plot_fun<-function(portcomplex){ 
  
  df<-inside.bounds%>%
    filter(port.complex==portcomplex)%>%
    dplyr::select( year,depth_ft,block_id, Max_Depthm,Min_Depthm, Depth_m,mpayear)%>%
    #filter(depth_ft>15)%>%
    mutate(log_depth=log(-1*Depth_m))
  
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
    ggtitle(paste0(portcomplex,' Port Complex'), subtitle = 'Depth Distributions of Lobster Trips')+
    # theme(legend.position = "bottom")+
    ylab("Logged Depth (m)")+
    xlab("Year")+
    geom_vline(xintercept = mpayear, linetype = "dotted", color = "red",size=1)+
    # Manually set x-axis breaks and labels
    scale_x_discrete(
      breaks = c(1980, 1990, 2000, 2010,2020),  # Specific breaks
      labels = c("1980", "1990", "2000", "2010", "2020")  # Corresponding labels
    ) 
   # scale_x_discrete(breaks = levels(df$year)) # Use discrete scale for factor years years
  
  name=paste0('Lobster/Figures/Exploratory/',portcomplex,' Depth Distributions.jpg')
  ggsave(filename = name, plot = p, width = 10, height = 6, units = "in", dpi = 300)
}
plot_fun("Santa Barbara")
plot_fun("Los Angeles")
plot_fun("San Diego")

#################