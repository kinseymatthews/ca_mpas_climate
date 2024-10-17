
#libraries
library(gganimate)
library(dplyr)
library(sf)
library(sp)


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
  left_join(all_blocks_df, by=c("block_most_reliable"="block_id"))%>%
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

# WAIT REMOVE BLOCKS TOO DEEP AND REMOVE BLOCKS NORTH OF MONTEREY BAY


explor<-explor%>%
mutate(dive.identifier.2=paste(date.logbooks,port_id_logbook,vessel_id,block_id,depth_min_ft, depth_max_ft, hours, dive_number, fisher_id,catch_lbs, sep="_"))
  
explor2<-explor%>%
  filter(!is.na(hours)) %>%#loose 2.5% of data
  filter(port.complex!="Unknown Major Port")

duplicates <- explor2 %>%
  group_by(dive.identifier.2) %>%
  filter(n() > 1) %>%
  ungroup()

duplicates #302


differing_groups <- duplicates %>%
  group_by(dive.identifier.2) %>%
  filter(n_distinct(comm_name) <= 1) %>%
  ungroup()
differing_groups

differing_groups2 <- differing_groups %>%
  group_by(dive.identifier) %>%
  filter(n_distinct(catch_lbs) <= 1)


explor2%>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))
