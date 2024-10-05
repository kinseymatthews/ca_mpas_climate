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
  filter(!is.na(port.complex))

saveRDS(explor,"CPFV/Data/Processed Data/CPFV_filtered.rds")

############


