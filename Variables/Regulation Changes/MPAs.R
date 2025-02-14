library(tidyverse)
library(sf)
############
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

mpas<- st_read("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/MPAs/raw data/MPAs.shp")
mpas<- st_transform(mpas, crs = 4326)

get_min_lat <- function(geom) min(st_coordinates(geom)[, 2], na.rm = TRUE)
get_max_lat <- function(geom) max(st_coordinates(geom)[, 2], na.rm = TRUE)

# Add new columns for min and max latitude
mpas<- mpas %>%
  mutate(
    min_lat = map_dbl(geometry, get_min_lat),
    max_lat = map_dbl(geometry, get_max_lat)
  )%>%
  rename(study_region=study_regi)

#add implementation yera and date from wc fish
mpa_blocks<-wcfish::mpas_ca%>%
  as.data.frame()

mpas<-mpas%>%
  left_join(select(mpa_blocks,name,implementation_year,implementation_date), by="name")

#upload block information
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")
all_blocks <- st_make_valid(all_blocks)
############



############
#add in info about when the mpas were implmented 

year<-data.frame(study_region=c("NCSR","NCCSR","CCSR","SCSR","SFBSR"), year=c(2012,2010,2007,2012, 2007))

mpas2 <- mpas %>%
  left_join(year, by="study_region")%>%
    mutate(implementation_year=coalesce(implementation_year,year))%>%
  rename(impl_year = implementation_year, 
         impl_date = implementation_date)


 st_write(mpas2,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/MPAs/processed data/MPAs.gpkg")
write.csv(mpas2,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/MPAs/processed data/MPAs_updated.csv")
############

regs<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/MPAs/processed data/MPA Regulations.csv")
  
  
  
# Find values in df2$value2 that are not in df1$value1
missing_values <- setdiff(  regs$name,mpas2$name)
  

# Add the missing values to the end of df1$value1
regs$X <- c(regs$X, missing_values)
  
  
  
############
#spatial join to see what percentage of blocks is "covered" by mpa
all_blocks

############
# Calculate area and tidy up
intersect_pct <- st_intersection(all_blocks, mpas2) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(block_id, name, year, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()%>%  # drop geometry as we don't need it
  group_by(block_id, year) %>%#group by block id
  summarize(area_mpa=sum(intersect_area))

#get area for all blocks

ggplot(data=mpas2, mapping=aes(fill=implementation_region)) +
  # Blocks
  geom_sf(color="grey50", linewidth=0.1)

