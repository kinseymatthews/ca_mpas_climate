library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(markdown)
library(shinycssloaders)
library(sf)
library(sp)
library(spatstat)
library(raster)


# Set the working directory to your app folder
print(getwd()) 
#.......................add in custom theme.......................
myCustomTheme <- function() {
  theme_light() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.position = "bottom",
      panel.border = element_rect(linewidth = 0.7),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )
}


#.......................spatial data for mapping.......................
#spatial data for graphing
usa_mexico_sf_proj<-st_read("data/usa_mexico_sf_proj.shp")
# State waters
state_waters <- readRDS(("data/CA_state_waters_polyline.Rds"))


#.......................wrangle data for time series.......................

# read in trip and vessel data
#vessels_trips<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_vessels_trips.rds")
vessels_trips<-readRDS("data/all_fisheries_vessels_trips.rds")

vessels_trips$port.complex<- factor(vessels_trips$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))

#read in depths
depths<-readRDS("data/all_fisheries_depth.rds")%>%
  group_by(fishery,port.complex)%>%
  filter(!duplicated(effort.identifier))%>%
  ungroup()%>%
  mutate(log_depth=log(-1*Depth_m))


#read in dataframe for latitude and extents, fishing gorund area, distance from shore, and effort in <PA
KD_timeseries<-readRDS("data/all_fisheries_lat_extent_shore_mpa_area.rds")
KD_timeseries$port.complex<- factor(KD_timeseries$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))

#.......................wrangle data for COG.......................
COG_sf<-readRDS("data/all_fisheries_COG.rds")%>%
  filter(fishery!="Drift gillnet")%>%
  st_as_sf( coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform( st_crs( 3310))

#.......................wrangle data for all years combined KD......................
KD_combinedyears<-readRDS("data/all_fisheries_KD_allyears.rds")
KD_combinedyears <- st_transform(KD_combinedyears, crs = 3310)
#.......................wrangle data for yearly KD......................
#KD_sepyears<-readRDS("data/all_fisheries_KD_sepyears.rds")
#KD_sepyears<-st_transform(KD_sepyears, crs = 3310)

#KD_densitiessepyears<-readRDS("data/allfisheries_KD_allrasters.rds")%>%
 # rename(port.complex=portcomplex)
KD_contours_sepyears <- st_read("data/allfisheries_KD_allcontours.gpkg")%>%
  rename(port.complex=portcomplex)


KD_contours_sepyears$port.complex<- factor(KD_contours_sepyears$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))
