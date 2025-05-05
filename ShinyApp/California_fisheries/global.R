# load libraries ----
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(markdown)
#.......................add in custom theme.......................
myCustomTheme <- function() {
  theme_light() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.position = "bottom",
      panel.border = element_rect(linewidth = 0.7),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )
}

#.......................MPA implementation.......................
#adding in mpa implementation data
port.complex=c("Eureka","Fort Bragg","Bodega Bay","Monterey","San Francisco","Morro Bay","Santa Barbara","Los Angeles","San Diego")
values=c(2012,2012,2007,2007,2007,2007,2012,2012,2012)
mpa<-data.frame(port.complex=port.complex,mpayear=values)
#.......................wrangle data.......................

# read in trip and vessel data
vessels_trips<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_vessels_trips.rds")
vessels_trips$port.complex<- factor(vessels_trips$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))

#read in depths
depths<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_depth.rds")%>%
  group_by(fishery,port.complex)%>%
  filter(!duplicated(effort.identifier))%>%
  ungroup()%>%
  mutate(log_depth=log(-1*Depth_m))

#read in dataframe for latitude and extents, fishing gorund area, distance from shore, and effort in <PA
KD_timeseries<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_lat_extent_shore_mpa_area.rds")
KD_timeseries$port.complex<- factor(KD_timeseries$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))
