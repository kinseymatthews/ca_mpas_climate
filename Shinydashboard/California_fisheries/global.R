library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(markdown)
library(shinycssloaders)
library(sf)
library(terra)
library(RColorBrewer)
library(shinymanager)

#.......................add in custom theme.......................
# Define your authorized credentials
readRenviron(".Renviron")
credentials <- data.frame(
  user = Sys.getenv("SHINY_APP_USER", "prop68team"),
  password = Sys.getenv("SHINY_APP_PASSWORD"),
  stringsAsFactors = FALSE
)

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
#.......................manual darker yellow color.......................


rb_cols <- rev(c(
  "#A50026",
  "#D73027",
  "#F46D43",
  "#FDAE61",
  "#FEE090",
  "#E0F3F8",
  "#ABD9E9",
  "#74ADD1",
  "#045A8D"
))

#.......................Read in ports.......................

#upload port information - only plotting the major port of each complex
ports<-read_sf('data/port codes_updated_April2025.shp')%>%
  rename(port=PortName)%>%
  mutate(port = str_to_title(port))%>%
  filter(port=="Eureka"| port=="Fort Bragg"| port=="Bodega Bay"| port=="San Francisco"| port=="Monterey"| port=="Morro Bay"| port=="Santa Barbara Harbor"| port=="Los Angeles"| port=="San Diego")%>%
  mutate(MajorPort = str_to_title(MajorPort))%>%
  rename(port.complex=MajorPort)

#.......................define port order.......................
port_order <- c(
  "Eureka",
  "Fort Bragg",
  "Bodega Bay",
  "San Francisco",
  "Monterey",
  "Morro Bay",
  "Santa Barbara",
  "Los Angeles",
  "San Diego"
)

names(rb_cols) <- port_order
#.......................spatial data for mapping.......................
#spatial data for graphing
usa_mexico_sf_proj<-st_read("data/usa_mexico_sf_proj.shp")
# State waters
state_waters <- readRDS(("data/CA_state_waters_polyline.Rds"))


#.......................wrangle data for time series.......................

# read in trip and vessel data
#vessels_trips<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_vessels_trips.rds")
vessels_trips<-readRDS("data/all_fisheries_vessels_trips.rds")%>%
  #filter for more than three vessels
  filter(nvessels >= 3)%>%
  mutate(year=as.integer(year))

vessels_trips$port.complex<- factor(vessels_trips$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))

#read in depths
depths <- readRDS("data/all_fisheries_depth.rds") %>%
  group_by(fishery, port.complex) %>%
  filter(!duplicated(effort.identifier)) %>%
  ungroup() %>%
  mutate(
    depth_abs = abs(Depth_m),
    depth_abs = pmin(depth_abs, 1000)   # truncate depths >1000 m to 1000 m
  ) %>%
  group_by(fishery, port.complex, year) %>%
  summarise(
    lower  = quantile(depth_abs, 0.25, na.rm = TRUE),
    middle = median(depth_abs, na.rm = TRUE),
    mean=mean(depth_abs, na.rm = TRUE),
    upper  = quantile(depth_abs, 0.75, na.rm = TRUE),
    ymin   = quantile(depth_abs, 0.05, na.rm = TRUE),
    ymax   = quantile(depth_abs, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.integer(as.character(year)))%>%
  inner_join(vessels_trips %>% dplyr::select(fishery, port.complex, year, nvessels),
             by = c("fishery", "port.complex", "year")) %>%   # keeps only years with >= 3 vessels
  mutate(year = factor(year)) 


#read in dataframe for latitude and extents, fishing gorund area, distance from shore, and effort in <PA
KD_timeseries<-readRDS("data/all_fisheries_lat_extent_shore_mpa_area.rds")
KD_timeseries$port.complex<- factor(KD_timeseries$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))

#.......................wrangle data for COG.......................
COG_sf<-readRDS("data/all_fisheries_COG.rds")%>%
  filter(fishery!="Drift gillnet")%>%
  st_as_sf( coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))%>%
  st_transform( st_crs( 3310))


#.......................wrangle data for yearly KD......................
#KD_sepyears<-readRDS("data/all_fisheries_KD_sepyears.rds")
#KD_sepyears<-st_transform(KD_sepyears, crs = 3310)

#KD_densitiessepyears<-readRDS("data/allfisheries_KD_allrasters.rds")%>%
 # rename(port.complex=portcomplex)
KD_contours_sepyears <- st_read("data/allfisheries_KD_allcontours_simplified.gpkg")%>%
  filter(!(port.complex == "Eureka" & fishery == "Cucumber trawl"))


KD_contours_sepyears$port.complex<- factor(KD_contours_sepyears$port.complex, levels = c( 'Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))


#.......................file names for spatial predictoins......................

pred_files <- list.files("www", pattern = "^predictions_.*_differences_oct2026\\.jpg$")
pred_info <- do.call(rbind, lapply(pred_files, function(f) {
  parts <- strsplit(gsub("\\.jpg$", "", f), "_")[[1]]
  data.frame(file= f, fishery = parts[2], port.complex = parts[3], variable = parts[4])
}))

pred_info$port.complex.label <- gsub("([a-z])([A-Z])", "\\1 \\2", pred_info$port.complex)
pred_info$fishery.label <- gsub("([a-z])([A-Z])", "\\1 \\2", pred_info$fishery)

nrow(pred_info)

# raster metadata ----
raster_files <- list.files("data/rasters/", pattern = "\\.tif$", full.names = FALSE)
raster_meta <- do.call(rbind, strsplit(gsub(".tif", "", raster_files), "_"))
colnames(raster_meta) <- c("fishery", "portcomplex", "year")
available_rasters <- as.data.frame(raster_meta, stringsAsFactors = FALSE)
available_rasters$year <- as.numeric(available_rasters$year)

number.vessels.KD <- read.csv("data/KD_nvessels_3.csv") %>%
  mutate(portcomplex = trimws(gsub("\u00a0", " ", portcomplex)))   # non-breaking space -> normal space

# CSV fishery name -> name used in the raster files
fishery_lookup <- c("cucumber_trawl"           = "Cucumber trawl",
                    "urchin_dive"              = "Urchin dive",
                    "cucumber_dive"            = "Cucumber dive",
                    "gillnet"                  = "Set gillnet",
                    "California_halibut_trawl" = "Halibut trawl",
                    "lobster_trap"             = "Lobster trap",
                    "ridgeback_trawl"          = "Ridgeback prawn trawl",
                    "pink_trawl"               = "Pink shrimp trawl",
                    "squid_seine"              = "Squid seine")

years_to_remove <- number.vessels.KD %>%
  transmute(fishery     = unname(fishery_lookup[fishery]),   # translate to raster names
            portcomplex = portcomplex,
            year        = as.numeric(year)) %>%
  distinct()

# Any fishery names that didn't translate? Should be 0
sum(is.na(years_to_remove$fishery))

available_rasters2 <- available_rasters %>%
  filter(!is.na(year)) %>%                       # drop the "_allyears" rasters (their "year" isn't a number)
  anti_join(years_to_remove, by = c("fishery", "portcomplex", "year"))
