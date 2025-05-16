library(tidyverse)
library(sf)
library(sp)
library(spatstat)
library(raster)
library(ggforce)
library(RColorBrewer)
############
#spatial data for graphing

usa_mexico_sf<-st_read("data/usa_mexico_sf.shp")
usa_mexico_sf_proj<-st_read("data/usa_mexico_sf_proj.shp")
# State waters
state_waters <- readRDS(("data/CA_state_waters_polyline.Rds"))

#.......................COG .......................
#Upload data for each fishery
cpfv<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/COG_distance_results_anglerhours_final.rds")%>%
  mutate(fishery="CPFV",year=as.numeric(year))
cuc_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/seacucumber_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Cucumber trawl",year=as.numeric(year))
cuc_dive<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/COG_distance_results_divehours_final_cucumber.rds")%>%
  mutate(fishery="Cucumber dive",year=as.numeric(year))
urchin<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/COG_distance_results_divehours_final_seaurchin.rds")%>%
  mutate(fishery="Urchin dive",year=as.numeric(year))
drift<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/driftgillnet_COG_distance_results_final.rds")%>%
  mutate(fishery="Drift gillnet",year=as.numeric(year))
set<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/setgillnet_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Set gillnet",year=as.numeric(year))
hal_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/halibut_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Halibut trawl",year=as.numeric(year))
lobster<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/COG_distance_results_traphours_final.rds")%>%
  mutate(fishery="Lobster trap",year=as.numeric(year))
pink_shrimp<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pinkshrimp_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Pink shrimp trawl",year=as.numeric(year))
ridge_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridgeback_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Ridgeback prawn trawl",year=as.numeric(year))
squid<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/squid_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Squid seine",year=as.numeric(year))

fisheries<-rbind(cpfv,cuc_trawl,cuc_dive,urchin, drift, set, hal_trawl, lobster, pink_shrimp, ridge_prawn,squid)%>%
  dplyr::select(-X, -cog_longitude)

saveRDS(fisheries, ("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/all_fisheries_COG.rds"))
saveRDS(fisheries, ("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/all_fisheries_COG.rds"))

#.......................annual KD .......................
cpfv<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/CPFV_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_angler_hours,geometry)%>%
  rename(effort=total_angler_hours)%>%
  mutate(fishery="CPFV")
cuc_trawl<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/cucumber_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Cucumber trawl")
cuc_dive<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/cucumberdive_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_dive_hours,geometry)%>%
  rename(effort=total_dive_hours)%>%
  mutate(fishery="Cucumber dive")
urch_dive<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/urchindive_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_dive_hours,geometry)%>%
  rename(effort=total_dive_hours)%>%
  mutate(fishery="Urchin dive")
set<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/set_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_set_hr,geometry)%>%
  rename(effort=total_set_hr)%>%
  mutate(fishery="Set gillnet")
hal_trawl<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/halibut_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_tow_hrs,geometry)%>%
  rename(effort=total_tow_hrs)%>%
  mutate(fishery="Halibut trawl")
lobster<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/lobster_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, trap_nights,geometry)%>%
  rename(effort=trap_nights)%>%
  mutate(fishery="Lobster trap")
ridge_prawn<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/ridge_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Ridgeback prawn trawl")
pink_shrimp<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/pink_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Pink shrimp trawl")
squid<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/squid_summarizeddata_yearly.rds")%>%
  dplyr::select(port.complex, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Squid seine")

fisheries<-rbind(cpfv,cuc_trawl,cuc_dive,urch_dive, set, hal_trawl, lobster, pink_shrimp, ridge_prawn,squid)
  
                 
saveRDS(fisheries, ("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/all_fisheries_KD_allyears.rds"))

#.......................yearly KD .......................
cpfv<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/CPFV_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_angler_hours,geometry)%>%
  rename(effort=total_angler_hours)%>%
  mutate(fishery="CPFV")
cuc_trawl<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/cucumber_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Cucumber trawl")
cuc_dive<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/cucdive_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_dive_hours,geometry)%>%
  rename(effort=total_dive_hours)%>%
  mutate(fishery="Cucumber dive")
urch_dive<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/urchindive_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex,  year,block_id, total_dive_hours,geometry)%>%
  rename(effort=total_dive_hours)%>%
  mutate(fishery="Urchin dive")
set<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/set_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_set_hr,geometry)%>%
  rename(effort=total_set_hr)%>%
  mutate(fishery="Set gillnet")
hal_trawl<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/halibut_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex,  year,block_id, total_tow_hrs,geometry)%>%
  rename(effort=total_tow_hrs)%>%
  mutate(fishery="Halibut trawl")
lobster<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/lobster_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex,  year,block_id, trap_nights,geometry)%>%
  rename(effort=trap_nights)%>%
  mutate(fishery="Lobster trap")
ridge_prawn<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/ridge_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex,  year,block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Ridgeback prawn trawl")
pink_shrimp<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard//scratch data/pink_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex,  year,block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Pink shrimp trawl")
squid<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/squid_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex,  year,block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Squid seine")

fisheries<-rbind(cpfv,cuc_trawl,cuc_dive,urch_dive, set, hal_trawl, lobster, pink_shrimp, ridge_prawn,squid)
fisheries<-st_transform(fisheries, crs = 3310)

saveRDS(fisheries, ("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/all_fisheries_KD_sepyears.rds"))


# run kernel densities -----

plot_fun<-function(fishery_subset){ 
  
  fish <- fisheries%>%
    filter(fishery==fishery_subset)
  
  unique_portcomplexes <- unique(fish$port.complex)
  
  
  #contours an densities for shapefile
  all_contours <- list()
  all_densities <- list()
  
  for (portcomplex in unique_portcomplexes) {
    
    location <- fish %>%
      filter(port.complex == portcomplex) %>%
      group_by(year) %>%
      filter(n() > 1) %>%
      ungroup()
  
  #get unique years in dataset
  unique_years<-unique(location$year)

  #Loop through each year and calculate kernel density
  for(year in unique_years){
    
    #subset data for the current year
    year_data<-location[location$year==year,]
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(year_data)
    #weights
    weights<-year_data$effort
    
    # Create a point pattern (ppp object)
    minx<-(min(coordinates[,1], na.rm = TRUE))
    maxx<-(max(coordinates[,1], na.rm = TRUE))
    miny<-(min(coordinates[,2], na.rm = TRUE))
    maxy<-(max(coordinates[,2], na.rm = TRUE))
    
    # Define the tolerance
    tolerance <- 0.0000001
    
    if (abs(minx - maxx) < tolerance) {
      minx <- minx - 0.025
      maxx <- maxx + 0.025
    }
    
    
    if (abs( miny - maxy)< tolerance) {
      miny <- miny - 0.025
      maxy <- maxy + 0.025
    }
    
    # Expand bounding box
    minx <- minx - 20000
    maxx <- maxx + 20000
    miny <- miny - 20000
    maxy <- maxy + 20000
    
    points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
    
    # Compute kernel density estimation using raw and logcdata
    sigma<-bw.diggle(points_ppp)
    
    density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE , edge=TRUE)
    
    # Convert density to a raster (assuming a reasonable raster resolution)
    density_raster <- as(raster(as.im(density)), "RasterLayer")
    
    
    # Extend the extent of the density raster by 200 meters
    buffer_distance <- 20000  # 200 meters
    extended_extent <- extend(extent(density_raster), buffer_distance)
    
    # Create a new raster with the extended extent
    extended_raster <- extend(density_raster, extended_extent, value = 0)
    
    # Mask the density raster with the land raster
    density_masked <- mask(extended_raster, usa_mexico_sf_proj, inverse=TRUE)
    
    # You already have the raster earlier — use that instead of df
    raster_file <- file.path(paste0("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/rasters/", fishery_subset, "_", portcomplex, "_", year, ".tif"))
    writeRaster(density_masked, raster_file, format = "GTiff", overwrite = TRUE)
    
    
    # Convert density to a data frame
    density_df <- as.data.frame(rasterToPoints(density_masked))
    names(density_df) <- c("x", "y", "value")
    
    # Calculate the cumulative distribution function (CDF)
    density_values <- sort(density_df$value, decreasing = TRUE)
    cdf <- cumsum(density_values) / sum(density_values)
    
    # Find the contour level corresponding to 75% volume
    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    
    # Create contour lines from the density raster
    contour_level_75_lines <- rasterToContour(density_masked, levels = c(contour_level_75))
    
    
    # Convert contour lines to sf polygons
    contour_75_sf <- st_as_sf(contour_level_75_lines)
    
    st_crs(contour_75_sf) <-  3310
    
    #save into dataframe
    contour_75_sf$year <- year
    contour_75_sf$portcomplex <- portcomplex
    contour_75_sf$fishery <- fishery_subset
    #all_contours[[as.character(year)]] <- contour_75_sf
    all_contours[[length(all_contours) + 1]] <- contour_75_sf
    
    
    # Normalize density values to the range 0-1
    density_df$value <- scales::rescale(density_df$value)
    density_df$year <- year
    density_df$portcomplex <- portcomplex
    density_df$fishery <- fishery_subset
   # all_densities[[as.character(year)]] <- density_df
    all_densities[[length(all_densities) + 1]] <- density_df
 
  }
  
  
  } 
  # ---- Save merged contours to contour directory ----
  merged_contours <- do.call(rbind, all_contours)
  merged_file <- file.path(paste0("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/", fishery_subset,"_KD_contours.gpkg"))
  st_write(merged_contours, merged_file, delete_layer = TRUE, quiet = TRUE)
  
  # Merge and write densities
 # merged_densities <- do.call(rbind, all_densities)
#  density_file <- file.path(paste0("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/", fishery_subset,"_KD_allrasters.rds"))
#  saveRDS(merged_densities, density_file)
  


  }
  
  


plot_fun("CPFV")
plot_fun("Cucumber trawl")
plot_fun("Cucumber dive")
plot_fun("Urchin dive")
plot_fun("Set gillnet")
plot_fun("Halibut trawl")
plot_fun("Lobster trap")
plot_fun("Ridgeback prawn trawl")
plot_fun("Pink shrimp trawl")
plot_fun("Squid seine")

#read these back in ----
# List of all fishery names
fisheries_list <- c("CPFV", "Cucumber trawl", "Cucumber dive", "Urchin dive", 
                    "Set gillnet", "Halibut trawl", "Lobster trap", 
                    "Ridgeback prawn trawl", "Pink shrimp trawl", "Squid seine")

# Base path where everything is saved
base_path <- "C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/"

# Initialize lists to store loaded data
all_contours_list <- list()
all_densities_list <- list()

# Loop through and read each file
for (fishery in fisheries_list) {
  
  # Build file paths
  contour_path <- file.path(base_path, paste0(fishery, "_KD_contours.gpkg"))
  density_path <- file.path(base_path, paste0(fishery, "_KD_allrasters.rds"))
  
  # Read contour (GeoPackage)
  if (file.exists(contour_path)) {
    contours <- st_read(contour_path, quiet = TRUE)
    contours$fishery <- fishery
    all_contours_list[[fishery]] <- contours
  } else {
    warning(paste("Missing contour file for", fishery))
  }
  
  # Read density (RDS)
  if (file.exists(density_path)) {
    densities <- readRDS(density_path)
    densities$fishery <- fishery
    all_densities_list[[fishery]] <- densities
  } else {
    warning(paste("Missing density file for", fishery))
  }
}

# Optionally bind into one large object
all_contours_combined <- do.call(rbind, all_contours_list)
contours <- file.path(("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/allfisheries_KD_allcontours.gpkg"))
st_write( all_contours_combined, contours, append=FALSE)

all_densities_combined <- do.call(rbind, all_densities_list)
density_file <- file.path(("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/allfisheries_KD_allrasters.rds"))
saveRDS(all_densities_combined, density_file)


