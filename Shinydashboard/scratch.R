library(tidyverse)
library(sf)
library(sp)
library(spatstat)
library(raster)
library(ggforce)
library(RColorBrewer)
library(magick)
############
#spatial data for graphing

usa_mexico_sf<-st_read("Shinydashboard/California_fisheries/data/usa_mexico_sf.shp")
usa_mexico_sf_proj<-st_read("Shinydashboard/California_fisheries/data/usa_mexico_sf_proj.shp")
# State waters
state_waters <- readRDS(("Shinydashboard/California_fisheries/data/CA_state_waters_polyline.Rds"))

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

fisheries_annual<-rbind(cpfv,cuc_trawl,cuc_dive,urch_dive, set, hal_trawl, lobster, pink_shrimp, ridge_prawn,squid)

saveRDS(fisheries_annual, ("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/all_fisheries_KD_allyears.rds"))

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
  dplyr::select(port.complex, year, block_id, total_dive_hours,geometry)%>%
  rename(effort=total_dive_hours)%>%
  mutate(fishery="Urchin dive")
set<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/set_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_set_hr,geometry)%>%
  rename(effort=total_set_hr)%>%
  mutate(fishery="Set gillnet")
hal_trawl<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/halibut_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_tow_hrs,geometry)%>%
  rename(effort=total_tow_hrs)%>%
  mutate(fishery="Halibut trawl")
lobster<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/lobster_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, trap_nights,geometry)%>%
  rename(effort=trap_nights)%>%
  mutate(fishery="Lobster trap")
ridge_prawn<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/ridge_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Ridgeback prawn trawl")
pink_shrimp<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard//scratch data/pink_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Pink shrimp trawl")
squid<-readRDS("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/scratch data/squid_summarizeddata_sepyears.rds")%>%
  dplyr::select(port.complex, year, block_id, total_tow_min,geometry)%>%
  rename(effort=total_tow_min)%>%
  mutate(fishery="Squid seine")

fisheries_yearly<-rbind(cpfv,cuc_trawl,cuc_dive,urch_dive, set, hal_trawl, lobster, pink_shrimp, ridge_prawn,squid)
fisheries_yearly<-st_transform(fisheries_yearly, crs = 3310)

saveRDS(fisheries_yearly, ("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/all_fisheries_KD_sepyears.rds"))


# run yearly kernel densities -----

plot_fun<-function(fishery_subset){ 
  
  fish <- fisheries_yearly%>%
    filter(fishery==fishery_subset)
  
  unique_portcomplexes <- unique(fish$port.complex)
  
  all_contours <- list()
  all_densities <- list()
  
  for (portcomplex in unique_portcomplexes) {
    
    location <- fish %>%
      filter(port.complex == portcomplex) %>%
      group_by(year) %>%
      filter(n() > 1) %>%
      ungroup()
    
    unique_years<-unique(location$year)
    
    for(year in unique_years){
      
      year_data<-location[location$year==year,]
      
      coordinates<-st_coordinates(year_data)
      weights<-year_data$effort
      
      minx<-(min(coordinates[,1], na.rm = TRUE))
      maxx<-(max(coordinates[,1], na.rm = TRUE))
      miny<-(min(coordinates[,2], na.rm = TRUE))
      maxy<-(max(coordinates[,2], na.rm = TRUE))
      
      tolerance <- 0.0000001
      
      if (abs(minx - maxx) < tolerance) {
        minx <- minx - 0.025
        maxx <- maxx + 0.025
      }
      
      if (abs(miny - maxy) < tolerance) {
        miny <- miny - 0.025
        maxy <- maxy + 0.025
      }
      
      minx <- minx - 20000
      maxx <- maxx + 20000
      miny <- miny - 20000
      maxy <- maxy + 20000
      
      points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
      
      sigma<-bw.diggle(points_ppp)
      
      density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE, edge=TRUE)
      
      density_raster <- as(raster(as.im(density)), "RasterLayer")
      
      buffer_distance <- 20000
      extended_extent <- extend(extent(density_raster), buffer_distance)
      extended_raster <- extend(density_raster, extended_extent, value = 0)
      density_masked <- mask(extended_raster, usa_mexico_sf_proj, inverse=TRUE)
      
      raster_file <- file.path(paste0("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/rasters/", fishery_subset, "_", portcomplex, "_", year, ".tif"))
      writeRaster(density_masked, raster_file, format = "GTiff", overwrite = TRUE)
      
      density_df <- as.data.frame(rasterToPoints(density_masked))
      names(density_df) <- c("x", "y", "value")
      
      density_values <- sort(density_df$value, decreasing = TRUE)
      cdf <- cumsum(density_values) / sum(density_values)
      
      contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
      
      contour_level_75_lines <- rasterToContour(density_masked, levels = c(contour_level_75))
      
      contour_75_sf <- st_as_sf(contour_level_75_lines)
      st_crs(contour_75_sf) <- 3310
      
      contour_75_sf$year <- year
      contour_75_sf$portcomplex <- portcomplex
      contour_75_sf$fishery <- fishery_subset
      all_contours[[length(all_contours) + 1]] <- contour_75_sf
      
      density_df$value <- scales::rescale(density_df$value)
      density_df$year <- year
      density_df$portcomplex <- portcomplex
      density_df$fishery <- fishery_subset
      all_densities[[length(all_densities) + 1]] <- density_df
    }
    
  } 
  
  merged_contours <- do.call(rbind, all_contours)
  merged_file <- file.path(paste0("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/", fishery_subset,"_KD_contours.gpkg"))
  st_write(merged_contours, merged_file, delete_layer = TRUE, quiet = TRUE)
  
  cat("Done:", fishery_subset, "\n")
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


# run all years combined kernel densities -----
allyears_fun <- function(fishery_subset) {
  
  fish <- fisheries_annual %>%
    filter(fishery == fishery_subset) %>%
    st_transform(crs = 3310)
  
  unique_portcomplexes <- unique(fish$port.complex)
  
  all_contours <- list()
  
  for (portcomplex in unique_portcomplexes) {
    
    location <- fish %>%
      filter(port.complex == portcomplex)
    
    if (nrow(location) < 2) next
    
    coordinates <- st_coordinates(location)
    weights <- location$effort
    
    minx <- min(coordinates[,1], na.rm = TRUE)
    maxx <- max(coordinates[,1], na.rm = TRUE)
    miny <- min(coordinates[,2], na.rm = TRUE)
    maxy <- max(coordinates[,2], na.rm = TRUE)
    
    tolerance <- 0.0000001
    if (abs(minx - maxx) < tolerance) { minx <- minx - 0.025; maxx <- maxx + 0.025 }
    if (abs(miny - maxy) < tolerance) { miny <- miny - 0.025; maxy <- maxy + 0.025 }
    
    minx <- minx - 20000; maxx <- maxx + 20000
    miny <- miny - 20000; maxy <- maxy + 20000
    
    points_ppp <- ppp(coordinates[,1], coordinates[,2],
                      window = owin(c(minx, maxx), c(miny, maxy)))
    
    sigma <- bw.diggle(points_ppp)
    density <- density.ppp(points_ppp, sigma = sigma, kernel = "gaussian",
                           weights = weights, diggle = TRUE,
                           scalekernel = TRUE, edge = TRUE)
    
    density_raster <- as(raster(as.im(density)), "RasterLayer")
    extended_extent <- extend(extent(density_raster), 20000)
    extended_raster <- extend(density_raster, extended_extent, value = 0)
    density_masked <- mask(extended_raster, usa_mexico_sf_proj, inverse = TRUE)
    
    # save raster named fishery_portcomplex_allyears.tif
    raster_file <- file.path(paste0(
      "C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/rasters/",
      fishery_subset, "_", portcomplex, "_allyears.tif"))
    writeRaster(density_masked, raster_file, format = "GTiff", overwrite = TRUE)
    
    density_df <- as.data.frame(rasterToPoints(density_masked))
    names(density_df) <- c("x", "y", "value")
    density_values <- sort(density_df$value, decreasing = TRUE)
    cdf <- cumsum(density_values) / sum(density_values)
    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    contour_lines <- rasterToContour(density_masked, levels = contour_level_75)
    contour_sf <- st_as_sf(contour_lines)
    st_crs(contour_sf) <- 3310
    contour_sf$portcomplex <- portcomplex
    contour_sf$fishery <- fishery_subset
    all_contours[[length(all_contours) + 1]] <- contour_sf
  }
  
  merged_contours <- do.call(rbind, all_contours)
  merged_file <- paste0(
    "C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/",
    fishery_subset, "_KD_allyears_contours.gpkg")
  st_write(merged_contours, merged_file, delete_layer = TRUE, quiet = TRUE)
  
  cat("Done:", fishery_subset, "\n")
}

allyears_fun("CPFV")
allyears_fun("Cucumber trawl")
allyears_fun("Cucumber dive")
allyears_fun("Urchin dive")
allyears_fun("Set gillnet")
allyears_fun("Halibut trawl")
allyears_fun("Lobster trap")
allyears_fun("Ridgeback prawn trawl")
allyears_fun("Pink shrimp trawl")
allyears_fun("Squid seine")


#read these back in ----
fisheries_list <- c("CPFV", "Cucumber trawl", "Cucumber dive", "Urchin dive", 
                    "Set gillnet", "Halibut trawl", "Lobster trap", 
                    "Ridgeback prawn trawl", "Pink shrimp trawl", "Squid seine")

base_path <- "C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/"

all_contours_list <- list()

for (fishery in fisheries_list) {
  contour_path <- file.path(base_path, paste0(fishery, "_KD_contours.gpkg"))
  if (file.exists(contour_path)) {
    contours <- st_read(contour_path, quiet = TRUE)
    contours$fishery <- fishery
    all_contours_list[[fishery]] <- contours
  } else {
    warning(paste("Missing contour file for", fishery))
  }
}

all_contours_combined <- do.call(rbind, all_contours_list)
contours <- file.path(("C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/data/allfisheries_KD_allcontours.gpkg"))
st_write(all_contours_combined, contours, append=FALSE)

###########
#animation
# Load everything the app uses: available_rasters (already filtered), KD_contours_sepyears,
# COG_sf, ports, usa_mexico_sf_proj, state_waters, myCustomTheme, etc.
source("global.R")

out_root  <- "www/animations"
frame_dir <- file.path(tempdir(), "kd_frames")   # temporary frame images
fps       <- 1                                   # frames (years) per second

# Fill palette: same as the app's yearly map
kd_cols <- c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1])

# Every fishery x port complex that still has years after the < 3 vessel filter
combos <- available_rasters2 %>% distinct(fishery, portcomplex)
message(nrow(combos), " fishery x port complex combinations to animate")

for (i in seq_len(nrow(combos))) {
  
  fish <- combos$fishery[i]
  port <- combos$portcomplex[i]
  
  years <- available_rasters2 %>%
    filter(fishery == fish, portcomplex == port) %>%
    pull(year) %>% sort()
  
  if (length(years) < 2) {                 # an animation needs at least 2 frames
    message("Skipping ", fish, " / ", port, ": only ", length(years), " year(s)")
    next
  }
  message("Animating ", fish, " / ", port, " (", length(years), " years)")
  
  # ---- Fixed map window for all frames: COG extent + 80 km (same as the app's yearly map) ----
  cog_xy <- COG_sf %>%
    filter(fishery == fish, port.complex == port) %>%
    st_transform(3310) %>%
    st_coordinates()
  if (nrow(cog_xy) == 0) { message("  No COG points, skipping"); next }
  xlim <- range(cog_xy[, 1], na.rm = TRUE) + c(-80000, 80000)
  ylim <- range(cog_xy[, 2], na.rm = TRUE) + c(-80000, 80000)
  
  port_pt <- st_transform(ports[ports$port.complex == port, ], 3310)
  
  # ---- Read every year's raster once ----
  dens <- lapply(years, function(y) {
    r  <- terra::rast(file.path("data/rasters", paste0(fish, "_", port, "_", y, ".tif")))
    df <- as.data.frame(r, xy = TRUE)
    colnames(df) <- c("x", "y", "value")
    df
  })
  names(dens) <- years
  
  # One colour scale across all years, so colours are comparable from frame to frame
  max_val <- max(sapply(dens, function(d) max(d$value, na.rm = TRUE)))
  
  # ---- One frame per year ----
  unlink(frame_dir, recursive = TRUE)
  dir.create(frame_dir, recursive = TRUE)
  frame_files <- character(0)
  
  for (y in years) {
    cont <- KD_contours_sepyears %>%
      filter(fishery == fish, port.complex == port, as.character(year) == as.character(y))
    
    p <- ggplot() +
      geom_raster(data = dens[[as.character(y)]], aes(x = x, y = y, fill = value)) +
      scale_fill_gradientn(name = "Kernel density", colors = kd_cols,
                           limits = c(0, max_val), na.value = "white") +
      geom_sf(data = cont, color = "black", linewidth = 1) +                                     # 75% contour
      geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = FALSE) +
      geom_sf(data = state_waters, color = "grey30", linewidth = 0.2, inherit.aes = FALSE) +
      geom_sf(data = port_pt, shape = 8, color = "red", size = 3, stroke = 1.2, inherit.aes = FALSE) +
      coord_sf(xlim = xlim, ylim = ylim) +
      labs(title = y) +
      myCustomTheme() +
      theme(legend.position = "none",
            axis.title = element_blank(),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    
    f <- file.path(frame_dir, paste0(y, ".png"))
    ggsave(f, p, width = 6, height = 6, dpi = 120)
    frame_files <- c(frame_files, f)
  }
  
  # ---- Stitch frames into a GIF ----
  out_dir <- file.path(out_root, fish)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  anim <- image_read(frame_files) %>% image_animate(fps = fps)
  image_write(anim, file.path(out_dir, paste0(port, "Kernel Densities_Animation.gif")))
  
  rm(dens, anim); gc()
}

message("Done. Animations are in ", normalizePath(out_root))
############