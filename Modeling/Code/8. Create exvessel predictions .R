library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(INLA)
library(broom)
library(spatstat)
library(raster)
library(sf)
############
#Upload data paths
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
model_folder <- paste0(base_path, "Modeling/Data/Processed Data/Model_Results/")


# base theme
base_theme <- theme(axis.text=element_text(size=7),
                    # axis.text.y = element_text(angle = 90, hjust = 0.5),
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

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
usa_mexico_sf_proj<- st_transform(usa_mexico_sf, crs = 3310)
# Union all geometries into one polygon
usa_mexico_union <- st_union(usa_mexico_sf_proj)

# State waters
state_waters <- readRDS( paste0(base_path,"All Fisheries Data/CA_state_waters_polyline.Rds"))
state_waters <- st_transform(state_waters, crs = 3310)


#upload pa models
model_files_pa <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT_pa_delta.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models_pa <- lapply(model_files_pa, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models_pa) <- gsub("^model_|\\_Depth_no_BT_pa_delta.rds$", "", basename(model_files_pa))
names(models_pa) <- gsub("_", " ", names(models_pa))
# Example: look at the summary for San Francisco Squid Seine
#m<-(models[["Los_Angeles_CPFV"]])

#upload abundance models
model_files_abun <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT_abundance_delta.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models_abun <- lapply(model_files_abun, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models_abun) <- gsub("^model_|\\_Depth_no_BT_abundance_delta.rds$", "", basename(model_files_abun))
names(models_abun) <- gsub("_", " ", names(models_abun))
# Example: look at the summary for San Francisco Squid Seine
#m<-(models[["Los_Angeles_CPFV"]])





#upload variable importance and edit to only include significant variables that are dynamic

coef_table<-readRDS(paste0(model_folder,'model_results_variable_importance_delta_abundance.rds'))
coef_table2<-coef_table%>%
  filter(term %in% c("cost_per_gal_adjusted_scaled",  "cost_per_pound_adjusted_scaled", "SST_scaled"))%>% # ← only terms to consider)
  filter(sign %in% c("Positive", "Negative"))%>%
  mutate(metier_variable=paste(model, term, sep = "_"))


#upload fisheries
fisheries<-readRDS(paste0(base_path,"Modeling/Data/Processed Data/fisheries_df_with_variables.rds"))
#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct", "Avg_Depthm")

fisheries <- fisheries%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))


#upload prediction grid
prediction_grid<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/prediction_grid_df.rds")%>%
  rename(block_long_dd=blck_ln_, block_lat_dd=blck_lt_ )%>%#rename to be same columns as mesh X and Y 
  separate(month_year, into = c("month", "year"), sep = "-", convert = TRUE, remove = FALSE)%>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         fyear = as.factor(year),
         fmonth_year = as.factor(month_year),
         sin12 = sin(2 * pi * month / 12),
         cos12 = cos(2 * pi * month / 12),
         closed_pct=(closed_pct*100),
         block_long_dd= as.numeric(block_long_dd),
         block_lat_dd =as.numeric(block_lat_dd )
         
  )

prediction_grid$month_year <- sub("^0+", "", prediction_grid$month_year)
prediction_grid$month_year <- sub("-", "_", prediction_grid$month_year)
prediction_grid$metier <- sub("_", " ", prediction_grid$metier)
# Convert to sf object
prediction_grid_sf <- st_as_sf(prediction_grid, coords = c("block_long_dd", "block_lat_dd"), crs = 4326)%>%
  st_transform(crs = st_crs(3310))

coords <- st_coordinates(prediction_grid_sf)
# Add to the polygon data as new columns
prediction_grid_sf$block_long_dd <- coords[,1]
prediction_grid_sf$block_lat_dd <- coords[,2]#/ 1000

#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct", "Avg_Depthm")

prediction_grid_sf <- prediction_grid_sf%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))

#upload block information
all_blocks<-st_read(paste0(base_path,"All Fisheries Data/Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d)

all_blocks<- st_transform(all_blocks, crs = st_crs(3310))
# Calculate centroids
centroids <- st_centroid(all_blocks)
# Extract lon/lat coordinates
coords <- st_coordinates(centroids)
# Add to the polygon data as new columns
all_blocks$block_long_dd <- coords[,1]/ 1000
all_blocks$block_lat_dd <- coords[,2]/ 1000

bathy.data<-read.csv(paste0(base_path, "All Fisheries Data/fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

##### merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")%>%
  mutate(block_id=as.factor(block_id))

all_blocks_sf<-all_blocks%>%
  dplyr::select(-c(blck_st, blck_ty, blck_sq, block_lat_dd, block_long_dd))%>%
  mutate(block_id=as.factor(block_id))

############

############
#find peak month for each fishery
peak.months=fisheries %>%
  mutate(year = year(date)) %>%                           
  filter(year >= max(year, na.rm = TRUE) - 9) %>%          # last 10 years
  group_by(metier,month)%>%
  summarize(total.effort=sum(total.hours, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(metier)%>%
  filter(total.effort==max(total.effort, na.rm = TRUE))%>%
  ungroup()
peak.months$metier <- sub("_", " ", peak.months$metier)
############

############
#edit prediction grid so it matches the peak months
prediction_grid_sf2<-prediction_grid_sf %>%
  semi_join(peak.months, by = c("month", "metier"))
############

############
# match original fisheries data with the significant coefficients so then we can calucate ther percentiles   
#getting models for predictions
coef_table2$model <- gsub("_", " ", coef_table2$model)   

models_predictions<-c(unique(coef_table2$model))
#replace "_" with space for matching
fisheries$metier<- gsub("_", " ", fisheries$metier)   
#filter fisheries data to get models 
fisheries_to_model<-fisheries%>%
  filter(metier %in% models_predictions)%>%
  semi_join(peak.months, by = c("month", "metier"))
############    

############   
#get Median, Average, 90th percentile, 10th percentile ("cost_per_gal_adjusted", "cost_per_pound_adjusted", "SST")
#for the high effort month of the specific metier


percentiles_scaled <- fisheries_to_model %>%
  group_by(metier, month) %>%
  summarize(
    p10_SST = quantile(SST, 0.10, na.rm=TRUE),
    median_SST = median(SST, na.rm=TRUE),
    p90_SST = quantile(SST, 0.90, na.rm=TRUE),
    mean_SST=mean(SST, na.rm=TRUE),
    sd_SST=sd(SST, na.rm=TRUE),
    
    p10_fuel_price = quantile(cost_per_gal_adjusted, 0.10, na.rm=TRUE),
    median_fuel_price = median(cost_per_gal_adjusted, na.rm=TRUE),
    p90_fuel_price = quantile(cost_per_gal_adjusted, 0.90, na.rm=TRUE),
    mean_fuel_price=mean(cost_per_gal_adjusted, na.rm=TRUE),
    sd_fuel_price=sd(cost_per_gal_adjusted, na.rm=TRUE),
    
    p10_pound_price = quantile(cost_per_pound_adjusted, 0.10, na.rm=TRUE),
    median_pound_price = median(cost_per_pound_adjusted, na.rm=TRUE),
    p90_pound_price = quantile(cost_per_pound_adjusted, 0.90, na.rm=TRUE),
    mean_pound_price=mean(cost_per_pound_adjusted, na.rm=TRUE),
    sd_pound_price=sd(cost_per_pound_adjusted, na.rm=TRUE)
  ) %>%
  mutate(
    p10_SST_scaled    = (p10_SST - mean_SST) / sd_SST,
    median_SST_scaled = (median_SST - mean_SST) / sd_SST,
    p90_SST_scaled    = (p90_SST - mean_SST) / sd_SST,
    
    p10_fuel_price_scaled    = (p10_fuel_price - mean_fuel_price) / sd_fuel_price,
    median_fuel_price_scaled = (median_fuel_price - mean_fuel_price) / sd_fuel_price,
    p90_fuel_price_scaled    = (p90_fuel_price - mean_fuel_price) / sd_fuel_price,
    
    p10_pound_price_scaled    = (p10_pound_price - mean_pound_price) / sd_pound_price,
    median_pound_price_scaled = (median_pound_price - mean_pound_price) / sd_pound_price,
    p90_pound_price_scaled    = (p90_pound_price - mean_pound_price) / sd_pound_price
  )
############  

############ 
#ex vessel spatial predictions

#get metiers specifically for SST
models_exvessel<-coef_table2 %>%
  filter(term == "cost_per_pound_adjusted_scaled") %>%
  pull(model) %>%
  unique()
#get specific models
models_subset_pa <- models_pa[names(models_pa) %in% models_exvessel]
models_subset_abund <- models_abun[names(models_abun) %in% models_exvessel]
############ 

############ 
#get all metiers for ex vessel
all_metiers <-# intersect(
  names(models_subset_abund)#,
# names(models_subset_pa)
#)

price_levels <- c("Low", "Average", "High")

out_dir <- paste0(base_path, "Modeling/spatial_outputs/exvessel/")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (met in all_metiers) {
  
  message("Processing: ", met)
  
  m_abun <- models_subset_abund[[met]]
  m_pa   <- models_subset_pa[[met]]
  #data the same between each model so just need to do this once
  data_df <- m_abun$data
  
  percentiles_metier <- percentiles_scaled %>%
    filter(metier == met)
  
  peak_month <- peak.months %>%
    filter(metier == met) %>%
    pull(month) %>%
    unique()
  
  valid_year <- data_df %>%
    mutate(
      year  = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m"))
    ) %>%
    filter(month %in% peak_month) %>%
    summarise(last_valid_year = max(year, na.rm = TRUE)) %>%
    pull(last_valid_year)
  
  # ---- base grid ----
  grid_base <- prediction_grid_sf2 %>%
    st_drop_geometry() %>%
    filter(metier == met) %>%
    arrange(date) %>%
    left_join(
      data_df %>%
        dplyr::select(date, time_index) %>%
        distinct(),
      by = "date"
    ) %>%
    mutate(
      block_long_dd = block_long_dd / 1000,
      block_lat_dd  = block_lat_dd  / 1000,
      cost_per_gal_adjusted_scaled   = percentiles_metier$median_fuel_price_scaled,
      SST_scaled = percentiles_metier$median_SST
    ) %>%
    filter(year ==  valid_year)
  
  # create low/avg/high versions
  grids <- list(
    Low  = grid_base %>% mutate(SST_scaled =percentiles_metier$median_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$p10_pound_price_scaled),
    Average   = grid_base %>% mutate(SST_scaled =percentiles_metier$median_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled),
    High = grid_base %>% mutate(SST_scaled = percentiles_metier $median_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$p90_pound_price_scaled)
  )
  
  pred_dfs <- list()
  
  #prediction loop
  
  for (price in price_levels) {
    
    g <- grids[[price]]
    
    # predictions
    p_pa   <- predict(m_pa,   newdata = g)
    p_abun <- predict(m_abun, newdata = g)
    
    pa_prob  <- m_pa$family$linkinv(p_pa$est)
    abun_exp <- m_abun$family$linkinv(p_abun$est)
    
    # delta expectation (manual formula)
    g$est_exp    <- pa_prob * abun_exp
    g$est_pa     <- pa_prob
    g$est_abun   <- abun_exp
    g$price      <- price
    
    pred_dfs[[price]] <- g
    
  }
  # combine predictions 
  pred_all <- bind_rows(pred_dfs) %>%
    left_join(all_blocks_sf, by = "block_id") %>%
    st_as_sf()
  
  # ---- SAVE COMBINED FILE ----
  st_write(pred_all,
           paste0(out_dir, met, "_exvessel_ALL.gpkg"),
           delete_layer = TRUE)
  
  # ---- SAVE SEPARATE FILES ----
  for (price in price_levels) {
    
    st_write(
      pred_all %>% filter(price == !!price),
      paste0(out_dir, met, "_exvessel_", toupper(price), ".gpkg"),
      delete_layer = TRUE
    )
  }
  
  #warm- average plot
  diff_df <- pred_all %>%
    st_drop_geometry() %>%
    dplyr::select(block_id, price, est_exp) %>%
    pivot_wider(names_from = price, values_from = est_exp) %>%
    mutate(diff_high_avg = High - Average) %>%
    left_join(all_blocks_sf, by = "block_id") %>%
    st_as_sf()
  
  st_write(diff_df,
           paste0(out_dir, met, "_exvessel_HIGH_minus_AVERAGE.gpkg"),
           delete_layer = TRUE)
  
  #low- average plot
  diff_df <- pred_all %>%
    st_drop_geometry() %>%
    dplyr::select(block_id, price, est_exp) %>%
    pivot_wider(names_from = price, values_from = est_exp) %>%
    mutate(diff_low_avg = Low - Average) %>%
    left_join(all_blocks_sf, by = "block_id") %>%
    st_as_sf()
  
  st_write(diff_df,
           paste0(out_dir, met, "_exvessel_LOW_minus_AVERAGE.gpkg"),
           delete_layer = TRUE)
  
  
  gc() 
  
}


############ 
#read back in predictions and plot kernel density contours for hot versus cold versus average years
#in addition, have center of gravities on plots 
# ---- READ IN SAVED SPATIAL PREDICTIONS ----

#create helper function to create contour
make_kde_contour <- function(pred_sf) {
  
  pred_sf <- pred_high %>%
    mutate(
      block_long_dd = block_long_dd * 1000,
      block_lat_dd  = block_lat_dd  * 1000
    ) %>%
    st_as_sf(coords = c("block_long_dd", "block_lat_dd"), crs = 3310) %>%
    as.data.frame()%>%
    st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = 3310)%>%
    #remove any bad spatial blocks
    filter(!st_is_empty(.))
  
  cutoff<- 0.99*sum(pred_sf$median)
  
  pred_trimmed <- pred_sf %>%
    arrange(desc(median)) %>%
    mutate(cum_effort = cumsum(median)) %>%
    filter(cum_effort <= cutoff | row_number() == which.max(cum_effort >= cutoff))
  
  coords <- st_coordinates(pred_trimmed)
  weights <- pred_trimmed$median
  

  # window for KDE
  minx <- min(coords[,1]) - 20000
  maxx <- max(coords[,1]) + 20000
  miny <- min(coords[,2]) - 20000
  maxy <- max(coords[,2]) + 20000
  
  points_ppp <- ppp(coords[,1], coords[,2],
                    window = owin(c(minx, maxx), c(miny, maxy)))
  
  sigma <- bw.diggle(points_ppp)
  
  density <- density.ppp(
    points_ppp,
    sigma = sigma,
    kernel = "gaussian",
    weights = weights,
    diggle = TRUE,
    scalekernel = TRUE
  )
  
  density_raster <- raster(as.im(density))
  density_masked <- mask(density_raster, usa_mexico_sf_proj, inverse = TRUE)
  
  # 75% contour
  density_df <- as.data.frame(rasterToPoints(density_masked))
  names(density_df) <- c("x", "y", "value")
  
  density_values <- sort(density_df$value, decreasing = TRUE)
  cdf <- cumsum(density_values) / sum(density_values)
  contour_level <- density_values[which.min(abs(cdf - 0.75))]
  
  contour_lines <- rasterToContour(density_masked, levels = contour_level)
  contour_sf <- st_as_sf(contour_lines)
  st_crs(contour_sf) <- 3310
  plot(contour_sf)
  
  return(contour_sf)
}

out_fig_dir <- paste0(base_path,
                      "Modeling/figures/kernel_contours/exvessel/")

for (met in all_metiers) {
  pred_cold <- st_read(paste0(base_path,
                              "Modeling/spatial_outputs/exvessel/", met,"_exvessel_LOW.gpkg"))
  
  pred_avg <- st_read(paste0(base_path,
                             "Modeling/spatial_outputs/exvessel/",met, "_exvessel_AVERAGE.gpkg"))
  
  pred_hot <- st_read(paste0(base_path,
                             "Modeling/spatial_outputs/exvessel/",met,"_exvessel_HIGH.gpkg"))
    "Los Angeles Urchin dive"
  
  
  #create contour for each level
  contour_cold <- make_kde_contour(pred_cold)
  contour_avg  <- make_kde_contour(pred_avg)
  contour_hot  <- make_kde_contour(pred_hot)
  
  contour_cold$temp <- "Low"
  contour_avg$temp  <- "Average"
  contour_hot$temp  <- "High"
  
  all_contours <- bind_rows(contour_cold,
                            contour_avg,
                            contour_hot)
  
  p<-ggplot() +
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", color = NA) +
    
    geom_sf(data = all_contours,
            aes(color = temp),
            linewidth = 0.5) +
    
    scale_color_manual(
      values = c("Low" = "blue",
                 "Average" = "black",
                 "High" = "red"),
      name = "Exvessel Scenario"
    ) +
    
    coord_sf(xlim = c(-450000, 350000),
             ylim = c(-600000, 450000)) +
    
    ggtitle(paste("75% KDE contours —", met)) +
    theme_bw() + base_theme 
  
  ggsave(
    filename = paste0(out_fig_dir, met, "_KDE_75pct_low_avg_high.jpg"),
    plot = p,
    width = 10, height = 7, dpi = 300
  )
  
}