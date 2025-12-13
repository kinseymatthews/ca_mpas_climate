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

#upload models
model_files <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models <- lapply(model_files, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models) <- gsub("^model_|\\_Depth_no_BT.rds$", "", basename(model_files))
names(models) <- gsub("_", " ", names(models))
# Example: look at the summary for San Francisco Squid Seine
#m<-(models[["Los_Angeles_CPFV"]])

#upload variable importance and edit to only include significant variables that are dynamic

coef_table<-readRDS(paste0(model_folder,'model_results_variable_importance.rds'))
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
  group_by(metier,month)%>%
  summarize(total.effort=sum(total.hours))%>%
  ungroup()%>%
  group_by(metier)%>%
  filter(total.effort==max(total.effort))
peak.months$metier <- sub("_", " ", peak.months$metier)
############

############
#edit prediction grid so it matches the peak months
prediction_grid_sf2<-prediction_grid_sf %>%
  semi_join(peak.months, by = c("month", "metier"))
############

        
############
# match orignal fisheries data with the significant coefficients so then we can calucate ther percentiles   

#getting models for predictions
coef_table2$model <- gsub("_", " ", coef_table2$model)   
        
models_predictions<-c(unique(coef_table2$model))
 #replace "_" with space for matching

#filter fisheries data to get models 
fisheries$metier<- gsub("_", " ", fisheries$metier)   

fisheries_to_model<-fisheries%>%
  #replace "_" with space for matching
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
#SST spatial predictions

#get metiers specifically for SST
models_SST<-coef_table2 %>%
  filter(term == "SST_scaled") %>%
  pull(model) %>%
  unique()
#get specific models
models_subset <- models[names(models) %in% models_SST]
############ 


############ 
#starting with one example where SST was an important variable for the model
met <- "Santa Barbara Lobster trap"
m <- models[[met]]
data_df<-m$data

# extract percentiles for this metier
percentiles_metier <- percentiles_scaled %>% filter(metier == met)

# base prediction grid 
grid_base <- prediction_grid_sf2 %>%
  st_drop_geometry() %>%
  filter(metier == met) %>%
  arrange(date)%>%
  #matching up the correct time_indexes from the real data to the prediction grid
  left_join(
    (data_df %>% 
      dplyr::select(date, time_index) %>% 
      distinct(date, .keep_all = TRUE)),   # keep one unique row per date
    by = "date"
  ) %>%
  mutate(
    #had to divide long and lat by 1000 before creating model so doing it here as well
    block_long_dd = block_long_dd / 1000,
    block_lat_dd  = block_lat_dd / 1000,
    #setting these other dynamic variables to the median
    cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled,
    cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled
  )%>%
 filter(year==2022)

# create cold/avg/warm versions
grids <- list(
  cold  = grid_base %>% mutate(SST_scaled =percentiles_metier$p10_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled),
  avg   = grid_base %>% mutate(SST_scaled =percentiles_metier$median_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled),
  warm  = grid_base %>% mutate(SST_scaled = percentiles_metier $p90_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled)
)

# predict for cold and warm year
pred_test_cold <- predict(m2, newdata = grids$cold)
pred_test_warm <- predict(m2, newdata = grids$warm)

#joining with spatial data 
predictions_sf_cold <- left_join(pred_test_cold,all_blocks_sf, by = "block_id")%>%
  st_as_sf()
predictions_sf_warm <- left_join(pred_test_warm,all_blocks_sf, by = "block_id")%>%
  st_as_sf()

#pred_cold_avg <- pred_test_cold %>%
# group_by(block_id) %>%
# summarize(pred_mean = mean(exp(est)))   # back-transformed

#pred_warm_avg <- pred_test_warm %>%
#  group_by(block_id) %>%
#  summarize(pred_mean = mean(exp(est)))   # back-transformed

#pred_cold_sf <- left_join(all_blocks_sf, pred_cold_avg, by="block_id")


#pred_warm_sf <- left_join(all_blocks_sf, pred_warm_avg, by="block_id")


ggplot(predictions_sf_cold) +
  geom_sf(aes(fill = exp(est)), color = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Cold-year Average Prediction") +
  theme_minimal()

ggplot(predictions_sf_warm) +
  geom_sf(aes(fill = exp(est)), color = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Warm-year Average Prediction") +
  theme_minimal()


plot_map <- function(dat, column) {
  ggplot(dat) +
    geom_sf(aes(fill = {{ column }}), color = NA) +  # NA removes polygon borders
    coord_sf() +
    scale_fill_viridis_c(trans = "sqrt") +
    theme_minimal()
}

plot_map(predictions_sf_cold, exp(est_non_rf)) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Prediction (fixed effects only)")


plot_map(predictions_sf_cold, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")



################
#############
#kernel densities


predictions_sf_warm<-predictions_sf_warm%>%
  as.data.frame()%>%
  mutate(
    block_long_dd=block_long_dd*1000,
    block_lat_dd=block_lat_dd*1000
  )%>%
  st_as_sf(coords=c("block_long_dd", "block_lat_dd"))%>%
  #remove any bad spatial blocks
  filter(!st_is_empty(.))

predictions_sf_warm <- st_set_crs(predictions_sf_warm, 3110)



# Coordinates
coords <- st_coordinates(predictions_sf_warm)

# Weights = predicted effort
weights <- exp(predictions_sf_warm$est)
# Bounding box with a small buffer
minx <- min(coords[,1]) - 20000
maxx <- max(coords[,1]) + 20000
miny <- min(coords[,2]) - 20000
maxy <- max(coords[,2]) + 20000

# ppp object
points_ppp <- ppp(coords[,1], coords[,2],
                  window = owin(c(minx, maxx), c(miny, maxy)))

# Bandwidth
sigma <- bw.diggle(points_ppp)

# Kernel density estimation
density <- density.ppp(points_ppp, sigma = sigma, kernel = "gaussian",
                       weights = weights, diggle = TRUE, scalekernel = TRUE)


# Convert to raster
density_raster <- raster(as.im(density))

# Mask out land (assuming usa_mexico_sf_proj is projected)
density_masked <- mask(density_raster, usa_mexico_sf_proj, inverse = TRUE)

# Convert to data frame for plotting
density_df <- as.data.frame(rasterToPoints(density_masked))
names(density_df) <- c("x", "y", "value")

# Sort density values
density_values <- sort(density_df$value, decreasing = TRUE)

# Cumulative distribution function
cdf <- cumsum(density_values) / sum(density_values)

# Level corresponding to 75% volume
contour_level <- density_values[which.min(abs(cdf - 0.75))]

# Create contour lines
contour_lines <- rasterToContour(density_masked, levels = contour_level)
contour_sf <- st_as_sf(contour_lines)
st_crs(contour_sf) <- st_crs(predictions_sf_warm)

ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = contour_sf, color = "black", linewidth = 1) +
  #geom_sf(data = usa_mexico_sf_proj, fill = "grey85") +
  scale_fill_viridis_c(trans = "sqrt") +
  coord_sf() +
  ggtitle("Cold-year Predicted Effort KDE with 75% Volume Contour") +
  theme_minimal()
