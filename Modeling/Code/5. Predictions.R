library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(INLA)
library(broom)
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
  rename(block_long_dd=blck_ln_, block_lat_dd=blck_lt_ )#rename to be same columns as mesh X and Y 
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
# match orignal fisheries data with the significant coefficients so then we can calucate ther percentiles   

#getting models for predictions
coef_table2$model <- gsub("_", " ", coef_table2$model)   
        
models_predictions<-c(unique(coef_table2$model))
 #replace "_" with space for matching

#filter fisheries data to get models 
fisheries$metier<- gsub("_", " ", fisheries$metier)   

fisheries_to_model<-fisheries%>%
  #replace "_" with space for matching
      filter(metier %in% models_predictions)
         
############     
############   
#get Median, Average, 90th percentile, 10th percentile ("cost_per_gal_adjusted", "cost_per_pound_adjusted", "SST")

percentiles<- fisheries_to_model%>%
  group_by(metier)%>%
  summarize(mean_SST=mean(SST_scaled, na.rm = TRUE),
            median_SST=median(SST_scaled, na.rm = TRUE),
            p10_SST = quantile(SST_scaled, 0.10, na.rm = TRUE),
            p90_SST = quantile(SST_scaled, 0.90, na.rm = TRUE),
            
            mean_fuel_price=mean(cost_per_gal_adjusted_scaled, na.rm = TRUE),
            median_fuel_price=median(cost_per_gal_adjusted_scaled, na.rm = TRUE),
            p10_fuel_price = quantile(cost_per_gal_adjusted_scaled, 0.10, na.rm = TRUE),
            p90_fuel_price = quantile(cost_per_gal_adjusted_scaled, 0.90, na.rm = TRUE),
           
             mean_pound_price=mean(cost_per_pound_adjusted_scaled, na.rm = TRUE),
            median_pound_price=median(cost_per_pound_adjusted_scaled, na.rm = TRUE),
            p10_pound_price = quantile(cost_per_pound_adjusted_scaled, 0.10, na.rm = TRUE),
            p90_pound_price = quantile(cost_per_pound_adjusted_scaled, 0.90, na.rm = TRUE)
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

met <- "Santa Barbara Lobster trap"
m2 <- models[[met]]
data_df<-m2$data
# extract percentiles for this metier
percentiles_metier <- percentiles %>% filter(metier == met)


# base prediction grid 
grid_base <- prediction_grid_sf %>%
  st_drop_geometry() %>%
  filter(metier == met,
         month_year %in% unique(m2$data$month_year)) %>%
  arrange(date)

grid_base$time_index <- match(grid_base$date,
                              sort(unique(grid_base$date)))
grid_base$time_index <- as.integer(grid_base$time_index)

grid_base$year <- year(ymd(grid_base$date))
grid_base$fyear <- as.factor(grid_base$year)

grid_base$month <- month(ymd(grid_base$date))

grid_base$sin12 <- sin(2 * pi * grid_base$month / 12)
grid_base$cos12 <- cos(2 * pi * grid_base$month / 12)
grid_base<- grid_base %>%
  mutate(
    block_long_dd = block_long_dd / 1000,
    block_lat_dd  = block_lat_dd / 1000
  )

# create cold/avg/warm versions
grids <- list(
  cold  = grid_base %>% mutate(SST_scaled =percentiles_metier$p10_SST, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price),
  avg   = grid_base %>% mutate(SST_scaled =percentiles_metier$median_SST, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price),
  warm  = grid_base %>% mutate(SST_scaled = percentiles_metier $p90_SST, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price)
)
# predict one scenario first to test
pred_test_cold <- predict(m2, newdata = grids$cold)
pred_test_warm <- predict(m2, newdata = grids$warm)


predictions_sf_cold <- left_join(pred_test_cold,all_blocks_sf, by = "block_id")%>%
  st_as_sf()

pred_cold_avg <- pred_test_cold %>%
  group_by(block_id) %>%
  summarize(pred_mean = mean(exp(est)))   # back-transformed

pred_warm_avg <- pred_test_warm %>%
  group_by(block_id) %>%
  summarize(pred_mean = mean(exp(est)))   # back-transformed

pred_cold_sf <- left_join(all_blocks_sf, pred_cold_avg, by="block_id")


pred_warm_sf <- left_join(all_blocks_sf, pred_warm_avg, by="block_id")


ggplot(pred_cold_sf) +
  geom_sf(aes(fill = pred_mean), color = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Cold-year Average Prediction") +
  theme_minimal()

ggplot(pred_warm_sf) +
  geom_sf(aes(fill = pred_mean), color = NA) +
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
