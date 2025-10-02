library(sp)
library(sf)
library(spatstat)
library(raster)
library(sf)
library(terra)
library(tidyverse)
###############


############### Base spatial extent of grid
# Create spatial extent of prediction grid
x_min <- -469999
x_max <- 300000
y_min <- -605000
y_max <- 450200

# Make the evenly spaced (on UTM) grid 
pred_grid <- expand.grid(
  X = seq(x_min, x_max, by = 8000),
  Y = seq(y_min, y_max, by = 8000)) # 4x4 km

# Convert grid to sf
pred_grid_sf <- st_as_sf(pred_grid, coords = c("X", "Y"), crs = 3310)

#add in land polygon
land <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")%>%
  st_transform(crs = 3310)

# Keep only points NOT on land
pred_grid_ocean <- pred_grid_sf[lengths(st_intersects(pred_grid_sf, land)) == 0, ]


#look at both together to double check
ggplot() +
  geom_sf(data = land, fill = "tan", color = "black") +
  geom_sf(data = pred_grid_ocean, color = "blue", alpha = 0.3, size = 0.5) +
  theme_light()+
  coord_sf(xlim = c(-450000, 350000), ylim = c(-600000,450000))

# Convert to SpatialPoints for raster::extract
pred_grid_sp <- as(pred_grid_ocean, "Spatial")
###############


#Add time dimension
#Monthly sequence from Jan 2000 – Dec 2023
dates <- seq.Date(from = as.Date("2000-01-01"),
                  to   = as.Date("2023-12-01"),
                  by   = "month")

# Expand grid in space & time
pred_grid_time <- tidyr::crossing(
  pred_grid_ocean %>% mutate(cell_id = row_number()),
  tibble(date = dates)
)%>%
  mutate(year = year(date),
         month = month(date))%>%
  st_as_sf()


############### Add in dynamic variables (SST and BT)
#read in SST and BT rasters
sst_filled<-rast( "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_sst_GLORYS_raster.tif")
bt_filled<-rast( "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_bt_GLORYS_raster.tif")

# Convert prediction grid to SpatialPoints to use with raster::extract
coordinates(pred_grid) <- ~X+Y
proj4string(pred_grid) <- crs(sst_filled)  # Match CRS with raster

# Function to extract multi-layer raster to grid points
extract_raster_to_grid <- function(rast_obj, pred_points, var_prefix) {
  # rast_obj: SpatRaster (multi-layer)
  # pred_points: SpatialPoints of prediction grid
  # var_prefix: variable name for extracted values ("SST" or "BT")
  
  n_layers <- nlyr(rast_obj)  # Number of layers in raster
  rast_stack <- stack(rast_obj)  # Convert SpatRaster to RasterStack
  out_list <- vector("list", n_layers)  # Prepare list to store results
  time_vals <- terra::time(rast_obj)    # Get time values from raster
  
  # Loop over each layer/month
  for(i in 1:n_layers) {
    r <- rast_stack[[i]]  # Take single raster layer
    vals <- raster::extract(r, pred_points, method = "bilinear")  # Extract values
    df <- data.frame(
      cell_id = 1:length(vals),   # Unique ID for each point
      value = vals,               # Raster values
      date = time_vals[i]         # Date for this layer
    )
    out_list[[i]] <- df
  }
  
  # Combine all layers into one long-format data.frame
  out_df <- bind_rows(out_list) %>%
    rename(!!var_prefix := value)  # Rename value column to variable name
  return(out_df)
}

# Extract SST values 
sst_df <- extract_raster_to_grid(sst_filled, pred_grid_sp, "SST")

# Extract Bottom Temperature values 
bt_df <- extract_raster_to_grid(bt_filled, pred_grid_sp, "BT")

# Combine SST and BT into one dataset 
sst_bt_pred <- left_join(bt_df, sst_df, by = c("cell_id", "date"))

# Add geometry column 

# Number of points and layers
n_points <- nrow(pred_grid_sp)
n_layers <- nlyr(sst_filled)  # assume same as BT

# Repeat coordinates for each layer
coords_rep <- coords[rep(1:n_points, times = n_layers), ]

# Add geometry column correctly
sst_bt_pred <- sst_bt_pred %>%
  mutate(X = coords_rep[,1],
         Y = coords_rep[,2]) %>%
  st_as_sf(coords = c("X", "Y"), crs = crs(sst_filled))

#practice graphing 

# Pick the date you want
plot_date <- as.Date("2000-01-01")  # example

# Filter the data for that date
bt_slice <- sst_bt_pred %>% filter(date == plot_date)

# Plot
ggplot(bt_slice) +
  geom_sf(aes(color = BT), size = 1) +     # points colored by BT
  scale_color_viridis_c(option = "C") +    # nice continuous color scale
  theme_light() +
  labs(title = paste("Bottom Temperature:", plot_date),
       color = "BT (°C)")
#looks good!
###############


############### #Add in static variables distance to shore
#will need to add in distance to each port complex and then later during the modeling
#process will have to select the correct port complex
distance_raster<-rast(( 'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/distance_shoreline.tif'))
# Set values above the threshold to 0
distance_raster[distance_raster== 0] <- NA
#reproject to EPSG:3310
distance_raster <- project(distance_raster, "EPSG:3310")
#plotting raster by itself
plot(distance_raster)
#turner raster into dataframe for plotting
raster_df <- as.data.frame(distance_raster, xy = TRUE)
#check that the spatial extent of the distance rastera and blocks are the same
ggplot(all_blocks) +
       geom_sf() +
       geom_raster(data = raster_df, aes(x = x, y = y, fill = distance_shoreline))


# Extract values from raster onto the predicition grid
distance_vals <- raster::extract(distance_raster_stack, pred_grid_sp, method = "bilinear")

# Create a data.frame
distance_df <- data.frame(
  cell_id = 1:nrow(pred_grid_sp),  # must match your cell_id
  distance_to_shore = distance_vals
)

# Since sst_bt_pred is long-format (point × time), repeat distance values for each layer
n_layers <- length(unique(sst_bt_pred$date))
distance_rep <- distance_df$distance_to_shore[rep(1:nrow(distance_df), times = n_layers)]

sst_bt_pred$distance_to_shore <- distance_rep
############### 


###############  # add in distance to port complex

dist_complex<-readRDS( "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")



