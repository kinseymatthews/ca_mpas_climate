library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(INLA)
library(broom)
library(rnaturalearth)
library(spatstat)
############
#R code for testing model that has an interaction between temperature and distance to shore
#to see if it leads to different spatial configurations

#Upload Data
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
# Make folder for saving models
model_folder <- paste0(base_path, "Modeling/Data/Processed Data/Model_Results/")

fisheries_variables<-readRDS(paste0(base_path,"Modeling/Data/Processed Data/fisheries_df_with_variables.rds"))

############
#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct", "Avg_Depthm")

fisheries_variables <- fisheries_variables%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
#shore data for mesh
ca <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")%>%
  filter(iso_3166_2=="US-CA" )%>%
  st_transform(crs = 3310)

#shore data for mesh
# US West Coast states
west_coast_us <- ne_states(
  country = "United States of America", 
  returnclass = "sf"
) %>%
  filter(iso_3166_2 %in% c("US-CA", "US-OR", "US-WA", "US-NV"))
#Mexican Baja states
baja <- ne_states(
  country = "Mexico", 
  returnclass = "sf"
) %>%
  filter(iso_3166_2 %in% c("MX-BCN", "MX-BCS"))

# Combine and transform
west_coast <- bind_rows(west_coast_us, baja) %>%
  st_transform(crs = 3310)

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



#read in prediction grid
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

############

############

# just going to look at SB lobster trap
i<-"Santa Barbara_Lobster trap"

  message("Fitting model for: ", i)
  na_var <- c("SST_scaled")
  
  test <- fisheries_variables %>%
    filter(metier ==i)%>%
    filter(if_all(all_of( na_var), ~ !is.na(.) & is.finite(.))) %>%
    arrange(date) 
  
  test2 <- test %>%
    filter(total.hours != 0)%>%
    mutate(time_index = match(date, sort(unique(date))))
  
  dates<-unique(test2$month_year)
  
  
  test <- test%>%
    filter(month_year %in% dates)%>%
    mutate(time_index = match(date, sort(unique(date))))
  
  absence_formula <- present ~
    cost_per_gal_adjusted_scaled +
    cost_per_pound_adjusted_scaled +
    distance_complex_km_scaled +
    Avg_Depthm_scaled +
    closed_pct_scaled +
    fyear +
    sin12 +
    cos12 +
    SST_scaled * distance_to_shore_km_scaled
  
  abundance_formula <- total.hours ~
    cost_per_gal_adjusted_scaled +
    cost_per_pound_adjusted_scaled +
    distance_complex_km_scaled +
    Avg_Depthm_scaled +
    closed_pct_scaled +
    fyear +
    sin12 +
    cos12 +
    SST_scaled * distance_to_shore_km_scaled
  
  # Create mesh = always has to go right before building the model!
  mesh <- make_mesh(test, xy_cols = c("block_long_dd","block_lat_dd"), cutoff = 10)
  mesh1 <- sdmTMBextra::add_barrier_mesh(mesh, 
                                         west_coast ,
                                         range_fraction = 0.02, proj_scaling = 1000, plot = TRUE)
  
  
  # Fit presnece/absence model
  set.seed(123)
  model1 <-  sdmTMB(
    data = test,
    formula = absence_formula,
    mesh = mesh1,
    family = binomial(link = "logit"),
    spatial = "off",
    spatiotemporal = "off"
  )
  
  test2 <- test %>%
    filter(total.hours != 0)
  
  mesh2 <- make_mesh(test2,
                     xy_cols = c("block_long_dd", "block_lat_dd"),
                     mesh = mesh1$mesh
  )
  
  model2 <- try(
    sdmTMB(
      data = test2,
      formula =  abundance_formula,
      mesh = mesh2,
      family = Gamma(link = "log"),
      spatial = "on",
      time = "time_index",
      spatiotemporal = "ar1"
    ),
    silent = TRUE
  )
  
  
  # Print summary and sanity
  print(summary(model1))
  print(sanity(model1))
  
  print(summary(model2))
  print(sanity(model2))
  
  # Save model
  saveRDS(model1, file = paste0(model_folder, "model_", gsub(" ","_", i), "_Depth_no_BT_pa_delta_test_interaction.rds"))
  saveRDS(model2, file = paste0(model_folder, "model_", gsub(" ","_", i), "_Depth_no_BT_abundance_delta_test_interaction.rds"))

############

  resids <- residuals(model1, type = "mle-mvn")
  resids <- resids[is.finite(resids)]
  

  qqnorm(resids, main = paste("QQ Plot of Residuals -", model1))
  qqline(resids, col = "red")

  ############
  #find peak month for each fishery
  peak.months=fisheries_variables%>%
    mutate(year = year(date)) %>%                           
    filter(year >= max(year, na.rm = TRUE) - 9) %>%          # last 10 years
    group_by(metier,month)%>%
    summarize(total.effort=sum(total.hours, na.rm = TRUE))%>%
    ungroup()%>%
    group_by(metier)%>%
    filter(total.effort==max(total.effort, na.rm = TRUE))%>%
    ungroup()

  ############
  
  ############
  #edit prediction grid so it matches the peak months
  prediction_grid_sf2<-prediction_grid_sf %>%
    semi_join(peak.months, by = c("month", "metier"))
  ############
  
  
  ############
  # match orignal fisheries data with the significant coefficients so then we can calucate ther percentiles   
  
  #getting models for predictions


  #filter fisheries data to get models 
  fisheries_to_model<-fisheries_variables%>%
    filter(metier =="Santa Barbara_Lobster trap")%>%
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
  
  met<-"Santa Barbara_Lobster trap"
    
    message("Processing: ", met)
    

    #data the same between each model so just need to do this once
    data_df <- model2$data
    
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
        cost_per_pound_adjusted_scaled = percentiles_metier$median_pound_price_scaled
      ) %>%
      filter(year ==  valid_year)
    
    # create cold/avg/warm versions
    grids <- list(
      Cold  = grid_base %>% mutate(SST_scaled =percentiles_metier$p10_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled),
      Average   = grid_base %>% mutate(SST_scaled =percentiles_metier$median_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled),
      Hot = grid_base %>% mutate(SST_scaled = percentiles_metier $p90_SST_scaled, cost_per_gal_adjusted_scaled=percentiles_metier$median_fuel_price_scaled, cost_per_pound_adjusted_scaled=percentiles_metier$median_pound_price_scaled)
    )
    
    pred_dfs <- list()
    
    #prediction loop
    temp_levels <- c("Cold", "Average", "Hot")
    
    for (temp in temp_levels) {
      
      g <- grids[[temp]]
      
      # predictions
      p_pa   <- predict(model1, newdata = g)
      p_abun <- predict(model2, newdata = g)
      
      pa_prob  <- (model1$family$linkinv(p_pa$est))
      abun_exp <- (model2$family$linkinv(p_abun$est))
      
      # delta expectation (manual formula)
      g$est_exp    <- pa_prob * abun_exp
      g$est_pa     <- pa_prob
      g$est_abun   <- abun_exp
      g$temp   <- temp
      
      pred_dfs[[temp]] <- g
      
    }
    
    
    # combine predictions 
    pred_all <- bind_rows(pred_dfs) %>%
      left_join(all_blocks_sf, by = "block_id") %>%
      st_as_sf()
    
    
    # ---- SAVE COMBINED FILE ----
  #  st_write(pred_all,
             paste0(out_dir, met, "_SST_ALL.gpkg"),
             delete_layer = TRUE)
    
    # ensure correct order
    pred_all$temp <- factor(pred_all$temp,
                            levels = c("Cold", "Average", "Hot"))
  
    ggplot(pred_all) +
      geom_sf(aes(fill = est_exp), color = NA) +
      scale_fill_viridis_c(trans = "sqrt",
                           name = "Predicted effort (sqrt)") +
      facet_wrap(~ temp) +
      #add black outline around density raster
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2))  +
      geom_sf(data = usa_mexico_sf_proj,
              fill = "grey85",
              col = "white",
              linewidth = 0.2,
              inherit.aes = FALSE) +
      geom_sf(data = state_waters,
              color = "grey30",
              linewidth = 0.2,
              inherit.aes = FALSE) +
      coord_sf(xlim = c(-450000, 350000),
               ylim = c(-600000, 450000)) +
      theme_bw() + 
      ggtitle(met)
    
    diff_cold <- st_read(
      paste0(in_dir, met, "_SST_COLD_minus_AVERAGE.gpkg"),
      quiet = TRUE
    )%>%
      mutate(
        comparison = "Cold - Average",
        diff = diff_cold_avg 
      )
    
    pred_diff <- pred_all %>%
    
      select(block_id, temp, est_exp) %>%
      pivot_wider(names_from = temp, values_from = est_exp) %>%
      mutate(
        `Cold-Average` = Cold - Average,
        `Hot-Average`  = Hot - Average
      ) %>%
      select(block_id, `Cold-Average`, `Hot-Average`) %>%
      pivot_longer(
        cols = c(`Cold-Average`, `Hot-Average`),
        names_to = "comparison",
        values_to = "diff"
      )

    
    coordinates<-pred_diff %>%
      
      st_coordinates()
    # Create bounding block for plot
    minx<-(min(coordinates[,1], na.rm = TRUE))
    maxx<-(max(coordinates[,1], na.rm = TRUE))
    miny<-(min(coordinates[,2], na.rm = TRUE))
    maxy<-(max(coordinates[,2], na.rm = TRUE))
    
ggplot( pred_diff) +
      geom_sf(aes(fill = diff), color = NA) +
      facet_wrap(~ comparison, ncol = 2) +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        # limits = c(-max_abs, max_abs),
        name = "Predicted difference"
      ) +
      guides(fill = guide_colorbar(
        ticks.colour = "black",
        frame.colour = "black",
        frame.linewidth = 0.2
      )) +
      geom_sf(data = usa_mexico_sf_proj,
              fill = "grey85",
              col = "white",
              linewidth = 0.2,
              inherit.aes = FALSE) +
      geom_sf(data = state_waters,
              color = "grey30",
              linewidth = 0.2,
              inherit.aes = FALSE) +
      # Crop the plot using the bounding box of the contour
      coord_sf(xlim = c(minx-1000, maxx+1000), ylim = c(miny-1000, maxy+1000)) +
      theme_bw()



#upload entire dataset
fisheries_full<-readRDS(paste0(base_path,"Modeling/Data/Processed Data/fisheries_df_with_variables.rds"))
fisheries_reduced<-fisheries_full%>%
  dplyr::select(block_id, fishery, port.complex, date, year, month,total.hours, present,month_year)%>%
  mutate(metier=paste(port.complex,fishery, sep=" "))



  
  month_year_met<-unique(pred_all$month_year)
  
  fisheries_reduced_met<-fisheries_reduced%>%
    filter(metier=="Santa Barbara Lobster trap"  & month_year==month_year_met)%>%# )present=="1"
    mutate(block_id=as.factor(block_id))
  
  temps<-unique(pred_all$temp)
  
  prediction_list <- list()
  
  for (temp_met in temps) {
    prediction<-pred_all%>%
      filter(temp==temp_met)%>%
      mutate(block_id=as.factor(block_id))%>%
      left_join(fisheries_reduced_met, by=c("block_id","fishery","port.complex","date","year"))
    
    prediction <- prediction %>%
      mutate(prob_pa = plogis(est_pa))
    
    roc <- pROC::roc(prediction$present, (prediction$prob_pa))
    plot(roc, print.auc = TRUE, col = "blue", legacy.axes = TRUE)
    auc <- pROC::auc(roc)
    auc
    #has Hot auc probably because it's saying everything is 0 but then only a few are 1 so only misclassifying a few
    opt <- pROC::coords(roc,
                        x = "best",
                        best.method = "closest.topleft",
                        ret = c("threshold", "sensitivity", "specificity", "accuracy"))
    
    opt
    threshold<-opt$threshold
    # in this example, threshold is 0.5185789, so, if remove all blocks with the probability of presence beCold this threshold value
    prediction_filtered<-prediction%>%
      filter(prob_pa>=threshold)
    
    prediction_list[[temp_met]] <- prediction_filtered
  }
  ############
  #add them all together
  df <- dplyr::bind_rows(prediction_list)
  
  make_kde_contour <- function(pred_sf) {
    
    pred_sf <- pred_sf %>%
      mutate(
        block_long_dd = block_long_dd * 1000,
        block_lat_dd  = block_lat_dd  * 1000
      ) %>%
      st_drop_geometry%>%
      st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = 3310)%>%
      #remove any bad spatial blocks
      filter(!st_is_empty(.))
    
    
    coords <- st_coordinates(pred_sf)
    weights <- pred_sf$est_exp
    
    # window for KDE
    minx <- min(coords[,1]) - 20000
    maxx <- max(coords[,1]) + 20000
    miny <- min(coords[,2]) - 20000
    maxy <- max(coords[,2]) + 20000
    
    points_ppp <- ppp(coords[,1], coords[,2],
                      window = owin(c(minx, maxx), c(miny, maxy)))
    
    sigma <- bw.diggle(points_ppp, hmax = 10000)
    
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
    
    return(contour_sf)
  }
  
  pred_hot <- df%>%
    filter(temp=="Hot")
  
  pred_cold<- df%>%
    filter(temp=="Cold")
  
  pred_avg <- df%>%
    filter(temp=="Average")
  
  
  #create contour for each level
  contour_cold <- make_kde_contour(pred_cold)
  contour_avg  <- make_kde_contour(pred_avg)
  contour_Hot  <- make_kde_contour(pred_hot)
  
  contour_cold$temp <- "Cold"
  contour_avg$temp  <- "Average"
  contour_Hot$temp  <- "Hot"
  
  all_contours <- bind_rows(contour_cold,
                            contour_avg,
                            contour_Hot)
  
  p<-ggplot() +
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", color = NA) +
    
    geom_sf(data = all_contours,
            aes(color = temp),
            linewidth = 0.5) +
    
    scale_color_manual(
      values = c("Cold" = "blue",
                 "Average" = "black",
                 "Hot" = "red"),
      name = "SST Scenario"
    ) +
    
    coord_sf(xlim = c(-450000, 350000),
             ylim = c(-600000, 450000)) +
    
    ggtitle(paste("75% KDE contours —", met)) +
    theme_bw()
  p
  
  ggsave(
    filename = paste0(out_fig_dir, met, "_KDE_Cold_avg_Hot.jpg"),
    plot = p,
    width = 10, height = 7, dpi = 300
  )
  
  dev.off()
}

