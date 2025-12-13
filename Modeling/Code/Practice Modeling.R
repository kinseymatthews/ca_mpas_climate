
library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(INLA)
library(rnaturalearth)
 ############
#upload fisheries information
fisheries<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/summarized_fishery_data_ready4model.rds")%>%
  filter(year>1999)%>%
  mutate(date = my(month_year, tz = "UTC"))
#upload management regulations
regulations<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/monthly_percent_closed_blocks.rds")
regulations$month_year <- gsub("-", "_", regulations$month_year)
regulations$month_year <- sub("^0+", "", regulations$month_year)
regulations$fishery <- gsub("\\.", " ", regulations$fishery)

#upload SST and BT
temp<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_sst_bt_GLORYS.rds")
#upload fuel prices
fuel<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Fuel price/data/processed/allportcomplexes_dieselprice_monthly_adjusted.rds")
#upload ex vessel price
exvessel<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/allfisheries_exvessel_port_year_month_adjusted.rds")%>%
  dplyr::select( port.complex, month_year,fishery, year, cost_per_pound_adjusted)%>%
  mutate(year=as.double(year))%>%
  mutate(fishery=case_when(fishery == "Halibut trawl" ~ "California halibut trawl",
    TRUE ~ fishery))
#upload distance to shore
dist_shore<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to shore/data/processed/block_shore_distances_complete.rds")
#upload distance to port complex
dist_complex<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")%>%
  rename(port.complex=port_complex, distance_complex_km=average.distance.km)%>%
  dplyr::select(-average.distance.m)

#upload block info
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#upload block information
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
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

bathy.data<-read.csv(paste0(general_data, "fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

##### merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")

all_blocks_sf<-all_blocks%>%
  dplyr::select(-c(blck_st, blck_ty, blck_sq, block_lat_dd, block_long_dd))

all_blocks<-all_blocks%>%
  dplyr::select(-c(blck_st, blck_ty, blck_sq, geometry))%>%
  st_drop_geometry()

#shore data for mesh
# US West Coast states
west_coast_us <- ne_states(
  country = "United States of America", 
  returnclass = "sf"
) %>%
  filter(iso_3166_2 %in% c("US-CA", "US-OR", "US-WA", "US-NV"))

# Mexican Baja states
baja <- ne_states(
  country = "Mexico", 
  returnclass = "sf"
) %>%
  filter(iso_3166_2 %in% c("MX-BCN", "MX-BCS"))

# Combine and transform
west_coast <- bind_rows(west_coast_us, baja) %>%
  st_transform(crs = 3310)


#prediction grid 
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

############

############
#expand effort data to go over each combo of fishery, port complex, month_year, and block id

#would need to elimate blocks that are entirely closed off to the fishery. 

#across all years, get all block IDs that are fished by a fishery, and use those as potential blocks/
#elimate any that are 100% closed to fishery. Then add in that effort is zero. OR should i do blockls that a port complex goes?
# 1. Get observed combinations
observed <- fisheries %>%
  dplyr::select(month_year, port.complex, fishery, block_id,year)

#find blocks in each fishery
fished_blocks <- fisheries %>%
  distinct(fishery, block_id)

#create dataframe with every combo
# Get unique values
months <- unique(fisheries$month_year)
ports  <- unique(fisheries$port.complex)


# Step 3: Expand each fishery to all ports and months (but only fished blocks!)
zero_grid <- fished_blocks %>%
  crossing(
    month_year = months,
    port.complex = ports
  )%>%
  mutate(total.hours=0, year = as.double(sub(".*_(\\d{4})", "\\1", month_year)))

# 4. Remove combinations that already exist in the data
zero_grid <- anti_join(zero_grid, observed,
                       by = c("month_year", "port.complex", "fishery", "block_id", "year"))

#combine with fisheries
fisheries_augmented <- bind_rows(fisheries, zero_grid)

############

############
#create full dataframe by adding explanatory variables 
fisheries_variables<-fisheries_augmented %>%
  mutate(date = my(month_year, tz = "UTC"))%>%
  left_join(fuel, by=c('port.complex','month_year','date'))%>%
  left_join(exvessel, by=c('port.complex','month_year','fishery','year'))%>%
  left_join(dist_shore, by=c('block_id'))%>%
  left_join(dist_complex, by=c('port.complex','block_id'))%>%
  left_join(temp, by=c('date','block_id'))%>%
  left_join(regulations, by=c('fishery','block_id','month_year'))%>%
  #add in XY cordinates of block
  left_join(all_blocks, by=c('block_id'))%>% 
#remove bad entries
  filter(!is.na(block_long_dd), !is.na(block_lat_dd))%>%
#remove 100% closed blocks with 0 fishing hours -> removing dummy blocks that we created where fishing can't occur.
  #also got rid of ~2 entries where there was logbook data - assuming they wrote the block number down incorrectly
filter(closed_pct!=1)%>%
#remove those of not CPFV with the exvessel price of NA - only had landings data to mid 2023
  filter(fishery == "CPFV" | !is.na(cost_per_pound_adjusted))%>%
  #remove block_id 101 which is inshore
  filter(block_id!="101") %>% 
  mutate(year = as.integer(year),
         metier=paste(port.complex, fishery, sep = "_"))


#make coordinates numeric
fisheries_variables$block_long_dd <- as.numeric(fisheries_variables$block_long_dd)
fisheries_variables$block_lat_dd  <- as.numeric(fisheries_variables$block_lat_dd)
#adds month for 0 variables
fisheries_variables$month <- format(fisheries_variables$date, "%m")

############

############
#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct", "Avg_Depthm")

fisheries_variables <- fisheries_variables%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))

fisheries_variables$fyear <- as.factor(fisheries_variables$year)
fisheries_variables$fmonth_year <- as.factor(fisheries_variables$month_year)
############

############
# Plot correlation between variables
test_cor <- fisheries_variables %>% dplyr::select("cost_per_gal_adjusted", ,
                                   "cost_per_pound_adjusted", ,
                                   "distance_to_shore_km", , 
                                   "distance_complex_km", , 
                                   "BT",  
                                   "SST", 
                                   "closed_pct","Avg_Depthm" )

corr <- round(cor(test_cor, use = "pairwise.complete.obs"), 1)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 2.5) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))
############





############
 ## ------------------------
 ## Editing DF
 ## ------------------------

fisheries_variables<-fisheries_variables%>%
  mutate(closed_pct=closed_pct*100)%>%
  #filter(total.hours!=0)%>%
  mutate(month=as.integer(month))%>%
  mutate(sin12 = (sin(2 * pi * month / 12)),
         cos12  = (cos(2 * pi * month / 12))
  )
#adding in variables that will represent the cyclical nature of months as putting
#month is a smooth term with cubic spline did not work
############


############

#practice - one fishery
test<-fisheries_variables%>%
  filter(metier=="Santa Barbara_Lobster trap")%>%
  arrange(date) %>%
  mutate(time_index = match(date, sort(unique(date))))%>%
  mutate(present=case_when(
    total.hours>0 ~ 1,
    total.hours==0 ~0
  ))

smooth_vars <- c("BT_scaled", "SST_scaled")
test <- test %>%
  filter(if_all(all_of(smooth_vars), ~ !is.na(.) & is.finite(.)))

#create mesh and barrier mesh
mesh <- make_mesh(
  test,
  c("block_long_dd", "block_lat_dd"),
  cutoff = 10
)

bspde <- sdmTMBextra::add_barrier_mesh(
  mesh, west_coast,
  range_fraction = 0.02,
  proj_scaling = 1000, plot = TRUE
)

m <- sdmTMB(
  data = test,
  formula = total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled+
    distance_to_shore_km_scaled + distance_complex_km_scaled +Avg_Depthm_scaled+
    SST_scaled+closed_pct_scaled  + fyear + sin12 + cos12,
  mesh = bspde,
  family = delta_gamma(),
  spatial = "on",
  time = "time_index",
  spatiotemporal = "ar1"
)

m2 <- sdmTMB(
  data = test,
  formula = total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled+
    distance_to_shore_km_scaled + distance_complex_km_scaled +Avg_Depthm_scaled+
    SST_scaled+closed_pct_scaled + fyear + sin12 + cos12,
  mesh = bspde,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "time_index",
  spatiotemporal = "ar1"
)

m3 <- sdmTMB(
  data = test,
  formula = total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled+
    distance_to_shore_km_scaled + distance_complex_km_scaled +Avg_Depthm_scaled+
    SST_scaled+closed_pct_scaled + fyear + sin12 + cos12,
  mesh = bspde,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "time_index",
  spatiotemporal = "iid"
)
AIC(m2,m3)

#delta model two ways
mesh1 <- make_mesh(
  test,
  c("block_long_dd", "block_lat_dd"),
  cutoff = 10
)
bspde <- sdmTMBextra::add_barrier_mesh(
  mesh1, west_coast,
  range_fraction = 0.02,
  proj_scaling = 1000, plot = TRUE
)
test2 <- subset(test, total.hours > 0)

mesh2 <- make_mesh(test2,
                   xy_cols = c("block_long_dd", "block_lat_dd"),
                   mesh = bspde$mesh
)


#fit presence absence data
m1 <- sdmTMB(
  data = test,
  formula = present ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled+
    distance_to_shore_km_scaled + distance_complex_km_scaled +Avg_Depthm_scaled+
    SST_scaled+closed_pct_scaled + fyear + sin12 + cos12,
  mesh = bspde,
  family = binomial(link = "logit"),
  spatial = "off",
#  time = "time_index",
  spatiotemporal = "off"
)
#fit non presence basnece data
m2 <- sdmTMB(
  data = test2,
  formula = total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled+
    distance_to_shore_km_scaled + distance_complex_km_scaled +Avg_Depthm_scaled+
    SST_scaled+closed_pct_scaled + fyear + sin12 + cos12,
  mesh = mesh2,
  family = Gamma(link = "log"),
  spatial = "on",
  time = "time_index",
  spatiotemporal = "ar1"
)
m1
sanity(m1)
m2
sanity(m2)

pred_test_cold_1 <- predict(m1, newdata = grids$cold)
pred_test_cold_2 <- predict(m2, newdata = grids$cold)
predictions_sf_cold_1 <- left_join(pred_test_cold_1,all_blocks_sf, by = "block_id")%>%
  st_as_sf()
predictions_sf_cold_2 <- left_join(pred_test_cold_2,all_blocks_sf, by = "block_id")%>%
  st_as_sf()

ggplot(predictions_sf_cold_1) +
  geom_sf(aes(fill = exp(est)), color = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Cold-year Average Prediction") +
  theme_minimal()

ggplot(predictions_sf_cold_2) +
  geom_sf(aes(fill = exp(est)), color = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Warm-year Average Prediction") +
  theme_minimal()
pred<-grids$cold
p_bin_prob <- m1$family$linkinv(pred_test_cold_1$est)
p_pos_exp <- m2$family$linkinv(pred_test_cold_2$est)
pred$est_exp <- p_bin_prob * p_pos_exp

pred_sf <- left_join(pred,all_blocks_sf, by = "block_id")%>%
  st_as_sf()

ggplot(pred_sf) +
  geom_sf(aes(fill = (est_exp)), color = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Cold-year Average Prediction") +
  theme_minimal()
############

############
#function to go through different spatiotemporal arguments - iid, ar1, rw
# list of unique metiers
metiers <- unique(fisheries_variables$metier)

# initialize results dataframe
aic_results <- data.frame(
  metier = character(),
  model = character(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)

for (met in metiers) {
  message("Processing metier: ", met)
  smooth_vars <- c("BT_scaled", "SST_scaled")
  
  # subset and prepare data
  test <- fisheries_variables %>%
    filter(metier == met) %>%
    arrange(date) %>%
    mutate(time_index = match(date, sort(unique(date)))) %>%
    filter(if_all(all_of(smooth_vars), ~ !is.na(.) & is.finite(.)))

  #build formula
  
  include_pound <- "cost_per_pound_adjusted_scaled" %in% colnames(test) &&
    !all(is.na(test$cost_per_pound_adjusted_scaled))
  
  if (include_pound) {
    formula1 <- total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled +
      distance_to_shore_km_scaled + distance_complex_km_scaled +
      s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
      closed_pct_scaled + fyear + sin12 + cos12
  } else {
    formula1 <- total.hours ~ cost_per_gal_adjusted_scaled +
      distance_to_shore_km_scaled + distance_complex_km_scaled +
      s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
      closed_pct_scaled + fyear + sin12 + cos12
  }
  
  
  # define spatiotemporal structures
  spatiotemp_types <- c("iid", "ar1", "rw")
  
  for (st in spatiotemp_types) {
    message("  Fitting spatiotemporal: ", st)
    
    # create mesh and barrier
    mesh <- make_mesh(
      test,
      xy_cols = c("block_long_dd", "block_lat_dd"),
      cutoff = 10
    )
    
    bspde <- sdmTMBextra::add_barrier_mesh(
      mesh, west_coast,
      range_fraction = 0.02,
      proj_scaling = 1000, plot = FALSE
    )
    
    # fit model safely
    fit <- 
      sdmTMB(
        data = test,
        formula = formula1,
        mesh = bspde,
        family = Gamma(link = "log"),
        spatial = "on",
        time = "time_index",
        spatiotemporal = st
      )
    
    aic_results <- rbind(aic_results, data.frame(
      metier = met,
      model = st,
      AIC = AIC(fit)))

  }
}

# view results
aic_results
############

############

## ------------------------
## Function to go through different metiers and type of models (Gamma, log normal) with and without smooth terms
## ------------------------
fit_models<-function(df, metier_name){
  #filter to a single metier
  test<-df%>%
    filter(metier==metier_name)
  
  # Time index
  test <- test %>%
    arrange(date) %>%
    mutate(time_index = match(date, sort(unique(date))))
  
  smooth_vars <- c("BT_scaled", "SST_scaled")
  test <- test %>%
    filter(if_all(all_of(smooth_vars), ~ !is.na(.) & is.finite(.)))
  
 
  # ------------------------
  # Determine if cost_per_pound_adjusted_scaled exists
  # ------------------------
  include_pound <- "cost_per_pound_adjusted_scaled" %in% colnames(test) &&
    !all(is.na(test$cost_per_pound_adjusted_scaled))
  
  # ------------------------
  # Build formulas
  # ------------------------
  # Base terms (common to all models)
  base_terms <- c("cost_per_gal_adjusted_scaled",
                  "distance_to_shore_km_scaled", "distance_complex_km_scaled",
                  "closed_pct_scaled",  "sin12", "cos12","Avg_Depthm_scaled","SST_scaled")
  
  if (include_pound) {
    base_terms <- c("cost_per_gal_adjusted_scaled", 
                    "cost_per_pound_adjusted_scaled",
                    "distance_to_shore_km_scaled", "distance_complex_km_scaled",
                    "closed_pct_scaled",  "sin12", "cos12", "Avg_Depthm_scaled","SST_scaled")
  }
  
  # Unsmooth models (linear BT and SST)
  f1 <- reformulate(c(base_terms, "fyear"), response = "total.hours")
 f2 <- reformulate(c(base_terms,"year" ), response = "total.hours")
  
  # Smooth models (splines on BT and SST)
  #if (include_pound) {
 #   f3 <- total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled +
 #    distance_to_shore_km_scaled + distance_complex_km_scaled + Avg_Depthm+
 #    s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
 #    closed_pct_scaled + fyear + sin12 + cos12
 #  
 #  f4 <- total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled +
 #    distance_to_shore_km_scaled + distance_complex_km_scaled + Avg_Depthm+
 #    s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
 #    closed_pct_scaled + fyear + sin12 + cos12
 #  } else {
 #  f3 <- total.hours ~ cost_per_gal_adjusted_scaled +
 #    distance_to_shore_km_scaled + distance_complex_km_scaled +Avg_Depthm+
 #    s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
 #    closed_pct_scaled + fyear + sin12 + cos12
    
 # f4 <- total.hours ~ cost_per_gal_adjusted_scaled +
 #   distance_to_shore_km_scaled + distance_complex_km_scaled +Avg_Depthm+
 #   s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
 #   closed_pct_scaled + fyear + sin12 + cos12
  #}
  
  #formulas <- list(m1 = f1, m2 = f2, m3 = f3, m4 = f4)
  formulas <- list(m1=f1, m2=f2)#, m3 = f3, m4 = f4)
  # ------------------------
  # Families
  # ------------------------
  families <- list(
    m1 = Gamma(link = "log"),
    m2 = Gamma(link = "log")
   # m3 = lognormal(link = "log"),
   # m4 = Gamma(link = "log")
  )
  
  #create mesh and barrier mesh
  mesh <- make_mesh(
    test,
    c("block_long_dd", "block_lat_dd"),
    cutoff = 10
  )
  
  bspde <- sdmTMBextra::add_barrier_mesh(
    mesh, west_coast,
    range_fraction = 0.02,
    proj_scaling = 1000
  )
  
  
  # ------------------------
  # Fit models
  # ------------------------
  models <- imap(formulas, ~ {
    fit <- sdmTMB(
      data = test,
      formula = .x,
      mesh = bspde,
      family = families[[.y]],
      spatial = "on",
      time = "time_index",
      spatiotemporal = "off"
    )
    list(model = fit, summary = summary(fit), sanity = sanity(fit))
  })
  
  return(models)
}



# ---- Run across multiple metiers ----
metiers <- c("Santa Barbara_Urchin dive","Los Angeles_CPFV", "Santa Barbara_CPFV", 
             "Los Angeles_Lobster trap","Los Angeles_Squid seine","Santa Barbara_Set gillnet",
             "Santa Barbara_Squid seine","Los Angeles_Urchin dive","San Diego_Urchin dive","Fort Bragg_Urchin dive",
             "Santa Barbara_Lobster trap","Santa Barbara_Ridgeback prawn trawl",
             "San Francisco_Squid seine" ,"Eureka_Pink shrimp trawl" ,"Santa Barbara_Cucumber trawl"  )
results <- map(metiers, ~ fit_models(
  df = fisheries_variables,
  metier_name = .x
))
names(results) <- metiers

# Example: see summary of model 2 for San Diego_CPFV
m1<-results[["Santa Barbara_Squid seine"]]$m1$model
m2<-results[["Santa Barbara_Squid seine"]]$m2$model

AIC(m1,m2)

aic_results <- map_df(names(results), function(met) {
  m1 <- results[[met]]$m1$model
  m2 <- results[[met]]$m2$model
  
  tibble(
    metier = met,
    AIC_m1 = AIC(m1),
    AIC_m2 = AIC(m2)
  )
})

aic_results

## ------------------------
## Function to go through different mesh sizes 
## ------------------------
fit_models<-function(df, metier_name){
  #filter to a single metier
  test<-df%>%
    filter(metier==metier_name) %>%
    arrange(date) %>%
    mutate(time_index = match(date, sort(unique(date))))
  
  smooth_vars <- c("BT_scaled", "SST_scaled")
  test <- test %>%
    filter(if_all(all_of(smooth_vars), ~ !is.na(.) & is.finite(.)))
  
#build formula
  
  include_pound <- "cost_per_pound_adjusted_scaled" %in% colnames(test) &&
    !all(is.na(test$cost_per_pound_adjusted_scaled))
  
  if (include_pound) {
    formula <- total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled +
      distance_to_shore_km_scaled + distance_complex_km_scaled +
      s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
      closed_pct_scaled + fyear + sin12 + cos12
  } else {
    formula <- total.hours ~ cost_per_gal_adjusted_scaled +
      distance_to_shore_km_scaled + distance_complex_km_scaled +
      s(BT_scaled, k = 10) + s(SST_scaled, k = 8) +
      closed_pct_scaled + fyear + sin12 + cos12
  }
  
  
  # ------------------------
  # Create mesh  function
  # ------------------------
  #mesh cutoffs to try
  cutoffs <- c(20, 15, 10, 5, 2)
  
  #loop over cutoffs
  
  results <- map(cutoffs, function(co) {
    message("Fitting mesh with cutoff = ", co)
    
    # build mesh and barrier mesh
    mesh <- make_mesh(
      test,
      c("block_long_dd", "block_lat_dd"),
      cutoff = co
    )
    
    
    bspde <- sdmTMBextra::add_barrier_mesh(
      mesh, west_coast,
      range_fraction = 0.02,
      proj_scaling = 1000
    )
    
    fit <- sdmTMB(
      data = test,
      formula = formula,
      mesh = bspde,
      family = Gamma(link = "log"),
      spatial = "on",
      time = "time_index",
      spatiotemporal = "iid"
    )
    
    list(
      cutoff = co,
      model = fit,
      summary = summary(fit),
      sanity = sanity(fit),
      AIC = AIC(fit),
      logLik = logLik(fit)
    )
  })
  
  names(results) <- paste0("cutoff_", cutoffs)
  return(results)
}

# ---- Run across multiple metiers ----
metiers <- c("Santa Barbara_CPFV")  # expand as needed ,"Los Angeles_CPFV", "Santa Barbara_CPFV", 
#"Los Angeles_Lobster trap","Los Angeles_Squid seine","Santa Barbara_Set gillnet",
#"Santa Barbara_Squid seine"

results <- map(metiers, ~ fit_models(
  df = fisheries_variables,
  metier_name = .x
))
names(results) <- metiers





#mesh <- make_mesh(test, c("block_long_dd", "block_lat_dd"),n_knots =85, type = "kmeans" ) #performed worse
mesh <- make_mesh(test, c("block_long_dd", "block_lat_dd"),cutoff=10)

plot(mesh)
#add on barrier
bspde<-sdmTMBextra::add_barrier_mesh(
  mesh, west_coast, range_fraction = .01,
  proj_scaling=1000, plot=FALSE
)

mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]
ggplot( west_coast) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 1, colour = "green")





















## ------------------------
## Confidence Intervals
## ------------------------

# We can view the confidence intervals on the fixed effects by using the tidy function:
tidy(m2, conf.int = TRUE)%>% arrange(desc(abs(estimate)))
#And similarly for the random effect and variance parameters:
tidy(m2, "ran_pars", conf.int = TRUE)

tidy(m2, conf.int = TRUE) %>% filter(!grepl("year", term)) %>% arrange(desc(estimate))

tidy(m2, conf.int = TRUE) %>% filter(!grepl("year", term)) %>% arrange(estimate)

# Average magnitude of fixed effects?
tidy(m2, conf.int = TRUE) %>%
  filter(!grepl("year", term)) %>%
  mutate(abs_coef = abs(estimate)) %>% 
  summarise(mean_abs_coef = mean(abs_coef))
#would use this for spatiotemporal effects
tidy(m2, conf.int = TRUE, effects = "ran_par") %>%
  filter(term %in% c("sigma_O", "sigma_E")) %>%
  summarise(mean_estimate = mean(estimate))


## ------------------------
## Model diagnostics
## ------------------------

#We can inspect randomized quantile residuals:
test$resids <- residuals(m2) # randomized quantile residuals
qqnorm(test$resids)
qqline(test$resids)
hist(test$resids, breaks = 40) 

# Plot residuals vs. fitted
plot(fitted(m), test$resids, pch = 16, cex = 0.5)
abline(h = 0, col = "red")


## ------------------------
## Smooth terms (via ggeffects)
## ------------------------

ggeffects::ggpredict(m2, "SST_scaled [all]") |> plot() +
  labs(y = "Predicted total.hours", x = "SST (°C)")
ggeffects::ggpredict(m2, "BT_scaled [all]") |> plot()+
  labs(y = "Predicted total.hours", x = "Bottom Temp (°C)")


## ------------------------
## Variable importance
## ------------------------


## ------------------------
## Marginal effects of fixed effects
## ------------------------
# Continuous predictors
me_cost_gal <- ggpredict(m2, "cost_per_gal_adjusted_scaled [all]")
plot(me_cost_gal) + labs(y = "Predicted total.hours", x = "Fuel cost (scaled)")

me_cost_lb <- ggpredict(m2, "cost_per_pound_adjusted_scaled [all]")
plot(me_cost_lb) + labs(y = "Predicted total.hours", x = "Price per pound (scaled)")

# Categorical predictor: year
me_year <- ggpredict(m2, "fyear")
plot(me_year) +
  labs(y = "Predicted total.hours", x = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Closure effect
me_closure <- ggpredict(m2, "closed_pct_scaled [all]")
plot(me_closure) + labs(y = "Predicted total.hours", x = "Closure % (scaled)")

# month effect
me_sin12 <- ggpredict(m2, "sin12 [all]")
plot(me_closure) + labs(y = "Predicted total.hours", x = "sin12")


## ------------------------
## Spatial & spatiotemporal predictions
## ------------------------

prediction_grid<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/prediction_grid_df.rds")%>%
  rename(block_long_dd=blck_ln_, block_lat_dd=blck_lt_ )#rename to be same columns as mesh X and Y 

# Convert to sf object
prediction_grid_sf <- st_as_sf(prediction_grid, coords = c("block_long_dd", "block_lat_dd"), crs = 4326)%>%
      st_transform(crs = st_crs(3310))%>%
  arrange(date) %>%
  mutate(time_index = match(date, sort(unique(date))))



coords <- st_coordinates(prediction_grid_sf)
# Add to the polygon data as new columns
prediction_grid_sf$block_long_dd <- coords[,1]
prediction_grid_sf$block_lat_dd <- coords[,2]#/ 1000

#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct")

prediction_grid_sf <- prediction_grid_sf%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))

#drop geometry and create
prediction_grid_final<-prediction_grid_sf%>%
                      st_drop_geometry()%>%
  filter(time_index %in% unique(test$time_index)) #filtering for same time in time_index for model

prediction_grid_final$year <- year(ymd(prediction_grid_final$date))
prediction_grid_final$fyear <- as.factor(prediction_grid_final$year)

prediction_grid_final$month <- month(ymd(prediction_grid_final$date))

prediction_grid_final$sin12 <- sin(2 * pi * prediction_grid_final$month / 12)
prediction_grid_final$cos12 <- cos(2 * pi * prediction_grid_final$month / 12)

prediction_grid_final<-prediction_grid_final%>%
                      filter(month %in% c(1,2,3,10,11,12))
                      
prediction_grid_final_sb_lob<-prediction_grid_final%>%
  filter(fishery=="Lobster trap" & port.complex=="Santa Barbara")%>%
  filter(!is.na(SST_scaled))

prediction_grid_final_sb_lob$block_long_dd<- prediction_grid_final_sb_lob$block_long_dd / 1000
prediction_grid_final_sb_lob$block_lat_dd <- prediction_grid_final_sb_lob$block_lat_dd / 1000

predictions <- predict(m, newdata = prediction_grid_final_sb_lob)


all_blocks_sf <-all_blocks_sf %>%
  mutate(block_id=as.factor(block_id))


predictions_sf <- left_join(predictions ,all_blocks_sf, by = "block_id")%>%
  st_as_sf()

#function to help make maps
plot_map <- function(dat, column) {
  ggplot(dat) +
    geom_sf(aes(fill = {{ column }}), color = NA) +  # NA removes polygon borders
    coord_sf() +
    scale_fill_viridis_c(trans = "sqrt") +
    theme_minimal()
}


plot_map(predictions_sf2, exp(est)) +
  facet_wrap(~year) +
  ggtitle("Prediction (fixed effects + all random effects)")+
scale_fill_viridis_c(
  trans = "sqrt",
  limits = c(0, quantile(exp(predictions_sf2$est), 0.995, na.rm = TRUE)),
  na.value = "yellow"
)

plot_map(predictions_sf2, exp(est_non_rf)) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Prediction (fixed effects only)")


plot_map(predictions_sf2, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")


#spatiotemporal random effects that represent deviation 
#from the fixed effect predictions and the spatial random 
#effect deviations. These represent biotic and abiotic factors 
#that are changing through time and are not accounted for in 
#the model.
plot_map(predictions_sf, epsilon_st) +
  scale_fill_gradient2() +
  facet_wrap(~year) +
  ggtitle("Spatiotemporal random effects only")
