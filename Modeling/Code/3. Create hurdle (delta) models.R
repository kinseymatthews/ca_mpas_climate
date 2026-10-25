library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(broom)
library(INLAspacetime)
library(INLA)
library(rnaturalearth)
library(magick)
############
#Upload Data
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
# Make folder for saving models
model_folder <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/")


#fisheries
fisheries<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/summarized_fishery_data_ready4model.rds")%>%
  filter(year>1999)%>%
  mutate(date = my(month_year, tz = "UTC"))

#upload management regulations
regulations <- readRDS(file.path(base_path, "Variables/Regulation/monthly_percent_closed_blocks.rds")) %>%
  mutate(
    month_year = gsub("-", "_", month_year),
    month_year = sub("^0+", "", month_year),
    fishery = gsub("\\.", " ", fishery)
  )%>%
  mutate(fishery = recode(fishery,
                          "Halibut trawl" = "California halibut trawl"))


#upload SST and BT
temp <- readRDS(file.path(base_path, "Variables/Temperature/processed/monthly_sst_bt_GLORYS.rds"))
#uplaod fuel
fuel <- readRDS(file.path(base_path, "Variables/Fuel price/data/processed/allportcomplexes_dieselprice_monthly_adjusted.rds"))
#upload ex vessel price
exvessel  <- readRDS(file.path(base_path, "Variables/ex vessel price/data/processed/allfisheries_exvessel_port_year_month_adjusted.rds")) %>%
  dplyr::select(port.complex, month_year, fishery, year, cost_per_pound_adjusted) %>%
  mutate(year = as.double(year),
         fishery = if_else(fishery == "Halibut trawl", 
                           "California halibut trawl", fishery))
#upload distance to shore
dist_shore   <- readRDS(file.path(base_path, "Variables/Distance to shore/data/processed/block_shore_distances_complete.rds"))
#upload distance to port complex
dist_complex <- readRDS(file.path(base_path, "Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")) %>%
  rename(port.complex = port_complex, distance_complex_km = average.distance.km) %>%
  dplyr::select(-average.distance.m)

#upload block info
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#upload block information
all_blocks <- st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp")) %>%
  st_transform(3310) %>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))

# Calculate centroids
centroids <- st_centroid(all_blocks)
coords <- st_coordinates(centroids)

# Add coordinates to sf object
all_blocks <- all_blocks %>%
  mutate(
    block_long_dd = coords[,1]/1000,
    block_lat_dd  = coords[,2]/1000
  ) %>%
  # Common filtering and renaming
  rename(block_id = block_d) %>%
  filter(blck_ty != "Offshore", !blck_st %in% c("Oregon","Washington"))

#upload bathy data
bathy.data<-read.csv(paste0(general_data, "fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")


# Create sf version for mapping
all_blocks_sf <- all_blocks %>%
  dplyr::select( -block_lat_dd, -block_long_dd)


# Create non-sf version for joining
all_blocks <- all_blocks %>%
  dplyr::select( -geometry) %>%
  st_drop_geometry()

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

# Mexican Baja states
baja <- ne_states(
  country = "Mexico", 
  returnclass = "sf"
) %>%
  filter(iso_3166_2 %in% c("MX-BCN", "MX-BCS"))

# Combine and transform
west_coast <- bind_rows(west_coast_us, baja) %>%
  st_transform(crs = 3310)
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

############

############
#expand effort data to go over each combo of fishery, port complex, month_year, and block id

#would need to elimate blocks that are entirely closed off to the fishery. 

#across all years, get all block IDs that are fished by a fishery, and use those as potential blocks/
#elimate any that are 100% closed to fishery. Then add in that effort is zero. OR should i do blockls that a port complex goes?

# get block id
all_blocks_ids <- sort(unique(all_blocks$block_id))
# 1. Get observed combinations
observed <- fisheries %>%
  dplyr::select(month_year, port.complex, fishery, year)%>%
  mutate(metier=paste(port.complex, fishery, sep = "_"))

zero_grid <- observed %>%
  crossing(block_id = all_blocks_ids) %>%
  mutate(total.hours = 0)


fisheries_augmented <- zero_grid %>%
  left_join(
    fisheries %>%
      select(port.complex, fishery, block_id, month_year, year, total.hours),
    by = c("port.complex", "fishery", "block_id", "month_year", "year")
  ) %>%
  mutate(
    total.hours = coalesce(total.hours.y, total.hours.x),
    metier = paste(port.complex, fishery, sep = "_")
  ) %>%
  select(-total.hours.x, -total.hours.y)

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
  #filter(closed_pct!=1)%>%
  #remove those of not CPFV with the exvessel price of NA - only had landings data to mid 2023
  filter(fishery == "CPFV" | !is.na(cost_per_pound_adjusted))%>%
  #remove block_id 101 which is inshore
  filter(block_id!="101") %>% 
  mutate(year = as.integer(year),
         month = as.integer(month(date)),
         fyear = as.factor(year),
         fmonth_year = as.factor(month_year),
         sin12 = sin(2 * pi * month / 12),
         cos12 = cos(2 * pi * month / 12),
         closed_pct=(closed_pct*100),
         block_long_dd= as.numeric(block_long_dd),
         block_lat_dd =as.numeric(block_lat_dd ),
         metier=paste(port.complex, fishery, sep = "_")#create metier column
         
  )%>%
  mutate(present=case_when(
    total.hours>0 ~ 1,
    total.hours==0 ~0
  ))


saveRDS(fisheries_variables,paste0(base_path,"Modeling/Data/Processed Data/fisheries_df_with_variables.rds"))

############

############
#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct", "Avg_Depthm")

fisheries_variables <- fisheries_variables%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
############

############
# Drop the leftover geometry column (no rows change; keeps saved models smaller)
if (inherits(fisheries_variables, "sf")) fisheries_variables <- st_drop_geometry(fisheries_variables)
fisheries_variables <- fisheries_variables %>% select(-any_of("geometry"))
############

############
# Plot correlation between variables
test_cor <- fisheries_variables %>% dplyr::select("cost_per_gal_adjusted", 
                                                  "cost_per_pound_adjusted", 
                                                  "distance_to_shore_km",  
                                                  "distance_complex_km",  
                                                  "BT", 
                                                  "SST", 
                                                  "closed_pct", "Avg_Depthm" )

corr <- round(cor(test_cor, use = "pairwise.complete.obs"), 1)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 2.5) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))
############


############
#--------------------------------------------------
# Acceptance rule, used for BOTH components:
#   ran without error, Hessian positive definite, sanity() passes,
#   and every random-field SD (sigma_O, sigma_E) < 10
#--------------------------------------------------
fit_ok <- function(m, max_sigma = 10) {
  if (inherits(m, "try-error") || !isTRUE(m$sd_report$pdHess)) return(FALSE)
  if (!isTRUE(sanity(m, silent = TRUE)$all_ok)) return(FALSE)
  rp  <- tidy(m, effects = "ran_pars")
  sig <- rp$estimate[rp$term %in% c("sigma_O", "sigma_E")]
  all(sig < max_sigma)          # TRUE automatically when no fields are on
}


#--------------------------------------------------
# Fallback ladder: the first rung that passes fit_ok() is kept.
# Whenever an AR1 spatiotemporal field fails, IID is tried before
# the spatiotemporal field is dropped entirely.
#   1) spatial on  + spatiotemporal AR1
#   2) spatial on  + spatiotemporal IID
#   3) spatial on  + spatiotemporal off
#   4) spatial off + spatiotemporal AR1
#   5) spatial off + spatiotemporal IID
#   6) spatial off + spatiotemporal off
#--------------------------------------------------
#--------------------------------------------------
fit_ladder <- function(data, formula, family, mesh, start_step = 1) {
  ladder <- list(
    list(label = "1_full_ar1",             spatial = "on",  spatiotemporal = "ar1"),
    list(label = "2_full_iid",             spatial = "on",  spatiotemporal = "iid"),
    list(label = "3_spatiotemporal_off",   spatial = "on",  spatiotemporal = "off"),
    list(label = "4_spatial_off_ar1",      spatial = "off", spatiotemporal = "ar1"),
    list(label = "5_spatial_off_iid",      spatial = "off", spatiotemporal = "iid"),
    list(label = "6_no_fields",            spatial = "off", spatiotemporal = "off")
  )
  
  for (rung in ladder[start_step:length(ladder)]) {
    message(format(Sys.time(), "%H:%M:%S"), "  Trying ", rung$label)
    args <- list(data = data, formula = formula, family = family,
                 spatial = rung$spatial, spatiotemporal = rung$spatiotemporal)
    # Mesh and time are only passed when a field needs them
    if (rung$spatial == "on" || rung$spatiotemporal != "off") args$mesh <- mesh
    if (rung$spatiotemporal != "off") args$time <- "time_index"
    
    set.seed(123)
    m <- try(do.call(sdmTMB, args), silent = TRUE)
    
    if (fit_ok(m)) {
      attr(m, "fit_level") <- rung$label
      message("  Accepted: ", rung$label)
      return(m)
    }
    message("  Rejected: ", rung$label)
  }
  
  # Nothing passed: keep the last attempt, flagged for review
  warning("No rung passed all checks; returning the no-fields attempt flagged for review")
  attr(m, "fit_level") <- if (inherits(m, "try-error")) "all_rungs_errored" else "6_no_fields_FAILED_CHECKS"
  m
}

#--------------------------------------------------
# Pull the numbers worth logging from one fitted model.
# Safe on a try-error, so a total failure can't crash the loop.
#--------------------------------------------------
fit_summary <- function(m) {
  if (inherits(m, "try-error")) {
    return(tibble(fit_level = attr(m, "fit_level"), sigma_O = NA_real_, sigma_E = NA_real_,
                  max_coef = NA_character_, max_abs_coef = NA_real_))
  }
  rp <- tryCatch(tidy(m, effects = "ran_pars"),
                 error = function(e) tibble(term = character(), estimate = numeric()))
  fx <- tidy(m) %>% filter(term != "(Intercept)")
  tibble(
    fit_level    = attr(m, "fit_level"),
    sigma_O      = rp$estimate[rp$term == "sigma_O"][1],
    sigma_E      = rp$estimate[rp$term == "sigma_E"][1],
    max_coef     = fx$term[which.max(abs(fx$estimate))],   # flags exploding coefficients
    max_abs_coef = max(abs(fx$estimate))
  )
}

# Print summary + sanity only when the model actually exists
print_fit <- function(m, label, metier) {
  if (inherits(m, "try-error")) {
    message(label, " failed to fit for ", metier, " — all rungs errored")
  } else {
    print(summary(m))
    print(sanity(m))
  }
}





#--------------------------------------------------
# Main loop: every métier, both delta components
#--------------------------------------------------
# Loop through each unique fishery/metier
for(i in unique(fisheries_variables$metier)) {
  
  nm         <- gsub(" ", "_", i)
  pa_file    <- paste0(out_folder, "model_", nm, "_Depth_no_BT_pa_delta_updated2026.rds")
  abund_file <- paste0(out_folder, "model_", nm, "_Depth_no_BT_abundance_delta_updated2026.rds")
  log_file   <- paste0(log_folder, nm, ".csv")
  
  # The log is written LAST, so it only exists if both models saved.
  # If a job dies mid-métier, that métier is refit from scratch on resubmit.
  if (file.exists(log_file)) {
    message("Skipping ", i, " — already finished")
    next
  }
  
  message("\n==== Fitting delta model for: ", i, " ====")
  
  
  na_var <- c("BT_scaled", "SST_scaled")
  
  # Keep rows with usable temperature data, in date order
  test <- fisheries_variables %>%
    filter(metier ==i)%>%
    filter(if_all(all_of( na_var), ~ !is.na(.) & is.finite(.))) %>%
    arrange(date) 
  
  # Months that had any fishing effort
  test2 <- test %>%
    filter(total.hours != 0)%>%
    mutate(time_index = match(date, sort(unique(date))))
  
  dates<-unique(test2$month_year)
  
  # Restrict the presence/absence data to months with effort, then index time
  test <- test%>%
    filter(month_year %in% dates)%>%
    mutate(time_index = match(date, sort(unique(date))))
  
  # Dynamically build formula
  base_terms <- c("cost_per_gal_adjusted_scaled",
                  "distance_to_shore_km_scaled", "distance_complex_km_scaled",
                  "SST_scaled",  "Avg_Depthm_scaled",#"BT_scaled",
                  "closed_pct_scaled", "fyear", "sin12", "cos12")
  
  # Include cost_per_pound_adjusted_scaled only if it exists and is not all NA
  if("cost_per_pound_adjusted_scaled" %in% colnames(test) &&
     !all(is.na(test$cost_per_pound_adjusted_scaled))) {
    base_terms <- c("cost_per_gal_adjusted_scaled", 
                    "cost_per_pound_adjusted_scaled", 
                    base_terms[-1]) # replace first element
  }
  
  absence_formula  <- as.formula(paste("present ~", paste(base_terms, collapse = " + ")))
  abundance_formula <- as.formula(paste("total.hours ~", paste(base_terms, collapse = " + ")))
  
  #create presence/absence models
  # Create mesh = always has to go right before building the model!
  mesh <- make_mesh(test, xy_cols = c("block_long_dd","block_lat_dd"), cutoff = 10)
  mesh1 <- sdmTMBextra::add_barrier_mesh(mesh, 
                                         west_coast ,
                                         range_fraction = 0.02, proj_scaling = 1000, plot = TRUE)
  
  
  model1 <- fit_ladder(test, absence_formula, binomial(link = "logit"), mesh1)
  print_fit(model1, "Presence/absence model", i)
  saveRDS(model1, pa_file)
  
  # Create effort models
  # Positive rows only; time_index is inherited from `test` so both
  # components share the same time steps
  test2 <- test %>%
    filter(total.hours != 0)
  
  mesh2 <- make_mesh(test2,
                     xy_cols = c("block_long_dd", "block_lat_dd"),
                     mesh = mesh1$mesh
  )
  
  model2 <- fit_ladder(test2, abundance_formula, Gamma(link = "log"), mesh2)
  print_fit(model2, "Effort model", i)
  saveRDS(model2, abund_file)
  
  
  # Log both components (written last = marks this métier as done)
  bind_rows(
    bind_cols(tibble(metier = i, component = "presence_absence"), fit_summary(model1)),
    bind_cols(tibble(metier = i, component = "effort"),           fit_summary(model2))
  ) %>%
    mutate(time = format(Sys.time())) %>%
    write_csv(log_file)
  
  # Free memory before the next métier (TMB objects hold compiled pointers)
  rm(model1, model2, mesh, mesh1, mesh2, test, test2)
  gc(full = TRUE)
  
  
  
  
}
# Combine every métier's log into one table


# Read everything as text first so NA-only columns don't clash across files
all_logs <- list.files(log_folder, pattern = "\\.csv$", full.names = TRUE) %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) %>%
  type_convert()

write_csv(all_logs, paste0(out_folder, "fit_levels_all_metiers.csv"))
print(all_logs, n = Inf)

message("Done.")


############

