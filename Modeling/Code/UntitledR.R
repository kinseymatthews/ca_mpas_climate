library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(INLA)
library(broom)
############
#Upload Data
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"

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
  )


#upload SST and BT
temp <- readRDS(file.path(base_path, "Variables/Temperature/processed/monthly_sst_bt_GLORYS.rds"))
#uplaod fuel
fuel <- readRDS(file.path(base_path, "Variables/Fuel price/data/processed/allportcomplexes_dieselprice_monthly_adjusted.rds"))
#upload ex vessel price
exvessel  <- readRDS(file.path(base_path, "Variables/ex vessel price/data/processed/allfisheries_exvessel_port_year_month_adjusted.rds")) %>%
  select(port.complex, month_year, fishery, year, cost_per_pound_adjusted) %>%
  mutate(year = as.double(year),
         fishery = if_else(fishery == "Halibut trawl", 
                           "California halibut trawl", fishery))
#upload distance to shore
dist_shore   <- readRDS(file.path(base_path, "Variables/Distance to shore/data/processed/block_shore_distances_complete.rds"))
#upload distance to port complex
dist_complex <- readRDS(file.path(base_path, "Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")) %>%
  rename(port.complex = port_complex, distance_complex_km = average.distance.km) %>%
  select(-average.distance.m)

#upload block info
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#upload block information
all_blocks <- st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp")) %>%
  st_transform(3310) 

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
  select(-block_id)%>%
  rename(block_id = block_d) %>%
  filter(blck_ty != "Offshore", !blck_st %in% c("Oregon","Washington"))%>%
  select(-Min_Depthm, -Max_Depthm, -Avg_Depthm, -blck_st, -blck_ty, -blck_sq)


# Create sf version for mapping
all_blocks_sf <- all_blocks %>%
  select( -block_lat_dd, -block_long_dd)


# Create non-sf version for joining
all_blocks <- all_blocks %>%
  select( -geometry) %>%
  st_drop_geometry()

#shore data for mesh
ca <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")%>%
  filter(iso_3166_2=="US-CA" )%>%
  st_transform(crs = 3310)


############



############
#create full dataframe by adding explanatory variables 
fisheries_variables<-fisheries %>%
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
         month = as.integer(month(date)),
         fyear = as.factor(year),
         fmonth_year = as.factor(month_year),
         sin12 = sin(2 * pi * month / 12),
         cos12 = cos(2 * pi * month / 12),
         closed_pct=(closed_pct*100),
         block_long_dd= as.numeric(block_long_dd),
         block_lat_dd =as.numeric(block_lat_dd ),
       metier=paste(port.complex, fishery, sep = "_")#create metier column
         
  ) 

############

############
#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct")

fisheries_variables <- fisheries_variables%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
############

############
# Plot correlation between variables
test_cor <- fisheries_variables %>% dplyr::select("cost_per_gal_adjusted", ,
                                                  "cost_per_pound_adjusted", ,
                                                  "distance_to_shore_km", , 
                                                  "distance_complex_km", , 
                                                  "BT",  
                                                  "SST", 
                                                  "closed_pct", )

corr <- round(cor(test_cor, use = "pairwise.complete.obs"), 1)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 2.5) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))
############


############
# Make folder for saving models
model_folder <- paste0(base_path, "Modeling/Data/Processed Data/Model_Results/")

# Loop through each unique fishery/metier
for(i in unique(fisheries_variables$metier)) {
  
  message("Fitting model for: ", i)
  
  test <- fisheries_variables %>%
    filter(metier == i, total.hours != 0)
  
  smooth_vars <- c("BT_scaled", "SST_scaled")
  
  test <- test %>%
    filter(!if_any(all_of(smooth_vars), is.na))
  

  # Time index
  test <- test %>%
    arrange(date) %>%
    mutate(time_index = match(date, sort(unique(date))))
  
  # Dynamically build formula
  base_terms <- c("cost_per_gal_adjusted_scaled",
                  "distance_to_shore_km_scaled", "distance_complex_km_scaled",
                  "(BT_scaled)", "(SST_scaled)",
                  "closed_pct_scaled", "fyear", "sin12", "cos12")
  
  # Include cost_per_pound_adjusted_scaled only if it exists and is not all NA
  if("cost_per_pound_adjusted_scaled" %in% colnames(test) &&
     !all(is.na(test$cost_per_pound_adjusted_scaled))) {
    base_terms <- c("cost_per_gal_adjusted_scaled", 
                    "cost_per_pound_adjusted_scaled", 
                    base_terms[-1]) # replace first element
  }
  
  formula_str <- paste("total.hours ~", paste(base_terms, collapse = " + "))
  
  # Create mesh = always has to go right before building the model!
  mesh <- make_mesh(test, xy_cols = c("block_long_dd","block_lat_dd"), cutoff = 10)
  bspde <- sdmTMBextra::add_barrier_mesh(mesh, 
                                         ca ,
                                         range_fraction = 0.02, proj_scaling = 1000, plot = TRUE)
  
 as.formula( formula_str)
  # Fit sdmTMB model
  set.seed(123)
  model <- sdmTMB(
    data = test,
    formula =  as.formula( formula_str),
    mesh = bspde,
    family = Gamma(link = "log") ,
    spatial = "on",
      time = "time_index",
     spatiotemporal = "iid"
  )
  
  
  summary(model)
  
  # Save model
  saveRDS(model, file = paste0(model_folder, "model_", gsub(" ","_", i), ".rds"))
}
############


############
#read models back in
# List all RDS model files
model_files <- list.files(model_folder, pattern = "^model_.*\\.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models <- lapply(model_files, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models) <- gsub("^model_|\\.rds$", "", basename(model_files))

# Example: look at the summary for San Francisco Squid Seine
m<-(models[["Santa_Barbara_Set_gillnet"]])
summary(m)
sanity(m)


# Loop through models, print results
for (f in model_files) {
  nm <- gsub("^model_|\\.rds$", "", basename(f))  # clean name
  
  message("\n==========================")
  message("Results for model: ", nm)
  message("==========================\n")
  
  m <- readRDS(f)
  
  # Print summary
  print(summary(m))
  
  # Print sanity check
  print(sanity(m))
}



coef_table <- lapply(names(models), function(nm) {
  coefs <- broom::tidy(models[[nm]])
  coefs$model <- nm
  coefs
}) %>% dplyr::bind_rows()

head(coef_table)

# Count positives vs negatives for each term
coef_summary <- coef_table %>%
  mutate(sign = case_when(
    estimate > 0 ~ "positive",
    estimate < 0 ~ "negative",
    TRUE ~ "zero"
  )) %>%
  group_by(term, sign) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sign, values_from = n, values_fill = 0)

coef_summary

#county positive and negative per term organized by fishery and by port complex
# Add city and fishery columns based on model name
# Known fishery/metiers
fishery_list <- c("CPFV", "Urchin_dive", "Squid_seine", "Set_gillnet", "Cucumber_dive", "Lobster_trap", "Cucumber_trawl", "Ridgeback_prawn", "Pink_shrimp_trawl")

coef_table2 <- coef_table %>%
  rowwise() %>%
  mutate(
    # Find fishery by matching the known list
    fishery = {
      f <- fishery_list[sapply(fishery_list, function(x) grepl(x, model))]
      if(length(f) == 0) NA_character_ else f[1]
    },
    # Port complex is what's left after removing fishery
    port.complex = if(!is.na(fishery)) sub(paste0("_", fishery, "$"), "", model) else model,
    port.complex = gsub("_", " ", port.complex),
    sign = case_when(
      estimate > 0 ~ "positive",
      estimate < 0 ~ "negative",
      TRUE ~ "zero"
    )
  ) %>%
  ungroup()



# Example: summarize by fishery
fishery_summary <- coef_table2 %>%
  group_by(fishery, term, sign) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sign, values_from = n, values_fill = 0)

port.complex_summary <- coef_table2 %>%
  group_by(port.complex, term, sign) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sign, values_from = n, values_fill = 0)
