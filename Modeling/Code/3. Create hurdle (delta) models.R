library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(INLA)
library(broom)
library(rnaturalearth)
library(magick)
############
#Upload Data
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
# Make folder for saving models
model_folder <- paste0(base_path, "Modeling/Data/Processed Data/Model_Results/")


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

# Loop through each unique fishery/metier
for(i in unique(fisheries_variables$metier)) {
  
  message("Fitting model for: ", i)
  na_var <- c("BT_scaled", "SST_scaled")
  
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
  
  ## Fallback 1: spatial OFF
  if (inherits(model2, "try-error") || !isTRUE(model2$sd_report$pdHess)) {
    message("⚠️ Full model failed — refitting with spatial OFF")
    model2 <- sdmTMB(
      data = test2,
      formula = abundance_formula,
      mesh = mesh2,
      family = Gamma(link = "log"),
      spatial = "off",
      time = "time_index",
      spatiotemporal = "ar1"
    )
  }
  
  ## Fallback 2: spatial + spatiotemporal OFF
  if (!isTRUE(model2$sd_report$pdHess)) {
    message("⚠️ Spatial-off model failed — refitting with no random effects")
    model2 <- sdmTMB(
      data = test2,
      formula =  abundance_formula,
      family = Gamma(link = "log"),
      spatial = "off",
      spatiotemporal = "off"
    )
  }
  
  
  # Print summary and sanity
  print(summary(model1))
  print(sanity(model1))
  
  print(summary(model2))
  print(sanity(model2))
  
  # Save model
  saveRDS(model1, file = paste0(model_folder, "model_", gsub(" ","_", i), "_Depth_no_BT_pa_delta.rds"))
  saveRDS(model2, file = paste0(model_folder, "model_", gsub(" ","_", i), "_Depth_no_BT_abundance_delta.rds"))
  }
############


############
#read models back in
# List all RDS model files for presence absence
model_files <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT_pa_delta.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models <- lapply(model_files, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models) <- gsub("^model_|\\_Depth_no_BT_pa_delta.rds$", "", basename(model_files))

# Example: look at the summary for San Francisco Squid Seine
m<-(models[["Santa_Barbara_Squid_seine"]])
summary(m)
sanity(m)
tidy(m)
AIC(m)

############

############
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
############

############ 

#QQ plots
for (f in model_files) {
  nm <- gsub("^model_|\\.rds$", "", basename(f))  # clean name
  
  m <- readRDS(f)
 
  # Safely attempt to extract residuals
  
  resids <- residuals(m, type = "mle-mvn")
  resids <- resids[is.finite(resids)]
  
  if (length(resids) < 5) {
    message("Skipping ", nm, ": no usable residuals")
    next
  }
  
  # Save QQ plot as a PNG file
  png(paste0(model_folder,"Diagnostics/QQ_plots/",nm,"_qqplot_resids_delta_pa.png"), width = 800, height = 600)
  qqnorm(resids, main = paste("QQ Plot of Residuals -", nm))
  qqline(resids, col = "red")
  
  dev.off() 
}
############

############
#read models back in
# List all RDS model files for presence absence
model_files_abund <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT_abundance_delta.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with Depth_no_BT_abundance_delta.rds”

# Read them back into a named list
models_abund  <- lapply(model_files_abund , readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models_abund ) <- gsub("^model_|\\_Depth_no_BT_abundance_delta.rds$", "", basename(model_files_abund ))

# Example: look at the summary for San Francisco Squid Seine
m<-(models[["Santa_Barbara_Squid_seine"]])
summary(m)
sanity(m)
tidy(m)
AIC(m)

############

############ 

#QQ plots
for (f in model_files_abund ) {
  nm <- gsub("^model_|\\.rds$", "", basename(f))  # clean name
  
  m <- readRDS(f)
  # Safely attempt to extract residuals
  
  resids <- residuals(m, type = "mle-mvn")
  resids <- resids[is.finite(resids)]
  
  if (length(resids) < 5) {
    message("Skipping ", nm, ": no usable residuals")
    next
  }
  
  # Save QQ plot as a PNG file
  png(paste0(model_folder,"Diagnostics/QQ_plots/",nm,"_qqplot_resids_delta_abundance.png"), width = 800, height = 600)
  
  qqnorm(resids, main = paste("QQ Plot of Residuals -", nm))
  qqline(resids, col = "red")
  
  dev.off() 
}
############

############
#R2 mariginal and conditional R2. mariginal represents proportion of variance explained by fixed effects alone, while conditiional accoutns for both fixed and random effects
R2_sdmTMB <- function(models) {
  
  # Function to compute R2 for one model
  compute_R2 <- function(mod, name) {
    # Fixed-effect variance on link scale
    pred_link <- predict(mod, type = "link")$est
    sigma2_f <- var(pred_link, na.rm = TRUE)
    
    # Random-effect variance
    random_vals <- mod$sd_report$par.random
    
    # Spatial variance
    sigma2_omega <- if (any(grepl("omega_s", names(random_vals)))) {
      var(random_vals[grep("omega_s", names(random_vals))], na.rm = TRUE)
    } else 0
    
    # Spatiotemporal variance
    sigma2_epsilon <- if (any(grepl("epsilon_st", names(random_vals)))) {
      var(random_vals[grep("epsilon_st", names(random_vals))], na.rm = TRUE)
    } else 0
    
    sigma2_r <- sigma2_omega + sigma2_epsilon
    
    # Distribution-specific variance (Gamma/log)
    ln_phi <- mod$sd_report$par.fixed["ln_phi"]
    phi <- exp(ln_phi)
    sigma2_d <- trigamma(phi)
    
    # Marginal and conditional R2
    R2_marginal <- sigma2_f / (sigma2_f + sigma2_r + sigma2_d)
    R2_conditional <- (sigma2_f + sigma2_r) / (sigma2_f + sigma2_r + sigma2_d)
    
    data.frame(
      model_name = name,
      R2_marginal = R2_marginal,
      R2_conditional = R2_conditional,
      sigma2_fixed = sigma2_f,
      sigma2_random = sigma2_r,
      sigma2_distribution = sigma2_d
    )
  }
  
  # Loop over all models and compute R2
  results <- do.call(
    rbind,
    lapply(names(models), function(nm) {
      mod <- models[[nm]]
      tryCatch(
        compute_R2(mod, nm),
        error = function(e) {
          data.frame(
            model_name = nm,
            R2_marginal = NA,
            R2_conditional = NA,
            sigma2_fixed = NA,
            sigma2_random = NA,
            sigma2_distribution = NA
          )
        }
      )
    })
  )
  
  return(results)
}

R2_df <- R2_sdmTMB(models)


saveRDS(R2_df ,paste0(base_path,"Modeling/Data/Processed Data/Model_Results/Diagnostics/R2/Rc_Rm_results_delta_pa.rds"))
############


############
#plotting figure worthy QQ plots
qq_folder <- paste0(model_folder, "Diagnostics/QQ_plots/")
out_folder <- paste0(base_path, "All Fisheries Figures/")
dir.create(out_folder, showWarnings = FALSE)

# List both sets of QQ plots
pa_files    <- list.files(qq_folder, pattern = "_pa_delta_qqplot.*\\.png$",        full.names = TRUE)
abund_files <- list.files(qq_folder, pattern = "_abundance_delta_qqplot.*\\.png$", full.names = TRUE)

# Extract clean metier name from PA files (use as key)
clean_metier <- function(f, type = "pa") {
  pattern <- if (type == "pa") "_Depth_no_BT_pa_delta_qqplot.*\\.png$" else "_Depth_no_BT_abundance_delta_qqplot.*\\.png$"
  basename(f) %>% gsub("^model_", "", .) %>% gsub(pattern, "", .)
}

# Make a clean readable label from metier string
# e.g. "Santa_Barbara_Squid_seine" -> "Santa Barbara | Squid Seine"
port_complexes <- c("Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco",
                    "Monterey_Bay", "Morro_Bay", "Santa_Barbara", "Los_Angeles", "San_Diego")

make_label <- function(metier) {
  for (pc in port_complexes) {
    if (startsWith(metier, pc)) {
      port    <- gsub("_", " ", pc)
      fishery <- gsub(paste0("^", pc, "_"), "", metier) %>% gsub("_", " ", .)
      return(paste0(port, "  |  ", fishery))
    }
  }
  gsub("_", " ", metier)
}

# Build lookup
pa_df <- data.frame(
  path   = pa_files,
  metier = sapply(pa_files, clean_metier, type = "pa"),
  stringsAsFactors = FALSE
)

ab_df <- data.frame(
  path   = abund_files,
  metier = sapply(abund_files, clean_metier, type = "abundance"),
  stringsAsFactors = FALSE
)
combined_df <- inner_join(pa_df, ab_df, by = "metier", suffix = c("_pa", "_ab"))

# Process each metier: crop title, label, join PA + abundance side by side
process_pair <- function(pa_path, ab_path, metier) {
  
  label <- make_label(metier)
  
  pa_img <- image_read(pa_path) %>%
    image_crop("800x560+0+40") %>%           # crop off base R title (~40px)
    image_annotate("Presence-Absence",        # add clean subplot label
                   size = 28, color = "gray30",
                   gravity = "north", location = "+0+5")
  
  ab_img <- image_read(ab_path) %>%
    image_crop("800x560+0+40") %>%
    image_annotate("Abundance",
                   size = 28, color = "gray30",
                   gravity = "north", location = "+0+5")
  
  # Combine side by side
  row_img <- image_append(c(pa_img, ab_img), stack = FALSE)
  
  # Add overall metier title above the pair
  title_bar <- image_blank(1600, 50, color = "white") %>%
    image_annotate(label, size = 32, color = "black",
                   gravity = "center")
  
  image_append(c(title_bar, row_img), stack = TRUE)
}

# Build paired images for all metiers
paired_imgs <- vector("list", nrow(combined_df))
for (i in seq_len(nrow(combined_df))) {
  message("Processing: ", combined_df$metier[i])
  paired_imgs[[i]] <- process_pair(
    combined_df$path_pa[i],
    combined_df$path_ab[i],
    combined_df$metier[i]
  )
}

# --- Save one figure per fishery ---
fishery_labels <- sapply(combined_df$metier, function(m) {
  for (pc in port_complexes) {
    if (startsWith(m, pc)) return(gsub(paste0("^", pc, "_"), "", m))
  }
  m
})

for (f in unique(fishery_labels)) {
  idx  <- which(fishery_labels == f)
  imgs <- image_join(paired_imgs[idx])
  
  # Stack all port complexes for this fishery vertically
  fig  <- image_append(imgs, stack = TRUE)
  
  out_path <- paste0(out_folder, gsub(" ", "_", f), "_QQplots_supplement.png")
  image_write(fig, out_path, quality = 95)
  message("Saved: ", out_path)
}
