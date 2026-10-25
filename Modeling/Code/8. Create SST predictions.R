
############
data_path    <-("G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/")   # fisheries data + prediction grid
library(tidyverse)
library(sdmTMB)
library(sf)
message("sdmTMB version: ", packageVersion("sdmTMB"))

############
# PATHS (local computer)
base_path    <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
data_path    <- ("G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/")   # fisheries data + prediction grid
model_folder <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/")
blocks_path  <- paste0(base_path, "All Fisheries Data/Blocks/All_Blocks_Updated_10Jul24.shp")
out_dir      <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/spatial_outputs/SST/")   
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


############

############
# SETTINGS
# A métier gets SST predictions if SST is clearly Positive/Negative in these responses
sst_select  <- "Effort"   # map a métier only if SST is clearly Positive/Negative in its EFFORT model
se_mult     <- 1          # same rule as your coefficient table (estimate ± 1 SE excludes 0)
temp_levels <- c(Cold = 0.10, Average = 0.50, Hot = 0.90)   # SST quantiles for each scenario

# End of the model file names. Only files ending in this are used, so older
# models in the same folder (e.g. "..._delta.rds") are ignored.
model_suffix <- "_delta_updated2026.rds"
############

############
# Helper: one key format for métier names from every source
# ("Morro Bay_CPFV", "Morro_Bay_CPFV", "Morro Bay CPFV" all -> "Morro_Bay_CPFV")
make_key <- function(x) gsub("[ _]+", "_", x)
############

############
# FISHERIES DATA + the scaling the models were FIT with
fisheries <- readRDS(paste0(data_path, "fisheries_df_with_variables.rds"))
if (inherits(fisheries, "sf")) fisheries <- st_drop_geometry(fisheries)
fisheries <- fisheries %>% select(-any_of("geometry")) %>% mutate(key = make_key(metier))

num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted",
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct", "Avg_Depthm")

# Global mean and SD of each variable across the WHOLE data set — exactly how the
# model covariates were scaled. Every value fed to predict() must use these.
scale_pars <- map_dfr(num_vars, ~ tibble(var  = .x,
                                         mean = mean(fisheries[[.x]], na.rm = TRUE),
                                         sd   = sd(fisheries[[.x]],   na.rm = TRUE)))
scale_with_fit <- function(x, var) {
  p <- scale_pars[scale_pars$var == var, ]
  (x - p$mean) / p$sd
}
############

############
# PEAK MONTH per métier (most total effort over the last 10 years)
peak_months <- fisheries %>%
  mutate(year = year(date)) %>%
  filter(year >= max(year, na.rm = TRUE) - 9) %>%
  group_by(key, month) %>%
  summarise(total_effort = sum(total.hours, na.rm = TRUE), .groups = "drop") %>%
  group_by(key) %>%
  slice_max(total_effort, n = 1, with_ties = FALSE) %>%   # exactly one peak month per métier
  ungroup()
############

############
# SST SCENARIOS — BLOCK BY BLOCK
# For each block and calendar month: that block's own 10th / 50th / 90th percentile
# of SST across years. "Hot" = every block at its own warm end for that month.
# SST depends only on block + month, so métiers sharing a peak month share these fields.
block_sst <- fisheries %>%
  distinct(block_id, year = year(date), month, SST) %>%     # one SST per block per month-year
  group_by(block_id, month) %>%
  summarise(
    SST_Cold    = quantile(SST, temp_levels["Cold"],    na.rm = TRUE),
    SST_Average = quantile(SST, temp_levels["Average"], na.rm = TRUE),
    SST_Hot     = quantile(SST, temp_levels["Hot"],     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(block_id = as.character(block_id))

# FUEL / PRICE held at each métier's peak-month median (raw units; scaled with the FIT scaling later)
scenario_vals <- fisheries %>%
  semi_join(peak_months, by = c("key", "month")) %>%
  group_by(key) %>%
  summarise(
    fuel_median  = median(cost_per_gal_adjusted, na.rm = TRUE),
    pound_median = if (all(is.na(cost_per_pound_adjusted))) NA_real_ else median(cost_per_pound_adjusted, na.rm = TRUE),
    .groups = "drop"
  )
############

############
# PREDICTION GRID
prediction_grid <- readRDS(paste0(data_path, "prediction_grid_df.rds")) %>%
  rename(block_long_dd = blck_ln_, block_lat_dd = blck_lt_) %>%
  separate(month_year, into = c("month", "year"), sep = "-", convert = TRUE, remove = FALSE) %>%
  mutate(year  = as.integer(year),
         month = as.integer(month),
         fyear = as.factor(year),
         sin12 = sin(2 * pi * month / 12),
         cos12 = cos(2 * pi * month / 12),
         closed_pct = closed_pct * 100,
         date  = as.Date(date),
         key   = make_key(metier))

# Grid lon/lat -> EPSG 3310 -> km (matches the mesh coordinates)
grid_xy <- st_as_sf(prediction_grid, coords = c("block_long_dd", "block_lat_dd"), crs = 4326) %>%
  st_transform(3310) %>%
  st_coordinates()
prediction_grid$block_long_dd <- grid_xy[, 1] / 1000
prediction_grid$block_lat_dd  <- grid_xy[, 2] / 1000

# Units check: grid and fitting data should be on the same scale (e.g., closed_pct both 0-100)
message("Range check (fitting data vs grid):")
for (v in c("closed_pct", "distance_to_shore_km", "distance_complex_km", "Avg_Depthm")) {
  message(sprintf("  %-22s data: %8.2f to %8.2f | grid: %8.2f to %8.2f", v,
                  min(fisheries[[v]], na.rm = TRUE),       max(fisheries[[v]], na.rm = TRUE),
                  min(prediction_grid[[v]], na.rm = TRUE), max(prediction_grid[[v]], na.rm = TRUE)))
}

# Scale the static covariates with the FIT scaling
for (v in c("distance_to_shore_km", "distance_complex_km", "closed_pct", "Avg_Depthm")) {
  prediction_grid[[paste0(v, "_scaled")]] <- scale_with_fit(prediction_grid[[v]], v)
}

# Keep only each métier's peak month
prediction_grid <- prediction_grid %>% semi_join(peak_months, by = c("key", "month"))
############

############
# BLOCK POLYGONS (for writing the maps out as spatial files)
all_blocks_sf <- st_read(blocks_path, quiet = TRUE) %>%
  select(-any_of(c("Min_Depthm", "Max_Depthm", "Avg_Depthm", "block_id"))) %>%
  rename(block_id = block_d) %>%
  st_transform(3310) %>%
  filter(blck_ty != "Offshore" & blck_st != "Oregon" & blck_st != "Washington") %>%
  mutate(block_id = as.factor(block_id)) %>%
  select(block_id)
############

############
# MODEL FILES: pa and effort both come from model_folder
suffix_regex   <- gsub(".", "\\.", model_suffix, fixed = TRUE)   # escape the dots for regex
effort_files   <- list.files(model_folder,
                             pattern = paste0("^model_.*_Depth_no_BT_abundance", suffix_regex, "$"),
                             full.names = TRUE)
model_keys     <- basename(effort_files) %>%
  str_remove("^model_") %>%
  str_remove(paste0("_Depth_no_BT_abundance", suffix_regex, "$"))

# Matching presence/absence file for a métier
pa_path <- function(k) paste0(model_folder, "model_", k, "_Depth_no_BT_pa", model_suffix)

# Load a model and drop the leftover geometry column so nothing downstream trips on it
load_model <- function(path) {
  m <- readRDS(path)
  if ("geometry" %in% names(m$data)) {
    keep   <- setdiff(names(m$data), "geometry")
    m$data <- as.data.frame(unclass(m$data)[keep], check.names = FALSE, stringsAsFactors = FALSE)
  }
  m
}

# Up-front check: every model file the loop will need, and whether it exists
file_check <- tibble(key = model_keys) %>%
  mutate(pa_file       = map_chr(key, pa_path),
         pa_exists     = file.exists(pa_file),
         effort_exists = file.exists(effort_files))
message("Model files: ", nrow(file_check), " métiers; missing pa: ", sum(!file_check$pa_exists),
        "; missing effort: ", sum(!file_check$effort_exists))
if (any(!file_check$pa_exists | !file_check$effort_exists)) {
  message("These will be skipped:")
  print(file_check %>% filter(!pa_exists | !effort_exists), n = Inf)
}

# SST direction using the same rule as your coefficient table
sst_sign <- function(m) {
  t <- tidy(m) %>% filter(term == "SST_scaled")
  case_when((t$estimate - se_mult * t$std.error) > 0 ~ "Positive",
            (t$estimate + se_mult * t$std.error) < 0 ~ "Negative",
            TRUE ~ "Neutral")
}
############

############
# MAIN LOOP: one métier at a time
run_log <- list()

for (k in model_keys) {
  
  out_all <- paste0(out_dir, k, "_SST_ALL.gpkg")
  if (file.exists(out_all)) { message("Already done, skipping: ", k); next }
  
  # Load both models; if a file is missing or unreadable, say which one and move on
  loaded <- tryCatch(list(pa   = load_model(pa_path(k)),
                          abun = load_model(effort_files[model_keys == k])),
                     error = function(e) { message("  Could not load models for ", k, ": ",
                                                   conditionMessage(e)); NULL })
  if (is.null(loaded)) next
  m_pa <- loaded$pa; m_abun <- loaded$abun
  
  signs <- c("Presence/absence" = sst_sign(m_pa), "Effort" = sst_sign(m_abun))
  if (!any(signs[sst_select] %in% c("Positive", "Negative"))) {
    message("SST neutral, skipping: ", k)
    rm(m_pa, m_abun, loaded); gc(); next
  }
  message("\n==== Predicting: ", k, " (pa SST: ", signs[1], ", effort SST: ", signs[2], ") ====")
  
  vals <- scenario_vals %>% filter(key == k)
  data_df <- m_abun$data %>% mutate(date = as.Date(date))
  
  # Most recent year in the fitted data that includes the peak month
  pk <- peak_months$month[peak_months$key == k]
  valid_year <- data_df %>%
    filter(as.integer(format(date, "%m")) == pk) %>%
    summarise(y = max(as.integer(format(date, "%Y")))) %>% pull(y)
  
  # Base grid: this métier, peak month, that year, with the fitted time index
  grid_base <- prediction_grid %>%
    filter(key == k, year == valid_year) %>%
    left_join(data_df %>% distinct(date, time_index), by = "date") %>%
    # each block's own Cold / Average / Hot SST for this peak month
    mutate(block_id = as.character(block_id)) %>%
    left_join(block_sst %>% filter(month == pk) %>% select(-month), by = "block_id") %>%
    filter(!is.na(SST_Average)) %>%                      # drop blocks with no SST record
    mutate(cost_per_gal_adjusted_scaled   = scale_with_fit(vals$fuel_median,  "cost_per_gal_adjusted"),
           cost_per_pound_adjusted_scaled = scale_with_fit(vals$pound_median, "cost_per_pound_adjusted"))
  
  # Models with a spatiotemporal field need a time index that exists in the fit
  if (any(c(m_pa$spatiotemporal, m_abun$spatiotemporal) != "off")) {
    grid_base <- grid_base %>% filter(!is.na(time_index))
  }
  if (nrow(grid_base) == 0) { message("  No grid rows for ", k, ", skipping"); rm(m_pa, m_abun, loaded); gc(); next }
  
  # Cold / Average / Hot: same grid; each block gets its OWN percentile SST (fit-scaled)
  pred_all <- map_dfr(names(temp_levels), function(temp) {
    g <- grid_base %>%
      mutate(SST_scenario = .data[[paste0("SST_", temp)]],          # raw °C used for this block
             SST_scaled   = scale_with_fit(SST_scenario, "SST"))
    pa_prob  <- m_pa$family$linkinv(predict(m_pa,   newdata = g)$est)
    abun_exp <- m_abun$family$linkinv(predict(m_abun, newdata = g)$est)
    g %>% mutate(est_pa   = pa_prob,             # probability of fishing
                 est_abun = abun_exp,            # expected hours if fished
                 est_exp  = pa_prob * abun_exp,  # expected hours (delta expectation)
                 temp     = temp)
  }) %>%
    mutate(block_id = as.factor(block_id)) %>%
    left_join(all_blocks_sf, by = "block_id") %>%
    st_as_sf()
  
  # Save combined + one file per scenario
  st_write(pred_all, out_all, delete_layer = TRUE, quiet = TRUE)
  for (temp in names(temp_levels)) {
    st_write(pred_all %>% filter(temp == !!temp),
             paste0(out_dir, k, "_SST_", toupper(temp), ".gpkg"),
             delete_layer = TRUE, quiet = TRUE)
  }
  
  run_log[[k]] <- tibble(metier = k, pa_SST = signs[1], effort_SST = signs[2],
                         peak_month = pk, year = valid_year, n_blocks = nrow(grid_base),
                         # SST varies by block: log the mean across blocks, plus the spread
                         # of the hot-minus-average gap (how unevenly blocks warm)
                         SST_cold_mean = mean(grid_base$SST_Cold),
                         SST_avg_mean  = mean(grid_base$SST_Average),
                         SST_hot_mean  = mean(grid_base$SST_Hot),
                         hot_gap_min   = min(grid_base$SST_Hot - grid_base$SST_Average),
                         hot_gap_max   = max(grid_base$SST_Hot - grid_base$SST_Average))
  message("  Saved: ", k)
  rm(m_pa, m_abun, loaded, pred_all, grid_base); gc(full = TRUE)
}

# What ran, and the block-average SST (°C) for each scenario
log_df <- bind_rows(run_log)
print(log_df, n = Inf)
log_file <- paste0(out_dir, "SST_prediction_log.csv")
write_csv(log_df, log_file, append = file.exists(log_file))
message("Done.")