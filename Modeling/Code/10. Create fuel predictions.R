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
out_dir      <- ("G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/spatial_outputs/fuel/")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
############

############
# SETTINGS
focal_term   <- "cost_per_gal_adjusted_scaled"   # fuel price
focal_select <- "Effort"   # map a métier only if fuel price is clearly Positive/Negative in its EFFORT model
se_mult      <- 1          # same rule as your coefficient table (estimate ± 1 SE excludes 0)
price_levels <- c(Low = 0.10, Average = 0.50, High = 0.90)   # fuel price quantiles for each scenario

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
# FUEL / SST / PRICE values for each métier in its peak month (raw units),
# then scaled with the FIT scaling (not the métier's own mean/SD)
scenario_vals <- fisheries %>%
  semi_join(peak_months, by = c("key", "month")) %>%
  group_by(key) %>%
  summarise(
    price_Low     = quantile(cost_per_gal_adjusted, price_levels["Low"],     na.rm = TRUE),
    price_Average = quantile(cost_per_gal_adjusted, price_levels["Average"], na.rm = TRUE),
    price_High    = quantile(cost_per_gal_adjusted, price_levels["High"],    na.rm = TRUE),
    SST_median    = median(SST, na.rm = TRUE),                       # held constant
    # held constant; NA for métiers with no ex-vessel price data (that term isn't in their model)
    pound_median  = if (all(is.na(cost_per_pound_adjusted))) NA_real_ else median(cost_per_pound_adjusted, na.rm = TRUE),
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
suffix_regex <- gsub(".", "\\.", model_suffix, fixed = TRUE)   # escape the dots for regex
effort_files <- list.files(model_folder,
                           pattern = paste0("^model_.*_Depth_no_BT_abundance", suffix_regex, "$"),
                           full.names = TRUE)
model_keys   <- basename(effort_files) %>%
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

# Direction of the focal term using the same rule as your coefficient table.
# Fuel price is in every model, but this guards against a term missing from the formula -> "Not in model"
term_sign <- function(m, term_name = focal_term) {
  t <- tidy(m) %>% filter(term == term_name)
  if (nrow(t) == 0) return("Not in model")
  case_when((t$estimate - se_mult * t$std.error) > 0 ~ "Positive",
            (t$estimate + se_mult * t$std.error) < 0 ~ "Negative",
            TRUE ~ "Neutral")
}
############

############
# MAIN LOOP: one métier at a time
run_log <- list()

for (k in model_keys) {
  
  out_all <- paste0(out_dir, k, "_fuel_ALL.gpkg")
  if (file.exists(out_all)) { message("Already done, skipping: ", k); next }
  
  # Load both models; if a file is missing or unreadable, say which one and move on
  loaded <- tryCatch(list(pa   = load_model(pa_path(k)),
                          abun = load_model(effort_files[model_keys == k])),
                     error = function(e) { message("  Could not load models for ", k, ": ",
                                                   conditionMessage(e)); NULL })
  if (is.null(loaded)) next
  m_pa <- loaded$pa; m_abun <- loaded$abun
  
  signs <- c("Presence/absence" = term_sign(m_pa), "Effort" = term_sign(m_abun))
  if (!any(signs[focal_select] %in% c("Positive", "Negative"))) {
    message("Fuel price ", tolower(signs["Effort"]), " in effort model, skipping: ", k)
    rm(m_pa, m_abun, loaded); gc(); next
  }
  message("\n==== Predicting: ", k, " (pa price: ", signs[1], ", effort price: ", signs[2], ") ====")
  
  vals <- scenario_vals %>% filter(key == k)
  data_df <- m_abun$data %>% mutate(date = as.Date(date))
  
  # Most recent year in the fitted data that includes the peak month
  pk <- peak_months$month[peak_months$key == k]
  valid_year <- data_df %>%
    filter(as.integer(format(date, "%m")) == pk) %>%
    summarise(y = max(as.integer(format(date, "%Y")))) %>% pull(y)
  
  # Base grid: this métier, peak month, that year, with the fitted time index.
  # SST and ex-vessel price held at their peak-month medians.
  grid_base <- prediction_grid %>%
    filter(key == k, year == valid_year) %>%
    left_join(data_df %>% distinct(date, time_index), by = "date") %>%
    mutate(SST_scaled                     = scale_with_fit(vals$SST_median,   "SST"),
           cost_per_pound_adjusted_scaled = scale_with_fit(vals$pound_median, "cost_per_pound_adjusted"))
  
  # Models with a spatiotemporal field need a time index that exists in the fit
  if (any(c(m_pa$spatiotemporal, m_abun$spatiotemporal) != "off")) {
    grid_base <- grid_base %>% filter(!is.na(time_index))
  }
  if (nrow(grid_base) == 0) { message("  No grid rows for ", k, ", skipping"); rm(m_pa, m_abun, loaded); gc(); next }
  
  # Low / Average / High: same grid, different (fit-scaled) fuel price
  pred_all <- map_dfr(names(price_levels), function(price) {
    g <- grid_base %>%
      mutate(cost_per_gal_adjusted_scaled =
               scale_with_fit(vals[[paste0("price_", price)]], "cost_per_gal_adjusted"))
    pa_prob  <- m_pa$family$linkinv(predict(m_pa,   newdata = g)$est)
    abun_exp <- m_abun$family$linkinv(predict(m_abun, newdata = g)$est)
    g %>% mutate(est_pa   = pa_prob,             # probability of fishing
                 est_abun = abun_exp,            # expected hours if fished
                 est_exp  = pa_prob * abun_exp,  # expected hours (delta expectation)
                 price    = price)
  }) %>%
    mutate(block_id = as.factor(block_id)) %>%
    left_join(all_blocks_sf, by = "block_id") %>%
    st_as_sf()
  
  # Save combined + one file per scenario
  st_write(pred_all, out_all, delete_layer = TRUE, quiet = TRUE)
  for (price in names(price_levels)) {
    st_write(pred_all %>% filter(price == !!price),
             paste0(out_dir, k, "_fuel_", toupper(price), ".gpkg"),
             delete_layer = TRUE, quiet = TRUE)
  }
  
  run_log[[k]] <- tibble(metier = k, pa_price = signs[1], effort_price = signs[2],
                         peak_month = pk, year = valid_year, n_blocks = nrow(grid_base),
                         price_low = vals$price_Low, price_avg = vals$price_Average,
                         price_high = vals$price_High)
  message("  Saved: ", k)
  rm(m_pa, m_abun, loaded, pred_all, grid_base); gc(full = TRUE)
}

# What ran, and the fuel prices used for each scenario (raw $/gal, adjusted)
log_df <- bind_rows(run_log)
print(log_df, n = Inf)
log_file <- paste0(out_dir, "fuel_prediction_log.csv")
write_csv(log_df, log_file, append = file.exists(log_file))
message("Done.")