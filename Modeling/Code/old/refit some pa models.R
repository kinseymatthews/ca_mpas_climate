############
# refit_pa_models.R
# Refit the presence/absence models that failed the stricter acceptance rule
# (runaway random fields) plus the two that were fit separately with iid fields,
# using ONE fallback ladder that applies to every métier:
#   1) spatial on  + spatiotemporal AR1   (full model)
#   2) spatial on  + spatiotemporal off
#   3) spatial off + spatiotemporal AR1
#   4) spatial off + spatiotemporal off   (no random fields)
# A step is accepted if: it ran, Hessian is positive definite, all sanity() checks pass,
# and every random-field SD (sigma_O, sigma_E) is < 10 on the link scale.
#
# The other 25 pa models already pass step 1 under this rule, so they don't need refitting.
# Effort models are untouched.
#
# Run on GRIT with the SAME sdmTMB version you analyze with on your laptop.
############

library(tidyverse)
library(sdmTMB)
library(sdmTMBextra)
library(sf)
library(rnaturalearth)

############
# SETUP — run whatever code you normally use to create these two objects before this script:
#   fisheries_variables : the full data set used to fit every métier
#   west_coast          : the coastline polygon used for the barrier mesh
# source("your_setup_script.R")

model_folder   <- "C:/Users/kinse/Nextcloud/bren-jsdm-bycatch/model_results/"   # edit
archive_folder <- file.path(model_folder, "archive_pre_refit")          # originals copied here first
dir.create(archive_folder, showWarnings = FALSE)
log_file       <- file.path(model_folder, "Diagnostics", "pa_refit_log.csv")
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
base_path  <- "C:/Users/kinse/Nextcloud/ca_mpas_climate/data/"

fisheries_variables<-readRDS(paste0(base_path,"fisheries_df_with_variables.rds"))


#scale variables
num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct", "Avg_Depthm")

fisheries_variables <- fisheries_variables%>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))


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


# Drop the leftover geometry column once, up front (doesn't change any rows)
if (inherits(fisheries_variables, "sf")) fisheries_variables <- st_drop_geometry(fisheries_variables)
fisheries_variables <- fisheries_variables %>% select(-any_of("geometry"))
############

############
# Models to refit (names as they appear in the saved file names)
refit_models <- c(
  # runaway random fields
  "Morro_Bay_CPFV",
  "Santa_Barbara_Ridgeback_prawn_trawl",
  "Santa_Barbara_California_halibut_trawl",
  "Santa_Barbara_Set_gillnet",
  "Los_Angeles_California_halibut_trawl",
  "San_Francisco_Squid_seine",
  "Monterey_Squid_seine"
  # previously fit separately with iid fields
 # "Fort_Bragg_California_halibut_trawl",
  #"Santa_Barbara_Cucumber_trawl"
)

# Match file-name versions to the métier values in the data
all_metiers   <- unique(fisheries_variables$metier)
metier_lookup <- set_names(all_metiers, gsub(" ", "_", all_metiers))
missing <- setdiff(refit_models, names(metier_lookup))
if (length(missing) > 0) stop("These names don't match a métier in the data: ",
                              paste(missing, collapse = ", "))
############

############
# Acceptance rule
fit_ok <- function(m, max_sigma = 10) {
  if (inherits(m, "try-error") || !isTRUE(m$sd_report$pdHess)) return(FALSE)
  if (!isTRUE(sanity(m, silent = TRUE)$all_ok)) return(FALSE)
  rp  <- tidy(m, effects = "ran_pars")
  sig <- rp$estimate[rp$term %in% c("sigma_O", "sigma_E")]
  all(sig < max_sigma)          # TRUE automatically when no fields are on
}

# Fallback ladder: returns the first fit that passes fit_ok(),
# tagged with which step it came from
fit_ladder <- function(data, formula, family, mesh) {
  ladder <- list(
    list(label = "1_full",                spatial = "on",  spatiotemporal = "ar1"),
    list(label = "2_spatiotemporal_off",  spatial = "on",  spatiotemporal = "off"),
    list(label = "3_spatial_off",         spatial = "off", spatiotemporal = "ar1"),
    list(label = "4_no_fields",           spatial = "off", spatiotemporal = "off")
  )
  
  for (rung in ladder) {
    message("  Trying ", rung$label)
    args <- list(data = data, formula = formula, family = family,
                 spatial = rung$spatial, spatiotemporal = rung$spatiotemporal)
    # Mesh and time only when a field needs them (mirrors your original code)
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
  
  # Nothing passed: keep the simplest model but flag it
  warning("No step passed all checks; returning the no-fields fit flagged for review")
  attr(m, "fit_level") <- "4_no_fields_FAILED_CHECKS"
  m
}
############

############
# Refit loop — data prep copied EXACTLY from the original fitting script,
# so these models use the same rows, time index, and covariates as the 25 you're keeping
for (nm in refit_models) {
  i <- metier_lookup[[nm]]
  message("\n==== Refitting pa model for: ", i, " ====")
  
  na_var <- c("BT_scaled", "SST_scaled")
  
  test <- fisheries_variables %>%
    filter(metier == i) %>%
    filter(if_all(all_of(na_var), ~ !is.na(.) & is.finite(.))) %>%
    arrange(date)
  
  test2 <- test %>%
    filter(total.hours != 0) %>%
    mutate(time_index = match(date, sort(unique(date))))
  
  dates <- unique(test2$month_year)
  
  test <- test %>%
    filter(month_year %in% dates) %>%
    mutate(time_index = match(date, sort(unique(date))))
  
  # Same formula-building logic as the original
  base_terms <- c("cost_per_gal_adjusted_scaled",
                  "distance_to_shore_km_scaled", "distance_complex_km_scaled",
                  "SST_scaled", "Avg_Depthm_scaled",
                  "closed_pct_scaled", "fyear", "sin12", "cos12")
  if ("cost_per_pound_adjusted_scaled" %in% colnames(test) &&
      !all(is.na(test$cost_per_pound_adjusted_scaled))) {
    base_terms <- c("cost_per_gal_adjusted_scaled",
                    "cost_per_pound_adjusted_scaled",
                    base_terms[-1])
  }
  absence_formula <- as.formula(paste("present ~", paste(base_terms, collapse = " + ")))
  
  # Same mesh settings as the original
  mesh  <- make_mesh(test, xy_cols = c("block_long_dd", "block_lat_dd"), cutoff = 10)
  mesh1 <- sdmTMBextra::add_barrier_mesh(mesh, west_coast,
                                         range_fraction = 0.02, proj_scaling = 1000,
                                         plot = FALSE)
  
  # Fit through the ladder
  model1 <- fit_ladder(test, absence_formula, binomial(link = "logit"), mesh1)
  
  # Back up the original, then save the refit under the SAME file name,
  # so the diagnostics scripts pick it up automatically
  out_file <- file.path(model_folder,
                        paste0("model_", nm, "_Depth_no_BT_pa_delta_updated2026.rds"))
  if (file.exists(out_file)) file.copy(out_file, archive_folder, overwrite = FALSE)
  saveRDS(model1, out_file)
  
  # Log which step each model landed on, plus the key parameters
  rp <- tryCatch(tidy(model1, effects = "ran_pars"),
                 error = function(e) tibble(term = character(), estimate = numeric()))
  fx <- tidy(model1) %>% filter(term != "(Intercept)")
  log_row <- tibble(
    model        = nm,
    fit_level    = attr(model1, "fit_level"),
    sigma_O      = rp$estimate[rp$term == "sigma_O"][1],
    sigma_E      = rp$estimate[rp$term == "sigma_E"][1],
    max_coef     = fx$term[which.max(abs(fx$estimate))],
    max_abs_coef = max(abs(fx$estimate)),
    time         = format(Sys.time())
  )
  write_csv(log_row, log_file, append = file.exists(log_file))
  
  rm(model1, test, test2, mesh, mesh1); gc()
}

message("\nDone. Fit levels:")
print(read_csv(log_file, show_col_types = FALSE))
############