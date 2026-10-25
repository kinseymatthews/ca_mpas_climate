############
# model_structure_check.R
# For every pa and effort model: which random fields are on, the field parameters,
# convergence checks, and the largest coefficient. Loads ONE model at a time.
# Progress is saved after every model, so if it stops you can rerun and it resumes.
############

library(tidyverse)
library(sdmTMB)
library(sf)   # lets R handle the leftover geometry column if it gets touched

############
# PATHS
model_folder <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/")

out_file     <- file.path(model_folder, "Diagnostics", "model_structure_check.csv")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

# All 64 updated models (pa + effort)
files <- list.files(model_folder,
                    pattern = "^model_.*_Depth_no_BT_(pa|abundance)_delta_updated2026\\.rds$",
                    full.names = TRUE)
message("Found ", length(files), " model files")
############

############
# Helper: pull one random-effect parameter from tidy(); NA if that field is off
get_par <- function(rp, term) {
  v <- rp$estimate[rp$term == term]
  if (length(v) == 0) NA_real_ else v[1]
}

# Inspect ONE model, return a one-row summary, then free the memory
check_one <- function(path) {
  nm <- basename(path)
  message(format(Sys.time(), "%H:%M:%S"), "  Checking ", nm)
  m <- readRDS(path)
  
  # Random-effect parameters (sigma_O, sigma_E, range, rho for AR1)
  rp <- tryCatch(tidy(m, effects = "ran_pars"),
                 error = function(e) tibble(term = character(), estimate = numeric()))
  
  # Fixed effects, excluding the intercept, to find the largest coefficient
  fx     <- tidy(m)
  fx_noi <- fx %>% filter(term != "(Intercept)")
  
  # sanity() summary: all_ok is TRUE only if every check passes
  s <- tryCatch(sanity(m, silent = TRUE), error = function(e) list(all_ok = NA))
  
  res <- tibble(
    file           = nm,
    model          = nm %>% str_remove("^model_") %>% str_remove("_Depth_no_BT_.*$"),
    response       = if_else(str_detect(nm, "_pa_"), "Presence/absence", "Effort"),
    family         = paste(m$family$family, collapse = "/"),
    spatial        = paste(m$spatial, collapse = "/"),          # "on" / "off"
    spatiotemporal = paste(m$spatiotemporal, collapse = "/"),   # "ar1" / "off"
    pdHess         = isTRUE(m$sd_report$pdHess),                # what your fallback rule checked
    sanity_all_ok  = isTRUE(s$all_ok),                          # stricter: includes huge-sigma checks
    sigma_O        = get_par(rp, "sigma_O"),
    sigma_E        = get_par(rp, "sigma_E"),
    range          = get_par(rp, "range"),
    rho            = get_par(rp, "rho"),
    intercept      = fx$estimate[fx$term == "(Intercept)"][1],
    max_coef_term  = fx_noi$term[which.max(abs(fx_noi$estimate))],
    max_abs_coef   = max(abs(fx_noi$estimate))
  )
  
  rm(m); gc()   # free memory before the next model
  res
}
############

############
# Run, skipping models already recorded (resume-safe)
done <- if (file.exists(out_file)) read_csv(out_file, show_col_types = FALSE)$file else character()
todo <- files[!basename(files) %in% done]
message(length(done), " already done, ", length(todo), " to go")

for (f in todo) {
  row <- tryCatch(check_one(f),
                  error = function(e) {
                    message("  Failed: ", conditionMessage(e))
                    tibble(file = basename(f))
                  })
  # Append this model's row to the CSV right away
  write_csv(row, out_file, append = file.exists(out_file))
}
############

############
# Summaries
structure_df <- read_csv(out_file, show_col_types = FALSE)

# 1) Which random-field structures exist, by response
structure_df %>% count(response, spatial, spatiotemporal)

# 2) Models that passed your Hessian check but fail sanity() or have a runaway field.
#    These are the ones your fallback rule let through.
structure_df %>%
  filter(pdHess, !sanity_all_ok | sigma_O > 5 | max_abs_coef > 10) %>%
  select(model, response, spatial, spatiotemporal, sigma_O, sigma_E,
         max_coef_term, max_abs_coef, sanity_all_ok) %>%
  arrange(response, desc(sigma_O))
############
