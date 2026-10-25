############
# run_diagnostics.R
# QQ plots + marginal/conditional R² for the pa and effort sdmTMB models
# Memory-safe version: loads ONE model at a time, extracts residuals + R²,
# saves a small checkpoint file, then frees the memory before the next model.
# Laptop version. BEFORE RUNNING: restart R with a clean workspace
# (Session > Restart R, with "Restore .RData" turned off) so nothing else is using memory.
############

library(tidyverse)
library(sdmTMB)
library(ragg)
library(sf)   # registers how tibbles should handle sf geometry columns (backup safety)

############
# PATHS 


model_folder <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/")
qq_folder <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/figures/QQ_plots/")

# Clear anything left over and report free memory before starting
rm(list = setdiff(ls(), "model_folder")); gc()
diag_folder  <- file.path(model_folder, "Diagnostics")
ckpt_folder  <- file.path(diag_folder, "checkpoints")              # one small .rds per model

dir.create(ckpt_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(qq_folder,   recursive = TRUE, showWarnings = FALSE)
############

############
# Base theme (same as your main script)
base_theme <- theme(axis.text = element_text(size = 7),
                    axis.title = element_text(size = 8),
                    legend.text = element_text(size = 8),
                    legend.title = element_text(size = 9),
                    strip.text = element_text(size = 10),
                    plot.tag = element_text(size = 9),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    legend.key = element_rect(fill = NA, color = NA),
                    legend.background = element_rect(fill = alpha("blue", 0)))
############

############
# Build a "job table": one row per model file, with its response type and delta component.
# Only file PATHS are stored here — nothing heavy is loaded yet.
pa_suffix <- "_Depth_no_BT_pa_delta_updated2026.rds$"
ab_suffix <- "_Depth_no_BT_abundance_delta_updated2026.rds$"

jobs <- bind_rows(
  tibble(path = list.files(model_folder, pattern = paste0("^model_.*", pa_suffix), full.names = TRUE),
         response = "Presence/absence", delta_component = 1, suffix = pa_suffix),
  tibble(path = list.files(model_folder, pattern = paste0("^model_.*", ab_suffix), full.names = TRUE),
         response = "Effort", delta_component = 2, suffix = ab_suffix)
) %>%
  mutate(model = str_remove(basename(path), "^model_") %>% str_remove(suffix),
         # Checkpoint file name, e.g. checkpoints/San_Diego_Lobster_trap__Effort.rds
         ckpt  = file.path(ckpt_folder, paste0(model, "__", str_replace_all(response, "[/ ]", "_"), ".rds")))

message("Found ", sum(jobs$response == "Presence/absence"), " pa models and ",
        sum(jobs$response == "Effort"), " effort models")
############

############
# R² for one model (your current version, unchanged in logic)
# Marginal = fixed effects only; conditional = fixed + spatial/spatiotemporal fields.
# Computed on the link scale for the delta component matching the response.
compute_R2 <- function(m, nm, response_label, delta_component) {
  is_delta <- length(m$family$family) == 2
  comp     <- if (is_delta) delta_component else 1
  fam      <- m$family$family[comp]
  suffix   <- if (is_delta) comp else ""
  
  # Fixed-effect variance: linear predictor WITHOUT random fields
  pred  <- predict(m)
  f_est <- pred[[paste0("est_non_rf", suffix)]]
  
  # Positive component is only fit to non-zero rows
  if (comp == 2) {
    y     <- m$data[[all.vars(m$formula[[1]])[1]]]
    f_est <- f_est[y > 0]
  }
  sigma2_f <- var(f_est, na.rm = TRUE)
  
  # Random-field variance from estimated SDs (0 if the field is off)
  rp      <- tidy(m, effects = "ran_pars", model = comp)
  get_par <- function(term) { v <- rp$estimate[rp$term == term]; if (length(v) == 0) 0 else v[1] }
  sigma2_r <- get_par("sigma_O")^2 + get_par("sigma_E")^2
  
  # Observation-level variance on the link scale
  sigma2_d <- switch(fam,
                     binomial  = pi^2 / 3,
                     Gamma     = trigamma(get_par("phi")),
                     lognormal = get_par("phi")^2,
                     { message("No R² formula for family ", fam, " (", nm, ")"); NA_real_ })
  
  total <- sigma2_f + sigma2_r + sigma2_d
  tibble(model = nm, response = response_label, family = fam,
         R2_marginal    = sigma2_f / total,
         R2_conditional = (sigma2_f + sigma2_r) / total,
         sigma2_fixed = sigma2_f, sigma2_random = sigma2_r, sigma2_distribution = sigma2_d)
}
############

############
# Process ONE model: load -> residuals + R² -> save checkpoint -> free memory
process_one <- function(path, model, response, delta_component, ckpt) {
  
  # Skip models already done (lets you resubmit after a crash without redoing work)
  if (file.exists(ckpt)) {
    message("Already done, skipping: ", model, " (", response, ")")
    return(invisible(NULL))
  }
  
  message(format(Sys.time(), "%H:%M:%S"), "  Loading ", model, " (", response, ")")
  m <- readRDS(path)
  
  # Drop the leftover sf geometry column: the model never uses it, and it breaks
  # residuals() when sf isn't loaded (tibbles can't hold an unrecognized sfc column)
  if ("geometry" %in% names(m$data)) {
    # unclass() turns the tibble into a plain list, so we can drop the column with base R
    # without tibble/vctrs ever inspecting the geometry column
    keep   <- setdiff(names(m$data), "geometry")
    m$data <- as.data.frame(unclass(m$data)[keep], check.names = FALSE, stringsAsFactors = FALSE)
  }
  
  is_delta <- length(m$family$family) == 2
  comp     <- if (is_delta) delta_component else 1
  
  # Residuals: seed set PER MODEL so results don't depend on processing order
  set.seed(2026)
  r <- tryCatch(residuals(m, type = "mle-mvn", model = comp),
                error = function(e) { message("  Residuals failed: ", conditionMessage(e)); NULL })
  resid_tbl <- if (!is.null(r) && sum(is.finite(r)) >= 5) {
    tibble(model = model, response = response, resid = r[is.finite(r)])
  } else NULL
  
  # R²
  r2_tbl <- tryCatch(compute_R2(m, model, response, delta_component),
                     error = function(e) {
                       message("  R² failed: ", conditionMessage(e))
                       tibble(model = model, response = response)
                     })
  
  # Save the small outputs, then drop the big model before the next one loads
  saveRDS(list(resid = resid_tbl, R2 = r2_tbl), ckpt)
  rm(m, r); gc()
  message("  Done: ", model, " (", response, ")")
  invisible(NULL)
}

# Run sequentially — only one model in memory at any time
pwalk(select(jobs, path, model, response, delta_component, ckpt), process_one)
############

############
# Combine checkpoints (all small) into results tables
ckpts    <- map(jobs$ckpt[file.exists(jobs$ckpt)], readRDS)
resid_df <- map_dfr(ckpts, "resid")
R2_df    <- map_dfr(ckpts, "R2")

# Split model name into port complex + fishery using the port-complex PREFIX
# (avoids needing a separate fishery_list object)
port_complexes <- c("Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco",
                    "Monterey", "Morro_Bay", "Santa_Barbara", "Los_Angeles", "San_Diego")

add_metier_labels <- function(df) {
  df %>%
    mutate(
      port_raw = map_chr(model, function(x) {
        pc <- port_complexes[startsWith(x, port_complexes)]
        if (length(pc) == 0) NA_character_ else pc[1]
      }),
      fishery      = str_remove(model, paste0("^", port_raw, "_")) %>% str_replace_all("_", " "),
      port.complex = factor(str_replace_all(port_raw, "_", " "),
                            levels = str_replace_all(port_complexes, "_", " ")),
      response     = factor(response, levels = c("Presence/absence", "Effort"))
    ) %>%
    select(-port_raw)
}

resid_df <- add_metier_labels(resid_df)
R2_df    <- add_metier_labels(R2_df) %>% arrange(response, fishery, port.complex)

# Any models that didn't match a port complex? Should print 0 rows
print(resid_df %>% filter(is.na(port.complex)) %>% distinct(model))

# Save the combined tables — these are what you pull back to your laptop
saveRDS(resid_df, file.path(diag_folder, "residuals_pa_and_effort.rds"))
write_csv(R2_df,  file.path(diag_folder, "R2_marginal_conditional_pa_and_effort.csv"))
############

############
# QQ plots: one figure per fishery, rows = port complexes, columns = pa vs effort
for (f in unique(resid_df$fishery)) {
  d       <- resid_df %>% filter(fishery == f)
  n_ports <- n_distinct(d$port.complex)
  
  p_qq <- ggplot(d, aes(sample = resid)) +
    stat_qq(size = 0.6, alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1, color = "red") +   # 1:1 line = perfect standard normal
    facet_grid(port.complex ~ response, scales = "free") +
    labs(x = "Theoretical quantiles", y = "Sample quantiles", title = f) +
    base_theme +
    theme(axis.text  = element_text(size = 9),
          axis.title = element_text(size = 11),
          strip.text = element_text(size = 10),
          plot.title = element_text(size = 12))
  
  ggsave(file.path(qq_folder, paste0(gsub(" ", "_", f), "_QQplots_supplement.png")),
         p_qq, width = 7, height = 1.5 + 2.2 * n_ports, units = "in", dpi = 300,
         device = ragg::agg_png, limitsize = FALSE)
  message("Saved QQ plots: ", f)
}

message("All diagnostics finished.")
############