
library(tidyverse)
library(sf)
library(patchwork)

############
# PATHS (local computer)
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"

# Must be the same folder exvessel_predictions_local.R wrote to
in_dir    <- ("G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/spatial_outputs/fuel/")
fig_dir   <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/figures/spatial_predictions/fuel/")
# Shiny copies go straight into the app's www/ folder
shiny_dir <- "C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/www/"
# Manuscript figure (new name, so the old spatial_predictions_exvessel.jpg is kept)
manuscript_file <- paste0(base_path, "All Fisheries Figures/spatial_predictions_fuel_oct2026.jpg")

dir.create(fig_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(shiny_dir, recursive = TRUE, showWarnings = FALSE)

fig_suffix <- "_fuel_oct2026"   # every figure name ends in _oct2026
############

############
# Basemap layers
usa    <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")
usa_mexico_sf_proj <- bind_rows(usa, mexico) %>% st_transform(3310)

# State waters
state_waters <- readRDS(paste0(base_path, "All Fisheries Data/CA_state_waters_polyline.Rds")) %>%
  st_transform(3310)

# MPAs, unioned so they show as one legend entry
mpa_blocks <- wcfish::mpas_ca %>%
  st_transform(3310) %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  st_union() %>%
  st_make_valid()

# Major port of each complex (red star)
ports <- read_sf(paste0(base_path, "Variables/Distance to Port Complex/Data/Raw/port codes_updated_April2025.shp")) %>%
  rename(port = PortName) %>%
  mutate(port = str_to_title(port)) %>%
  filter(port %in% c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", "Monterey",
                     "Morro Bay", "Santa Barbara Harbor", "Los Angeles", "San Diego")) %>%
  mutate(MajorPort = str_to_title(MajorPort)) %>%
  rename(port.complex = MajorPort) %>%
  st_transform(3310)

known_ports <- c("Fort Bragg", "Los Angeles", "Morro Bay", "San Diego",
                 "San Francisco", "Santa Barbara", "Eureka", "Monterey", "Bodega Bay")
############

############
# Which métiers have predictions (file names use underscores)
files_all   <- list.files(in_dir, pattern = "_fuel_ALL\\.gpkg$")
all_metiers <- str_remove(files_all, "_fuel_ALL\\.gpkg$")
message(length(all_metiers), " métiers with fuel prediction files:")
print(gsub("_", " ", all_metiers))

# Log from the prediction script: price direction per model and the Low/Avg/High prices used ($/gal)
log_file <- paste0(in_dir, "fuel_prediction_log.csv")
if (file.exists(log_file)) print(read_csv(log_file, show_col_types = FALSE), n = Inf)
############

############
# Shiny file name, e.g. "SquidSeine_SanFrancisco"
clean_met_name <- function(met) {
  port       <- known_ports[sapply(known_ports, function(p) startsWith(met, p))]
  fishery    <- str_trim(str_remove(met, fixed(port)))
  port_clean <- str_remove_all(port, " ")
  to_camel <- function(x) {
    words <- str_split(x, " ")[[1]]
    words <- sapply(words, function(w) {
      if (w == toupper(w) && nchar(w) > 1) w   # keep CPFV as-is
      else paste0(toupper(substr(w, 1, 1)), tolower(substr(w, 2, nchar(w))))
    })
    paste(words, collapse = "")
  }
  paste0(to_camel(fishery), "_", port_clean)
}
############

############
plot_list <- list()

for (met_file in all_metiers) {
  
  met <- gsub("_", " ", met_file)   # label with spaces: ports, titles, plot_list
  message("Plotting: ", met)
  
  # ---- LOAD COMBINED (skip unreadable files instead of stopping) ----
  pred_all <- tryCatch(st_read(paste0(in_dir, met_file, "_fuel_ALL.gpkg"), quiet = TRUE),
                       error = function(e) { message("  Could not read: ", conditionMessage(e)); NULL })
  if (is.null(pred_all)) next
  
  # Skip leftovers from the old prediction script (métiers the new models no longer
  # flag for fuel price never get overwritten, so their old files stay in the folder)
  if (!"pred_version" %in% names(pred_all)) {
    message("  Old output (not from the updated models), skipping")
    next
  }
  
  pred_all <- pred_all %>%
    filter(!st_is_empty(.)) %>%
    mutate(price = factor(price, levels = c("Low", "Average", "High")))
  
  # ---- FACET MAP: Low / Average / High predicted effort ----
  p_main <- ggplot(pred_all) +
    geom_sf(aes(fill = est_exp), color = NA) +
    scale_fill_viridis_c(trans = "sqrt", name = "Predicted effort (sqrt)") +
    facet_wrap(~ price) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = FALSE) +
    geom_sf(data = state_waters, color = "grey30", linewidth = 0.2, inherit.aes = FALSE) +
    coord_sf(xlim = c(-450000, 350000), ylim = c(-600000, 450000)) +
    theme_bw() + base_theme +
    ggtitle(met)
  
  ggsave(paste0(fig_dir, met_file, "_low_avg_high", fig_suffix, ".jpg"),
         p_main, width = 10, height = 6, dpi = 300)
  
  # ---- DIFFERENCES FROM AVERAGE, block by block ----
  wide <- pred_all %>%
    st_drop_geometry() %>%
    select(block_id, price, est_exp) %>%
    pivot_wider(names_from = price, values_from = est_exp, values_fn = mean) %>%
    mutate(diff_high_avg = High - Average,
           diff_low_avg  = Low  - Average)
  
  block_geom <- pred_all %>% filter(price == "Average") %>% select(block_id)   # one polygon per block
  
  diff_high <- block_geom %>% left_join(wide, by = "block_id") %>%
    mutate(comparison = "High - Average", diff = diff_high_avg)
  diff_low  <- block_geom %>% left_join(wide, by = "block_id") %>%
    mutate(comparison = "Low - Average",  diff = diff_low_avg)
  
  # Single-difference maps (whole coast)
  plot_diff <- function(d, title) {
    ggplot(d) +
      geom_sf(aes(fill = diff), color = NA) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                           name = "Predicted difference") +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
      geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = FALSE) +
      geom_sf(data = state_waters, color = "grey30", linewidth = 0.2, inherit.aes = FALSE) +
      coord_sf(xlim = c(-450000, 350000), ylim = c(-600000, 450000)) +
      theme_bw() + base_theme +
      ggtitle(title)
  }
  ggsave(paste0(fig_dir, met_file, "_high_minus_avg", fig_suffix, ".jpg"),
         plot_diff(diff_high, paste(met, "— High minus Average fuel price")),
         width = 10, height = 6, dpi = 300)
  ggsave(paste0(fig_dir, met_file, "_low_minus_avg", fig_suffix, ".jpg"),
         plot_diff(diff_low, paste(met, "— Low minus Average fuel price")),
         width = 10, height = 6, dpi = 300)
  
  # ---- SIDE BY SIDE, zoomed to the blocks that change most ----
  diff_all <- bind_rows(diff_high, diff_low) %>%
    mutate(comparison = factor(comparison, levels = c("Low - Average", "High - Average")))
  
  # Keep the top 20% of blocks by size of change
  threshold <- quantile(abs(diff_all$diff), 0.8, na.rm = TRUE)
  diff_all  <- diff_all %>% filter(abs(diff) > threshold)
  if (nrow(diff_all) == 0) { message("  No blocks above threshold, skipping zoomed map"); next }
  
  bb <- st_bbox(diff_all)   # zoom window
  
  met_port       <- known_ports[sapply(known_ports, function(p) startsWith(met, p))]
  ports_filtered <- ports %>% filter(port.complex %in% met_port)
  
  p_diff_combined <- ggplot(diff_all) +
    geom_sf(aes(fill = diff), color = "darkgrey", linewidth = 0.1) +
    facet_wrap(~ comparison, ncol = 2) +
    scale_fill_gradient2(low = "blue", mid = "gray96", high = "red", midpoint = 0,
                         name = "Predicted difference\nin relative effort") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = FALSE) +
    geom_sf(data = state_waters, color = "grey30", linewidth = 0.2, inherit.aes = FALSE) +
    # MPAs on their own colour scale so they get a separate legend entry
    geom_sf(data = mpa_blocks, aes(colour = "Marine Protected Area"), fill = "black", linewidth = 0.2) +
    scale_colour_manual(values = c("Marine Protected Area" = "black"), name = NULL,
                        guide = guide_legend(override.aes = list(fill = "black", colour = "black"))) +
    geom_sf(data = ports_filtered, shape = 8, color = "red", size = 3, stroke = 1.2, inherit.aes = FALSE) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
    scale_x_continuous(breaks = seq(-180, 180, by = 2)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 1)) +
    theme_bw() + base_theme
  
  plot_list[[met]] <- p_diff_combined   # keyed by label with spaces, e.g. "San Francisco Squid seine"
  
  ggsave(paste0(fig_dir, met_file, "_differences_side_by_side", fig_suffix, ".jpg"),
         p_diff_combined, width = 12, height = 6, dpi = 300)
  
  # Shiny copy (saved directly into the app's www/ folder)
  ggsave(paste0(shiny_dir, "predictions_", clean_met_name(met), "_fuel_differences_oct2026.jpg"),
         p_diff_combined, width = 12, height = 6, dpi = 300)
  
  gc()
}
############

############
# MANUSCRIPT FIGURE (fuel price): San Francisco squid seine
#
# - shows the top-20% blocks from the loop (incl. near-zero "gray" blocks)
# - map window set in longitude/latitude (edit box_ll to zoom or shift)
# - signed log colour scale so moderate changes still show colour
############

print(names(plot_list))   # métiers available

fig_metier <- "San Francisco Squid seine"

# Stop with a clear message if this métier wasn't mapped
# (e.g., its fuel price is no longer clearly Positive/Negative in the updated models)
if (!fig_metier %in% names(plot_list)) stop("No fuel map for ", fig_metier,
                                            ". Choose from: ", paste(names(plot_list), collapse = ", "))

# Map window in degrees (edit to zoom in/out or shift)
box_ll <- st_bbox(c(xmin = -124.5, xmax = -120.5,    # longitude: west, east
                    ymin =   35.3, ymax =   39.6),   # latitude: south, north
                  crs = 4326)
box <- st_bbox(st_transform(st_as_sfc(box_ll), 3310))   # convert to the map's projection (metres)

# Shared styling: legend order, text sizes, spacing between the Low and High facets
style_panel <- function(p) {
  p +
    guides(fill   = guide_colorbar(order = 1, ticks.colour = "black",
                                   frame.colour = "black", frame.linewidth = 0.2),
           colour = guide_legend(order = 2, override.aes = list(fill = "black", colour = "black"))) +
    theme(axis.text       = element_text(size = 10),
          plot.tag        = element_text(face = "bold", size = 14),
          panel.spacing.x = unit(1.2, "lines"),
          legend.box      = "vertical",
          legend.box.just = "left",
          legend.position = "right")
}

# Build the panel
p <- plot_list[[fig_metier]]
d <- p$data                                   # top-20% blocks for this métier

# Signed log colour scale, symmetric around 0
core <- d %>%                                 # blocks carrying 95% of the change (sets where log scaling starts)
  arrange(comparison, desc(abs(diff))) %>%
  group_by(comparison) %>%
  mutate(cum_share = cumsum(abs(diff)) / sum(abs(diff))) %>%
  filter(lag(cum_share, default = 0) < 0.95) %>%
  ungroup()
sigma <- quantile(abs(core$diff), 0.25, na.rm = TRUE)
lim   <- max(abs(d$diff), na.rm = TRUE)

fuel_panel <- style_panel(p) +
  scale_fill_gradient2(low = "blue", mid = "gray96", high = "red", midpoint = 0,
                       trans  = scales::pseudo_log_trans(sigma = sigma),
                       limits = c(-lim, lim),
                       breaks = signif(c(-lim, -lim / 10, 0, lim / 10, lim), 1),   # evenly spaced on a log scale
                       labels = function(x) format(x, drop0trailing = TRUE),
                       oob    = scales::squish,
                       name   = "Predicted difference\nin relative effort") +
  coord_sf(xlim = c(box[["xmin"]], box[["xmax"]]),
           ylim = c(box[["ymin"]], box[["ymax"]]),
           expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, by = 1)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 0.5))

fuel_panel   # preview in RStudio

ggsave(manuscript_file, fuel_panel, width = 8.5, height = 5, dpi = 300)

# Fuel prices behind the panel, for the caption ($/gal)
if (file.exists(log_file)) {
  read_csv(log_file, show_col_types = FALSE) %>%
    mutate(label = gsub("_", " ", metier)) %>%
    filter(label == fig_metier) %>%
    select(label, peak_month, year, price_low, price_avg, price_high) %>%
    mutate(across(starts_with("price"), ~ round(.x, 2)))
}
############
