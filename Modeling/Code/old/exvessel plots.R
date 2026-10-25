library(tidyverse)
library(sf)
library(patchwork)
############
# Plot ex-vessel price spatial predictions (Low / Average / High price)
# Reads <métier>_exvessel_ALL.gpkg from GRIT and calculates High - Average and
# Low - Average here (the GRIT script doesn't save separate difference files).
############
base_path  <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
next_cloud <- "C:/Users/kinse/Nextcloud/bren-jsdm-bycatch/model_results/"

# base theme
base_theme <- theme(axis.text = element_text(size = 9),
                    axis.title = element_text(size = 8),
                    legend.text = element_text(size = 8),
                    legend.title = element_text(size = 10),
                    strip.text = element_text(size = 12),
                    plot.tag = element_text(size = 9),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    legend.key = element_rect(fill = NA, color = NA),
                    legend.background = element_rect(fill = alpha("blue", 0)))

############
# Basemap layers
usa    <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")
usa_mexico_sf_proj <- bind_rows(usa, mexico) %>% st_transform(3310)

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
# Folders
in_dir  <- paste0(next_cloud, "spatial_outputs/exvessel/")
fig_dir <- paste0(base_path, "Modeling/figures/spatial_predictions/exvessel/")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
shiny_dir <- "C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/www/"

fig_suffix <- "_exvessel_oct2026"   # change to keep new figures from overwriting older ones
############

############
# Which métiers have predictions (file names use underscores)
files_all   <- list.files(in_dir, pattern = "_exvessel_ALL\\.gpkg$")
all_metiers <- str_remove(files_all, "_exvessel_ALL\\.gpkg$")
message(length(all_metiers), " métiers with ex-vessel predictions:")
print(gsub("_", " ", all_metiers))

# Log from GRIT: price direction per model and the Low/Avg/High prices used ($/lb)
log_file <- paste0(in_dir, "exvessel_prediction_log.csv")
if (file.exists(log_file)) print(read_csv(log_file, show_col_types = FALSE), n = Inf)
############

############
# Shiny file name, e.g. "LobsterTrap_SantaBarbara"
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
  
  # ---- LOAD COMBINED ----
  pred_all <- st_read(paste0(in_dir, met_file, "_exvessel_ALL.gpkg"), quiet = TRUE) %>%
    filter(!st_is_empty(.)) %>%
    mutate(price = factor(price, levels = c("Low", "Average", "High")))
  
  # ---- FACET MAP: Low / Average / High ----
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
         plot_diff(diff_high, paste(met, "— High minus Average ex-vessel price")),
         width = 10, height = 6, dpi = 300)
  ggsave(paste0(fig_dir, met_file, "_low_minus_avg", fig_suffix, ".jpg"),
         plot_diff(diff_low, paste(met, "— Low minus Average ex-vessel price")),
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
  
  plot_list[[met]] <- p_diff_combined   # keyed by label with spaces, e.g. "Santa Barbara Lobster trap"
  
  ggsave(paste0(fig_dir, met_file, "_differences_side_by_side", fig_suffix, ".jpg"),
         p_diff_combined, width = 12, height = 6, dpi = 300)
  
  # Shiny copy
  if (dir.exists(shiny_dir)) {
    ggsave(paste0(shiny_dir, "predictions_", clean_met_name(met), "_exvessel_differences_oct2026.jpg"),
           p_diff_combined, width = 12, height = 6, dpi = 300)
  }
  
  gc()
}
############

############
# MANUSCRIPT FIGURE: pick métiers from names(plot_list)
names(plot_list)

fig_metiers <- character(0)   # e.g. c("Santa Barbara Lobster trap", "Los Angeles Squid seine")

style_panel <- function(p) {
  p +
    guides(fill   = guide_colorbar(order = 1, ticks.colour = "black",
                                   frame.colour = "black", frame.linewidth = 0.2),
           colour = guide_legend(order = 2, override.aes = list(fill = "black", colour = "black"))) +
    theme(axis.text = element_text(size = 12),
          plot.tag  = element_text(face = "bold", size = 14),
          legend.box = "vertical", legend.box.just = "left", legend.position = "right")
}

if (length(fig_metiers) > 0) {
  combined_plot <- wrap_plots(map(plot_list[fig_metiers], style_panel), ncol = 1) +
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
  combined_plot
  
  ggsave(paste0(base_path, "All Fisheries Figures/spatial_predictions_exvessel_oct2026.jpg"),
         combined_plot, width = 7, height = 10 * length(fig_metiers) / 3, dpi = 300)
}
############