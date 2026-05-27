library(tidyverse)
library(sf)
library(pROC)
library(patchwork)
############ 
# Plot SST spatial predictions
############ 
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"

# base theme
base_theme <- theme(axis.text=element_text(size=9),
                    # axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=10),
                    strip.text = element_text(size=12),
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

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
usa_mexico_sf_proj<- st_transform(usa_mexico_sf, crs = 3310)
# Union all geometries into one polygon
usa_mexico_union <- st_union(usa_mexico_sf_proj)

# State waters
state_waters <- readRDS( paste0(base_path,"All Fisheries Data/CA_state_waters_polyline.Rds"))
state_waters <- st_transform(state_waters, crs = 3310)


#mpas
mpa_blocks<-wcfish::mpas_ca
mpa_blocks<- st_transform(mpa_blocks, crs = 3310)%>%
  st_make_valid()
mpa_blocks <- st_cast(mpa_blocks, "POLYGON")
# Union the MPAs so they are treated as a single shape
mpa_blocks <- st_union(mpa_blocks)
mpa_blocks <- st_make_valid(mpa_blocks)


in_dir  <- paste0(base_path, "Modeling/spatial_outputs/SST/")
fig_dir <- paste0(base_path, "Modeling/figures/spatial_predictions/SST/")
out_fig_dir <- paste0(base_path,
                      "Modeling/figures/kernel_contours/SST/")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
# ---- automatically detect metiers from saved files ----
files_all <- list.files(in_dir,
                        pattern = "_SST_ALL.gpkg$",
                        full.names = FALSE)

all_metiers <- str_remove(files_all, "_SST_ALL.gpkg")

plot_list <- list()

clean_met_name <- function(met) {
  known_ports <- c("Fort Bragg", "Los Angeles", "Morro Bay", "San Diego",
                   "San Francisco", "Santa Barbara", "Eureka", "Monterey",
                   "Bodega Bay")
  
  port      <- known_ports[sapply(known_ports, function(p) startsWith(met, p))]
  fishery   <- str_trim(str_remove(met, fixed(port)))
  port_clean <- str_remove_all(port, " ")  # SanFrancisco, FortBragg, etc.
  
  # CamelCase each word, but preserve all-caps words like CPFV
  to_camel <- function(x) {
    words <- str_split(x, " ")[[1]]
    words <- sapply(words, function(w) {
      if (w == toupper(w) && nchar(w) > 1) w  # keep CPFV as-is
      else paste0(toupper(substr(w, 1, 1)), tolower(substr(w, 2, nchar(w))))
    })
    paste(words, collapse = "")
  }
  
  paste0(to_camel(fishery), "_", port_clean)
}

for (met in all_metiers) {
  
  message("Plotting: ", met)
  
  # ---- LOAD COMBINED ----
  pred_all <- st_read(
    paste0(in_dir, met, "_SST_ALL.gpkg"),
    quiet = TRUE
  )
  
  # ensure correct order
  pred_all$temp <- factor(pred_all$temp,
                          levels = c("Cold", "Average", "Hot"))
  
  # ---- FACET MAP ----
  p_main <- ggplot(pred_all) +
    geom_sf(aes(fill = est_exp), color = NA) +
    scale_fill_viridis_c(trans = "sqrt",
                         name = "Predicted effort (sqrt)") +
    facet_wrap(~ temp) +
    #add black outline around density raster
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2))  +
    geom_sf(data = usa_mexico_sf_proj,
            fill = "grey85",
            col = "white",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    geom_sf(data = state_waters,
            color = "grey30",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    coord_sf(xlim = c(-450000, 350000),
             ylim = c(-600000, 450000)) +
    theme_bw() + base_theme +
    ggtitle(met)
  
  ggsave(
    paste0(fig_dir, met, "_SST_cold_avg_hot.jpg"),
    p_main,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- HOT - AVG ----
  diff_hot <- st_read(
    paste0(in_dir, met, "_SST_HOT_minus_AVERAGE.gpkg"),
    quiet = TRUE
  ) %>%
    mutate(
      comparison = "Hot - Average",
      diff = diff_warm_avg
    )
  
  
  p_hot <- ggplot(diff_hot) +
    geom_sf(aes(fill = diff), color = NA) +
    scale_fill_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 0,
                         name = "Predicted difference") +
    #add black outline around density raster
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2))  +
    geom_sf(data = usa_mexico_sf_proj,
            fill = "grey85",
            col = "white",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    geom_sf(data = state_waters,
            color = "grey30",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    coord_sf(xlim = c(-450000, 350000),
             ylim = c(-600000, 450000)) +
    theme_bw() + base_theme +
    ggtitle(paste(met, "— Hot minus Average SST"))
  
  ggsave(
    paste0(fig_dir, met, "_SST_hot_minus_avg.jpg"),
    p_hot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- COLD - AVG ----
  diff_cold <- st_read(
    paste0(in_dir, met, "_SST_COLD_minus_AVERAGE.gpkg"),
    quiet = TRUE
  )%>%
    mutate(
      comparison = "Cold - Average",
      diff = diff_cold_avg 
    )
  
  p_cold <- ggplot(diff_cold) +
    geom_sf(aes(fill = diff), color = NA) +
    scale_fill_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 0,
                         name = "Predicted difference") +
    #add black outline around density raster
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2))  +
    
    geom_sf(data = usa_mexico_sf_proj,
            fill = "grey85",
            col = "white",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    geom_sf(data = state_waters,
            color = "grey30",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    coord_sf(xlim = c(-450000, 350000),
             ylim = c(-600000, 450000)) +
    theme_bw() + base_theme +
    ggtitle(paste(met, "— Cold minus Average SST"))
  
  ggsave(
    paste0(fig_dir, met, "_SST_cold_minus_avg.jpg"),
    p_cold,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- In one figure ----
  # ---- COMBINE ----
  diff_all <- bind_rows(diff_hot, diff_cold)
  
  # Determine threshold per loop (e.g., 10th percentile)
  threshold <- quantile(abs(diff_all$diff), 0.8, na.rm = TRUE)  # bottom 10% removed
  
  #now filter out blocks outside that percentile

  diff_all <-  diff_all%>%
    filter(abs(diff) > threshold)
  

  # ---- FORCE SYMMETRIC COLOR SCALE ----
 # max_abs <- max(abs(diff_all$diff), na.rm = TRUE)
  
  # get max and min coordinates for plotting
  coordinates<-diff_all%>%
   
    st_coordinates()
  # Create bounding block for plot
  minx<-(min(coordinates[,1], na.rm = TRUE))
  maxx<-(max(coordinates[,1], na.rm = TRUE))
  miny<-(min(coordinates[,2], na.rm = TRUE))
  maxy<-(max(coordinates[,2], na.rm = TRUE))
  
  p_diff_combined <- ggplot(diff_all) +
    geom_sf(aes(fill = diff), color = NA) +
    facet_wrap(~ comparison, ncol = 2) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
    # limits = c(-max_abs, max_abs),
      name = "Predicted difference\nin effort"
    ) +
    guides(fill = guide_colorbar(
      ticks.colour = "black",
      frame.colour = "black",
      frame.linewidth = 0.2
    )) +
    geom_sf(data = usa_mexico_sf_proj,
            fill = "grey85",
            col = "white",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    geom_sf(data = state_waters,
            color = "grey30",
            linewidth = 0.2,
            inherit.aes = FALSE) +
    geom_sf(data = mpa_blocks, fill = "black", col = "white", linewidth = 0.2) +
   
    # Crop the plot using the bounding box of the contour
    coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy)) +
    # Whole-number degree breaks on both axes
    scale_x_continuous(breaks = seq(-180, 180, by = 2)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 1)) +
    theme_bw() + base_theme 
   # ggtitle(paste(met, "— SST Differences"))
  
  # ── Store the ggplot object with the metier name as the key ──────────────── 
  plot_list[[met]] <- p_diff_combined
  
  
  ggsave(
    paste0(fig_dir, met, "_SST_differences_side_by_side.jpg"),
    p_diff_combined,
    width = 12,
    height = 6,
    dpi = 300
  )
  
  # ── Save to Shiny www directory ──────────────────────────────────────────────
  shiny_dir <- "C:/Users/kinse/Desktop/mpa climate fisheries/ca_mpas_climate/Shinydashboard/California_fisheries/www/"
  
  ggsave(
    paste0(shiny_dir, "predictions_", clean_met_name(met), "_SST_differences.jpg"),
    p_diff_combined,
    width = 12, height = 6, dpi = 300
  )
  
  gc()
}

############

#panels i want
sst_sb_lobster <- plot_list[["Santa Barbara Lobster trap"]]+ theme(axis.text = element_text(size = 12),plot.tag = element_text(face = "bold", size = 14)
)
sst_sb_cpfv    <- plot_list[["Santa Barbara CPFV"]]+ theme(axis.text = element_text(size = 12),plot.tag = element_text(face = "bold", size = 14)
)
sst_mb_cpfv    <- plot_list[["Morro Bay CPFV"]] +theme(axis.text = element_text(size = 12),plot.tag = element_text(face = "bold", size = 14)
)

combined_plot <- sst_sb_lobster / sst_sb_cpfv / sst_mb_cpfv +
                    plot_layout(nrow = 3, heights = c(1, 1, 1)) +
                    plot_annotation(
                      tag_levels = "a",
                      tag_prefix = "(",
                      tag_suffix = ")"
                    ) 

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/spatial_predictions_SST.jpg')
ggsave(filename = filename1, plot =  combined_plot , width = 7, height = 10, units = "in", dpi = 300)


############
