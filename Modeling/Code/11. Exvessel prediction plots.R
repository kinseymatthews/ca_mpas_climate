library(tidyverse)
library(sf)
library(pROC)
library(patchwork)
############ 
# Plot exvessel spatial predictions
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


in_dir  <- paste0(base_path, "Modeling/spatial_outputs/exvessel/")
fig_dir <- paste0(base_path, "Modeling/figures/spatial_predictions/exvessel/")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
# ---- automatically detect metiers from saved files ----
files_all <- list.files(in_dir,
                        pattern = "_exvessel_ALL.gpkg$",
                        full.names = FALSE)

all_metiers <- str_remove(files_all, "_exvessel_ALL.gpkg")

plot_list <- list()


for (met in all_metiers) {
  
  message("Plotting: ", met)
  
  # ---- LOAD COMBINED ----
  pred_all <- st_read(
    paste0(in_dir, met, "_exvessel_ALL.gpkg"),
    quiet = TRUE
  )
  
  # ensure correct order
  pred_all$price <- factor(pred_all$price,
                          levels = c("Low", "Average", "High"))
  
  # ---- FACET MAP ----
  p_main <- ggplot(pred_all) +
    geom_sf(aes(fill = est_exp), color = NA) +
    scale_fill_viridis_c(trans = "sqrt",
                         name = "Predicted effort") +
    facet_wrap(~ price) +
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
    paste0(fig_dir, met, "_exvessel_low_avg_high.jpg"),
    p_main,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- HIGH - AVG ----
  diff_high <- st_read(
    paste0(in_dir, met, "_exvessel_HIGH_minus_AVERAGE.gpkg"),
    quiet = TRUE
  ) %>%
    mutate(
      comparison = "High - Average",
      diff = diff_high_avg
    )
  
  
  p_high <- ggplot(diff_high) +
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
    theme_bw() + base_theme 
   # ggtitle(paste(met, "— High minus Average exvessel"))
  
  ggsave(
    paste0(fig_dir, met, "_exvessel_high_minus_avg.jpg"),
    p_high,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- LOW- AVG ----
  diff_low <- st_read(
    paste0(in_dir, met, "_exvessel_LOW_minus_AVERAGE.gpkg"),
    quiet = TRUE
  )%>%
    mutate(
      comparison = "Low - Average",
      diff = diff_low_avg #wrote this wrong in script 1. should have been diff_cold_avg
    )
  
  p_low <- ggplot(diff_low) +
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
    ggtitle(paste(met, "— Low minus Average exvessel"))
  
  ggsave(
    paste0(fig_dir, met, "_exvessel_low_minus_avg.jpg"),
    p_low,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- In one figure ----
  # ---- COMBINE ----
  diff_all <- bind_rows(diff_high, diff_low)
  
  
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
    # Crop the plot using the bounding box of the contour
    coord_sf(xlim = c(minx-1000, maxx+1000), ylim = c(miny-1000, maxy+1000)) +
    # Whole-number degree breaks on both axes
    scale_x_continuous(breaks = seq(-180, 180, by = 2)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 1)) +
    theme_bw() + base_theme 
   # ggtitle(paste(met, "— exvessel Differences"))
  
  # ── Store the ggplot object with the metier name as the key ──────────────── 
  plot_list[[met]] <- p_diff_combined
  
  
  
  ggsave(
    paste0(fig_dir, met, "_exvessel_differences_side_by_side.jpg"),
    p_diff_combined,
    width = 12,
    height = 6,
    dpi = 300
  )
  gc()
}
############
my_theme <- theme(
  axis.text    = element_text(size = 12),
  plot.tag     = element_text(face = "bold", size = 14),
  aspect.ratio = 1.2   # tweak this — higher = taller/narrower
)
#panels i want
exves_sb_cuc <- plot_list[["Santa Barbara Cucumber dive"]]+ my_theme
exves_la_cuc  <- plot_list[["Los Angeles Cucumber dive"]]+ my_theme
exves_mb_calhal   <- plot_list[["Morro Bay California halibut trawl"]] + my_theme
exves_mont_calhal   <- plot_list[["Monterey California halibut trawl"]] + my_theme


combined_plot <- #panels i want
  exves_sb_cuc / exves_la_cuc / exves_mb_calhal / exves_mont_calhal +
  plot_layout(nrow = 4, heights = c(1, 1, 1,1)) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) 

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/spatial_predictions_exvessel.jpg')
ggsave(filename = filename1, plot =  combined_plot , width = 7, height = 12, units = "in", dpi = 300)

############
