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
#Kernel density contours

#using the presence/absence part of the hurdle/delta model to tell the model
#which block IDs to include in the model and which ones to not include.
#use the best threshold calculated from ROC to divide blocks in absence or presence
# only include presences in effort model


#but, does this make sense to do when you're predicting to artifical values? like the 90th, 10th, and median for SST?
#cause 
#upload entire dataset
fisheries_full<-readRDS(paste0(base_path,"Modeling/Data/Processed Data/fisheries_df_with_variables.rds"))
fisheries_reduced<-fisheries_full%>%
  dplyr::select(block_id, fishery, port.complex, date, year, month,total.hours, present,month_year)%>%
  mutate(metier=paste(port.complex,fishery, sep=" "))


for (met in all_metiers) {
  
  message("Plotting: ", met)
  
  pred_all <- st_read(
    paste0(in_dir, met, "_SST_ALL.gpkg"),
    quiet = TRUE
  )
  
  month_year_met<-unique(pred_all$month_year)
  
  fisheries_reduced_met<-fisheries_reduced%>%
    filter(metier==met  & month_year==month_year_met)%>%# )present=="1"
    mutate(block_id=as.factor(block_id))
  
  temps<-unique(pred_all$temp)
  
  prediction_list <- list()
  
  for (temp_met in temps) {
    prediction<-pred_all%>%
      filter(temp==temp_met)%>%
      mutate(block_id=as.factor(block_id))%>%
      left_join(fisheries_reduced_met, by=c("block_id","fishery","port.complex","date","year"))
    
    prediction <- prediction %>%
      mutate(prob_pa = plogis(est_pa))
    
    roc <- pROC::roc(prediction$present, (prediction$prob_pa))
    plot(roc, print.auc = TRUE, col = "blue", legacy.axes = TRUE)
    auc <- pROC::auc(roc)
    auc
    #has Hot auc probably because it's saying everything is 0 but then only a few are 1 so only misclassifying a few
    opt <- pROC::coords(roc,
                        x = "best",
                        best.method = "closest.topleft",
                        ret = c("threshold", "sensitivity", "specificity", "accuracy"))
    
    opt
    threshold<-opt$threshold
    # in this example, threshold is 0.5185789, so, if remove all blocks with the probability of presence beCold this threshold value
    prediction_filtered<-prediction%>%
      filter(prob_pa>=threshold)
    
    prediction_list[[temp_met]] <- prediction_filtered
  }
  ############
  #add them all together
  df <- dplyr::bind_rows(prediction_list)
  
  make_kde_contour <- function(pred_sf) {
    
    pred_sf <- pred_sf %>%
      mutate(
        block_long_dd = block_long_dd * 1000,
        block_lat_dd  = block_lat_dd  * 1000
      ) %>%
      st_drop_geometry%>%
      st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs = 3310)%>%
      #remove any bad spatial blocks
      filter(!st_is_empty(.))
    
    
    coords <- st_coordinates(pred_sf)
    weights <- pred_sf$est_exp
    
    # window for KDE
    minx <- min(coords[,1]) - 20000
    maxx <- max(coords[,1]) + 20000
    miny <- min(coords[,2]) - 20000
    maxy <- max(coords[,2]) + 20000
    
    points_ppp <- ppp(coords[,1], coords[,2],
                      window = owin(c(minx, maxx), c(miny, maxy)))
    
    sigma <- bw.diggle(points_ppp, hmax = 10000)
    
    density <- density.ppp(
      points_ppp,
      sigma = sigma,
      kernel = "gaussian",
      weights = weights,
      diggle = TRUE,
      scalekernel = TRUE
    )
    
    density_raster <- raster(as.im(density))
    density_masked <- mask(density_raster, usa_mexico_sf_proj, inverse = TRUE)
    
    # 75% contour
    density_df <- as.data.frame(rasterToPoints(density_masked))
    names(density_df) <- c("x", "y", "value")
    
    density_values <- sort(density_df$value, decreasing = TRUE)
    cdf <- cumsum(density_values) / sum(density_values)
    contour_level <- density_values[which.min(abs(cdf - 0.75))]
    
    contour_lines <- rasterToContour(density_masked, levels = contour_level)
    contour_sf <- st_as_sf(contour_lines)
    st_crs(contour_sf) <- 3310
    
    return(contour_sf)
  }
  
  pred_hot <- df%>%
    filter(temp=="Hot")
  
  pred_cold<- df%>%
    filter(temp=="Cold")
  
  pred_avg <- df%>%
    filter(temp=="Average")
  
  
  #create contour for each level
  contour_cold <- make_kde_contour(pred_cold)
  contour_avg  <- make_kde_contour(pred_avg)
  contour_Hot  <- make_kde_contour(pred_hot)
  
  contour_cold$temp <- "Cold"
  contour_avg$temp  <- "Average"
  contour_Hot$temp  <- "Hot"
  
  all_contours <- bind_rows(contour_cold,
                            contour_avg,
                            contour_Hot)
  
  p<-ggplot() +
    geom_sf(data = usa_mexico_sf_proj, fill = "grey85", color = NA) +
    
    geom_sf(data = all_contours,
            aes(color = temp),
            linewidth = 0.5) +
    
    scale_color_manual(
      values = c("Cold" = "blue",
                 "Average" = "black",
                 "Hot" = "red"),
      name = "SST Scenario"
    ) +
    
    coord_sf(xlim = c(-450000, 350000),
             ylim = c(-600000, 450000)) +
    
    ggtitle(paste("75% KDE contours —", met)) +
    theme_bw() + base_theme 
  p
  
  ggsave(
    filename = paste0(out_fig_dir, met, "_KDE_Cold_avg_Hot.jpg"),
    plot = p,
    width = 10, height = 7, dpi = 300
  )
  
  dev.off()
}
