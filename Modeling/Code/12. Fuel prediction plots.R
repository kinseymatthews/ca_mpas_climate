library(tidyverse)
library(sf)
library(pROC)
############ 
# Plot SST spatial predictions
############ 
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"

# base theme
base_theme <- theme(axis.text=element_text(size=7),
                    # axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text = element_text(size=10),
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


in_dir  <- paste0(base_path, "Modeling/spatial_outputs/fuel/")
fig_dir <- paste0(base_path, "Modeling/figures/spatial_predictions/fuel/")
out_fig_dir <- paste0(base_path,
                      "Modeling/figures/kernel_contours/fuel/")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
# ---- automatically detect metiers from saved files ----
files_all <- list.files(in_dir,
                        pattern = "_fuel_ALL.gpkg$",
                        full.names = FALSE)

all_metiers <- str_remove(files_all, "_fuel_ALL.gpkg")



for (met in all_metiers) {
  
  message("Plotting: ", met)
  
  # ---- LOAD COMBINED ----
  pred_all <- st_read(
    paste0(in_dir, met, "_fuel_ALL.gpkg"),
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
    paste0(fig_dir, met, "_fuel_low_avg_high.jpg"),
    p_main,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- HIGH - AVG ----
  diff_high <- st_read(
    paste0(in_dir, met, "_fuel_HIGH_minus_AVERAGE.gpkg"),
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
    theme_bw() + base_theme +
    ggtitle(paste(met, "— High minus Average fuel"))
  
  ggsave(
    paste0(fig_dir, met, "_fuel_high_minus_avg.jpg"),
    p_high,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- LOW- AVG ----
  diff_low <- st_read(
    paste0(in_dir, met, "_fuel_LOW_minus_AVERAGE.gpkg"),
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
    ggtitle(paste(met, "— Low minus Average SST"))
  
  ggsave(
    paste0(fig_dir, met, "_fuel_low_minus_avg.jpg"),
    p_low,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # ---- In one figure ----
  # ---- COMBINE ----
  diff_all <- bind_rows(diff_high, diff_low)
  
  # ---- FORCE SYMMETRIC COLOR SCALE ----
  max_abs <- max(abs(diff_all$diff), na.rm = TRUE)
  
  p_diff_combined <- ggplot(diff_all) +
    geom_sf(aes(fill = diff), color = NA) +
    facet_wrap(~ comparison, ncol = 2) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-max_abs, max_abs),
      name = "Predicted difference"
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
    coord_sf(xlim = c(-450000, 350000),
             ylim = c(-600000, 450000)) +
    theme_bw() + base_theme +
    ggtitle(paste(met, "— fuel Differences"))
  
  ggsave(
    paste0(fig_dir, met, "_fuel_differences_side_by_side.jpg"),
    p_diff_combined,
    width = 12,
    height = 6,
    dpi = 300
  )
  gc()
}

############



############
#Kernel density contours

#using the presence/absence part of the hurdle/delta model to tell the model
#which block IDs to include in the model and which ones to not include.
#use the best threshold calculated from ROC to divide blocks in absence or presence
# only include presences in effort model


#but, does this make sense to do when you're predicting to artifical values? like the 90th, 10th, and median for fuel?
#cause 
#upload entire dataset
fisheries_full<-readRDS(paste0(base_path,"Modeling/Data/Processed Data/fisheries_df_with_variables.rds"))
fisheries_reduced<-fisheries_full%>%
  dplyr::select(block_id, fishery, port.complex, date, year, month,total.hours, present,month_year)%>%
  mutate(metier=paste(port.complex,fishery, sep=" "))


for (met in all_metiers) {
  
  message("Plotting: ", met)
  
pred_all <- st_read(
  paste0(in_dir, met, "_fuel_ALL.gpkg"),
  quiet = TRUE
)

month_year_met<-unique(pred_all$month_year)

fisheries_reduced_met<-fisheries_reduced%>%
  filter(metier==met  & month_year==month_year_met)%>%# )present=="1"
  mutate(block_id=as.factor(block_id))

prices<-unique(pred_all$price)

prediction_list <- list()

for (price_met in prices) {
prediction<-pred_all%>%
  filter(price==price_met)%>%
  mutate(block_id=as.factor(block_id))%>%
  left_join(fisheries_reduced_met, by=c("block_id","fishery","port.complex","date","year"))

prediction <- prediction %>%
  mutate(prob_pa = plogis(est_pa))

roc <- pROC::roc(prediction$present, (prediction$prob_pa))
plot(roc, print.auc = TRUE, col = "blue", legacy.axes = TRUE)
auc <- pROC::auc(roc)
auc
#has high auc probably because it's saying everything is 0 but then only a few are 1 so only misclassifying a few
opt <- pROC::coords(roc,
                    x = "best",
                    best.method = "closest.topleft",
                    ret = c("threshold", "sensitivity", "specificity", "accuracy"))

opt
threshold<-opt$threshold
# in this example, threshold is 0.5185789, so, if remove all blocks with the probability of presence below this threshold value
prediction_filtered<-prediction%>%
  filter(prob_pa>=threshold)

prediction_list[[price_met]] <- prediction_filtered
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

pred_high <- df%>%
  filter(price=="High")

pred_low<- df%>%
  filter(price=="Low")

pred_avg <- df%>%
  filter(price=="Average")


#create contour for each level
contour_low <- make_kde_contour(pred_low)
contour_avg  <- make_kde_contour(pred_avg)
contour_high  <- make_kde_contour(pred_high)

contour_low$price <- "Low"
contour_avg$price  <- "Average"
contour_high$price  <- "High"

all_contours <- bind_rows(contour_low,
                          contour_avg,
                          contour_high)

p<-ggplot() +
  geom_sf(data = usa_mexico_sf_proj, fill = "grey85", color = NA) +
  
  geom_sf(data = all_contours,
          aes(color = price),
          linewidth = 0.5) +
  
  scale_color_manual(
    values = c("Low" = "blue",
               "Average" = "black",
               "High" = "red"),
    name = "Fuel Scenario"
  ) +
  
  coord_sf(xlim = c(-450000, 350000),
           ylim = c(-600000, 450000)) +
  
  ggtitle(paste("75% KDE contours —", met)) +
  theme_bw() + base_theme 
p

ggsave(
  filename = paste0(out_fig_dir, met, "_KDE_low_avg_high.jpg"),
  plot = p,
  width = 10, height = 7, dpi = 300
)


}