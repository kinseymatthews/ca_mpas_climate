library(cowplot)
library(magick)
library(ggplot2)
library(patchwork)
############ 
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/figures/spatial_predictions/"


#helper function 
# Loads a JPG and converts it to a ggplot-compatible grob with a row label
load_panel <- function(path) {
  img <- image_read(path)
  info <- image_info(img)
  img_cropped <- image_crop(
    img
  )
  ggdraw() + draw_image(img_cropped)
}

############ 
#figure A - SST
#Each row is one fishery; columns are Cold-Avg and Hot-Avg (already in your side-by-side images)
# Since your "differences_side_by_side" files already have both panels, use those

sst_sb_lobster  <- load_panel(paste0(base_path,"SST/Santa Barbara Lobster trap_SST_differences_side_by_side.jpg"))
sst_sb_cpfv     <- load_panel(paste0(base_path,"SST/Santa Barbara CPFV_SST_differences_side_by_side.jpg"))
sst_mb_cpfv     <- load_panel(paste0(base_path,"SST/Morro Bay CPFV_SST_differences_side_by_side.jpg"))

# Stack the three rows into one figure
p <- plot_grid(
  sst_sb_lobster,
  sst_sb_cpfv,
  sst_mb_cpfv,
  ncol = 1,          # one column of side-by-side maps stacked vertically
  rel_heights = c(1.5, 1.5, 1.5)
)

p

combined_plot <- (sst_sb_lobster / sst_sb_cpfv / sst_mb_cpfv +
                    plot_layout(nrow = 3, heights = c(1, 1, 1)) +
                    plot_annotation(
                      tag_levels = "a",
                      tag_prefix = "(",
                      tag_suffix = ")"
                    )) &
  theme(
    plot.tag = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/spatial_predictions_SST.jpg')
ggsave(filename = filename1, plot =  p , width = 8, height = 10, units = "in", dpi = 300)

