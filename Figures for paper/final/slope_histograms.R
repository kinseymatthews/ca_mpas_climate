
library(tidyverse)

############
#Upload data for each metric
filepath<-('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/data/')

area_slope<-readRDS(paste0(filepath, 'area_slope.rds'))%>%
  rename(slope=slope.area, slope2=slope.area2)%>%
  mutate(metric="(a) Area (km\u00B2/year)", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  dplyr::select(c(slope,fishery2,port_complex,metric,log_slope))

latitude_slope<-readRDS(paste0(filepath, 'latitude_slope.rds'))%>%
  rename(slope=slope.latitude, slope2=slope.latitude2)%>%
  mutate(metric="(b) Latitude (degrees/year)", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  dplyr:: select(c(slope,fishery2,port_complex,metric,log_slope))

extent_north_slope<-readRDS(paste0(filepath, 'extents_slope.rds'))%>%
  dplyr::select(-c(slope.south, CI_lower_bound_south,CI_upper_bound_south, CI_lower_clipped_south, CI_upper_clipped_south, CI_length_south,midpoint_south,CI_adjusted_length_south,CI_clipped_south, slope.south2,CI_lower_bound_south_2,CI_upper_bound_south_2))%>%
  rename(slope=slope.north, slope2=slope.north2, CI_lower_bound_2=CI_lower_bound_north_2, CI_upper_bound_2=CI_upper_bound_north_2)%>%
  mutate(metric="(c) Northern extent (degrees/year)", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  dplyr::select(c(slope,fishery2,port_complex,metric,log_slope))
# Rename columns
colnames(extent_north_slope) <- gsub("_north$", "", colnames(extent_north_slope))

extent_south_slope<-readRDS(paste0(filepath, 'extents_slope.rds'))%>%
  dplyr::select(-c(slope.north, CI_lower_bound_north,CI_upper_bound_north, CI_lower_clipped_north, CI_upper_clipped_north, CI_length_north,midpoint_north,CI_adjusted_length_north,CI_clipped_north, slope.north2,CI_lower_bound_north_2,CI_upper_bound_north_2))%>%
  rename(slope=slope.south, slope2=slope.south2, CI_lower_bound_2=CI_lower_bound_south_2, CI_upper_bound_2=CI_upper_bound_south_2)%>%
  mutate(metric="(d) Southern extent (degrees/year)", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  dplyr::select(c(slope,fishery2,port_complex,metric,log_slope))
# Rename columns
colnames(extent_south_slope) <- gsub("_south$", "", colnames(extent_south_slope))

distance_slope<-readRDS(paste0(filepath, 'distance_slope.rds'))%>%
  rename(slope=slope.distance)%>%
  mutate(slope2=slope, metric="(e) Distance to mainland (km/year)", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  dplyr:: select(c(slope,fishery2,port_complex,metric,log_slope))

reg_slope<-readRDS(paste0(filepath, 'reg_slope.rds'))%>%
  rename(slope=slope.reg)%>%
  mutate(slope2=slope, metric="(f) Percent area in regulation (%/year)", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  dplyr::select(c(slope,fishery2,port_complex,metric,log_slope))

all.metrics<-rbind(area_slope, latitude_slope, extent_north_slope, extent_south_slope,distance_slope,reg_slope)

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
############


############
#raw histograms
ggplot(area_slope, aes(x = slope)) +            # Draw two histograms in same plot
  geom_histogram(bins=100,alpha = 0.5, position = "dodge", aes(y=..density..))+
  geom_density(alpha = 0.1, fill = "red")


ggplot(latitude_slope, aes(x = slope, fill = fishery2)) +            # Draw two histograms in same plot
  geom_histogram(bins=100,alpha = 0.5, position = "identity", aes(y=..density..))+
  geom_density(alpha = 0.1, fill = "red")

ggplot(extent_north_slope, aes(x = slope, fill = fishery2)) +            # Draw two histograms in same plot
  geom_histogram(bins=100,alpha = 0.5, position = "identity", aes(y=..density..))+
  geom_density(alpha = 0.1, fill = "red")

ggplot(extent_south_slope, aes(x = slope, fill = fishery2)) +            # Draw two histograms in same plot
  geom_histogram(bins=100,alpha = 0.5, position = "identity", aes(y=..density..))+
  geom_density(alpha = 0.1, fill = "red")


ggplot(distance_slope, aes(x = slope, fill = fishery2)) +            # Draw two histograms in same plot
  geom_histogram(bins=100,alpha = 0.5, position = "identity", aes(y=..density..))+
  geom_density(alpha = 0.1, fill = "red")

ggplot(reg_slope, aes(x = slope, fill = fishery2)) +            # Draw two histograms in same plot
  geom_histogram(bins=100,alpha = 0.5, position = "identity", aes(y=..density..))+
  geom_density(alpha = 0.1, fill = "red")
############

############

# Annotation data (customize coordinates as needed)
annotations_arrows <- data.frame(
  metric = rep(c("(a) Area (km\u00B2/year)", "(b) Latitude (degrees/year)", "(c) Northern extent (degrees/year)", "(d) Southern extent (degrees/year)", "(e) Distance to mainland (km/year)",
                 "(f) Percent area in regulation (%/year)"), each = 2),
  x = c(-18, 18, -0.005, 0.005, -0.006, 0.006, -0.03,0.03,-50, 50, -.5,.5),         # start of arrow
  xend = c(-300, 300, -0.13, 0.13, -0.09, 0.09,-0.6,0.6, -3000, 3200,-11,11),      # end of arrow
  y = c(0.0055, 0.0055, 75, 75, 55, 55,38,38, 0.00155, 0.00155, 2.2,2.2),
  yend = c(0.0055, 0.0055, 75, 75, 55, 55,38,38,0.00155, 0.00155, 2.2,2.2)
)


annotations_text <- data.frame(
  metric = c("(a) Area (km\u00B2/year)", "(a) Area (km\u00B2/year)", "(b) Latitude (degrees/year)", "(b) Latitude (degrees/year)", "(c) Northern extent (degrees/year)", "(c) Northern extent (degrees/year)","(d) Southern extent (degrees/year)","(d) Southern extent (degrees/year)", "(e) Distance to mainland (km/year)",
             "(e) Distance to mainland (km/year)", "(f) Percent area in regulation (%/year)", "(f) Percent area in regulation (%/year)"),
  x = c(-145, 135, -0.055, 0.055, -0.04, 0.04, -0.28,0.28, -1600, 1800, -6,6),
  y = c(0.006, 0.006, 82, 82, 60, 60,41.5,41.5,0.00167,0.00167,2.4,2.4),
  label = c("Getting smaller", "Getting larger", "Moving south", "Moving north", "Moving south", "Moving north","Moving south", "Moving north",
            "Closer to mainland", "Farther from mainland", "Less area",  "More area")
)

p<-ggplot(all.metrics, aes(x = slope)) + 
  facet_wrap(~metric, scales="free") + 
  geom_density(alpha = 0.1, fill = "red")+
  geom_vline(xintercept=0, linetype="dashed", color="black", linewidth=0.5)+
  labs(y="Density", x="Slope")+
  base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=12,margin = margin(t = 1)),
          strip.text.x = element_text(hjust = 0, size = 16,
                                      margin = margin(t = 2, b = 18)),
          strip.background = element_rect(fill = "white", color = NA),  # White strip background
          legend.position = "none",
          axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          panel.spacing.x = unit(1.0, "lines"),   # space between columns
          panel.spacing.y = unit(2, "lines"),     # space between rows
          plot.margin = margin(10, 20, 10, 10))+
geom_segment(data = annotations_arrows,
             aes(x = x, xend = xend, y = y, yend = yend),
             arrow = arrow(length = unit(0.15, "inches")),
             inherit.aes = FALSE) +
  geom_text(data = annotations_text, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, size = 5) 
p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/histograms_slopes.jpg')
ggsave(filename = filename1, plot =  p , width = 14, height = 6, units = "in", dpi = 300)

############