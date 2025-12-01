
library(tidyverse)

############
#Upload data for each metric
filepath<-('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/data/')

area_slope<-readRDS(paste0(filepath, 'area_slope.rds'))%>%
  rename(slope=slope.area, slope2=slope.area2)%>%
  mutate(metric="(a) Area", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  select(c(slope,fishery2,port_complex,metric,log_slope))

latitude_slope<-readRDS(paste0(filepath, 'latitude_slope.rds'))%>%
  rename(slope=slope.latitude, slope2=slope.latitude2)%>%
  mutate(metric="(b) Latitude", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  select(c(slope,fishery2,port_complex,metric,log_slope))

extent_north_slope<-readRDS(paste0(filepath, 'extents_slope.rds'))%>%
  dplyr::select(-c(slope.south, CI_lower_bound_south,CI_upper_bound_south, CI_lower_clipped_south, CI_upper_clipped_south, CI_length_south,midpoint_south,CI_adjusted_length_south,CI_clipped_south, slope.south2,CI_lower_bound_south_2,CI_upper_bound_south_2))%>%
  rename(slope=slope.north, slope2=slope.north2, CI_lower_bound_2=CI_lower_bound_north_2, CI_upper_bound_2=CI_upper_bound_north_2)%>%
  mutate(metric="(c) Northern extent", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  select(c(slope,fishery2,port_complex,metric,log_slope))
# Rename columns
colnames(extent_north_slope) <- gsub("_north$", "", colnames(extent_north_slope))

extent_south_slope<-readRDS(paste0(filepath, 'extents_slope.rds'))%>%
  dplyr::select(-c(slope.north, CI_lower_bound_north,CI_upper_bound_north, CI_lower_clipped_north, CI_upper_clipped_north, CI_length_north,midpoint_north,CI_adjusted_length_north,CI_clipped_north, slope.north2,CI_lower_bound_north_2,CI_upper_bound_north_2))%>%
  rename(slope=slope.south, slope2=slope.south2, CI_lower_bound_2=CI_lower_bound_south_2, CI_upper_bound_2=CI_upper_bound_south_2)%>%
  mutate(metric="(d) Southern extent", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  select(c(slope,fishery2,port_complex,metric,log_slope))
# Rename columns
colnames(extent_south_slope) <- gsub("_south$", "", colnames(extent_south_slope))

distance_slope<-readRDS(paste0(filepath, 'distance_slope.rds'))%>%
  rename(slope=slope.distance)%>%
  mutate(slope2=slope, metric="(e) Distance to shore", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  select(c(slope,fishery2,port_complex,metric,log_slope))

reg_slope<-readRDS(paste0(filepath, 'reg_slope.rds'))%>%
  rename(slope=slope.reg)%>%
  mutate(slope2=slope, metric="(f) Percent area in regulation", log_slope=sign(slope)*log10(1 + abs(slope)))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  select(c(slope,fishery2,port_complex,metric,log_slope))

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
  metric = rep(c("(a) Area", "(b) Latitude", "(c) Northern extent", "(d) Southern extent", "(e) Distance to shore",
                 "(f) Percent area in regulation"), each = 2),
  x = c(-15, 15, -0.01, 0.01, -0.006, 0.006, -0.03,0.03,-50, 50, -.5,.5),         # start of arrow
  xend = c(-180, 180, -0.1, .1, -0.07, 0.07,-0.35,0.35, -800, 850,-9.5,9.5),      # end of arrow
  y = c(0.0055, 0.0055, 80, 80, 52, 52,40,40, 0.0018, 0.0018, 2.3,2.3),
  yend = c(0.0055, 0.0055, 80, 80, 52, 52,40,40,0.0018, 0.0018, 2.3,2.3)
)


annotations_text <- data.frame(
  metric = c("(a) Area", "(a) Area", "(b) Latitude", "(b) Latitude", "(c) Northern extent", "(c) Northern extent","(d) Southern extent","(d) Southern extent", "(e) Distance to shore",
             "(e) Distance to shore", "(f) Percent area in regulation", "(f) Percent area in regulation"),
  x = c(-85, 75, -0.05, 0.05, -0.03, 0.03, -0.15,0.15, -350, 400, -4.5,4.5),
  y = c(0.0057, 0.0057, 83, 83, 54, 54,41.5,41.5,0.00188,0.00188,2.4,2.4),
  label = c("Getting smaller", "Getting larger", "Moving south", "Moving north", "Moving south", "Moving north","Moving south", "Moving north",
            "Closer to shore", "Farther from shore", "Less area in regulation",  "More area in regulation")
)

p<-ggplot(all.metrics, aes(x = slope)) + 
  facet_wrap(~metric, scales="free") + 
  geom_density(alpha = 0.1, fill = "red")+
  geom_vline(xintercept=0, linetype="dashed", color="black", linewidth=0.5)+
  labs(y="Density", x="Slope")+
  base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size=10,margin = margin(t = 1)),
          strip.text.x = element_text(hjust = 0),
          strip.background = element_rect(fill = "white", color = NA),  # White strip background
          legend.position = "none")+
geom_segment(data = annotations_arrows,
             aes(x = x, xend = xend, y = y, yend = yend),
             arrow = arrow(length = unit(0.15, "inches")),
             inherit.aes = FALSE) +
  geom_text(data = annotations_text, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, size = 3) 
p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/histograms_slopes.jpg')
ggsave(filename = filename1, plot =  p , width = 14, height = 6, units = "in", dpi = 300)

############