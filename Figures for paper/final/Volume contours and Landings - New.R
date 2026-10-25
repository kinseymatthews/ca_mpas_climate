library(tidyverse)
library(sf)
library(cowplot)
library(RColorBrewer)
library(patchwork)
##################
# Shared base path
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate"

#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
usa_mexico_sf_proj<- st_transform(usa_mexico_sf, crs = 3310)

# State waters
state_waters <- readRDS( paste0(base_path,"/All Fisheries Data/CA_state_waters_polyline.Rds"))
state_waters<- st_transform(state_waters, crs = 3310)

#port 
port<-read.csv( paste0(base_path,"/All Fisheries Data/Port Codes.csv"))

#upload port information - only plotting the major port of each complex
ports<-read_sf(paste0(base_path,'/Variables/Distance to Port Complex/Data/Raw/port codes_updated_April2025.shp'))%>%
  rename(port=PortName)%>%
  mutate(port = str_to_title(port))%>%
  filter(port=="Eureka"| port=="Fort Bragg"| port=="Bodega Bay"| port=="San Francisco"| port=="Monterey"| port=="Morro Bay"| port=="Santa Barbara Harbor"| port=="Los Angeles"| port=="San Diego")%>%
  mutate(MajorPort = str_to_title(MajorPort))%>%
  rename(port.complex=MajorPort)


# base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
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
                    legend.background = element_rect(fill=alpha('blue', 0)),
                    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8))

# Helper function to read and label files for fishery volume contour data
read_fishery_data <- function(base_path, fishery_folder, files, fishery_name) {
  file_paths <- file.path(base_path, fishery_folder, "Data/Processed Data", files)
  data_list <- map(file_paths, st_read)
  bind_rows(data_list) %>%
    mutate(fishery = fishery_name)%>%
    group_by(portcomplex)%>%
    ungroup()
}

# Read data for each fishery
cpfv <- read_fishery_data(base_path, "CPFV", "all_portcomplex_contours_75_last5years.gpkg", "CPFV")%>%
  mutate(comm_name="Mult-species_CPFV")

cuc_trawl <- read_fishery_data(base_path, "Cucumber", "all_portcomplex_contours_75_last5years.gpkg", "Cucumber trawl")%>%
  mutate(comm_name="Giant red sea cucumber")

cuc_dive <- read_fishery_data(base_path, "Dive","cucumber_all_portcomplex_contours_75_last5years.gpkg" , "Cucumber dive")%>%
  mutate(comm_name="Warty sea cucumber")

urchin <- read_fishery_data(base_path, "Dive", "urchin_all_portcomplex_contours_75_last5years.gpkg", "Urchin dive")%>%
  mutate(comm_name="Red sea urchin")


gillnet <- read_fishery_data(base_path, "Gillnet","set_all_portcomplex_contours_75_last5years.gpkg", "Set gillnet")%>%
  mutate(comm_name="Mult--species_gillnet")

halibut <- read_fishery_data(base_path, "Halibut","all_portcomplex_contours_75_last5years.gpkg", "Halibut trawl")%>%
  mutate(comm_name="California halibut")

lobster <- read_fishery_data(base_path, "Lobster","all_portcomplex_contours_75_last5years.gpkg", "Lobster trap")%>%
  mutate(comm_name="California spiny lobster")


pink_shrimp <- read_fishery_data(base_path, "Shrimp", "pink_all_portcomplex_contours_75_last5years.gpkg", "Pink shrimp trawl")%>%
  mutate(comm_name="Pacific pink shrimp")

ridgeback <- read_fishery_data(base_path, "Shrimp","ridge_all_portcomplex_contours_75_last5years.gpkg" , "Ridgeback prawn trawl")%>%
  mutate(comm_name="Ridgeback prawn")

squid <- read_fishery_data(base_path, "Squid","all_portcomplex_contours_75_last5years.gpkg", "Squid seine")%>%
  mutate(comm_name="Market squid")

# Combine them all
all_fisheries <- bind_rows(cpfv, cuc_trawl, cuc_dive, urchin, gillnet, halibut, lobster, pink_shrimp, ridgeback, squid)
all_fisheries$portcomplex<- factor(all_fisheries$portcomplex, levels = c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))


#upload landings data and group by fishery and port
##################
landings<-readRDS('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/1980_2022_landings_receipts.Rds')

trawl_gears <- unique(landings$gear[grepl("trawl", landings$gear, ignore.case = TRUE)])
trawl_gears <- setdiff(trawl_gears, c("Midwater trawl", "Pelagic trawl"))
# Define valid species-gear combos

species_gear <- list(
  'Giant red sea cucumber' = trawl_gears,
  'Unspecified sea cucumber' = c(trawl_gears, 'Diving'),
  'Warty sea cucumber' = c('Diving'),
  'Red sea urchin' = c('Diving', 'Diving /hooks (sea urchins)'),
  'California halibut' =  trawl_gears,
  'Market squid' = c('Purse seine'),
  'Ridgeback prawn' = trawl_gears,
  'Pacific pink shrimp' = trawl_gears,
  'California spiny lobster' = c('Crab or lobster trap')
)

# Combine filtered data for each species-gear combo
df_species<- map2_dfr( # loops through each species and its associated gear types to filter rows accordingly.
  names(species_gear),
  species_gear,
  ~ landings %>%
    filter(comm_name == .x, gear %in% .y)
) %>%
  mutate(comm_name = case_when(
    comm_name == "Unspecified sea cucumber" & gear == "Diving" ~ "Warty sea cucumber",
    comm_name == "Unspecified sea cucumber" ~ "Giant red sea cucumber",
    TRUE ~ comm_name
  ))

#now for set gillnet
df_gillnet <- landings %>%
  filter(gear == "Set gill net") %>%
  mutate(comm_name = "Mult-species_gillnet") %>%  # tag them so they don't overwrite existing names
  mutate(gear=recode(gear, "Set gill net"="Set gillnet"))
# 🔹 Bind together
df <- bind_rows(df_species, df_gillnet)

# Summarize the filtered data
landings_weights <- df %>%
  left_join(port, by='port')%>%
  group_by(comm_name, port.complex) %>%
  summarise(weight_lbs = sum(landings_lbs, na.rm = TRUE), .groups = "drop")%>%
  group_by(comm_name)%>%
  mutate(total_weight=sum(weight_lbs))%>%
  mutate(percentage=(weight_lbs/total_weight)*100)%>%
  left_join(ports, by='port.complex')%>%
  st_as_sf()%>%
  rename(portcomplex=port.complex)

landings_weights$portcomplex<- factor(landings_weights$portcomplex, levels = c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego'))
portcomplexes <- unique(c(landings_weights$portcomplex, landings_weights$portcomplex))
palette_colors <- setNames(brewer.pal(n = length(portcomplexes ), name = "RdYlBu")[1:length(portcomplexes)],
                           portcomplexes)

colors <- brewer.pal(n = 11, name = "RdYlBu")

port_colors <- c(
  "Eureka"  = "#313695",
  "Fort Bragg"="#4575B4",
  "Bodega Bay"="#74ADD1" ,
  "San Francisco" =  "#E0F3F8",
  "Monterey"="#FEE090",
  "Morro Bay"     = "#FDAE61",
  "Santa Barbara" = "#F46D43",
  "Los Angeles"   = "#D73027",
  "San Diego"     =  "#A50026"
)

plot_fun <- function(fish) {
  
  fishery_subset <- all_fisheries %>%
    filter(fishery == fish)
  
  name  <- fishery_subset$comm_name
  ports <- unique(fishery_subset$portcomplex)
  
  landings_subset <- landings_weights %>%
    filter(comm_name %in% name) %>%
    filter(portcomplex %in% ports)
  
  # ── Reproject landings to match plot CRS (EPSG:3310) ─────────────────────
  landings_subset <- landings_subset %>%
    st_transform(3310)
  
  # ── Extract coordinates after reprojection ────────────────────────────────
  landings_coords <- landings_subset %>%
    mutate(
      X = st_coordinates(.)[, 1],
      Y = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry()
  
  # ── Offshore x position in EPSG:3310 meters
  offshore_x <- -480000
  
  # ── Create offshore points as sf in EPSG:3310
  landings_offshore <- landings_coords %>%
    mutate(X_offshore = offshore_x) %>%
    st_as_sf(coords = c("X_offshore", "Y"), crs = 3310)
  
  # ── Plot 
  p <- ggplot() +
    # Land
    geom_sf(data = usa_mexico_sf_proj, fill = "gray", col = "black",
            linewidth = 0.2, inherit.aes = FALSE) +
    # Fishery polygons
    geom_sf(data = fishery_subset, aes(fill = portcomplex), alpha = 0.4) +
    # Dotted leader lines from offshore dots to real port locations
    geom_segment(data = landings_coords %>% mutate(X_offshore = offshore_x),
                 aes(x = X_offshore, xend = X, y = Y, yend = Y),
                 color = "grey50", linewidth = 0.3, linetype = "dotted") +
    # Offshore dots colored and outlined by port complex
    geom_sf(data = landings_offshore,
            aes(size = percentage, fill = portcomplex,color = portcomplex),
             pch = 21, stroke = 0.5, color="black") +
    # Color scales — same name merges into one legend
    scale_fill_manual(name  = "Port Complex", values = port_colors) +
    scale_color_manual(name = "Port Complex", values = port_colors) +
    # Size scale
    scale_size_continuous(
      name   = "Landings (%)",
      limits = c(0, 100),
      breaks = c(1, 25, 50, 90),
      labels = paste0(c(1, 25, 50, 90), "%"),
      range  = c(0.5, 5)
    ) +
    # X-axis labels: show fewer ticks
    scale_x_continuous(breaks = seq(-128, -117, 4)) +
    # Crop
    coord_sf(xlim = c(-510000, 270000), ylim = c(-600000, 460000)) +
    ggtitle(paste0(fish)) +
    base_theme +
    theme(plot.title    = element_text(size = 12),
          axis.text.x   = element_text(size = 11),
          axis.text.y   = element_text(size = 11),
          axis.title    = element_blank(),          # removes both X and Y labels
          legend.margin = margin(0, 0, 0, -10),
          plot.margin   = margin(2, 2, 2, 2))
  return(p)
  
  # filename1 <- paste0('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/', fish, 'Allports_LastVC.jpg')
  # ggsave(filename = filename1, plot = p, width = 15, height = 8, units = "in", dpi = 300)
}
a <- plot_fun('Pink shrimp trawl')
b <- plot_fun('Ridgeback prawn trawl')
c <- plot_fun('Halibut trawl')
d <- plot_fun('Cucumber trawl')
e <- plot_fun('Squid seine')
f <- plot_fun('Set gillnet')
g <- plot_fun('Lobster trap')
h <- plot_fun('Urchin dive')
i <- plot_fun('Cucumber dive')
j <- plot_fun('CPFV')


# Create dummy data for consistent legend
dummy_df <- expand.grid(
  portcomplex = names(port_colors),
  percentage = c(1, 25, 50,  90)
)
dummy_df$lon <- -999999
dummy_df$lat <- -999999
dummy_sf <- st_as_sf(dummy_df, coords = c("lon", "lat"), crs = st_crs(usa_mexico_sf_proj))

# Dummy plot just for extracting the legend
legend_plot <- ggplot() +
  geom_sf(data = dummy_sf, aes(size = percentage, fill = portcomplex),
          shape = 21, color = "black", stroke = 1.2) +
  scale_fill_manual(name = "Port complex", values = port_colors) +
  scale_size_continuous(
    name = "Landings (%)",
    limits = c(0, 100),
    breaks = c(1, 25,  50,  90),  # consistent legend
    labels = paste0(c(1, 25, 50,  90), "%"),
    range = c(.5, 5)  # << Smaller point range
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(size = 5)),
    size = guide_legend(order = 2)
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.key = element_blank(),  
    legend.margin = margin(0, 0, 0, -10),
    plot.margin   = margin(1, 2, 2, 2)
     )

legend_only <- cowplot::get_legend(legend_plot)

# Helper function to clean up individual plots
clean_plot <- function(p, remove_y = TRUE, remove_x=TRUE) {
  p + theme(
    legend.position = "none",
    axis.text.y = if (remove_y) element_blank() else element_text(size = 12),
    axis.text.x = if (remove_x) element_blank()else element_text(size = 12)
  )
}

# Clean all plots
a <- clean_plot(a, remove_y = FALSE)
b <- clean_plot(b)
c <- clean_plot(c)
d <- clean_plot(d)
e <- clean_plot(e)

f <- clean_plot(f, remove_y = FALSE, remove_x = FALSE)
g <- clean_plot(g, remove_x = FALSE)
h <- clean_plot(h, remove_x = FALSE)
i <- clean_plot(i, remove_x = FALSE)
j <- clean_plot(j, remove_x = FALSE)

# Combine plots
top_row <- a + b + c + d + e + plot_layout(nrow = 1)
bottom_row <- f + g + h + i + j + plot_layout(nrow = 1)

# Combine with legend
final_plot <- plot_grid(
  plot_grid(top_row / bottom_row, ncol = 1),
  legend_only,
  ncol = 2,
  rel_widths = c(5, .9),
  align = "hv",
  axis = "tblr"
)

final_plot


# Save the final output
filename1 <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/All fisheries_landings_5years_2.jpg"
ggsave(filename = filename1, plot = final_plot, width = 12, height = 6, units = "in", dpi = 300)
