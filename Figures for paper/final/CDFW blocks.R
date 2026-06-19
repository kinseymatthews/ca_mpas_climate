
library(sf)
library(sp)
library(tidyverse)
library(cowplot)
#################
#filepaths
base_path<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/'
#################
#data
#spatial mapping data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
usa_mexico_sf <- dplyr::bind_rows(list(usa,mexico))%>%
  st_transform(usa_mexico_sf, crs = "+proj=longlat +datum=WGS84")
california <- usa[usa$name == "California", ]
# State waters
state_waters <- readRDS( paste0(base_path,"All Fisheries Data/CA_state_waters_polyline.Rds"))

#block data 
all_blocks<-st_read(paste0(base_path,"All Fisheries Data/Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()%>%
  filter(blck_ty=="Inshore" |blck_ty=="Midshore" )%>%
  filter(blck_st=="California")

#upload port information - only plotting the major port of each complex
ports<-read_sf(paste0(base_path,'Variables/Distance to Port Complex/Data/Raw/port codes_updated_April2025.shp'))%>%
  rename(port=PortName)%>%
  mutate(port = str_to_title(port))%>%
  filter(port=="Eureka"| port=="Fort Bragg"| port=="Bodega Bay"| port=="San Francisco"| port=="Monterey"| port=="Morro Bay"| port=="Santa Barbara Harbor"| port=="Los Angeles"| port=="San Diego")%>%
  mutate(MajorPort = str_to_title(MajorPort))%>%
  rename(port.complex=MajorPort)


#################
# base theme
base_theme <- theme(axis.text=element_text(size=7),
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
                    legend.background = element_rect(fill=alpha('blue', 0)))
#################
# Define your ports
ports <- data.frame(
  name = c("San Francisco", "San Diego", "Los Angeles", "Santa Barbara",
           "Monterey", "Morro Bay", "Bodega Bay", "Eureka", "Fort Bragg"),
  x = c(-13627232, -13043736, -13161254, -13323925,
        -13568975, -13454548, -13698044, -13821615, -13781778),
  y = c(4552429, 3857689, 3996020, 4083595,
        4384277, 4213724, 4626578, 4984091, 4782525)
)

# Convert to sf in EPSG:3857, then reproject to WGS84 to match your map
ports_sf <- st_as_sf(ports, coords = c("x", "y"), crs = 3857) %>%
  st_transform(crs = 4326)

#plot
main_plot<-ggplot(data=all_blocks, mapping=aes(fill=blck_ty)) +
  geom_sf() + 
  scale_fill_manual(values = c("Inshore" = "#F77F00", "Midshore" = "#0099CC"),
                    name = "Block Type")+
 # Land
  geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  # State waters
  geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
  # Port points
  geom_sf(data = ports_sf, inherit.aes = FALSE,
          color = "black", size = 1.5) +
  # Port labels
  geom_sf_text(data = ports_sf, aes(label = name),
               inherit.aes = FALSE,
               size = 3.5, nudge_x = 0.3, hjust = 0) +
  # Crop
  coord_sf(xlim = c(-115.5, -126), ylim = c(32.5,42)) +
  # Axes
  scale_x_continuous(breaks=seq(-128, -117, 2)) +
  scale_y_continuous(breaks=seq(32, 44, 2)) + 
 #no X and Y labels
  labs(x = NULL, y = NULL) +
  # Theme
  theme_bw() + base_theme 

# Inset map
inset <- ggplot() +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth = 0.3) +
  # Bounding box matching main plot extent
  annotate("rect", xmin = -126, xmax = -117, ymin = 32.5, ymax = 42,
           color = "black", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA),panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

# Combine
p<-ggdraw(main_plot) +
  draw_plot(inset, x = 0.422, y = 0.765, width = 0.28, height = 0.22)
p
filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CDFW blocks.jpg')
ggsave(filename = filename1, plot =  p , width = 10, height = 5, units = "in", dpi = 300)

