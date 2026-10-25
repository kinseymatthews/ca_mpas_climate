
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
state_waters <- readRDS(paste0(base_path,"All Fisheries Data/CA_state_waters_polyline.Rds"))

#block data 
# block data
all_blocks<-st_read(paste0(base_path,"All Fisheries Data/Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(4326))%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d, block_long_dd=blck_ln_,block_lat_dd=blck_lt_)

#read  in bathymetry data with blocks
bathy.data<-read.csv(paste0(base_path,"All Fisheries Data/fishingblockstatistics.csv"))%>%
  dplyr::select(BLOCK_ID,MIN, MAX, RANGE, MEAN)%>%
  rename(block_id=BLOCK_ID, Max_Depthm=MIN, Min_Depthm=MAX, Range_Depth=RANGE, Avg_Depthm=MEAN)

bathy.data$block_id=as.double(bathy.data$block_id)

# merge bathymetry data with block data
all_blocks<-left_join(all_blocks,bathy.data, by="block_id")%>%
  filter(blck_ty != "Offshore" & blck_st != "Oregon")
all_blocks_df <- all_blocks %>% sf::st_drop_geometry()


#upload port information - only plotting the major port of each complex
ports<-read_sf(paste0(base_path,'Variables/Distance to Port Complex/Data/Raw/port codes_updated_April2025.shp'))%>%
  rename(port=PortName)%>%
  mutate(port = str_to_title(port))%>%
  filter(port=="Eureka"| port=="Fort Bragg"| port=="Bodega Bay"| port=="San Francisco"| port=="Monterey"| port=="Morro Bay"| port=="Santa Barbara Harbor"| port=="Los Angeles"| port=="San Diego")%>%
  mutate(MajorPort = str_to_title(MajorPort))%>%
  rename(port.complex=MajorPort)

#upload Eureka CPFV non-confidential plots
cpfv_block_data<-readRDS(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/CPFV_summarizeddata_sepyears.rds"))


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
#################

#################
#get CPFV eureka informaiton


CPFV_blocks<-cpfv_block_data%>%
  st_drop_geometry%>%
  filter(port.complex %in% c("Eureka", "Monterey", "Los Angeles"),
         year == 2022)%>%
  # Rule of three: only now drop blocks with fewer than 3 vessels
  filter(nvessels >= 3) %>%
    group_by(port.complex)%>%
    mutate(port_total_hours=sum(total_angler_hours, na.rm=TRUE),
           pct_annual_effort = 100 * total_angler_hours / port_total_hours)%>%
  ungroup() %>%    
  left_join(all_blocks %>% 
            dplyr::select(block_id))%>% 
  sf::st_as_sf()%>%
  filter(!st_is_empty(.))%>%
  #remove blocks that dont exist
  st_make_valid()


#plot
main_plot <- ggplot(data = CPFV_blocks,
                    mapping = aes(fill = port.complex, alpha = pct_annual_effort)) +
  # All CDFW blocks as light grey background (drawn first so CPFV blocks sit on top)
  geom_sf(data = all_blocks, fill = "white", color = "grey60",
          linewidth = 0.15, inherit.aes = FALSE) +
  # CPFV blocks: color = port, transparency = share of that port's annual effort
  geom_sf(linewidth = 0.1, color = "grey40") +

  scale_fill_manual(name = "Port complex",
                    values = c("Eureka"      = "#0072B2",   # blue
                               "Monterey"    = "#009E73",   # green
                               "Los Angeles" = "#D55E00"),  # orange (Okabe-Ito, colorblind-safe)
                    breaks = c("Eureka", "Monterey", "Los Angeles")) +  # legend order, north to south
  scale_alpha_continuous(name = "Percent of annual\nangler hours",
                         range = c(0.15, 1),                # faintest blocks still visible
                         trans = "sqrt",                    # spreads out the many small values
                         labels = scales::label_number(accuracy = 1, suffix = "%")) +
  guides(
    # Port legend: black outline + full opacity so the port colors read clearly
    fill  = guide_legend(override.aes = list(color = "black", linewidth = 0.5, alpha = 1)),
    # Effort legend: grey boxes (so it doesn't look tied to one port)
    alpha = guide_legend(override.aes = list(fill = "grey20"))
  ) +
  # Land
  geom_sf(data = usa_mexico_sf, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = FALSE) +
  # State waters
  geom_sf(data = state_waters, color = "grey30", linewidth = 0.2, inherit.aes = FALSE) +
  # Port points
  geom_sf(data = ports_sf, inherit.aes = FALSE, color = "black", size = 1.5) +
  # Port labels
  geom_sf_text(data = ports_sf, aes(label = name), inherit.aes = FALSE,
               size = 3.5, nudge_x = 0.3, hjust = 0) +
  # Crop
  coord_sf(xlim = c(-115.5, -126), ylim = c(32.5, 42)) +
  # Axes
  scale_x_continuous(breaks = seq(-128, -117, 2)) +
  scale_y_continuous(breaks = seq(32, 44, 2)) +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme

main_plot
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
  draw_plot(inset, x = 0.475, y = 0.783, width = 0.28, height = 0.22)

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CDFW blocks.jpg')
ggsave(filename = filename1, plot =  p , width = 6, height = 5, units = "in", dpi = 300)

