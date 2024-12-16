library(tidyverse)

############
#Upload data for each fishery
cpfv<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/COG_distance_results_anglerhours_final.rds")%>%
  mutate(fishery="CPFV",year=as.numeric(year))
cuc_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/seacucumber_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Cucumber trawl",year=as.numeric(year))
cuc_dive<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/COG_distance_results_divehours_final_cucumber.rds")%>%
  mutate(fishery="Cucumber dive",year=as.numeric(year))
urchin<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/COG_distance_results_divehours_final_seaurchin.rds")%>%
  mutate(fishery="Urchin dive",year=as.numeric(year))
drift<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/driftgillnet_COG_distance_results_final.rds")%>%
  mutate(fishery="Drift gillnet",year=as.numeric(year))
set<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/setgillnet_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Set gillnet",year=as.numeric(year))
hal_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/halibut_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Halibut trawl",year=as.numeric(year))
lobster<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/COG_distance_results_traphours_final.rds")%>%
  mutate(fishery="Lobster trap",year=as.numeric(year))
pink_shrimp<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pinkshrimp_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Pink shrimp trawl",year=as.numeric(year))
ridge_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridgeback_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Ridgeback prawn trawl",year=as.numeric(year))
spot_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/spot_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Spot prawn trawl",year=as.numeric(year))
squid<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/squid_COG_distance_results_towhours_final.rds")%>%
  mutate(fishery="Squid seine",year=as.numeric(year))

fisheries<-rbind(cpfv,cuc_trawl,cuc_dive,urchin, drift, set, hal_trawl, lobster, pink_shrimp,spot_prawn, ridge_prawn,squid)%>%
  dplyr::select(-X, -cog_longitude)
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
#################



############
# Convert to sf object and transform to new CRS (WGS84 - longlat)
fisheries_sf <- st_as_sf(fisheries, coords = c("longitude_calculated", "cog_latitude"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
fisheries_transformed <- st_transform(fisheries_sf, st_crs("+proj=longlat +datum=WGS84 +units=m +no_defs"))

# Add longitude and latitude to the data frame
fisheries_transformed$latitude <- st_coordinates(fisheries_transformed)[, "Y"]  # Extracts latitude
fisheries_transformed$longitude <- st_coordinates(fisheries_transformed)[, "X"]
fisheries_transformed<-fisheries_transformed %>% sf::st_drop_geometry()
############


############
#Latitude data manipulation

#get list of each fishery
distinct_fishery<-unique(fisheries$fishery)

# Corrected function
plot_fun <- function(fishery2) {
  # Filter data for the selected fishery
  fish <- fisheries_transformed %>%
    filter(fishery == fishery2)
  
  # Get list of distinct port complexes
  distinct_complexes <- unique(fish$port.complex)
  
  # Empty list to store results
  datalist <- list()
  
  for (port.complex in distinct_complexes) {
    # Subset data for the current port complex
    complex_data <- fish[fish$port.complex == port.complex, ]
    
    # Check if the subset data is not empty
    if (nrow(complex_data) > 0) {
      # Run linear regression
      model <- lm(latitude~ year, data = complex_data)
      
      # Save slope and confidence interval
      cf <- coef(model)
      slope <- cf[2]
      CI <- confint(model, level = 0.95)
      lower_bound <- CI[2, 1]
      upper_bound <- CI[2, 2]
      
      # Save in a dataframe
      df <- data.frame(
        slope.latitude = slope,
        CI_lower_bound = lower_bound,
        CI_upper_bound = upper_bound,
        fishery2 = fishery2,
        port_complex = port.complex
      )
      
      datalist[[port.complex]] <- df
    } else {
      # Append NA values for empty data
      df <- data.frame(
        slope = NA,
        CI_lower_bound = NA,
        CI_upper_bound = NA,
        fishery2 = fishery2,
        port_complex = port.complex
      )
      datalist[[port.complex]] <- df
    }
  }
  
  # Combine all results into a single dataframe
  all.data <- dplyr::bind_rows(datalist)
  return(all.data)
}

# Run the function
a <- plot_fun("CPFV")
b<-plot_fun("Cucumber trawl")
c<-plot_fun("Cucumber dive")
d<-plot_fun("Urchin dive")
e<-plot_fun("Drift gillnet")
f<-plot_fun("Set gillnet")
g<-plot_fun("Halibut trawl")
h<-plot_fun("Lobster trap")
i<-plot_fun("Pink shrimp trawl")
j<-plot_fun("Spot prawn trawl")
k<-plot_fun("Ridgeback prawn trawl")
l<-plot_fun("Squid seine")

latitude.results <- dplyr::bind_rows(a, b, c, d, e, f, g, h, i, j, k, l)
latitude.results$port_complex<- factor(latitude.results$port_complex, levels = rev(c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego')))
############

############
#Latitude fishery Figure
p<-ggplot(data=latitude.results, aes(x= slope.latitude, y=fishery2, color=port_complex))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound, xmax = CI_upper_bound), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "Slope of Latitude over Time",
    y = "Fishery",
    color = "Port Complex"
  )+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1)

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/Latitude__fishery_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############

############
#Latitude complex Figure
p<-ggplot(data=latitude.results, aes(x= slope.latitude, y=port_complex, color=fishery2))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound, xmax = CI_upper_bound), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "Slope of Latitude over Time",
    y = "Fishery",
    color = "Fishery"
  )+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))
  #scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1)

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/Latitude__complex_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############


