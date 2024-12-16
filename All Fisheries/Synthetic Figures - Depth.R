library(tidyverse)

############
#Upload data for each fishery
cpfv<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="CPFV")%>%
  filter(!is.na(port.complex))
cuc_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Cucumber trawl")%>%
  filter(port.complex=="Eureka" |port.complex=="Los Angeles" |port.complex=="Monterey" |port.complex=="Morro Bay" |port.complex=="Santa Barbara" )
cuc_dive<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/cucumber_depth.rds")%>%
  select(port.complex,year,Med_depth_m_recorded)%>%
  mutate(fishery="Cucumber dive")%>%
  rename(Depth_m=Med_depth_m_recorded)%>%
  filter(port.complex=="Los Angeles" |port.complex=="Santa Barbara" |port.complex=="San Diego" )
urchin<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/urchin_depth.rds")%>%
  select(port.complex,year,Med_depth_m_recorded)%>%
  mutate(fishery="Urchin dive")%>%
  rename(Depth_m=Med_depth_m_recorded)%>%
  filter(port.complex=="Fort Bragg" |port.complex=="Los Angeles" |port.complex=="San Diego" |port.complex=="Santa Barbara" )
drift<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/drift_depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Drift gillnet")%>%
  filter(port.complex=="Eureka" |port.complex=="Los Angeles" |port.complex=="Monterey" |port.complex=="Morro Bay" |port.complex=="Santa Barbara" |port.complex=="San Francisco"|port.complex=="San Diego")
set<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/set_depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Set gillnet")%>%
  filter(port.complex=="Bodega Bay" |port.complex=="Los Angeles" |port.complex=="Monterey" |port.complex=="Morro Bay" |port.complex=="Santa Barbara" |port.complex=="San Francisco"|port.complex=="San Diego")
hal_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Halibut trawl")%>%
  filter(port.complex=="Eureka" |port.complex=="Bodega Bay" |port.complex=="Fort Bragg" |port.complex=="Los Angeles" |port.complex=="Monterey" |port.complex=="Morro Bay" |port.complex=="Santa Barbara" |port.complex=="San Francisco")
lobster<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Lobster trap")%>%
  filter(port.complex=="Los Angeles" |port.complex=="San Diego"  |port.complex=="Santa Barbara" )
pink_shrimp<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pink_depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Pink shrimp trawl")%>%
  filter(port.complex=="Eureka" |port.complex=="Monterey" |port.complex=="Morro Bay"  |port.complex=="Santa Barbara" )
ridge_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridge_depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Ridgeback prawn trawl")%>%
  filter(port.complex=="Los Angeles" |port.complex=="Santa Barbara" )
spot_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/spot_depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Spot prawn trawl")%>%
  filter(port.complex=="Bodega Bay" |port.complex=="Los Angeles" |port.complex=="Monterey" |port.complex=="Morro Bay" |port.complex=="Santa Barbara" |port.complex=="San Francisco")
squid<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/depth.rds")%>%
  select(port.complex,year,Depth_m)%>%
  mutate(fishery="Squid seine")%>%
  filter(port.complex=="Los Angeles" |port.complex=="Monterey" |port.complex=="Santa Barbara" |port.complex=="San Francisco")

fisheries<-rbind(cpfv,cuc_trawl,cuc_dive,urchin, drift, set, hal_trawl, lobster, pink_shrimp,spot_prawn, ridge_prawn,squid)
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
#depth data manipulation
fisheries.avg<-fisheries%>%
  group_by(fishery,port.complex,year)%>%
  summarize(Depth_avg = mean(Depth_m), .groups = "drop")%>%
  mutate(year = as.numeric(year))

#get list of each fishery
distinct_fishery<-unique(fisheries.avg$fishery)

# Corrected function
plot_fun <- function(fishery2) {
  # Filter data for the selected fishery
  fish <- fisheries.avg%>%
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
      model <- lm(Depth_avg~ year, data = complex_data)
      
      # Save slope and confidence interval
      cf <- coef(model)
      slope <- cf[2]
      CI <- confint(model, level = 0.95)
      lower_bound <- CI[2, 1]
      upper_bound <- CI[2, 2]
      
      # Save in a dataframe
      df <- data.frame(
        slope.depth = slope,
        CI_lower_bound = lower_bound,
        CI_upper_bound = upper_bound,
        fishery2 = fishery2,
        port_complex = port.complex
      )
      
      datalist[[port.complex]] <- df
    } else {
      # Append NA values for empty data
      df <- data.frame(
        slope.depth = NA,
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

results <- dplyr::bind_rows(a, b, c, d, e, f, g, h, i, j, k, l)
results$port_complex<- factor(results$port_complex, levels = rev(c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego')))
############

############
#depthFigure by fishery
p<-ggplot(data=results, aes(x= slope.depth, y=fishery2, color=port_complex))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound, xmax = CI_upper_bound), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "Slope of depth over time",
    y = "Fishery",
    color = "Port Complex"
  )+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1)

p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/depth_fishery_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############

############
#Distance to shore Figure by complex
p<-ggplot(data=results, aes(x= slope.depth, y=port_complex, color=fishery2))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound, xmax = CI_upper_bound), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "Slope of distance to shore over time",
    y = "Fishery",
    color = "Fishery"
  )+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))
#scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1)
p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/depth_complex_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############

