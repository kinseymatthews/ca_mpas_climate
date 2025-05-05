
library(tidyverse)

############
#Upload data for each fishery
cpfv<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/MPA_area.rds")%>%
  mutate(fishery="CPFV",Year=as.numeric(Year))
cuc_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/MPA_area.rds")%>%
  mutate(fishery="Cucumber trawl",Year=as.numeric(Year))
cuc_dive<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/Cucumber_MPA_area.rds")%>%
  mutate(fishery="Cucumber dive",Year=as.numeric(Year))
urchin<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/Urchin_MPA_area.rds")%>%
  mutate(fishery="Urchin dive",Year=as.numeric(Year))
drift<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/drift_MPA_area.rds")%>%
  mutate(fishery="Drift gillnet",Year=as.numeric(Year))
set<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/setgillnet_MPA_area.rds")%>%
  mutate(fishery="Set gillnet",Year=as.numeric(Year))
hal_trawl<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/MPA_area.rds")%>%
  mutate(fishery="Halibut trawl",Year=as.numeric(Year))
lobster<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/MPA_area.rds")%>%
  mutate(fishery="Lobster trap",Year=as.numeric(Year))
pink_shrimp<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pinkMPA_area.rds")%>%
  mutate(fishery="Pink shrimp trawl",Year=as.numeric(Year))
ridge_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridgeMPA_area.rds")%>%
  mutate(fishery="Ridgeback prawn trawl",Year=as.numeric(Year))
spot_prawn<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/spotMPA_area.rds")%>%
  mutate(fishery="Spot prawn trawl",Year=as.numeric(Year))
squid<- readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/MPA_area.rds")%>%
  mutate(fishery="Squid seine",Year=as.numeric(Year))

fisheries<-rbind(cpfv,cuc_trawl,cuc_dive,urchin, drift, set, hal_trawl, lobster, pink_shrimp, ridge_prawn,squid)


saveRDS(fisheries,"G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/allfisheries_effortmpa.rds")

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
#MPA data manipulation

#get list of each fishery
distinct_fishery<-unique(fisheries$fishery)

# Corrected function
plot_fun <- function(fishery2) {
  # Filter data for the selected fishery
  fish <- fisheries %>%
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
      model <- lm(percent_inmpa~ Year, data = complex_data)
      
      # Save slope and confidence interval
      cf <- coef(model)
      slope <- cf[2]
      CI <- confint(model, level = 0.95)
      lower_bound <- CI[2, 1]
      upper_bound <- CI[2, 2]
      
      # Save in a dataframe
      df <- data.frame(
        slope.mpa = slope,
        CI_lower_bound = lower_bound,
        CI_upper_bound = upper_bound,
        fishery2 = fishery2,
        port_complex = port.complex
      )
      
      datalist[[port.complex]] <- df
    } else {
      # Append NA values for empty data
      df <- data.frame(
        slope.mpa = NA,
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
results$fishery2<- factor(results$fishery2, levels = rev(c( 'Pink shrimp trawl', 'Ridgeback prawn trawl', 'Spot prawn trawl', 'Halibut trawl', 'Cucumber trawl', 'Squid seine', 'Drift gillnet', 'Set gillnet', 'Lobster trap','Urchin dive', 'Cucumber dive','CPFV')))
############

############
############
#calculating lower and upper fences and truncating 
lower_quartile_lower <- quantile(results$CI_lower_bound, 0.25, na.rm = TRUE)
upper_quartile_lower <- quantile(results$CI_lower_bound, 0.75, na.rm = TRUE)
iqr_lower <- upper_quartile_lower - lower_quartile_lower
lower_fence_lower <- lower_quartile_lower - 1.5 * iqr_lower
upper_fence_lower <- upper_quartile_lower + 1.5 * iqr_lower

# Calculate IQR fences for upper confidence intervals
lower_quartile_upper <- quantile(results$CI_upper_bound, 0.25, na.rm = TRUE)
upper_quartile_upper <- quantile(results$CI_upper_bound, 0.75, na.rm = TRUE)
iqr_upper <- upper_quartile_upper - lower_quartile_upper
lower_fence_upper <- lower_quartile_upper - 1.5 * iqr_upper
upper_fence_upper <- upper_quartile_upper + 1.5 * iqr_upper
############


############
#truncating values
results2 <- results %>%
  mutate(
    CI_lower_clipped = CI_lower_bound < lower_fence_lower | CI_lower_bound > upper_fence_upper,
    CI_upper_clipped = CI_upper_bound < lower_fence_lower | CI_upper_bound > upper_fence_upper,
    CI_length = CI_upper_bound - CI_lower_bound,  # Original interval length
    midpoint = (CI_upper_bound + CI_lower_bound) / 2,  # Midpoint of the interval
    CI_adjusted_length = pmin(upper_fence_upper - lower_fence_lower, CI_length),  # Maximum allowed length
    CI_lower_bound = ifelse(CI_lower_clipped , 
                            lower_fence_lower , 
                            CI_lower_bound),
    CI_upper_bound = ifelse( CI_upper_clipped, 
                             upper_fence_upper, 
                             CI_upper_bound),
    CI_clipped = CI_lower_clipped | CI_upper_clipped  # Combined clipping indicator
  )
############

############
results3<-results2%>%
  filter(!(port_complex == "Los Angeles" & fishery2 == "Spot prawn trawl trawl"))

#replace the original slopes of the ones above with the new values
#find max and min of confidence intervals
x=max(results3$CI_upper_bound) 
y=min(results3$CI_lower_bound) 

results3<- results2 %>%
  mutate(
    slope.mpa2 = case_when(
      slope.mpa >x ~ x ,
      slope.mpa < y ~ y ,
      TRUE ~ slope.mpa ),
    
    CI_lower_bound_2 = case_when(
      slope.mpa > x~ 0,
      slope.mpa < y ~ 0,
      TRUE ~ CI_lower_bound),
    
    CI_upper_bound_2 = case_when(
      slope.mpa > x ~ 0,
      slope.mpa < y ~ 0,
      TRUE ~ CI_upper_bound),
    
    CI_clipped = case_when(
      slope.mpa > x  ~ TRUE,
      slope.mpa< y~ TRUE,
      TRUE ~ CI_clipped)
    
  )

saveRDS(results3,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/data/mpa_slope.rds')

############


############
#mpaFigure by fishery
p<-ggplot(data=results3, aes(x= slope.mpa2, y=fishery2, color=port_complex))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound_2, xmax = CI_upper_bound_2), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  geom_text(
    aes(
      x = ifelse(CI_clipped, slope.mpa2, NA),  # Position at CI_upper_bound
      label = ifelse(CI_clipped, "*", "")          # Label only for clipped intervals
    ),
    hjust = -0.6,  # Offset slightly to the right of the CI
    position = position_dodge(width = 0.5)
  ) +
  #astericks for changed slope values
  geom_text(
    aes(
      x = (slope.mpa2),  # Position at point
      label = ifelse(CI_lower_bound_2==0, "*", "")          # Label only for clipped intervals
    ),
    hjust = -1.6,  # Offset slightly to the right of the CI
    position = position_dodge(width = 0.5)
    
  ) +
  labs(
    x = "Slope of 75% KD area in MPAs",
    y = "Fishery",
    color = "Port Complex"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                     guide = guide_legend(reverse = TRUE) )

p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/MPA_fishery_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############

############
#Latitude complex Figure
p<-ggplot(data=results3, aes(x= slope.mpa2, y=port_complex, color=fishery2))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound_2, xmax = CI_upper_bound_2), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  geom_text(
    aes(
      x = ifelse(CI_clipped, slope.mpa2, NA),  # Position at CI_upper_bound
      label = ifelse(CI_clipped, "*", "")          # Label only for clipped intervals
    ),
    hjust = -0.6,  # Offset slightly to the right of the CI
    position = position_dodge(width = 0.5)
  ) +
  #astericks for changed slope values
  geom_text(
    aes(
      x = (slope.mpa2),  # Position at point
      label = ifelse(CI_lower_bound_2==0, "*", "")          # Label only for clipped intervals
    ),
    hjust = -1.6,  # Offset slightly to the right of the CI
    position = position_dodge(width = 0.5)
    
  ) +
  labs(
    x = "Slope of 75% KD area in MPAs",
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

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/mpa_complex_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############


############
############
#other synethic figures
#critical value for CI
z <- qnorm((1 + 0.95) / 2) 

# Corrected function
plot_fun <- function(fishery2) {
  # Filter data for the selected fishery
  fish <- fisheries %>%
    filter(fishery == fishery2)
  
  # Get list of distinct port complexes
  distinct_complexes <- unique(fish$port.complex)
  
  # Empty list to store results
  datalist <- list()
  
  for (port.complex in distinct_complexes) {
    # Subset data for the current port complex
    complex_data <- fish[fish$port.complex == port.complex, ]
    
    sigma=sd(complex_data$percent_inmpa)
    mu=mean(complex_data$percent_inmpa)  
    cv=sigma/abs(mu)
    n=nrow(complex_data) #n
    se_cv <- sqrt(1 / (2 * n))  # Standard error of CV
    upper.ci <- cv * (1 + z * se_cv)
    lower.ci <- cv * (1 - z * se_cv)
    

    
    df <- data.frame(
      CV=cv,
      lower.CI=lower.ci,
      upper.CI= upper.ci,
      fishery2 = fishery2,
      port_complex = port.complex
    )
    
    datalist[[port.complex]] <- df
    
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
results$fishery2<- factor(results$fishery2, levels = rev(c( 'Pink shrimp trawl', 'Ridgeback prawn trawl', 'Spot prawn trawl', 'Halibut trawl', 'Cucumber trawl', 'Squid seine', 'Drift gillnet', 'Set gillnet', 'Lobster trap','Urchin dive', 'Cucumber dive','CPFV')))

saveRDS(results,'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/data/mpa_cv.rds')

############

############
#Distance to shore Figure by fishery
p<-ggplot(data=results, aes(x= CV, y=fishery2, color=port_complex))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = lower.CI, xmax = upper.CI), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  #  geom_errorbarh(aes(xmin = lower.CI.2, xmax = upper.CI.2), height = 0.3, position = position_dodge(width = 0.6)) +
  labs(
    x = "CV of Area in MPA",
    y = "Fishery",
    color = "Port Complex"
  )+
  
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                     guide = guide_legend(reverse = TRUE) )
p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CV_MPA_fishery_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############

############
#areaFigure by complex
p<-ggplot(data=results, aes(x= CV, y=port_complex, color=fishery2))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = lower.CI, xmax = upper.CI), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "CV of Area",
    y = "Port Complex",
    color = "Fishery"
  )+
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))
#scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=-1)
p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CV_MPA_complex_synthetic figure_2.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############

