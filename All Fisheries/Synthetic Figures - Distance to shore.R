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
#Distance to shore data manipulation

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
      model <- lm(cog_dist_shore~ year, data = complex_data)
      
      # Save slope and confidence interval
      cf <- coef(model)
      slope <- cf[2]
      CI <- confint(model, level = 0.95)
      lower_bound <- CI[2, 1]
      upper_bound <- CI[2, 2]
      
      # Save in a dataframe
      df <- data.frame(
        slope.distance = slope,
        CI_lower_bound = lower_bound,
        CI_upper_bound = upper_bound,
        fishery2 = fishery2,
        port_complex = port.complex
      )
      
      datalist[[port.complex]] <- df
    } else {
      # Append NA values for empty data
      df <- data.frame(
        slope.distance = NA,
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

distance.results <- dplyr::bind_rows(a, b, c, d, e, f, g, h, i, j, k, l)
distance.results$port_complex<- factor(distance.results$port_complex, levels = rev(c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego')))
############

############
#Distance to shore Figure by fishery
p<-ggplot(data=distance.results, aes(x= slope.distance, y=fishery2, color=port_complex))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound, xmax = CI_upper_bound), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "Slope of distance to shore over time",
    y = "Fishery",
    color = "Port Complex"
  )+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"))+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                     guide = guide_legend(reverse = TRUE) )
filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/Distance2shore_fishery_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############


############
#Distance to shore Figure by complex
p<-ggplot(data=distance.results, aes(x= slope.distance, y=port_complex, color=fishery2))+
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

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/Distance2shore_complex_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############



############
# GPQ-based Confidence Interval for CV
# adapted from https://scholarworks.gsu.edu/cgi/viewcontent.cgi?article=1116&context=math_theses
gpq_ci_cv <- function(data, alpha = 0.05, n_sim = 10000) {
  n <- length(data)
  if (n <= 1) stop("Sample size must be greater than 1")
  
  # STEP 1: Compute the sample mean and sample standard deviation using original
  #observation samples.
  
  sample_mean <- mean(data)
  sample_sd <- sd(data)
  
  #stop if sample mean is 0
  if (sample_mean == 0) stop("Mean is zero; CV is undefined.")
  
  # Placeholder for simulated Rk values
  Rk_values <- numeric(n_sim)
  
  #STEP 2: Randomly generate one U from corresponding chi-squared distribution with
  #  n − 1 degree of freedom and one Z from corresponding normal distribution.
  
  # Monte Carlo Simulation
  for (i in 1:n_sim) {
    # Step 2: Generate random U and Z
    U <- rchisq(1, df = n - 1)  #will mimick a chi squared distribution with n-1 degrees of freedom
    Z <- rnorm(1, mean = 0, sd = 1)  # will generate a random value from a standard normal distribution with mean of 0 and sd of 1
    
    # Step 3: Calculate Rσ²
    R_sigma2 <- ((n - 1) * sample_sd^2) / U
    
    # Step 4: Calculate Rµ
    R_mu <- sample_mean - sqrt(R_sigma2 / n) * Z
    
    # Step 5: Calculate Rk
    Rk_values[i] <- sqrt(R_sigma2) / abs(R_mu)
  }
  
  # Confidence interval
  lower_bound <- quantile(Rk_values, probs = alpha / 2)
  upper_bound <- quantile(Rk_values, probs = 1 - alpha / 2)
  
  # CV estimate
  cv_estimate <- sample_sd / abs(sample_mean)
  
  list(
    CV = cv_estimate,
    Lower_CI = lower_bound,
    Upper_CI = upper_bound
  )
}
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
    
    sigma=sd(complex_data$cog_dist_shore)
    mu=mean(complex_data$cog_dist_shore)  
    cv=sigma/abs(mu)
    n=nrow(complex_data) #n
    se_cv <- sqrt(1 / (2 * n))  # Standard error of CV
    upper.ci <- cv * (1 + z * se_cv)
    lower.ci <- cv * (1 - z * se_cv)
    
    results<- gpq_ci_cv(complex_data$cog_dist_shore)
    # Extract the lower and upper bounds from the result
    upper.ci.2 <- results$Upper_CI
    lower.ci.2 <- results$Lower_CI
    
    df <- data.frame(
      CV=cv,
      lower.CI=lower.ci,
      upper.CI= upper.ci,
      lower.CI.2=lower.ci.2, 
      upper.CI.2 = upper.ci.2 ,
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
a<- plot_fun("CPFV")
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
#DepthFigure by fishery
p<-ggplot(data=results, aes(x= CV, y=fishery2, color=port_complex))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = lower.CI, xmax = upper.CI), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
 # geom_errorbarh(aes(xmin = lower.CI.2, xmax = upper.CI.2), height = 0.3, position = position_dodge(width = 0.6)) +
  labs(
    x = "CV of Distance to Shore",
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

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CV_shore_fishery_synthetic figure.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############

############
#areaFigure by complex
p<-ggplot(data=results, aes(x= CV, y=port_complex, color=fishery2))+
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = lower.CI, xmax = upper.CI), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "CV of Distance to Shore",
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

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/CV_shore_complex_synthetic figure_2.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 6, units = "in", dpi = 300)
############


