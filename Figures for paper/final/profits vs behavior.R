library(tidyverse)


###########
#load in data
#plot of cs of slope of profits vs cv of slope of area
profits<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/allfisheries_exvessel_port_adjusted.rds")

filepath<-('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/data/')
area_cv<-readRDS(paste0(filepath, 'area_cv.rds'))%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")%>%
  dplyr::select(-c(lower.CI.2,upper.CI.2))%>%
  rename(CV.area=CV, lower.CI.area=lower.CI,upper.CI.area=upper.CI)
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
###########

###########
#edit profits data
#slope and then cv of slope

#critical value for CI
z <- qnorm((1 + 0.95) / 2) 

# Corrected function
plot_fun <- function(fishery2) {
  # Filter data for the selected fishery
  fish <- profits %>%
    filter(fishery == fishery2)
  
  # Get list of distinct port complexes
  distinct_complexes <- unique(fish$port.complex)
  
  # Empty list to store results
  datalist <- list()
  
  for (port.complex in distinct_complexes) {
    # Subset data for the current port complex
    complex_data <- fish[fish$port.complex == port.complex, ]
    
    sigma=sd(complex_data$all_value_adjusted)
    mu=mean(complex_data$all_value_adjusted)  
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

b<-plot_fun("Cucumber trawl")
c<-plot_fun("Cucumber dive")
d<-plot_fun("Urchin dive")
f<-plot_fun("Set gillnet")
g<-plot_fun("Halibut trawl")
h<-plot_fun("Lobster trap")
i<-plot_fun("Pink shrimp trawl")
k<-plot_fun("Ridgeback prawn trawl")
l<-plot_fun("Squid seine")

profit_cv <- dplyr::bind_rows( b, c, d,  f, g, h, i,  k, l)%>%
  rename(CV.profits=CV, lower.CI.proft=lower.CI,upper.CI.profit=upper.CI)
###########

###########
#now graph them together

#merge
area_profit_cv<-area_cv%>%
  left_join(profit_cv, by=c("fishery2", "port_complex"))%>%
  filter(fishery2!="CPFV")%>%
  filter(!(fishery2=="Cucumber trawl" & port_complex=="Eureka"))

area_profit_cv$port_complex<- factor(area_profit_cv$port_complex, levels = rev(c('Eureka', 'Fort Bragg', 'Bodega Bay', 'San Francisco', 'Monterey', 'Morro Bay', 'Santa Barbara', 'Los Angeles', 'San Diego')))

ggplot(area_profit_cv, aes(x=CV.area, y=CV.profits, ))+
  geom_point(size=2.5, aes(color=port_complex, shape=fishery2))+ 
  geom_smooth(method="lm")+
  scale_shape_manual(values = c(0:5, 6:10)) + # Add more as needed
    scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                       guide = guide_legend(reverse = TRUE) )+
  labs(y="Revenue", x="Behavior")+
  base_theme

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/Revenue_vs_Profits.jpg')
ggsave(filename = filename1, plot =  plot , width = 10, height = 8, units = "in", dpi = 300)


ggplot(area_profit_cv, aes(x=CV.area, y=CV.profits ))+
  facet_wrap(~fishery2)+
  geom_point(size=2.5, aes(color=port_complex))+ 
 # geom_smooth(method="lm")+
  scale_shape_manual(values = c(0:5, 6:10)) + # Add more as needed
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                     guide = guide_legend(reverse = TRUE) )+
  labs(y="Revenue", x="Behavior")+
  base_theme
