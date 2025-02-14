
library(tidyverse)
library(ggpubr)
library(patchwork)

############
#Upload data for each metric
filepath<-('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/data/')

area_slope<-readRDS(paste0(filepath, 'area_slope.rds'))%>%
  rename(slope=slope.area, slope2=slope.area2)%>%
  mutate(metric="(a) Area")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

area_cv<-readRDS(paste0(filepath, 'area_cv.rds'))%>%
  mutate(metric="(a) Area")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

latitude_slope<-readRDS(paste0(filepath, 'latitude_slope.rds'))%>%
  rename(slope=slope.latitude, slope2=slope.latitude2)%>%
  mutate(metric="(b) Latitude")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

latitude_cv<-readRDS(paste0(filepath, 'latitude_cv.rds'))%>%
  mutate(metric="(b) Latitude")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

depth_slope<-readRDS(paste0(filepath, 'depth_slope.rds'))%>%
  rename(slope=slope.depth, slope2=slope.depth)%>%
  mutate(metric="(e) Depth")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

depth_cv<-readRDS(paste0(filepath, 'depth_cv.rds'))%>%
  mutate(metric="(e) Depth")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

mpa_slope<-readRDS(paste0(filepath, 'mpa_slope.rds'))%>%
  rename(slope=slope.mpa, slope2=slope.mpa2)%>%
  mutate(metric="(e) MPA")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

mpa_cv<-readRDS(paste0(filepath, 'mpa_cv.rds'))%>%
  mutate(metric="(e) MPA")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

distance_slope<-readRDS(paste0(filepath, 'distance_slope.rds'))%>%
  rename(slope=slope.distance)%>%
  mutate(metric="(e) Distance to shore")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")


distance_cv<-readRDS(paste0(filepath, 'distance_cv.rds'))%>%
  mutate(metric="(e) Distance to shore")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

extent_north_slope<-readRDS(paste0(filepath, 'extents_slope.rds'))%>%
  dplyr::select(-c(slope.south, CI_lower_bound_south,CI_upper_bound_south, CI_lower_clipped_south, CI_upper_clipped_south, CI_length_south,midpoint_south,CI_adjusted_length_south,CI_clipped_south, slope.south2,CI_lower_bound_south_2,CI_upper_bound_south_2))%>%
  rename(slope=slope.north, slope2=slope.north2, CI_lower_bound_2=CI_lower_bound_north_2, CI_upper_bound_2=CI_upper_bound_north_2)%>%
  mutate(metric="(c) Northern extent")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")
# Rename columns
colnames(extent_north_slope) <- gsub("_north$", "", colnames(extent_north_slope))

extent_south_slope<-readRDS(paste0(filepath, 'extents_slope.rds'))%>%
  dplyr::select(-c(slope.north, CI_lower_bound_north,CI_upper_bound_north, CI_lower_clipped_north, CI_upper_clipped_north, CI_length_north,midpoint_north,CI_adjusted_length_north,CI_clipped_north, slope.north2,CI_lower_bound_north_2,CI_upper_bound_north_2))%>%
  rename(slope=slope.south, slope2=slope.south2, CI_lower_bound_2=CI_lower_bound_south_2, CI_upper_bound_2=CI_upper_bound_south_2)%>%
  mutate(metric="(d) Southern extent")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")
# Rename columns
colnames(extent_south_slope) <- gsub("_south$", "", colnames(extent_south_slope))

extent_north_cv<-readRDS(paste0(filepath, 'extents_cv.rds'))%>%
  dplyr::select(-c(CV2, lower.CI2, upper.CI2,lower.CI2.2, upper.CI2.2))%>%
  mutate(metric="(c) Northern extent")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")

extent_south_cv<-readRDS(paste0(filepath, 'extents_cv.rds'))%>%
  dplyr::select(-c(CV, lower.CI, upper.CI,lower.CI.2, upper.CI.2))%>%
  mutate(metric="(c) Northern extent")%>%
  rename(CV=CV2, lower.CI=lower.CI2, upper.CI=upper.CI2,lower.CI.2=lower.CI2.2, upper.CI.2=upper.CI2.2)%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet")
############

############
#create function to plot the different graphs

plot_fun<-function(df){
  
  title <- unique(df[['metric']])
  
p<-ggplot(data=df, aes(x= slope2, y=fishery2, color=port_complex))+
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound_2, xmax = CI_upper_bound_2), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "Slope",
    y = "Fishery",
    color = "Port Complex"
  )+
  ggtitle(title)+
  #astericks for clipped confidence intervals
  geom_text(
    aes(
      x = ifelse(CI_clipped, slope2, NA),  # Position at main point
      label = ifelse(CI_clipped, "*", "")          # Label only for clipped intervals
    ),
    hjust = -0.6,  # Offset slightly to the right of the CI
    position = position_dodge(width = 0.5)
    # color = "black", size = 3
  ) +
  #astericks for changed slope values
  geom_text(
    aes(
      x = (slope2),  # Position at point
      label = ifelse(CI_lower_bound_2==0, "*", "")          # Label only for clipped intervals
    ),
    hjust = -1.6,  # Offset slightly to the right of the CI
    position = position_dodge(width = 0.5)
    
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"),
          plot.title=element_text(size=10,margin = margin(t = 1))) +
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                     guide = guide_legend(reverse = TRUE) )
p

}

a<-plot_fun(area_slope)
b<-plot_fun(latitude_slope)
c<-plot_fun(extent_north_slope)
d<-plot_fun(extent_south_slope)
#e<-plot_fun(mpa_slope)

f<-ggplot(data=distance_slope, aes(x= slope, y=fishery2, color=port_complex))+
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
  geom_errorbarh(aes(xmin = CI_lower_bound, xmax = CI_upper_bound), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
  labs(
    x = "Slope",
    y = "Fishery",
    color = "Port Complex"
  )+
  ggtitle('(e) Distance to shore')+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  #base_theme+
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "lightgray"),
          plot.title=element_text(size=10,margin = margin(t = 1)) )+
  scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                     guide = guide_legend(reverse = TRUE) )
############


############
#create function for cv graph

plot_fun<-function(df){
  
  
  p<-ggplot(data=df, aes(x= CV, y=fishery2, color=port_complex))+
    geom_point(size = 1.5, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
    geom_errorbarh(aes(xmin = lower.CI, xmax = upper.CI), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
    #  geom_errorbarh(aes(xmin = lower.CI.2, xmax = upper.CI.2), height = 0.3, position = position_dodge(width = 0.6)) +
    labs(
      x = "CV",
      y = "Fishery",
      color = "Port Complex"
    )+
    
    #base_theme+
    theme(  panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "lightgray"),
            plot.title=element_text(size=10,margin = margin(t = 1))) +
    scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                       guide = guide_legend(reverse = TRUE) )
  p
  
}

a2<-plot_fun(area_cv)
b2<-plot_fun(latitude_cv)
c2<-plot_fun(extent_north_cv)
d2<-plot_fun(extent_south_cv)
#e2<-plot_fun(mpa_cv)
f2<-plot_fun(distance_cv)
############


############

  combined_plot <- a+b+c+d+f+
    plot_layout(ncol = 5, nrow=1 ,widths = c(1, 1,1,1,1,1)) &
    theme(axis.text.y = element_blank(),  # Remove y-axis text
          axis.title.x = element_blank(),# Remove x-axis text
          axis.ticks.y = element_blank(), # Remove y-axis ticks
          axis.title.y = element_blank(), # Remove y-axis title
          legend.position = "none", #Hide legends by default
          plot.margin=margin(0,0,0,0)
    )      
  
    combined_plot2 <- a2+b2+c2+d2+f2+
    plot_layout(ncol = 5, nrow=1 ,widths = c(1, 1,1,1,1,1)) &
      theme(axis.text.y = element_blank(),  # Remove y-axis text
            axis.title.x = element_blank(),# Remove x-axis text
            axis.ticks.y = element_blank(), # Remove y-axis ticks
            axis.title.y = element_blank(), # Remove y-axis title
            legend.position = "none", #Hide legends by default
            plot.margin=margin(0,0,0,0) )      
    
  # Add the y-axis back only for the first plot
  combined_plot[[1]] <- combined_plot[[1]] + theme(
    axis.text.y = element_text(),
    axis.ticks.y = element_line()
    
  )
  
  combined_plot2[[1]] <- combined_plot2[[1]] + theme(
    axis.text.y = element_text(),
    axis.ticks.y = element_line()
  )
  
  # Increase the margin only for plot 6 and plot 12 (the last column)
  combined_plot[[5]] <- combined_plot[[5]] + theme(
    plot.margin = margin(0, 15, 0, 0)  # Increase space on the right for plot 6
  )
  
  combined_plot2[[5]] <- combined_plot2[[5]] + theme(
    plot.margin = margin(15, 15, 0, 0)  # Increase space on the right for plot 12
  )
  
  combined_plot[[5]] <- combined_plot[[5]] + theme(legend.position = "right",  # Place the legend to the far right for plot 6
                                                   legend.title = element_text(size = 9),  # Smaller font size for legend title
                                                   legend.text = element_text(size = 8) ) 

  
  # Create an invisible plot with text between the two rows
  text_plot <- ggplot() +
    geom_text(aes(x = .1, y = .1, label = "Slope"), size = 3.5) +
    theme_void() +  # Remove axes and background
    theme(
      plot.margin = margin(0, 0, 0, 0)  # Remove margins
    )
  
  text_plot2 <- ggplot() +
    geom_text(aes(x = .1, y = .1, label = "CV"), size = 3.5) +
    theme_void() +  # Remove axes and background
    theme(
      plot.margin = margin(0, 0, 0, 0)  # Remove margins
    )
  
  # Create the combined layout with the text plot between the two rows
  combined_plot_with_text <- (combined_plot / text_plot / combined_plot2 / text_plot2) +
    plot_layout( nrow=4, heights = c(1, .1,1,.1))
  combined_plot_with_text 
  
  filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/Combined_synthetic figure_1.jpg')
  ggsave(filename = filename1, plot =   combined_plot_with_text , width = 15, height = 8, units = "in", dpi = 300)
  ############
  
  
  
  #Graph 2 other way
############
  #create function to plot the different graphs
  
  plot_fun<-function(df){
    
    title <- unique(df[['metric']])
    
    p<-ggplot(data=df, aes(x= slope2, y=port_complex, color=fishery2))+
      geom_point(size = 1.5, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
      geom_errorbarh(aes(xmin = CI_lower_bound_2, xmax = CI_upper_bound_2), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
      labs(
        x = "Slope",
        y = "Port Complex",
        color = "Fishery"
      )+
      ggtitle(title)+
      #astericks for clipped confidence intervals
      geom_text(
        aes(
          x = ifelse(CI_clipped, slope2, NA),  # Position at main point
          label = ifelse(CI_clipped, "*", "")          # Label only for clipped intervals
        ),
        hjust = -0.6,  # Offset slightly to the right of the CI
        position = position_dodge(width = 0.5)
        # color = "black", size = 3
      ) +
      #astericks for changed slope values
      geom_text(
        aes(
          x = (slope2),  # Position at point
          label = ifelse(CI_lower_bound_2==0, "*", "")          # Label only for clipped intervals
        ),
        hjust = -1.6,  # Offset slightly to the right of the CI
        position = position_dodge(width = 0.5)
        
      ) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
      #base_theme+
      theme(  panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "lightgray"),
              plot.title=element_text(size=10,margin = margin(t = 1))) 
     # scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
      #                   guide = guide_legend(reverse = TRUE) )
    p
    
  }
  
  a<-plot_fun(area_slope)
  b<-plot_fun(latitude_slope)
  c<-plot_fun(extent_north_slope)
  d<-plot_fun(extent_south_slope)
 # e<-plot_fun(mpa_slope)
  
  f<-ggplot(data=distance_slope, aes(x= slope, y=port_complex, color=fishery2))+
    geom_point(size = 1.5, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
    geom_errorbarh(aes(xmin = CI_lower_bound, xmax = CI_upper_bound), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
    labs(
      x = "Slope",
      y = "Port Complex",
      color = "Fishery"
    )+
    ggtitle('(e) Distance to shore')+
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
    #base_theme+
    theme(  panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "lightgray"),
            plot.title=element_text(size=10,margin = margin(t = 1)) )
   # scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
    #                   guide = guide_legend(reverse = TRUE) )
  ############
  
  
  ############
  #create function for cv graph
  
  plot_fun<-function(df){
    
    
    p<-ggplot(data=df, aes(x= CV, y=port_complex, color=fishery2))+
      geom_point(size = 1.5, position = position_dodge(width = 0.5)) +  # Dodge points to avoid overlap
      geom_errorbarh(aes(xmin = lower.CI, xmax = upper.CI), height = 0.2, position = position_dodge(width = 0.5)) +  # Apply dodge to error bars
      #  geom_errorbarh(aes(xmin = lower.CI.2, xmax = upper.CI.2), height = 0.3, position = position_dodge(width = 0.6)) +
      labs(
        x = "CV",
        y = "Port Complex",
        color = "Fishery"
      )+
      
      #base_theme+
      theme(  panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "lightgray"),
              plot.title=element_text(size=10,margin = margin(t = 1))) 
     # scale_color_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
      #                   guide = guide_legend(reverse = TRUE) )
    p
    
  }
  
  a2<-plot_fun(area_cv)
  b2<-plot_fun(latitude_cv)
  c2<-plot_fun(extent_north_cv)
  d2<-plot_fun(extent_south_cv)
  #e2<-plot_fun(mpa_cv)
  f2<-plot_fun(distance_cv)
  ############
  
  
  ############
  
  combined_plot <- a+b+c+d+f+
    plot_layout(ncol = 5, nrow=1 ,widths = c(1, 1,1,1,1,1)) &
    theme(axis.text.y = element_blank(),  # Remove y-axis text
          axis.title.x = element_blank(),# Remove x-axis text
          axis.ticks.y = element_blank(), # Remove y-axis ticks
          axis.title.y = element_blank(), # Remove y-axis title
          legend.position = "none", #Hide legends by default
          plot.margin=margin(0,0,0,0)
    )      
  
  combined_plot2 <- a2+b2+c2+d2+f2+
    plot_layout(ncol = 5, nrow=1 ,widths = c(1, 1,1,1,1,1)) &
    theme(axis.text.y = element_blank(),  # Remove y-axis text
          axis.title.x = element_blank(),# Remove x-axis text
          axis.ticks.y = element_blank(), # Remove y-axis ticks
          axis.title.y = element_blank(), # Remove y-axis title
          legend.position = "none", #Hide legends by default
          plot.margin=margin(0,0,0,0) )      
  
  # Add the y-axis back only for the first plot
  combined_plot[[1]] <- combined_plot[[1]] + theme(
    axis.text.y = element_text(),
    axis.ticks.y = element_line()
    
  )
  
  combined_plot2[[1]] <- combined_plot2[[1]] + theme(
    axis.text.y = element_text(),
    axis.ticks.y = element_line()
  )
  
  # Increase the margin only for plot 6 and plot 12 (the last column)
  combined_plot[[5]] <- combined_plot[[5]] + theme(
    plot.margin = margin(0, 15, 0, 0)  # Increase space on the right for plot 6
  )
  
  combined_plot2[[5]] <- combined_plot2[[5]] + theme(
    plot.margin = margin(15, 15, 0, 0)  # Increase space on the right for plot 12
  )
  
  combined_plot[[5]] <- combined_plot[[5]] + theme(legend.position = "right",  # Place the legend to the far right for plot 6
                                                   legend.title = element_text(size = 9),  # Smaller font size for legend title
                                                   legend.text = element_text(size = 8) ) 
  
  
  # Create an invisible plot with text between the two rows
  text_plot <- ggplot() +
    geom_text(aes(x = .1, y = .1, label = "Slope"), size = 3.5) +
    theme_void() +  # Remove axes and background
    theme(
      plot.margin = margin(0, 0, 0, 0)  # Remove margins
    )
  
  text_plot2 <- ggplot() +
    geom_text(aes(x = .1, y = .1, label = "CV"), size = 3.5) +
    theme_void() +  # Remove axes and background
    theme(
      plot.margin = margin(0, 0, 0, 0)  # Remove margins
    )
  
  # Create the combined layout with the text plot between the two rows
  combined_plot_with_text <- (combined_plot / text_plot / combined_plot2 / text_plot2) +
    plot_layout( nrow=4, heights = c(1, .1,1,.1))
  combined_plot_with_text 
  
  filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/Combined_synthetic figure_2.jpg')
  ggsave(filename = filename1, plot =   combined_plot_with_text , width = 15, height = 8, units = "in", dpi = 300)
  ############
  