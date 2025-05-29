library(tidyverse)

#########
#upload adata
filepath<-('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/data/')

#area CV
area_cv<-readRDS(paste0(filepath, 'area_cv.rds'))%>%
  mutate(metric="(a) Area")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet" &
           !(fishery2 == "Pink shrimp trawl" & port_complex == "Monterey"))
#latittude CV
latitude_cv<-readRDS(paste0(filepath, 'latitude_cv.rds'))%>%
  mutate(metric="(b) Latitude")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet" &
           !(fishery2 == "Pink shrimp trawl" & port_complex == "Monterey"))
#northern extent CV
extent_north_cv<-readRDS(paste0(filepath, 'extents_cv.rds'))%>%
  dplyr::select(-c(CV2, lower.CI2, upper.CI2,lower.CI2.2, upper.CI2.2))%>%
  mutate(metric="(c) Northern extent")%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet" &
           !(fishery2 == "Pink shrimp trawl" & port_complex == "Monterey"))
#southern extent CV
extent_south_cv<-readRDS(paste0(filepath, 'extents_cv.rds'))%>%
  dplyr::select(-c(CV, lower.CI, upper.CI,lower.CI.2, upper.CI.2))%>%
  mutate(metric="(c) Northern extent")%>%
  rename(CV=CV2, lower.CI=lower.CI2, upper.CI=upper.CI2,lower.CI.2=lower.CI2.2, upper.CI.2=upper.CI2.2)%>%
  filter(fishery2!="Spot prawn trawl" & fishery2!="Drift gillnet" &
           !(fishery2 == "Pink shrimp trawl" & port_complex == "Monterey"))
#########

#########

#create function for cv graph

ggplot(data=extent_north_cv, aes(x= CV, y=port_complex))+
  geom_boxplot() 

ggplot(data=extent_north_cv, aes(x= CV, y=port_complex))+
  geom_point(size = 1.5, position = position_dodge(width = 0.5))

plot_fun<-function(df){
  
  
  p<-ggplot(data=df, aes(x= CV, y=port_complex, fill=port_complex))+
    geom_boxplot() +  # Dodge points to avoid overlap
   
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
    scale_fill_brewer(name = "Port Complex",palette = 'RdYlBu', direction=1,
                       guide = guide_legend(reverse = TRUE) )
  p
  
}

a2<-plot_fun(area_cv)
b2<-plot_fun(latitude_cv)
c2<-plot_fun(extent_north_cv)
d2<-plot_fun(extent_south_cv)

a2
b2
c2
d2
filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/boxplot_cv_south.jpg')
ggsave(filename = filename1, plot =  d2 , width = 10, height = 8, units = "in", dpi = 300)

