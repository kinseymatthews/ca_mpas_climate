library(tidyverse)
library(sf)
library(rstatix)
library(multcompView)

#########
#paths
output_dir <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/"
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
#########

#########
# base theme
base_theme <- theme(axis.text=element_text(size=9),
                    # axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=10),
                    strip.text = element_text(size=12),
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

#########
#upload data from all fisheries

cpfv<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/CPFV/Data/Processed Data/CPFV_filtered.rds'))%>%
  mutate(fishery="CPFV")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(angler_hours))
cuc_trawl<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Cucumber/Data/Processed Data/cuctrawl_filtered.rds'))%>%
  mutate(
    fishery = "Cucumber trawl")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(duration_min / 60))
cuc_dive<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seacucumber_filtered.rds'))%>%
  mutate(year=as.numeric(year))%>%
  mutate(fishery="Cucumber dive")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(hours))
urch<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Dive/Data/Processed Data/seaurchin_filtered.rds'))%>%
  mutate(year=as.numeric(year))%>%
  mutate(fishery="Urchin dive")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(hours))
gill<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/Data/Processed Data/setgillnet_filtered.rds'))%>%
  mutate(fishery="Gillnet",fishery_portcomplex=paste(fishery, port.complex, sep = "_"))%>%
  filter(!is.na(date.logbooks))%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(soak_hr_num))
hal<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Halibut/Data/Processed Data/halibuttrawl_filtered.rds'))%>%
  mutate(fishery="Halibut trawl")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(duration_hrs))
lob<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/Data/Processed Data/lobster_filtered.rds'))%>%
  mutate(fishery="Lobster trap")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(trap_nights))
pink<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/pinktrawl_filtered.rds'))%>%
  mutate(fishery="Pink shrimp trawl")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(duration_min / 60))
ridge<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Shrimp/Data/Processed Data/ridegback_filtered.rds'))%>%
  mutate(fishery="Ridgeback prawn trawl")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(duration_min / 60))
squid<-readRDS(('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Squid/Data/Processed Data/squid_filtered.rds'))%>%
  mutate(fishery="Squid seine")%>%
  group_by(year,fishery)%>%
  summarize(total.effort=sum(duration_min / 60))

effort.data<-rbind(cpfv,cuc_trawl,cuc_dive,urch, gill, hal, lob, pink, ridge,squid)
#########

#########
#landings data 
landings<-readRDS('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/1980_2022_landings_receipts.Rds')


trawl_gears <- unique(landings$gear[grepl("trawl", landings$gear, ignore.case = TRUE)])
trawl_gears <- setdiff(trawl_gears, c("Midwater trawl", "Pelagic trawl"))
# Define valid species-gear combos

species_gear <- list(
  'Giant red sea cucumber' = trawl_gears,
  'Unspecified sea cucumber' = trawl_gears,
  'Warty sea cucumber' = c('Diving'),
  'Red sea urchin' = c('Diving', 'Diving /hooks (sea urchins)'),
  'California halibut' =  trawl_gears,
  'Market squid' = c('Purse seine'),
  'Ridgeback prawn' = trawl_gears,
  'Pacific pink shrimp' = trawl_gears,
  'California spiny lobster' = c('Crab or lobster trap')
)

# Combine filtered data for each species-gear combo
df<- map2_dfr( # loops through each species and its associated gear types to filter rows accordingly.
  names(species_gear),
  species_gear,
  ~ landings %>%
    filter(comm_name == .x, gear %in% .y)
) %>%
  mutate(comm_name = case_when(
    comm_name %in% c("Giant red sea cucumber", "Unspecified sea cucumber") ~ "Giant red sea cucumber",
    TRUE ~ comm_name
  ))

# Summarize the filtered data
landings_weights <- df %>%
  group_by(comm_name,year) %>%
  summarise(weight_lbs = sum(landings_lbs, na.rm = TRUE), .groups = "drop")%>%
  mutate(fishery = recode(comm_name,
                         "California halibut" = "Halibut trawl",
                         "Red sea urchin" = "Urchin dive",
                         "Market squid" = "Squid seine",
                         "Ridgeback prawn" = "Ridgeback prawn trawl",
                         "Pacific pink shrimp" = "Pink shrimp trawl",
                         "California spiny lobster" = "Lobster trap",
                         "Giant red sea cucumber" = "Cucumber trawl",
                         "Unspecified sea cucumber" = "Cucumber trawl",
                         "Warty sea cucumber" = "Cucumber dive"))
#########

#########
#join effort and fisheries landings data together
joined.data<-effort.data%>%
  left_join(landings_weights, by=c("fishery","year"))
#########



#########
#plot of covid years versus non covid year for effort and landings
fisheries<-unique(joined.data$fishery)
plots <- list()

for (i in fisheries) {
  df_fishery <- joined.data %>% filter(fishery == i)
  has_landings <- any(!is.na(df_fishery$weight_lbs))
  
  if (has_landings) {
    sf <- max(df_fishery$total.effort, na.rm = TRUE) / max(df_fishery$weight_lbs, na.rm = TRUE)
    df_fishery$weight_scaled <- df_fishery$weight_lbs * sf
  }
  
  p <- ggplot(df_fishery, aes(x = year)) +
    annotate("rect", xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "grey70") +
    geom_line(aes(y = total.effort, color = "Effort"), linewidth = 1.2) +
    base_theme +
    labs(x = "Year", color = NULL, title = i)
  
  if (has_landings) {
    sf_local <- sf  # capture value for sec_axis formula
    p <- p +
      geom_line(aes(y = weight_scaled, color = "Landings"), linewidth = 1.2) +
      scale_y_continuous(
        name = "Total Effort",
        sec.axis = sec_axis(~ . / sf_local, name = "Landings (lbs)")
      ) +
      scale_color_manual(values = c("Effort" = "blue", "Landings" = "red"))
  } else {
    p <- p + scale_y_continuous(name = "Total Effort") +
      scale_color_manual(values = c("Effort" = "blue"))
  }
  
  plots[[i]] <- p
  ggsave(
    filename = file.path(output_dir, paste0(gsub(" ", "_", i), ".png")),
    plot = p,
    width = 8, height = 5, dpi = 300
  )
  
 }

plots[["Lobster trap"]]
plots[["Squid seine"]]
plots[["Halibut trawl"]]
plots[["Urchin dive"]]
plots[["Cucumber dive"]]
plots[["Pink shrimp trawl"]]
plots[["Ridgeback prawn trawl"]]
plots[["CPFV"]]
plots[["Gillnet"]]
#########

#########
#looking at dynamic variables and whether or not they were different in covid versus non covid years

variables<-readRDS(paste0(base_path,"Modeling/Data/Processed Data/fisheries_df_with_variables.rds"))



variables_summary <- variables %>%
  st_drop_geometry() %>%
  mutate(period = case_when(
    year < 2020 ~ "Pre-COVID",
    year >= 2020 & year <= 2022 ~ "COVID",
    year > 2022 ~ "Post-COVID"
  ) %>% factor(levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>%
  group_by(year, month, fishery, period) %>%
  summarise(
    mean_sst = mean(SST, na.rm = TRUE),
    fuel_cost = mean(cost_per_gal_adjusted, na.rm = TRUE),
    ex_vessel_price = mean(cost_per_pound_adjusted, na.rm = TRUE),
    .groups = "drop"
  )


variables_long <- variables_summary %>%
  pivot_longer(cols = c(mean_sst, fuel_cost, ex_vessel_price),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "mean_sst" = "SST (°C)",
                           "fuel_cost" = "Fuel Cost",
                           "ex_vessel_price" = "Ex-Vessel Price"))

fisheries <- unique(variables_long$fishery)
box_plots <- list()

for (i in fisheries) {
  p <- variables_long %>%
    filter(fishery == i) %>%
    ggplot(aes(x = period, y = value, fill = period)) +
    geom_violin(trim = FALSE) +
    geom_jitter(width = 0.1, alpha = 0.3, size = 0.8) +
    facet_wrap(~ variable, scales = "free_y") +
    scale_fill_manual(values = c("Pre-COVID" = "steelblue", 
                                 "COVID" = "tomato", 
                                 "Post-COVID" = "goldenrod")) +
    labs(x = NULL, y = NULL, title = i, fill = NULL) +
    base_theme +
    theme(legend.position = "none")
  
  box_plots[[i]] <- p
  
  ggsave(
    filename = file.path(output_dir, paste0("boxplot_", gsub(" ", "_", i), ".png")),
    plot = p,
    width = 8, height = 4, dpi = 300
  )
}
box_plots[["Lobster trap"]]
box_plots[["Squid seine"]]
box_plots[["California halibut trawl"]]
box_plots[["Urchin dive"]]
box_plots[["Cucumber dive"]]
box_plots[["Pink shrimp trawl"]]
box_plots[["Ridgeback prawn trawl"]]
box_plots[["CPFV"]]
box_plots[["Set gillnet"]]
