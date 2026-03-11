library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)
library(ggcorrplot)
library(INLA)
library(broom)
############
#Upload data paths
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/"
model_folder <- paste0(base_path, "Modeling/Data/Processed Data/Model_Results/")

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
############


############
#upload models
model_files <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT_abundance_delta.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models <- lapply(model_files, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models) <- gsub("^model_|\\_Depth_no_BT_abundance_delta.rds$", "", basename(model_files))
# Example: look at the summary for San Francisco Squid Seine
m<-(models[["Santa_Barbara_Squid_seine"]])
############


############
fishery_list <- c("CPFV", "Urchin_dive", "Squid_seine", "California_halibut_trawl","Set_gillnet", "Cucumber_dive", "Lobster_trap", "Cucumber_trawl", "Ridgeback_prawn_trawl", "Pink_shrimp_trawl")

coef_table <- lapply(names(models), function(nm) {
  coefs <- broom::tidy(models[[nm]])
  coefs$model <- nm
  coefs
}) %>% dplyr::bind_rows()%>%
  rowwise() %>%
  mutate(
    # Find fishery by matching the known list
    fishery = {
      f <- fishery_list[sapply(fishery_list, function(x) grepl(x, model))]
      if(length(f) == 0) NA_character_ else f[1]
    },
    # Port complex is what's left after removing fishery
    port.complex = if(!is.na(fishery)) sub(paste0("_", fishery, "$"), "", model) else model,
    port.complex = gsub("_", " ", port.complex),
    fishery=gsub("_", " ", fishery),
    
    # Classify based on estimate and standard error
    sign = case_when(
      (estimate - std.error) > 0 ~ "Positive",   # lower bound > 0
      (estimate + std.error) < 0 ~ "Negative",   # upper bound < 0
      TRUE ~ "Neutral"                            # crosses 0
    )
  )%>%
  ungroup()%>%
  mutate(absolute.value.coef=abs(estimate))

saveRDS(coef_table,paste0(model_folder,'model_results_variable_importance_delta_abundance.rds'))


############

############
coef_table<-readRDS(paste0(model_folder,'model_results_variable_importance_delta_abundance.rds'))
#for each fishing metier, figure out the most important coefficient, then for a fishery calculate 
#the percent a variable is number one?
coef_import<-coef_table%>%
  group_by(model)%>%
  mutate(
    most.important = if_else(
      term %in% c("cost_per_gal_adjusted_scaled", "distance_to_shore_km_scaled", "distance_complex_km_scaled", "closed_pct_scaled","cost_per_pound_adjusted_scaled", "SST_scaled", "Avg_Depthm_scaled"),  # ← only terms to consider
      if_else(abs(estimate) == max(abs(estimate[term %in% c("cost_per_gal_adjusted_scaled", "distance_to_shore_km_scaled", "distance_complex_km_scaled", "closed_pct_scaled","cost_per_pound_adjusted_scaled", "SST_scaled", "Avg_Depthm_scaled")])), 1, 0),
      0
    )
  )%>%
  ungroup()%>%
  mutate(term_type= case_when(
    term %in% c("cost_per_gal_adjusted_scaled","cost_per_pound_adjusted_scaled","distance_complex_km_scaled") ~ "Market",
    term %in% c("closed_pct_scaled") ~ "Regulation",
    term %in% c("distance_to_shore_km_scaled", "Avg_Depthm_scaled") ~ "Physical",
    term %in% c("SST_scaled") ~ "Environmental"
  ))%>%
  group_by(fishery,term_type)%>%
  summarise(
    count_1 = sum(most.important == 1),
    count_0 = sum(most.important == 0),
    total_metiers=n_distinct(model)
  )%>%
  mutate(percent=(count_1/total_metiers)*100)%>%
  ungroup()%>%
  filter(!is.na(term_type))

coef_import$fishery <- factor(coef_import$fishery, levels = c('Pink shrimp trawl', 'Ridgeback prawn trawl', 'California halibut trawl', 'Cucumber trawl', 'Squid seine',  'Set gillnet', 'Lobster trap','Urchin dive', 'Cucumber dive','CPFV'))


p<-ggplot(coef_import, aes(fill=term_type, y=fishery, x=percent)) + 
  geom_bar(position="stack", stat="identity", color = "black" ,size = 0.2 )+
  base_theme+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),  
        axis.text.x = element_text(size = 10),  
        axis.title.x = element_text(size = 14)  
  )+ 
  labs(x="Variable listed as most important (%)")+
  labs(fill = "Term type")+
  geom_text(aes(label = paste0("n = ", total_metiers), x = 100+3), # Add sample size to the right
            hjust = 0, size = 3) +
  scale_y_discrete(limits = rev(levels(coef_import$fishery)))
p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/model_variable_importance_delta_abundance.jpg')
ggsave(filename = filename1, plot =  p , width = 10, height = 6, units = "in", dpi = 300)

############



############
#per fishery, per variable, number of positive, negative, and neutral vairables (or percentage instead of number)
df<-coef_table%>%
  filter(term %in% c("cost_per_gal_adjusted_scaled", "distance_to_shore_km_scaled", "distance_complex_km_scaled", "closed_pct_scaled","cost_per_pound_adjusted_scaled", "SST_scaled", "Avg_Depthm_scaled"))%>%  # ← only terms to consider)
  group_by(fishery,term,sign) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(fishery,term)%>%
  mutate(total.metiers=sum(count))%>%
  ungroup()%>%
  group_by(fishery,term,sign)%>%
  mutate(percent=count/total.metiers)%>%
  mutate(term = recode(term,
                       "cost_per_pound_adjusted_scaled" = "Ex-vessel price",
                       "cost_per_gal_adjusted_scaled" = "Fuel price",
                       "distance_complex_km_scaled"="Distance to port complex",
                       "distance_to_shore_km_scaled" ="Distance to shore",
                       "Avg_Depthm_scaled"="Depth",
                       "closed_pct_scaled" = "Closed area",
                       "SST_scaled"="SST"
                       
  ))
df$term <- factor(df$term, levels = rev(c(
  "Ex-vessel price", 
  "Fuel price",
  "Distance to port complex",
  "Distance to shore",
  "Depth",
  "Closed area",
  "SST"
)))

# Reorder fishery levels by total.metiers (descending)
df <- df %>%
  group_by(fishery) %>%
  mutate(total.metiers = unique(total.metiers)) %>%
  ungroup() %>%
  mutate(fishery = reorder(fishery, -total.metiers))  # negative sign for descending order


labels <- df %>%
  distinct(fishery, total.metiers) %>%
  mutate(label = paste0(fishery, "\n(n = ", total.metiers, ")")) %>%
  with(setNames(label, fishery))


p<- ggplot(df, aes(fill=sign, y=term, x=percent)) + 
  geom_bar(position="stack", stat="identity", color = "black" ,size = 0.2  )+
  base_theme+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),  
        axis.text.x = element_text(size = 10),  
        axis.title.x = element_text(size = 14)  
  )+ 
  facet_wrap(~fishery, labeller = labeller(fishery = labels))+
  scale_fill_manual(values = c("Positive" = "#E69F00", "Negative" = "#0072B2", "Neutral" = "#009E73"))+
  labs(x="Percent")+
  labs(fill = "Direction")

p

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/model_variable_direction_delta_abundance.jpg')
ggsave(filename = filename1, plot =  p , width = 13, height = 6, units = "in", dpi = 300)

############

#Change x and facet wrap



p<-ggplot(df, aes(fill=sign, y=fishery, x=percent)) + 
  geom_bar(position="stack", stat="identity", color = "black" ,size = 0.2  )+
  base_theme+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),  
        axis.text.x = element_text(size = 10),  
        axis.title.x = element_text(size = 14)  
  )+ 
  facet_wrap(~term, labeller = labeller(fishery = labels))+
  scale_fill_manual(values = c("Positive" = "#E69F00", "Negative" = "#0072B2", "Neutral" = "#009E73"))+
  labs(x="Percent")+
  labs(fill = "Direction")

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/model_variable_direction_delta_abundance_2.jpg')
ggsave(filename = filename1, plot =  p , width = 13, height = 6, units = "in", dpi = 300)
