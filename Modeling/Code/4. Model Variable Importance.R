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
model_files <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models <- lapply(model_files, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models) <- gsub("^model_|\\_Depth_no_BT.rds$", "", basename(model_files))
# Example: look at the summary for San Francisco Squid Seine
m<-(models[["Los_Angeles_CPFV"]])
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

saveRDS(coef_table,paste0(model_folder,'model_results_variable_importance.rds'))


############

############
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

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/model_variable_importance.jpg')
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

filename1 <- ('G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Figures/model_variable_direction.jpg')
ggsave(filename = filename1, plot =  p , width = 13, height = 6, units = "in", dpi = 300)

############







############ DELETE
#per fishing metier, sie of variable importance
coef_table2<-coef_table%>%
  mutate(term_type= case_when(
    term %in% c("cost_per_gal_adjusted_scaled","cost_per_pound_adjusted_scaled") ~ "Market",
    term %in% c("closed_pct_scaled") ~ "Regulation",
    term %in% c("distance_to_shore_km_scaled", "distance_complex_km_scaled") ~ "Physical",
    term %in% c("SST_scaled", "BT_scaled") ~ "Environmental"
  ))%>%
  filter(term %in% c("cost_per_gal_adjusted_scaled","cost_per_pound_adjusted_scaled","closed_pct_scaled",
                     "distance_to_shore_km_scaled", "distance_complex_km_scaled",
                     "SST_scaled", "BT_scaled"))%>%
  mutate(estimate.abs=abs(estimate))

ggplot(coef_table2, aes(color=term_type, y=port.complex, x=term, group=term_type)) + 
  geom_point()+
  # geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate+ std.error), width = 0.2, position=position_dodge(width=0.75)) +
  facet_wrap(~fishery)

coef_table2$port.complex <- ordered(coef_table2$port.complex, levels=c("Eureka","Fort Bragg","Bodega Bay","San Francisco", "Monterey",
                                                                       "Morro Bay", "Santa Barbara","Los Angeles","San Diego"))

# define terms and explicit order (edit if desired)
terms <- c(
  "cost_per_gal_adjusted_scaled","cost_per_pound_adjusted_scaled",
  "closed_pct_scaled","distance_to_shore_km_scaled",
  "distance_complex_km_scaled","SST_scaled","BT_scaled"
)

# 1. get full lists for fishery and port
fishery_levels <- coef_table2 %>% distinct(fishery) %>% arrange(fishery) %>% pull(fishery)
port_levels    <- coef_table2 %>% distinct(port.complex) %>% arrange(port.complex) %>% pull(port.complex)

# 2. ensure exact naming (trim whitespace) to avoid silly mismatches
coef_clean <- coef_table2 %>%
  mutate(
    term = trimws(as.character(term)),
    fishery = trimws(as.character(fishery)),
    port.complex = trimws(as.character(port.complex))
  )

# 3. scale within term (across all fisheries) — we compute scale_factors from the raw data
scale_table <- coef_clean %>%
  filter(term %in% terms) %>%
  group_by(term) %>%
  summarize(scale_factor = max(abs(estimate), na.rm = TRUE), .groups = "drop")

# 4. make a complete grid of fishery x term x port to avoid dropped rows
full_grid <- expand.grid(
  fishery = fishery_levels,
  term = terms,
  port.complex = port_levels,
  stringsAsFactors = FALSE
) %>%
  as_tibble()

# 5. left_join your data onto the full grid so missing combos show up as NA
df_full <- full_grid %>%
  left_join(coef_clean, by = c("fishery", "term", "port.complex")) %>%
  # bring in scale factors
  left_join(scale_table, by = "term") %>%
  mutate(
    # if there's no scale_factor (all NA) then set to 1 to avoid NA division
    scale_factor = ifelse(is.na(scale_factor) | scale_factor == 0, 1, scale_factor),
    est_scaled = estimate / scale_factor,
    est_lower = (estimate - std.error) / scale_factor,
    est_upper = (estimate + std.error) / scale_factor,
    # set factor levels deterministically
    term = factor(term, levels = terms),
    port.complex = factor(port.complex, levels = port_levels),
    fishery = factor(fishery, levels = fishery_levels)
  ) %>%
  mutate(
    term_id = as.numeric(term),
    port_id = as.numeric(port.complex),
    axis_halfwidth = 0.35,
    x_pos = term_id + (est_scaled * axis_halfwidth),
    x_min  = term_id + (est_lower * axis_halfwidth),
    x_max  = term_id + (est_upper * axis_halfwidth),
    y_pos  = port_id
  )

# desired order of port complexes (top to bottom)
port_order <- c(
  "Eureka", "Fort Bragg", "Bodega Bay", "San Francisco",
  "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego"
)

# reorder port.complex factor using your desired order
df_full <- df_full %>%
  mutate(port.complex = factor(port.complex, levels = port_order),
         port_id = as.numeric(port.complex))  # update y positions

# also update term_axes just in case
term_axes <- df_full %>%
  distinct(term, term_id, axis_halfwidth) %>%
  mutate(axis_y = max(df_full$port_id, na.rm = TRUE) + 0.6)

ggplot(df_full, aes(y = y_pos)) +
  geom_errorbarh(
    aes(xmin = x_min, xmax = x_max, color = term_type),
    height = 0.2, alpha = 0.8,
    na.rm = TRUE
  ) +
  geom_point(aes(x = x_pos, color = term_type), size = 3, alpha = 0.9, na.rm = TRUE) +
  geom_vline(
    data = term_axes,
    aes(xintercept = term_id),
    color = "gray60", linetype = "dashed", linewidth = 0.4
  ) +
  facet_wrap(~ fishery, ncol = 2, scales = "free_y") +
  scale_x_continuous(
    breaks = unique(df_full$term_id),
    labels = levels(df_full$term),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_y_reverse(   # <---- reverses the y-axis
    breaks = unique(df_full$port_id),
    labels = levels(df_full$port.complex),
    expand = expansion(add = c(0.3, 0.6))
  )  +
  scale_color_brewer(palette = "Set1", name = "Term type") +
  labs(x = "Variable (scaled to ±1 within term)", y = "Port Complex") +
  # theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom")
###################### 


###################### MAYBE DELETE

# Count positives vs negatives for each term
coef_summary <- coef_table %>%
  mutate(sign = case_when(
    estimate > 0 ~ "positive",
    estimate < 0 ~ "negative",
    TRUE ~ "zero"
  )) %>%
  group_by(term, sign) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sign, values_from = n, values_fill = 0)

coef_summary

#county positive and negative per term organized by fishery and by port complex
# Example: summarize by fishery
fishery_summary <- coef_table %>%
  group_by(fishery, term, sign) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sign, values_from = n, values_fill = 0)

port.complex_summary <- coef_table %>%
  group_by(port.complex, term, sign) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sign, values_from = n, values_fill = 0)

