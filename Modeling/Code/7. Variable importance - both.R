library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggcorrplot)
library(broom)
library(ragg)
############

# model folder
model_folder <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/")


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
#upload presence and absence models
model_files_pa <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT_pa_delta_updated2026.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models_pa <- lapply(model_files_pa, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models_pa) <- gsub("^model_|\\_Depth_no_BT_pa_delta_updated2026.rds$", "", basename(model_files_pa))
# Example: look at the summary for San Francisco Squid Seine
m<-(models_pa[["Morro_Bay_CPFV"]])
############

############
#upload effort models
model_files_abund <- list.files(model_folder, pattern = "^model_.*\\_Depth_no_BT_abundance_delta_updated2026.rds$", full.names = TRUE) #“Match any file that starts with model_ and ends with .rds.”

# Read them back into a named list
models_abund <- lapply(model_files_abund, readRDS)

# Strip off "model_" and ".rds" so names look like "San_Francisco_Squid_Seine"
names(models_abund) <- gsub("^model_|\\_Depth_no_BT_abundance_delta_updated2026.rds$", "", basename(model_files_abund))
# Example: look at the summary for San Francisco Squid Seine
#m<-(models_abund[["San_Diego_Lobster_trap"]])
############





############
# Quality checks (run these every time you refit)

# 1. Same métiers in both sets? Both should print character(0)
setdiff(names(models_pa), names(models_abund))
setdiff(names(models_abund), names(models_pa))

# 2. Does each model's data contain the port its name says?
#    (this is the check that caught the Eureka/LA CPFV mix-up)
port_col <- grep("port", names(models_pa[[1]]$data), ignore.case = TRUE, value = TRUE)[1]
port_col   # make sure this is the right column; hardcode it if not

norm <- function(x) tolower(gsub("[ _.]", "", x))   # "Los_Angeles" == "Los Angeles"

check_ports <- function(models, label) {
  imap_dfr(models, function(m, nm) {
    ports <- sort(unique(m$data[[port_col]]))
    tibble(set = label, model = nm,
           ports_in_data = paste(ports, collapse = "; "),
           ok = length(ports) == 1 && str_detect(norm(nm), fixed(norm(ports))))
  })
}

port_check <- bind_rows(check_ports(models_pa, "pa"), check_ports(models_abund, "effort"))
port_check %>% filter(!ok)   # should be empty

# 3. Model families: decides which delta component tidy() should report
fam_check <- bind_rows(
  imap_dfr(models_pa,    ~ tibble(set = "pa",     model = .y, family = paste(.x$family$family, collapse = " / "))),
  imap_dfr(models_abund, ~ tibble(set = "effort", model = .y, family = paste(.x$family$family, collapse = " / ")))
)
count(fam_check, set, family)
############

############
# Settings used throughout (edit here, not further down)

# Fisheries as they appear in model names
fishery_list <- c("CPFV", "Urchin_dive", "Squid_seine", "California_halibut_trawl",
                  "Set_gillnet", "Cucumber_dive", "Lobster_trap", "Cucumber_trawl",
                  "Ridgeback_prawn_trawl", "Pink_shrimp_trawl")

# Plotting order for fisheries (top to bottom)
fishery_order <- c("Pink shrimp trawl", "Ridgeback prawn trawl", "California halibut trawl",
                   "Cucumber trawl", "Squid seine", "Set gillnet", "Lobster trap",
                   "Urchin dive", "Cucumber dive", "CPFV")

# Terms that compete for "most important"
importance_terms <- c("cost_per_gal_adjusted_scaled", "cost_per_pound_adjusted_scaled",
                      "distance_to_shore_km_scaled", "distance_complex_km_scaled",
                      "closed_pct_scaled", "SST_scaled", "Avg_Depthm_scaled")

# Term -> category
term_types <- c("cost_per_gal_adjusted_scaled"   = "Market",
                "cost_per_pound_adjusted_scaled" = "Market",
                "distance_complex_km_scaled"     = "Market",
                "closed_pct_scaled"              = "Regulation",
                "distance_to_shore_km_scaled"    = "Physical",
                "Avg_Depthm_scaled"              = "Physical",
                "SST_scaled"                     = "Environmental")

# Term -> readable label (order here = order in the direction plot)
term_labels <- c("cost_per_pound_adjusted_scaled" = "Ex-vessel price",
                 "cost_per_gal_adjusted_scaled"   = "Fuel price",
                 "distance_complex_km_scaled"     = "Distance to port complex",
                 "distance_to_shore_km_scaled"    = "Distance to mainland",
                 "Avg_Depthm_scaled"              = "Depth",
                 "closed_pct_scaled"              = "Closed area",
                 "SST_scaled"                     = "SST")

# Colors (Okabe-Ito, colorblind-safe), fixed so both panels always match
term_type_cols <- c("Market" = "#E69F00", "Regulation" = "#CC79A7",
                    "Physical" = "#56B4E9", "Environmental" = "#009E73")
sign_cols      <- c("Positive" = "#E69F00", "Neutral" = "#009E73", "Negative" = "#0072B2")

# How many SEs a coefficient must be from 0 to count as Positive/Negative
# 1 = ~68% interval (what you had before); 1.96 = 95% interval
se_mult <- 1

# Response labels (order = left-to-right panel order)
response_levels <- c("Presence/absence", "Effort")
############

############
# Coefficient table (both responses in one table)

# Tidy each model. For delta models, tidy() returns component 1 (binomial) by default,
# so effort coefficients need component 2. Non-delta models always use component 1.
get_coefs <- function(models, response_label, delta_component) {
  imap_dfr(models, function(m, nm) {
    is_delta <- length(m$family$family) == 2
    comp     <- if (is_delta) delta_component else 1
    broom::tidy(m, model = comp) %>%
      mutate(model = nm, component = comp)
  }) %>%
    mutate(response = response_label)
}

coef_table <- bind_rows(
  get_coefs(models_pa,    "Presence/absence", delta_component = 1),
  get_coefs(models_abund, "Effort",           delta_component = 2)
) %>%
  mutate(
    # Fishery = first entry of fishery_list found in the model name
    fishery = map_chr(model, function(x) {
      f <- fishery_list[str_detect(x, fixed(fishery_list))]
      if (length(f) == 0) NA_character_ else f[1]
    }),
    # Port complex = model name minus the fishery
    port.complex = if_else(is.na(fishery), model,
                           str_remove(model, paste0("_", fishery, "$"))),
    port.complex = gsub("_", " ", port.complex),
    fishery      = gsub("_", " ", fishery),
    # Direction: does the interval (estimate +/- se_mult * SE) exclude 0?
    sign = case_when(
      (estimate - se_mult * std.error) > 0 ~ "Positive",
      (estimate + se_mult * std.error) < 0 ~ "Negative",
      TRUE                                 ~ "Neutral"
    ),
    absolute.value.coef = abs(estimate),
    response = factor(response, levels = response_levels)
  )

# Any model names that didn't match a fishery? Should be empty
coef_table %>% filter(is.na(fishery)) %>% distinct(model)

saveRDS(coef_table, paste0(model_folder, "model_results_coefs_pa_and_effort.rds"))
############


############
# Variable importance: % of métiers where each term type has the largest |coefficient|

# Number of métiers per fishery per response (the "n =" labels)
n_metiers <- coef_table %>%
  distinct(response, fishery, model) %>%
  count(response, fishery, name = "n_metiers")

# Top term for each model (with_ties = FALSE guarantees exactly one per model)
top_terms <- coef_table %>%
  filter(term %in% importance_terms) %>%
  group_by(response, fishery, port.complex, model) %>%
  slice_max(abs(estimate), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(term_type = unname(term_types[term]))

# Per-model winners, handy for checking individual métiers (e.g., LA CPFV)
write_csv(top_terms %>% select(response, fishery, port.complex, term, term_type, estimate, std.error),
          paste0(model_folder, "top_term_per_metier_pa_and_effort.csv"))

# Percent of métiers per fishery where each term type is #1
varimp <- top_terms %>%
  count(response, fishery, term_type, name = "n_top") %>%
  left_join(n_metiers, by = c("response", "fishery")) %>%
  mutate(percent  = 100 * n_top / n_metiers,
         fishery  = factor(fishery, levels = fishery_order))

# Only show fisheries that are actually in the data
fisheries_present <- rev(intersect(fishery_order, unique(varimp$fishery)))

p_varimp <- ggplot(varimp, aes(x = percent, y = fishery, fill = term_type)) +
  geom_col(color = "black", linewidth = 0.2) +
  # Sample size to the right of each bar
  geom_text(data = n_metiers,
            aes(x = 103, y = fishery, label = paste0("n = ", n_metiers)),
            inherit.aes = FALSE, hjust = 0, size = 3) +
  facet_wrap(~ response) +                          # pa and effort side by side
  scale_fill_manual(values = term_type_cols, name = "Term type") +
  scale_y_discrete(limits = fisheries_present) +
  coord_cartesian(xlim = c(0, 100), clip = "off") + # lets the n labels sit outside the panel
  labs(x = "Percentage of métiers", y = NULL) +
  base_theme +
  theme(text = element_text(family = "Arial"), 
        axis.text.y  = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        strip.text   = element_text(size = 16),
        legend.box.spacing = unit(2, "lines"), 
        panel.spacing = unit(3, "lines"),           # room for left panel's n labels
        plot.margin  = margin(5, 40, 5, 5), #room for right panel's n labels
        legend.text  = element_text(size = 12),   
        legend.title = element_text(size = 13))         
p_varimp

filename1 <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/bothmodels_varimp.jpg")
ggsave(filename = filename1, plot =  p_varimp
       , width = 12, height = 5, units = "in", dpi = 300)
############


############
# Direction of effects: % of métiers per fishery where each term is Positive / Neutral / Negative
# Made separately for each response (pa and effort)

# Summary table: count and percent of métiers in each sign category
direction_df <- coef_table %>%
  filter(term %in% importance_terms) %>%                      # only the competing terms
  count(response, fishery, term, sign, name = "count") %>%    # keeps pa and effort apart
  group_by(response, fishery, term) %>%
  mutate(total.metiers = sum(count),
         percent       = 100 * count / total.metiers) %>%     # 0-100 to match the varimp plot
  ungroup() %>%
  mutate(term = factor(unname(term_labels[term]),             # readable labels from settings
                       levels = rev(unname(term_labels))),    # first label plots at the top
         sign = factor(sign, levels = names(sign_cols)))      # Positive / Neutral / Negative

# Function that makes both direction plots for one response
plot_direction <- function(resp) {
  d      <- direction_df %>% filter(response == resp)
  n_resp <- n_metiers    %>% filter(response == resp)
  
  # --- Plot 1: one panel per fishery, terms on the y-axis ---
  # Fishery panels ordered by number of métiers (most first)
  fish_by_n <- n_resp %>% arrange(desc(n_metiers)) %>% pull(fishery)
  # Facet strip labels with sample size on a second line
  strip_labels <- setNames(paste0(n_resp$fishery, "\n(n = ", n_resp$n_metiers, ")"),
                           n_resp$fishery)
  
  p_by_fishery <- ggplot(d %>% mutate(fishery = factor(fishery, levels = fish_by_n)),
                         aes(x = percent, y = term, fill = sign)) +
    geom_col(color = "black", linewidth = 0.2) +
    facet_wrap(~ fishery, labeller = labeller(fishery = strip_labels)) +
    scale_fill_manual(values = sign_cols, name = "Direction") +
    labs(x = "Percentage of métiers", y = NULL, title = resp) +
    base_theme +
    theme(axis.text.y  = element_text(size = 10),
          axis.text.x  = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          strip.text   = element_text(size = 11),
          plot.title   = element_text(size = 14))
  
  # --- Plot 2: one panel per term, fisheries on the y-axis ---
  # Same top-to-bottom fishery order as the varimp plot
  fish_present <- rev(intersect(fishery_order, n_resp$fishery))
  # n labels: one per fishery, only on panels with empty space to their right
  # (with ncol = 3: Depth and Fuel price are the right column, Ex-vessel price is alone in the last row)
  n_labels <- n_resp %>%
    crossing(term = c("Depth", "Fuel price", "Ex-vessel price")) %>%   # copy each fishery's n to those panels
    mutate(term = factor(term, levels = levels(d$term)))                # match facet levels
  
  p_by_term <- ggplot(d, aes(x = percent, y = fishery, fill = sign)) +
    geom_col(color = "black", linewidth = 0.2) +
    # Sample size to the right of each bar
    geom_text(data = n_labels,
              aes(x = 101, y = fishery, label = paste0("n = ", n_metiers)),
              inherit.aes = FALSE, hjust = 0, size = 4) +
    facet_wrap(~ term, ncol = 3) +
    scale_y_discrete(limits = fish_present) +
    scale_fill_manual(values = sign_cols, name = "Direction",
                      breaks = c("Negative", "Neutral", "Positive")) +
    coord_cartesian(xlim = c(0, 100), clip = "off") +   # lets the n labels sit outside the panel
    labs(x = "Percentage of port complexes", y = NULL) +
    base_theme +
    theme(text = element_text(family = "Arial"), 
          axis.text.y  = element_text(size = 14),
          axis.text.x  = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          strip.text   = element_text(size = 16),
          panel.spacing      = unit(1, "lines"),
          legend.box.spacing = unit(2, "lines"),
          legend.text  = element_text(size = 11),   # Negative / Neutral / Positive
          legend.title = element_text(size = 12))   
  
  list(by_fishery = p_by_fishery, by_term = p_by_term)
}

# Make plots for both responses (named list: direction_plots$Effort$by_term, etc.)
direction_plots <- map(set_names(response_levels), plot_direction)

# Look at them
direction_plots$`Presence/absence`$by_fishery
direction_plots$`Presence/absence`$by_term
direction_plots$Effort$by_fishery
direction_plots$Effort$by_term

#other plot
p1<-direction_plots$`Presence/absence`$by_term
p2<-direction_plots$Effort$by_term


filename1 <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/pa_direction.jpg")
ggsave(filename = filename1, plot =  p1, width = 13, height = 8, units = "in", dpi = 300)


filename2 <- ( "G:/Shared drives/bren-fisheriesclimate-modeling/Modeling/Data/Processed Data/Model_Results/effort_direction.jpg")
ggsave(filename = filename2, plot =  p2, width = 13, height = 8, units = "in", dpi = 300)

############