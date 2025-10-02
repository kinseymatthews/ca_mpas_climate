library(tidyverse)

library(purrr)

# Define base path and filenames
base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/"
#file info = file name to upload, hours_col to sum for total effort, duration_min - if TRUE needs conversin to hours, and then 
#fishery = final name to apply to fishery
file_info <- list(
  CPFV              = list(file = "CPFV_filtered_modeling.rds", hours_col = "angler_hours", fishery = "CPFV"),
  CUC_Trawl         = list(file = "cuctrawl_filtered_modeling.rds", duration_min = TRUE, fishery = "Cucumber trawl"),
  CUC_Dive          = list(file = "cucdive_filtered_modeling.rds", hours_col = "hours", fishery = "Cucumber dive"),
  Urchin            = list(file = "urch_filtered_modeling.rds", hours_col = "hours", fishery = "Urchin dive"),
  Gill              = list(file = "gill_filtered_modeling.rds", hours_col = "soak_hr_num", fishery = "Set gillnet"),
  Halibut           = list(file = "hal_filtered_modeling.rds", hours_col = "duration_hrs", fishery = "California halibut trawl"),
  Lobster           = list(file = "lob_filtered_modeling.rds", hours_col = "trap_nights", fishery = "Lobster trap"),
  PinkShrimp        = list(file = "pink_filtered_modeling.rds", duration_min = TRUE, fishery = "Pink shrimp trawl"),
  Ridgeback         = list(file = "ridge_filtered_modeling.rds", duration_min = TRUE, fishery = "Ridgeback prawn trawl"),
  Squid             = list(file = "squid_filtered_modeling.rds", duration_min = TRUE, fishery = "Squid seine")
)

# function to read, mutate and summarize
process_fishery <- function(file, fishery, hours_col = NULL, duration_min = FALSE) {
  #load and read the RDS file 
  df <- readRDS(file.path(base_path, file)) %>%
    #adding month_year column
    mutate(month_year = paste(month, year, sep = "_"))
  
  #if duration_min exists (duration_min=TRUE), then convert to duration_hrs and set it as the column to sum
  if (duration_min) {
    df <- df %>% mutate(duration_hrs = duration_min / 60)
    hours_col <- "duration_hrs"
  }
  
  #summarize total effort by port compelx, block id, and time.
  df %>%
    group_by(port.complex, block_id, month, year, month_year) %>%
    # .data[[hours_col]] is summarizing in the data column we specified earlier, removing NA and dropping groups
    summarize(total.hours = sum(.data[[hours_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(fishery = fishery)
}

# Apply function across all fisheries listed in file_info
#imap_dfr runs the function on each list element and binds rows into a single data frame
all_fisheries <- imap_dfr(file_info, ~process_fishery(.x$file, .x$fishery, .x$hours_col, isTRUE(.x$duration_min)))

all_fisheries<-all_fisheries%>%
  filter(!is.na(block_id))


#specific fishery regulations
# need to remove months where the lobster fishery doesnt operate (4 to 9)


saveRDS(all_fisheries, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/summarized_fishery_data_ready4model.rds")


