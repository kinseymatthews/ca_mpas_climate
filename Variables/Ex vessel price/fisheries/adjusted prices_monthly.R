library(tidyverse)
library(priceR) #https://cran.r-project.org/web/packages/priceR/priceR.pdf


base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/"

lobster    <- readRDS(file.path(base_path, "lobster_landing_exvessel_port_year_month_notadjusted.rds"))%>%
  mutate(fishery="Lobster trap", month_year=as.character(month_year))
cuc_trawl  <- readRDS(file.path(base_path, "cuc_landing_exvessel_port_month_year_notadjusted.rds"))%>%
  mutate(fishery="Cucumber trawl", month_year=as.character(month_year))
cuc_dive   <- readRDS(file.path(base_path, "cuc_dive_landing_exvessel_port_month_year_notadjusted.rds"))%>%
  mutate(fishery="Cucumber dive", month_year=as.character(month_year))
urchin     <- readRDS(file.path(base_path, "urch_dive_landing_exvessel_port_month_year_notadjusted.rds"))%>%
  mutate(fishery="Urchin dive", month_year=as.character(month_year))
squid      <- readRDS(file.path(base_path, "squid_landing_exvessel_port_year_month_notadjusted.rds"))%>%
  mutate(fishery="Squid seine", month_year=as.character(month_year))
halibut    <- readRDS(file.path(base_path, "halibut_exvessel_port_month_year_notadjusted.rds"))%>%
  mutate(fishery="Halibut trawl", month_year=as.character(month_year))
pink       <- readRDS(file.path(base_path, "pinkshrimp__landings_exvessel_port_yeaR_month_notadjusted.rds"))%>%
  mutate(fishery="Pink shrimp trawl", month_year=as.character(month_year))
ridge      <- readRDS(file.path(base_path, "ridge_landing_exvessel_port_yeaR_month_notadjusted.rds"))%>%
  mutate(fishery="Ridgeback prawn trawl", month_year=as.character(month_year))
set      <- readRDS(file.path(base_path, "setgillnet_exvessel_port_year_month_notadjusted.rds"))%>%
  mutate(fishery="Set gillnet", month_year=as.character(month_year))

all_fisheries<-bind_rows(lobster, cuc_trawl, cuc_dive, urchin, squid,
                         halibut, pink, ridge, set)
all_fisheries$year <- stringr::str_extract(all_fisheries$month_year, "\\d{4}")

# Assign these variables once
country <- "United States"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()

#write function to go through process
unique_fisheries<-unique(all_fisheries$fishery)
#create mepty dataframe
adjusted_fisheries <- list()

#for loop to go through each fishery and save the adjusted prices as individual .rds files
for (fish in unique_fisheries) {
  fish_df<-all_fisheries%>%
    filter(fishery==fish)
  
  fish_df$cost_per_pound_adjusted<-adjust_for_inflation( fish_df$price_lbs,
                                                         fish_df$year, 
                                                        country="United States", 
                                                        to_date =  2022,
                                                        inflation_dataframe = inflation_dataframe,
                                                        countries_dataframe = countries_dataframe )
  
  
  # Save the adjusted data
  saveRDS(
    fish_df,
    file.path(
      "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed",
      paste0(gsub(" ", "_", tolower(fish)), "_exvessel_port_month_year_adjusted_prices.rds")
    )
  )
  
  
  # add to the list
  adjusted_fisheries[[fish]] <- fish_df
}

# Combine list of adjusted fisheries into one dataframe
all_adjusted_fisheries <- bind_rows(adjusted_fisheries)

#and save into one dataframr
saveRDS(all_adjusted_fisheries, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/allfisheries_exvessel_port_year_month_adjusted.rds")
