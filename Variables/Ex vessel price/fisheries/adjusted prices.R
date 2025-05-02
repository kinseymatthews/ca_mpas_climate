library(tidyverse)
library(priceR) #https://cran.r-project.org/web/packages/priceR/priceR.pdf


base_path <- "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed"

lobster    <- readRDS(file.path(base_path, "lobster_exvessel_port_notadjusted.rds"))
cuc_trawl  <- readRDS(file.path(base_path, "cuc_landing_exvessel_port_notadjusted.rds"))
cuc_dive   <- readRDS(file.path(base_path, "cucumberdive_exvessel_port_notadjusted.rds"))
urchin     <- readRDS(file.path(base_path, "urchindive_exvessel_port_notadjusted.rds"))
squid      <- readRDS(file.path(base_path, "squid_exvessel_port_notadjusted.rds"))
halibut    <- readRDS(file.path(base_path, "halibut_exvessel_port_notadjusted.rds"))
pink       <- readRDS(file.path(base_path, "pinkshrimp_exvessel_port_notadjusted.rds"))
ridge      <- readRDS(file.path(base_path, "ridge_logbook_exvessel_port_notadjusted.rds"))

# Assign these variables once
country <- "United States"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()


#write function to go through process
adjust_function<-function(fishery){
  fishery$cost_per_pound_adjusted<-adjust_for_inflation(fishery$price_lbs,
                                                        fishery$year, 
                                                        country="United States", 
                                                        to_date =  2022,
                                                        inflation_dataframe = inflation_dataframe,
                                                        countries_dataframe = countries_dataframe )
  return(fishery)
}

lobster   <- adjust_function(lobster)
cuc_trawl <- adjust_function(cuc_trawl)
cuc_dive  <- adjust_function(cuc_dive)
urchin    <- adjust_function(urchin)
squid     <- adjust_function(squid)
halibut   <- adjust_function(halibut)
pink      <- adjust_function(pink)
ridge     <- adjust_function(ridge)
