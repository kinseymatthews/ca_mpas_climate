library(tidyverse)
library(priceR) #https://cran.r-project.org/web/packages/priceR/priceR.pdf
library(slider)
#Fuel data from Efin
fuel<-read.csv("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Fuel price/data/raw/fuelca.csv")%>%
  rename(port_code=port, port=portname)%>%
  mutate(port = case_when(
    port == "Eureka (incl. Fields Landing)" ~ "Eureka",
    port == "Santa Barb" ~ "Santa Barbara",
    port == "Crescent C" ~ "Crescent City",
    port == "San Franci" ~ "San Francisco",
    port == "Port Huene" ~ "Port Hueneme",
    port == "Princeton" ~ "Princeton-Half Moon",
    TRUE ~ port #keep other values unchanged
  ))%>%
  #remove entries where they didnt get a price so they put 0
  filter(pxquoted!="No Price Quoted - See Notes")

#port 
ports<-read.csv(("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/Port Codes.csv"))%>%
#have port id 1006 Santa Barbara go to Santa Barbara port complex instead of Uknown major port.
#dont have to worry about this for earlier analyses but the fuel data jsut lists Santa Barbara instead of the harbor or something
mutate(port.complex=case_when( port == "Santa Barbara" ~ "Santa Barbara",
                               TRUE~ port.complex))

#merge fuel and port ocmplexes
fuel_ports<-fuel%>%
  left_join(ports, by="port" )%>%
  #some of the rows include prices that are taxed - get rid of
filter(!notes %in% c("INCLUDES TAX", 
                 "INCLUDES TAX-wouldn't give before tax",
                 "Tax included; won't give w/o tax",
                 "INCL TAX",
                 "with tax, doesn't know tax rate",
                 "with taxes",
                "includes tax",
                "	includes tax, didn't know rate",
                "INCLUDES 7.255 TAX",
                "1.555-7.25% tax",
                "1.16-7%",
                "1.115-7.25% tax",
                "$1.512-7.25% tax",
                "$1.477-7% sales tax",
                "$1.34-8.25% sales tax",
                "$1.34-7.5% tax",
                "$1.212-7.25%",
                "$1.13-7.8% tax",
                "includes tax, didn't know rate",
                "INCLUDES 7.25% TAX"))

ggplot(fuel_ports, aes(x=YEAR, y=pricegal, color=port))+
  geom_point()

#something weird going on with south San Francisco
a<-fuel_ports%>%
  filter(DOCKCODE=="SF_3")

ggplot(a, aes(x=YEAR, y=pricegal))+
  geom_point()

fuel_ports<-fuel_ports%>%
  filter(DOCKCODE!="SF_3")

ggplot(fuel_ports, aes(x=YEAR, y=pricegal, color=port))+
  geom_point()

a<-fuel_ports%>%
  filter(port=="Morro Bay")

ggplot(a, aes(x=YEAR, y=pricegal))+
  geom_point()


fuel_ports_sum<-fuel_ports%>%
  mutate(month_year=paste(MONTH,YEAR, sep="_"))%>%
  group_by(port.complex,month_year)%>%
  summarize(price.gal.avg=mean(pricegal))

fuel_ports_sum$year <- stringr::str_extract(fuel_ports_sum$month_year, "\\d{4}")

#adjusting price
# Assign these variables once
country <- "United States"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()

fuel_ports_sum$cost_per_gal_adjusted<-adjust_for_inflation(fuel_ports_sum$price.gal.avg,
                                                           fuel_ports_sum$year, 
                                                       country="United States", 
                                                       to_date =  2022,
                                                       inflation_dataframe = inflation_dataframe,
                                                       countries_dataframe = countries_dataframe )


#  Create a helper dataframe with fallback prices
fallback_ports <- c(
  "Bodega Bay" = "San Francisco",
  "Fort Bragg" = "Eureka",
  "Los Angeles" = "Santa Barbara",
  "Monterey" = "San Francisco",
  "San Diego" = "Los Angeles"
)

#  Get all combinations of port.complex and month_year
all_months <- unique(fuel_ports_sum$month_year)
all_ports  <- unique(fuel_ports_sum$port.complex)

full_grid <- expand.grid(
  port.complex = all_ports,
  month_year = all_months,
  stringsAsFactors = FALSE
)

# Join to create missing rows as NA
fuel_df_full <- full_grid %>%
  left_join(fuel_ports_sum, by = c("port.complex", "month_year"))

#  Create a fallback dataset
fallback_df <- fuel_ports_sum %>%
  filter(port.complex %in% unique(fallback_ports)) %>%
  rename(fallback_port = port.complex,
         fallback_price = cost_per_gal_adjusted)

#  Add fallback port based on mapping
fuel_df_filled <- fuel_df_full %>%
  mutate(fallback_port = fallback_ports[port.complex]) %>%
  left_join(fallback_df, by = c("fallback_port", "month_year")) %>%
  mutate(cost_per_gal_adjusted = if_else(is.na(cost_per_gal_adjusted), fallback_price, cost_per_gal_adjusted)) %>%
  select(port.complex, month_year, cost_per_gal_adjusted)



#  Convert 'month_year' to actual date
fuel_df_filled <- fuel_df_filled %>%
  mutate(
    # Split month_year into two parts: month and year
    parts = str_split_fixed(month_year, "_", 2),
    month = as.integer(parts[,1]),
    year  = as.integer(parts[,2]),
    # Create a valid date (first day of month)
    date = make_date(year = year, month = month, day = 1)
  ) %>%
  select(-parts)  # Drop temporary split column


# Create complete monthly time series per port
all_dates <- seq.Date(from = min(fuel_df_filled$date, na.rm = TRUE),
                      to = max(fuel_df_filled$date, na.rm = TRUE),
                      by = "month")

all_ports <- unique(fuel_df_filled$port.complex)

full_grid <- expand.grid(
  port.complex = all_ports,
  date = all_dates,
  stringsAsFactors = FALSE
)

fuel_complete <- full_grid %>%
  left_join(fuel_df_filled, by = c("port.complex", "date")) %>%
  arrange(port.complex, date)

# Fill missing months using average of neighbors
fuel_filled_avg <- fuel_complete %>%
  arrange(port.complex, date) %>%
  group_by(port.complex) %>%
  mutate(
    # Create a 7-month sliding window (±3 months centered)
    smoothed_price = slide_dbl(cost_per_gal_adjusted,
                               .f = ~mean(.x, na.rm = TRUE),
                               .before = 3, .after = 3,
                               .complete = FALSE),
    
    # Replace missing values with smoothed values
    cost_per_gal_adjusted = if_else(
      is.na(cost_per_gal_adjusted),
      smoothed_price,
      cost_per_gal_adjusted
    )
  )  %>%
  ungroup() %>%
  select(port.complex, date, month_year,cost_per_gal_adjusted)

san_diego_df <- fuel_filled_avg  %>%
  filter(port.complex == "Los Angeles") %>%
  mutate(port.complex = "San Diego")

# Add to your full dataset
fuel_filled_avg  <- bind_rows(fuel_filled_avg , san_diego_df)



# Average SB + Monterey fuel prices per month
sb_mtry_avg <- fuel_filled_avg %>%
  filter(port.complex %in% c("Santa Barbara", "Monterey")) %>%
  group_by(month_year) %>%
  summarize(avg_fuel = mean(cost_per_gal_adjusted, na.rm = TRUE), .groups = "drop")

# Fill in Morro Bay missing fuel prices with average
fuel_filled_avg <- fuel_filled_avg%>%
  left_join(sb_mtry_avg, by = "month_year") %>%
  mutate(cost_per_gal_adjusted = if_else(
    port.complex == "Morro Bay" & is.na(cost_per_gal_adjusted),
    avg_fuel,
    cost_per_gal_adjusted
  )) %>%
  select(-avg_fuel)

#and save into one dataframr
saveRDS(fuel_filled_avg, "G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Fuel price/data/processed/allportcomplexes_dieselprice_monthly_adjusted.rds")
