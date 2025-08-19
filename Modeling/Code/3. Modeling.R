
library(tidyverse)
library(sdmTMB)
library(sf)
library(sdmTMBextra)
library(ggeffects)

 ############
#upload fisheries information
fisheries<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Modeling/Data/Processed Data/summarized_fishery_data_ready4model.rds")%>%
  filter(year>1999)
#upload management regulations
regulations<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Regulation/monthly_percent_closed_blocks.rds")
regulations$month_year <- gsub("-", "_", regulations$month_year)
regulations$month_year <- sub("^0+", "", regulations$month_year)
regulations$fishery <- gsub("\\.", " ", regulations$fishery)

#upload SST and BT
temp<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Temperature/processed/monthly_sst_bt_GLORYS.rds")
#upload fuel prices
fuel<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Fuel price/data/processed/allportcomplexes_dieselprice_monthly_adjusted.rds")
#upload ex vessel price
exvessel<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/ex vessel price/data/processed/allfisheries_exvessel_port_year_month_adjusted.rds")%>%
  dplyr::select( port.complex, month_year,fishery, year, cost_per_pound_adjusted)%>%
  mutate(year=as.double(year))%>%
  mutate(fishery=case_when(fishery == "Halibut trawl" ~ "California halibut trawl",
    TRUE ~ fishery))
#upload distance to shore
dist_shore<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to shore/data/processed/block_shore_distances_complete.rds")
#upload distance to port complex
dist_complex<-readRDS("G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Variables/Distance to Port Complex/Data/Processed/port_block_distances_complete.rds")%>%
  rename(port.complex=port_complex, distance_complex_km=average.distance.km)%>%
  dplyr::select(-average.distance.m)

#upload block info
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'

#upload block information
all_blocks<-st_read(paste0(general_data,"Blocks/All_Blocks_Updated_10Jul24.shp"))%>%
  st_as_sf()

all_blocks<- st_transform(all_blocks, crs = st_crs(3310))

# Calculate centroids
centroids <- st_centroid(all_blocks)

# Extract lon/lat coordinates
coords <- st_coordinates(centroids)


# Add to the polygon data as new columns
all_blocks$block_long_dd <- coords[,1]/ 1000
all_blocks$block_lat_dd <- coords[,2]/ 1000


all_blocks<-all_blocks%>%
  dplyr::select(!c(Min_Depthm,Max_Depthm,Avg_Depthm,block_id))%>%
  rename(block_id=block_d)%>%
  filter(blck_ty!="Offshore" & blck_st!="Oregon"& blck_st!="Washington")%>%
  dplyr::select(-c(blck_st, blck_ty, blck_sq, geometry))%>%
  st_drop_geometry()

############

############
#expand effort data to go over each combo of fishery, port complex, month_year, and block id

#would need to elimate blocks that are entirely closed off to the fishery. 

#across all years, get all block IDs that are fished by a fishery, and use those as potential blocks/
#elimate any that are 100% closed to fishery. Then add in that effort is zero. OR should i do blockls that a port complex goes?
# 1. Get observed combinations
observed <- fisheries %>%
  dplyr::select(month_year, port.complex, fishery, block_id,year)

#find blocks in each fishery
fished_blocks <- fisheries %>%
  distinct(fishery, block_id)

#create dataframe with every combo
# Get unique values
months <- unique(fisheries$month_year)
ports  <- unique(fisheries$port.complex)


# Step 3: Expand each fishery to all ports and months (but only fished blocks!)
zero_grid <- fished_blocks %>%
  crossing(
    month_year = months,
    port.complex = ports
  )%>%
  mutate(total.hours=0, year = as.double(sub(".*_(\\d{4})", "\\1", month_year)))

# 4. Remove combinations that already exist in the data
zero_grid <- anti_join(zero_grid, observed,
                       by = c("month_year", "port.complex", "fishery", "block_id", "year"))

#combine with fisheries
fisheries_augmented <- bind_rows(fisheries, zero_grid)

############

############
#create full dataframe by adding explanatory variables 
fisheries_variables<-fisheries_augmented %>%
  left_join(fuel, by=c('port.complex','month_year'))%>%
  left_join(exvessel, by=c('port.complex','month_year','fishery','year'))%>%
  left_join(dist_shore, by=c('block_id'))%>%
  left_join(dist_complex, by=c('port.complex','block_id'))%>%
  left_join(temp, by=c('date','block_id'))%>%
  left_join(regulations, by=c('fishery','block_id','month_year'))%>%
  #add in XY cordinates of block
  left_join(all_blocks, by=c('block_id'))%>% 
#remove bad entries
  filter(!is.na(block_long_dd), !is.na(block_lat_dd))%>%
#remove 100% closed blocks with 0 fishing hours -> removing dummy blocks that we created where fishing can't occur.
  #also got rid of ~2 entries where there was logbook data - assuming they wrote the block number down incorrectly
filter(closed_pct!=1)
#remove those of not CPFV with the exvessel price of NA - only had landings data to mid 2023
fisheries_variables2<-fisheries_variables%>%
  filter(fishery == "CPFV" | !is.na(cost_per_pound_adjusted))%>%
  #remove block_id one which is inshore
  filter(block_id!="101")

############


############
fisheries_variables$block_long_dd <- as.numeric(fisheries_variables$block_long_dd)
fisheries_variables$block_lat_dd  <- as.numeric(fisheries_variables$block_lat_dd)

#create mesh
#mesh <- make_mesh(fisheries_variables, c("block_long_dd", "block_lat_dd"), cutoff = 10)
#plot(mesh)

test<-fisheries_variables2%>%
  filter(fishery=="Lobster trap" & port.complex=="Santa Barbara")%>%
  mutate(closed_pct=closed_pct*100)%>%
  mutate(date = my(month_year, tz = "UTC"))

mesh <- make_mesh(test, c("block_long_dd", "block_lat_dd"), cutoff = 10)
plot(mesh)

num_vars <- c("cost_per_gal_adjusted", "cost_per_pound_adjusted", 
              "distance_to_shore_km", "distance_complex_km", "BT", "SST", "closed_pct")

test <- test %>%
  mutate(across(all_of(num_vars), ~ as.numeric(scale(.)), .names = "{.col}_scaled"))

# Check means and sds
#sapply(test_scaled[num_vars], mean)  # all ~0
#sapply(test_scaled[num_vars], sd)    # all ~1
#create model
test$fyear <- as.factor(test$year)


test<-test%>%
  arrange(date) %>%
  mutate(time_index = as.integer(factor(date, levels = unique(date))))

test <- test %>%
  arrange(date) %>%
  mutate(time_index = match(date, sort(unique(date))))

m <- sdmTMB(
  data = test,
  formula = total.hours ~ cost_per_gal_adjusted_scaled + cost_per_pound_adjusted_scaled +distance_to_shore_km_scaled  +
    distance_complex_km_scaled + s(BT) + s(SST) + (closed_pct_scaled) + fyear, 
  mesh = mesh, 
  family = tweedie(link = "log"),
  spatial = "on",
  time = "date",
  spatiotemporal = "IID"
)
summary(m)
sanity(m)

# We can view the confidence intervals on the fixed effects by using the tidy function:
tidy(m, conf.int = TRUE)
#And similarly for the random effect and variance parameters:
tidy(m, "ran_pars", conf.int = TRUE)

#We can inspect randomized quantile residuals:
test$resids <- residuals(m) # randomized quantile residuals
qqnorm(test$resids)
qqline(test$resids)

ggplot(test, aes(block_long_dd, block_lat_dd, col = resids)) +
  scale_colour_gradient2() +
  geom_point() +
  facet_wrap(~date) +
  coord_fixed()


#Those were fast to calculate but can look ‘off’ even when the model is 
#consistent with the data. MCMC-based residuals are more reliable but slow. 
#We can calculate them through some help from the sdmTMBextra package. 
#https://github.com/pbs-assess/sdmTMBextra. In practice you would like want
#more mcmc_iter and mcmc_warmup. Total samples is mcmc_iter - mcmc_warmup. 
#We will also just use the spatial model here so the vignette builds quickly.

set.seed(123)
samps <- sdmTMBextra::predict_mle_mcmc(m, mcmc_warmup = 50, mcmc_iter = 51)

r <- residuals(m3, "mle-mcmc", mcmc_samples = samps)
qqnorm(r)
qqline(r)


#mariginal effects
g <- ggpredict(m, "cost_per_gal_adjusted_scaled")
#> Model contains splines or polynomial terms. Consider using `terms="depth
#>   [all]"` to get smooth plots. See also package-vignette 'Adjusted
#>   Predictions at Specific Values'.
plot(g)
