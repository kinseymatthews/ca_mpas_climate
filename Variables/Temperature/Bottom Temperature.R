# Install and load the ncdf4 package
install.packages("ncdf4")
library(ncdf4)
library(ggplot2)
library(reshape2)


# Open the NetCDF file
nc_file <- nc_open("C:/Users/kinse/Downloads/mercatorglorys12v1_gl12_mean_199301.nc")

# Print the file's contents to explore
print(nc_file)



# Read a specific variable (replace 'your_variable_name' with the actual variable name)
variable_name <- "bottomT"
bottom_temp <- ncvar_get(nc_file, variable_name)

# Get the dimensions of the variable
dim_info <- ncvar_get(nc_file, variable_name)

lon <- ncvar_get(nc_file, "longitude")  # Replace "lon" with actual coordinate name
lat <- ncvar_get(nc_file, "latitude")  # Replace "lat" with actual coordinate name


#Reshape the data for plotting
temperature_df <- melt(bottom_temp)
colnames(temperature_df) <- c("lon", "lat", "value")

# Create the plot
ggplot(temperature_df, aes(x = lon, y = lat, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +  # Use viridis color palette
  labs(title = "Temperature", x = "Longitude", y = "Latitude") +
  theme_minimal()

print(dim(temperature))  # Should show [lon, lat, time]
