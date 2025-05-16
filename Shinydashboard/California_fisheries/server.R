server <- function(input, output) {
  
  # Show modal only once at the beginning
  observe({
    showModal(modalDialog(
      title = "Welcome to the California Fisheries Data Explorer",
      "This dashboard helps you interactively explore California fisheries. Use the side menu to navigate through different tabs and adjust filters to display different fisheries and port complexes",
      easyClose = FALSE,
      footer = modalButton("OK")
    ))
  })
  
  
  # vesels nad trips reactive dataframe ----
  
  vessels_trips_filtered <- reactive ({
    
    validate(
      need(length(input$port.complex.input) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    vessels_trips%>%
      filter(fishery %in% c(input$fishery.input.exploratory)) %>%
      filter( port.complex %in% c(input$port.complex.input.exploratory))
    
  }) #end reactive dataframe
  
  
  #trips plot ----
  output$trips_plot_output <-renderPlot({ 
    
    ggplot(vessels_trips_filtered(), aes(x=year, y=ntrips, color=port.complex))+
      geom_point()+
      labs(x="Year", y="Number of trips", color="Port complex")+
      geom_smooth(method="lm")+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      myCustomTheme()+
      theme(legend.position = "none")
    
  },
  alt = "A plot of the relationship between the year and number of trips for a fishery and port complex."
  
  
  ) #END trips plot
  
  #vessel plot ----
  output$vessels_plot_output <-renderPlot({ 
    
    ggplot(vessels_trips_filtered(), aes(x=year, y=nvessels, color=port.complex))+
      geom_point()+
      labs(x="Year", y="Number of vessels", color="Port complex")+
      geom_smooth(method="lm")+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      myCustomTheme()+
      theme(legend.position = "none")
    
  }) #END vessels plot
  
  # vdepth reactive dataframe ----
  depth_filtered <- reactive ({
    
    validate(
      need(length(input$port.complex.input) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    depths%>%
      filter(fishery %in% c(input$fishery.input.exploratory)) %>%
      filter( port.complex %in% c(input$port.complex.input.exploratory))
    
  })
  
  #depth scatterplot ----
  output$depth_output <-renderPlot({ 
    ggplot( depth_filtered(), aes(x=year,y=log_depth, color=port.complex))+
      geom_rect(
        aes(xmin = "2014", xmax = "2016",
            ymin = -Inf, ymax = Inf),
        fill = "gray80", alpha = 0.1, inherit.aes = FALSE)+
      geom_boxplot()+
      labs(y= "Logged depth (m)" ,x="Year", color="Port complex")+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      myCustomTheme()
    
  }) #END depth scatterplot
  
  # KD time series dataframe ----
  KD_timeseries_filtered <- reactive ({
    
    validate(
      need(length(input$port.complex.input) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    KD_timeseries%>%
      filter(fishery %in% c(input$fishery.input)) %>%
      filter( port.complex %in% c(input$port.complex.input))
    
  }) #end reactive dataframe
  
  # latitude and extent graph ----
  output$lat_extent_output <-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, color=port.complex))+
      geom_line(aes(y=COG_latitude),size=.5)+
      geom_line(aes(y=northernmost_latitude_75), linetype = "dotted",size=.5)+
      geom_line(aes(y=southernmost_latitude_75), linetype = "dotted",size=.5)+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      labs(x="Year", y="Latitude", color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "none")
  }) #END latitude extent plot
  
  # fishing ground area graph ----
  output$area_output <-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, color=port.complex))+
      geom_line(aes(y=area_75_km2),size=.5)+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      labs(x="Year", y = bquote(atop("Fishing ground area", "(" ~ km^2 ~ ")")), color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "right")
  }) #END fishing ground area plot
  
  
  # distance to shore graph ----
  output$shore_output<-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, y=distance_shore_km, color=port.complex))+
      geom_line(size=.5)+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      labs(x="Year", y = ('Distance from shore \n(km)'), color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "none")
  }) #END distance to shore plot
  
  # distance to shore graph ----
  output$effort_mpa_output<-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, y=percent_inmpa, color=port.complex))+
      geom_line(size=.5)+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      labs(x="Year", y = ('Effort in MPA \n(%)'), color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "none")
  }) #END distance to shore plot
  
  # COG reactivbe dataframe ----
  COG_filtered <- reactive ({
    
    validate(
      need(length(input$port.complex.input) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    COG_sf%>%
      filter(fishery %in% c(input$fishery.input.kdmaps)) %>%
      filter( port.complex %in% c(input$port.complex.input.kdmaps))
    
  }) #end reactive dataframe
  
  # COG figure ----
  output$cog_output <-renderPlot({ 
    # Get the filtered reactive data
    cog_data <- COG_filtered()
    
    #extracting coordinates 
    coordinates<-st_coordinates(cog_data)
    # Create a point pattern (ppp object)
    minx<-(min(coordinates[,1]))
    maxx<-(max(coordinates[,1]))
    miny<-(min(coordinates[,2]))
    maxy<-(max(coordinates[,2]))
    
    ggplot() +
      # Land
     
      geom_sf(data=usa_mexico_sf_proj, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      # points
      geom_sf(data=cog_data , aes(color=(year)),size=.75)+
      # Crop
      coord_sf(xlim = c(minx-80000, maxx+80000), ylim = c(miny-80000, maxy+80000)) +
      # Axes
    #  scale_x_continuous(breaks=seq(minx-.5, maxx+.5, .5),labels=scales::number_format(accuracy=0.50)) +
     # scale_y_continuous(breaks=seq(maxx+.5, maxy+.5, .5),labels=scales::number_format(accuracy=0.50)) + 
      scale_color_viridis_c(name = "Year", option = "D") +
      # Theme
      myCustomTheme()+
      theme(legend.position = "right")
    
  }) #END fishing ground area plot
  output$cog_caption <- renderText({
    "Yearly centers of gravity"
  })
  
  # KD all years combined reactive dataframe ----
  KD_combinedyears_filtered <- reactive ({
    
    validate(
      need(length(input$port.complex.input) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    KD_combinedyears%>%
      filter(fishery %in% c(input$fishery.input.kdmaps)) %>%
      filter( port.complex %in% c(input$port.complex.input.kdmaps))
    
  }) #end reactive dataframe
  
  # KD figure ----
  output$kd_allyears_output <-renderPlot({ 
    # Get the filtered reactive data
    kd_data <-  KD_combinedyears_filtered()
   
    
    #extracting coordinates and weights
    coordinates<-st_coordinates(kd_data)
    weights<-kd_data$effort
    
    # Create a point pattern (ppp object)
    minx<-(min(coordinates[,1]))
    maxx<-(max(coordinates[,1]))
    miny<-(min(coordinates[,2]))
    maxy<-(max(coordinates[,2]))
    
    points_ppp <- ppp(coordinates[,1], coordinates[,2], window = owin(c(minx, maxx), c(miny, maxy)))
    
    # Compute kernel density estimation using raw and logcdata
    sigma<-bw.diggle(points_ppp)
    density <- density.ppp(points_ppp, sigma = sigma, kernel="gaussian",weights=weights, diggle=TRUE, scalekernel=TRUE, edge=TRUE)
    
    # Convert density to a raster (assuming a reasonable raster resolution)
    density_raster <- as(raster(as.im(density)), "RasterLayer")
    
    # Mask the density raster with the land raster
    density_masked <- mask(density_raster, usa_mexico_sf_proj, inverse=TRUE)
    
    # Convert density to a data frame
    density_df <- as.data.frame(rasterToPoints(density_masked))
    names(density_df) <- c("x", "y", "value")
    

    # Calculate the cumulative distribution function (CDF)
    density_values <- sort(density_df$value, decreasing = TRUE)
    cdf <- cumsum(density_values) / sum(density_values)
    
    # Find the contour level corresponding to 75% volume
    contour_level_75 <- density_values[which.min(abs(cdf - 0.75))]
    # Create contour lines from the density raster
    contour_level_75_lines <- rasterToContour(density_masked, levels = c(contour_level_75))
    
    
    # Convert contour lines to sf polygons
    contour_75_sf <- st_as_sf(contour_level_75_lines)
    st_crs(contour_75_sf) <-  3310    
    # Normalize density values to the range 0-1
    density_df$value <- scales::rescale(density_df$value)
    
    
    ggplot() +
      geom_raster(data = density_df, aes(x=x,y=y,fill = value)) +
      
      scale_fill_gradientn(name = "Kernel Density", 
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                           limits = c(0, max(density_df$value)),  # Adjust limits as needed
                           na.value = "white") +  # Set NA values to white
      
      # Add contour lines at 75% volume boundary
      geom_sf(data = contour_75_sf, color = "black", linewidth = 1)  +
      
      # Land
      geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      # Crop
      coord_sf(xlim = c(-450000, 350000), ylim = c(-600000,450000)) +
      # Axes
     # scale_x_continuous(breaks=seq(-125, -117, 2)) +
      #scale_y_continuous(breaks=seq(32, 44, 2))+
      myCustomTheme()+
      theme(legend.position = "right",
            axis.title=element_blank())
    
  }) #END fishing ground area plot
  
  output$kd_caption <- renderText({
   "Kernel density for all years"
  })
  
  #Input for yearly KDs  ----
  # List all saved raster files (assumes file names like: CPFV_Central_2015.tif)
  raster_files <- list.files("data/rasters/", pattern = "\\.tif$", full.names = FALSE)
  
  # Split filenames into components: fishery_portcomplex_year
  raster_meta <- do.call(rbind, strsplit(gsub(".tif", "", raster_files), "_"))
  colnames(raster_meta) <- c("fishery", "portcomplex", "year")
  
  # Convert to data frame
  available_rasters <- as.data.frame(raster_meta, stringsAsFactors = FALSE)
  available_rasters$year <- as.numeric(available_rasters$year)
  
  
  output$year_selector <- renderUI({
    req(input$fishery.input, input$port.complex.input)
    
    # Filter by selected fishery and port complex
    available_years <- available_rasters %>%
      filter(fishery == input$fishery.input,
             portcomplex == input$port.complex.input) %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    # Handle case where no matching years are found
    if (length(available_years) == 0) {
      return(selectInput("year_input", "Select a year", choices = NULL, selected = NULL))
    }
    
    # Return year selector
    selectInput(
      inputId = "year_input",
      label = "Select a year",
      choices = available_years,
      selected = available_years[1]
    )
  })
  
  # KD years serpate combined reactivbe dataframe ----
  KD_sepyears_density_filtered <- reactive ({
 
    req(input$fishery.input.kdmaps, input$port.complex.input.kdmaps, input$year_input)
    raster_file <- file.path("data/rasters", paste0(input$fishery.input.kdmaps, "_", input$port.complex.input.kdmaps, "_", input$year_input, ".tif"))
    r <- raster::raster(raster_file)
    df <- as.data.frame(raster::rasterToPoints(r))
    colnames(df) <- c("x", "y", "value")
    df
  }) #end reactive dataframe
  
  
  #reactive dataframe for contours
  KD_sepyears_contours_filtered  <- reactive ({
    
  
    KD_contours_sepyears%>%
      filter(fishery %in% c(input$fishery.input.kdmaps)) %>%
      filter( port.complex %in% c(input$port.complex.input.kdmaps))%>%
      filter(year %in% c(input$year_input))
    
  
  })
  
  # KD yearly figure ----
  output$kd_yearsseperate_output <-renderPlot({
    # Get the filtered reactive data fframes
    kd_den_data <-   KD_sepyears_density_filtered()
    kd_cont_data <- KD_sepyears_contours_filtered()

    # Get the filtered reactive data
    cog_data <- COG_filtered()%>%
      st_transform(crs = 3310)
    
    #extracting coordinates and weights
    coordinates<-st_coordinates( cog_data)
    # Create bounding block for plot
    minx<-(min(coordinates[,1], na.rm = TRUE))
    maxx<-(max(coordinates[,1], na.rm = TRUE))
    miny<-(min(coordinates[,2], na.rm = TRUE))
    maxy<-(max(coordinates[,2], na.rm = TRUE))
    

    ggplot() +
      geom_raster(data = kd_den_data , aes(x=x,y=y,fill = value)) +
      
      scale_fill_gradientn(name = "Kernel Density", 
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),  # Spectral palette with white for 0
                           limits = c(0, max(kd_den_data$value)),  # Adjust limits as needed
                           na.value = "white") +  # Set NA values to white
      
      # Add contour lines at 75% volume boundary
      geom_sf(data = kd_cont_data, color = "black", linewidth = 1)  +
      # Land
      geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
      # State waters
      geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
      # Crop
      coord_sf(xlim = c(minx-80000, maxx+80000), ylim = c(miny-80000, maxy+80000)) +
      # Axes
     # scale_x_continuous(breaks=seq(-125, -117, 2)) +
    #  scale_y_continuous(breaks=seq(32, 44, 2))+
      myCustomTheme()+
      theme(legend.position = "none",
            axis.title=element_blank())
    
    
  })
}

