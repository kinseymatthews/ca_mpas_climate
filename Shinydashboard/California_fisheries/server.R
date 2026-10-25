server <- function(input, output,session) {
  
  
  # Add this one line at the very top of your server
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
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
      need(length(input$port.complex.input.exploratory) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    vessels_trips%>%
      filter(fishery %in% c(input$fishery.input.exploratory)) %>%
      filter( port.complex %in% c(input$port.complex.input.exploratory))%>%
    mutate(port.complex = factor(port.complex, levels = port_order))
    
  }) %>% bindCache(input$fishery.input.exploratory, input$port.complex.input.exploratory)
  #end reactive dataframe
  
  
  #trips plot ----
  output$trips_plot_output <-renderPlot({ 
    
    ggplot(vessels_trips_filtered(), aes(x=year, y=ntrips, color=port.complex))+
      geom_point(size=2.5)+
      labs(x="Year", y="Number of trips", color="Port complex")+
      geom_smooth(
        aes(group = port.complex, fill = port.complex),
        method = "glm",
        method.args = list(family = quasipoisson),
        se = TRUE,
        alpha = 0.25
      ) +
      scale_fill_manual(values = rb_cols) +
      scale_color_manual(values = rb_cols) +
      myCustomTheme()+
      theme(legend.position = "none")
    
  },
  alt = "A plot of the relationship between the year and number of trips for a fishery and port complex."
  
  
  ) #END trips plot
  
  output$trips_caption <- renderText({
    "Annual number of fishing trips in the selected fishery by port complex. Lines and confidence intervals represent a linear regression fit to the data to illustrate trends in effort. Years with fewer than three vessels are suppressed for confidentiality."
  })
  #vessel plot ----
  output$vessels_plot_output <- renderPlot({
    
    ggplot(vessels_trips_filtered(),
           aes(x = year, y = nvessels, color = port.complex)) +
      
      geom_point(size = 2.5) +
      
      geom_smooth(
        aes(group = port.complex, fill = port.complex),
        method = "glm",
        method.args = list(family = poisson),
        se = TRUE,
        alpha = 0.25
      ) +
      
      geom_hline(yintercept = 3, linetype = "dashed") +
      
      labs(x = "Year", y = "Number of vessels", color = "Port complex") +
      
      scale_color_manual(values = rb_cols) +
      scale_fill_manual(values = rb_cols) +
      
      myCustomTheme() +
      theme(legend.position = "none")
  })#END vessels plot
  
  output$vessels_caption <- renderText({
    "Annual number of unique vessels landing in the selected fishery by port complex. The dashed black line marks three vessels, the minimum required to display confidential fisheries data; years below this threshold are suppressed. Lines and confidence intervals represent a linear regression fit to the data to illustrate trends in participation."
  })
  
  # vdepth reactive dataframe ----
  # depth reactive dataframe ----
  depth_filtered <- reactive({
    validate(
      need(length(input$port.complex.input.exploratory) > 0, 
           "Please select at least one port complex to visualize data for.")
    )
    depths %>%
      filter(fishery %in% input$fishery.input.exploratory) %>%
      filter(port.complex %in% input$port.complex.input.exploratory)%>%
      mutate(
        port.complex = factor(port.complex, levels = port_order)
      )
  })  %>% bindCache(input$fishery.input.exploratory, input$port.complex.input.exploratory)
  
  # depth plot ----
  output$depth_output <- renderPlot({
    
    df <- depth_filtered()
    
    avg_df <- df %>%
      group_by(port.complex) %>%
      summarise(avg_depth = mean(mean, na.rm = TRUE), .groups = "drop")
    
    ggplot(depth_filtered(), aes(x = year, color = port.complex)) +
      annotate("rect", xmin = "2014", xmax = "2016",
               ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.1) +
      geom_boxplot(aes(ymin = ymin, lower = lower, middle = middle,
                       upper = upper, ymax = ymax, fill=port.complex),
                   stat = "identity", alpha=0.4) +
      labs(y = "Depth (m)", x = "Year", color = "Port complex") +
      scale_y_continuous(trans="log10") +
      scale_color_manual(values = rb_cols) +
      scale_fill_manual(values = rb_cols) +
      geom_hline(
        data = avg_df,
        aes(yintercept = avg_depth, color = port.complex),
        linetype = "dashed",
        linewidth = 0.8
      )+
      guides(fill = "none")+
      myCustomTheme()
  }) # END depth plot
  
  output$depth_caption <- renderText({
    "Annual distribution of fishing depth (m). Boxes show the interquartile range (25th–75th percentiles) with the median as the center line; whiskers extend to 1.5× the interquartile range, and points beyond the whiskers are shown individually. The colored horizontal line shows the mean depth across all years for that specific port complex. Years with fewer than three vessels are suppressed for confidentiality."
  })
  
  # update port complexes based on fishery (exploratory) ----
  observe({
    
    req(input$fishery.input.exploratory)
    
    available_ports <- vessels_trips %>%
      filter(fishery == input$fishery.input.exploratory) %>%
      pull(port.complex) %>%
      unique()
    
    ordered_ports <- port_order[port_order %in% available_ports]
    
    updatePickerInput(
      session,
      "port.complex.input.exploratory",
      choices = ordered_ports,
      selected = ordered_ports[1]
    )
  })
  
  # KD time series dataframe ----
  KD_timeseries_filtered <- reactive ({
    
    validate(
      need(length(input$port.complex.input) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    KD_timeseries%>%
      filter(fishery %in% c(input$fishery.input)) %>%
      filter( port.complex %in% c(input$port.complex.input))
    
  }) %>% bindCache(input$fishery.input, input$port.complex.input) #end reactive dataframe
  
  #shared legend ----
  # shared legend ----
  output$kd_legend_output <- renderPlot({
    p <- ggplot(KD_timeseries_filtered(), aes(x = year, color = port.complex)) +
      geom_line(aes(y = COG_latitude)) +
      scale_color_manual(values = rb_cols) +
      labs(color = "Port complex") +
      myCustomTheme() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal")
    
    g <- ggplotGrob(p)
    legend_grob <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
    
    grid::grid.newpage()
    grid::grid.draw(legend_grob)
  }) # END shared legend
  
  
  # latitude and extent graph ----
  output$lat_extent_output <-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, color=port.complex))+
      geom_line(aes(y=COG_latitude),size=1)+
      geom_line(aes(y=northernmost_latitude_75), linetype = "dotted",size=1)+
      geom_line(aes(y=southernmost_latitude_75), linetype = "dotted",size=1)+
      scale_color_manual(values = rb_cols) +
      labs(x="Year", y="Latitude  (°N)", color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "none")
  }) #END latitude extent plot
  
  output$latitude_caption <- renderText({
    "Annual latitude (°N) of the fishing center of gravity, the effort-weighted mean location of fishing activity, by port complex. Dotted lines indicate the northern and southernmost extent of the fishing grounds mapped through kernel density estimation."
  })
  
  # fishing ground area graph ----
  output$area_output <-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, color=port.complex))+
      geom_line(aes(y=area_75_km2),size=1)+
      scale_color_manual(values = rb_cols) +
      labs(x="Year", y = bquote(atop("Fishing ground area", "(" ~ km^2 ~ ")")), color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "none")
  }) #END fishing ground area plot
  
  output$area_caption <- renderText({
    "Annual area (km²) of core fishing grounds, defined as the 75% volume contour of a kernel density estimate of fishing locations, by port complex. "
  })
  
  # distance to shore graph ----
  output$shore_output<-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, y=distance_shore_km, color=port.complex))+
      geom_line(size=1)+
      scale_color_manual(values = rb_cols) +
      labs(x="Year", y = ('Distance from mainland \n(km)'), color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "none")
  }) #END distance to shore plot
  
  output$shore_caption <- renderText({
    "Distance (km) of the annual fishing center of gravity to the nearest point on the mainland by port complex."
  })
  
  # deffort mpa graph ----
  output$effort_mpa_output<-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, y=percent_inmpa, color=port.complex))+
      geom_line(size=1)+
      scale_color_manual(values = rb_cols) +
      labs(x="Year", y = ('% of desirable fishing grounds in MPAs'), color="Port complex")+
      myCustomTheme()+
      theme(legend.position = "none")
  }) #END distance to shore plot
  
  output$mpa_caption <- renderText({
    "Percent of core fishing ground area (75% KDE contour) that overlaps with marine protected areas by port complex. Overlap does not indicate fishing occurred inside MPAs; it reflects the share of core grounds potentially affected by spatial management."
  })
  
  # update port complexes based on fishery (kdmetrics) ----
  observe({
    req(input$fishery.input)
    available_ports <- KD_timeseries %>%
      filter(fishery == input$fishery.input) %>%
      pull(port.complex) %>%
      unique()
    
    updatePickerInput(session, "port.complex.input",
                      choices = available_ports,
                      selected = available_ports[1])
  })
  
  
  # COG reactivbe dataframe ----
  COG_filtered <- reactive ({
    
    validate(
      need(length(input$port.complex.input.kdmaps) > 0, "Please select at least one port complex to visualize data for.")
    )
    
    COG_sf%>%
      filter(fishery %in% c(input$fishery.input.kdmaps)) %>%
      filter( port.complex %in% c(input$port.complex.input.kdmaps))
    
  }) %>% bindCache(input$fishery.input.kdmaps, input$port.complex.input.kdmaps) #end reactive dataframe
  
  # COG figure ----
  output$cog_output <-renderPlot({ 
    # Get the filtered reactive data
    cog_data <- COG_filtered()

    # Filter ports to only selected port complexes 
    ports_filtered <- ports %>%
      filter(port.complex %in% input$port.complex.input.kdmaps) %>%
      st_transform(st_crs(cog_data))  # make sure CRS matches
    
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
      geom_sf(data=cog_data , aes(color=(year)),size=1.5)+
      # Port locations — star shape, black fill, stands out from COG points
      # Port locations
      geom_sf(data = ports_filtered,
              shape  = 8,
              color  = "red",
              size   = 3,
              stroke = 1.2,
              inherit.aes = FALSE) +
      
      scale_color_viridis_c(
        name  = NULL,
        option = "D",
        guide  = guide_colorbar(order = 1,
                                ticks.colour    = "black",
                                frame.colour    = "black",
                                frame.linewidth = 0.2)
      ) +
      scale_shape_manual(
        name   = NULL,
        values = c("Port Complex" = 8),   # 8 = asterisk star
        guide  = guide_legend(order = 2,
                              override.aes = list(color = "black", size = 3))
      ) +
      # Crop
      coord_sf(xlim = c(minx-40000, maxx+40000), ylim = c(miny-40000, maxy+40000)) +
      # Axes
      scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +  # only 3 x-axis ticks
      scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
      # Theme
      myCustomTheme()+
      theme(legend.position   = "bottom",
            axis.title        = element_blank(),
            legend.key.width  = unit(2, "cm"),
            legend.key.height = unit(0.4, "cm"),
            legend.box        = "vertical",        # stack legends vertically
            plot.margin       = margin(5, 40, 30, 5))  # increase right margin
    
  }) #END COG plot
  output$cog_caption <- renderText({
    "Annual fishing center of gravity (effort-weighted mean location) with points colored by year. The red star marks the port complex."
  })
  
  # KD all years combined reactive dataframe ----
  KD_combinedyears_filtered <- reactive({
    req(input$fishery.input.kdmaps, input$port.complex.input.kdmaps)
    
    raster_file <- file.path("data/rasters", paste0(
      input$fishery.input.kdmaps, "_",
      input$port.complex.input.kdmaps, "_allyears.tif"))
    
    validate(need(file.exists(raster_file), "No data available for this selection."))
    
    r <- terra::rast(raster_file)
    df <- as.data.frame(r, xy = TRUE)
    colnames(df) <- c("x", "y", "value")
    df$value <- scales::rescale(df$value)
    df
  }) %>% bindCache(input$fishery.input.kdmaps, input$port.complex.input.kdmaps) 
  #end reactive dataframe
  
  # KD all years contours reactive ----
  KD_combinedyears_contours_filtered <- reactive({
    req(input$fishery.input.kdmaps, input$port.complex.input.kdmaps)
    
    contour_file <- file.path("data", paste0(
      input$fishery.input.kdmaps, "_KD_allyears_contours.gpkg"))
    
    validate(need(file.exists(contour_file), "No contour data available for this selection."))
    
    st_read(contour_file, quiet = TRUE) %>%
      filter(portcomplex == input$port.complex.input.kdmaps)
  }) %>% bindCache(input$fishery.input.kdmaps, input$port.complex.input.kdmaps)
  
 
  # KD figure ----
  output$kd_allyears_output <- renderPlot({
    kd_data <- KD_combinedyears_filtered()
    contour_data <- KD_combinedyears_contours_filtered()
    
    # Filter ports to selected port complexes 
    ports_filtered <- ports %>%
      filter(port.complex %in% input$port.complex.input.kdmaps) %>%
      st_transform(3310)
    
    ggplot() +
      geom_raster(data = kd_data, aes(x = x, y = y, fill = value)) +
      scale_fill_gradientn(name = NULL,
                           colors = c("white", rev(RColorBrewer::brewer.pal(10, "Spectral"))[-1]),
                           limits = c(0, 1),
                           breaks = c(0, 0.25, 0.5, 0.75, 1.0),
                           labels = c("0", "0.25", "0.5", "0.75", "1.0"),
                           na.value = "white",
                           guide = guide_colorbar(
                             ticks.colour = "black",
                             frame.colour = "black",
                             frame.linewidth = 0.2)) +
      geom_sf(data = contour_data, color = "black", linewidth = 1) +
      geom_sf(data = usa_mexico_sf_proj, fill = "grey85", col = "white",
              linewidth = 0.2, inherit.aes = FALSE) +
      geom_sf(data = state_waters, color = "grey30", linewidth = 0.2,
              inherit.aes = FALSE) +
      # Port star markers
      geom_sf(data = ports_filtered,
              shape  = 8,
              color  = "red",
              size   = 3,
              stroke = 1.2,
              inherit.aes = FALSE) +
      coord_sf(xlim = c(-450000, 350000), ylim = c(-600000, 450000)) +
      myCustomTheme() +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(0.4, "cm"),
            plot.margin = margin(5, 5, 30, 5))
  }) #END kd all years plot

  
  output$kd_caption <- renderText({
   "Kernel density of fishing locations in the selected fishery across all years. Warmer colors indicate higher relative density. The black line marks the 75% volume contour (core fishing grounds), and the red star marks the port complex."
  })
  
  
  #input for animation KD ----
  output$kd_animation_output <- renderImage({
    
    req(input$fishery.input.kdmaps, input$port.complex.input.kdmaps)
    
    fishery <- input$fishery.input.kdmaps
    
    port <- input$port.complex.input.kdmaps
    
    gif_file <- file.path(
      "www",
      "animations",
      fishery,
      paste0(port, "Kernel Densities_Animation.gif")
    )
    
    validate(need(file.exists(gif_file), paste("Not found:", gif_file)))
    
    list(
      src = normalizePath(gif_file),
      contentType = "image/gif",
      width = "100%"
    )
    
  }, deleteFile = FALSE)
  
  #Input for yearly KDs  ----

  
  output$year_selector <- renderUI({
    req(input$fishery.input.kdmaps, input$port.complex.input.kdmaps)
    
    # Filter by selected fishery and port complex
    available_years <- available_rasters2 %>%
      filter(fishery == input$fishery.input.kdmaps,
             portcomplex == input$port.complex.input.kdmaps) %>%
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

    # Filter ports to selected port complexes 
    ports_filtered <- ports %>%
      filter(port.complex %in% input$port.complex.input.kdmaps) %>%
      st_transform(3310)
    
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
      # Port star markers
      geom_sf(data = ports_filtered,
              shape  = 8,
              color  = "red",
              size   = 3,
              stroke = 1.2,
              inherit.aes = FALSE) +
      # Crop
      coord_sf(xlim = c(minx-80000, maxx+80000), ylim = c(miny-80000, maxy+80000)) +
      # Axes
     # scale_x_continuous(breaks=seq(-125, -117, 2)) +
    #  scale_y_continuous(breaks=seq(32, 44, 2))+
      myCustomTheme()+
      theme(legend.position = "none",
            axis.title=element_blank())
    
    
  }) #END KD yearly figure
  
  output$yearlykd_caption <- renderText({
    "Kernel density of fishing locations for the selected year. Warmer colors indicate higher relative density. The black line marks the 75% volume contour (core fishing grounds), and the red star marks the port complex. Years with fewer than three vessels are suppressed for confidentiality."
  })
  
  # update port complexes based on fishery (kdmaps) ----
  observe({
    req(input$fishery.input.kdmaps)
    available_ports <- KD_contours_sepyears %>%
      filter(fishery == input$fishery.input.kdmaps) %>%
      pull(port.complex) %>%
      unique()%>%
      as.character()
    
    ordered_ports <- port_order[port_order %in% available_ports]
    
    
    updatePickerInput(session, "port.complex.input.kdmaps",
                      choices = ordered_ports,
                      selected = ordered_ports[1])
  })
  
  # spatial predictions plots ----
  
  # update fishery based on variable ----

  observe({
    req(input$variable.input.predictions)
    available_fisheries <- pred_info[pred_info$variable == input$variable.input.predictions, ]
    
    fishery_choices <- setNames(available_fisheries$fishery, available_fisheries$fishery.label)
    fishery_choices <- fishery_choices[!duplicated(fishery_choices)]
    
    updatePickerInput(session, "fishery.input.predictions",
                      choices = fishery_choices,
                      selected = fishery_choices[1])
  })

  # update port complex based on fishery ----
  observe({
    req(input$fishery.input.predictions, input$variable.input.predictions)
    available_ports <- pred_info[
      pred_info$fishery == input$fishery.input.predictions &
        pred_info$variable == input$variable.input.predictions, ]
    
    port_choices <- setNames(available_ports$port.complex, available_ports$port.complex.label)
    port_choices <- port_choices[!duplicated(port_choices)]
    
    updatePickerInput(session, "portcomplex.input.predictions",
                      choices = port_choices,
                      selected = port_choices[1])
  })
  
  # render prediction image ----
  output$prediction_plot <- renderImage({
    req(input$fishery.input.predictions,
        input$portcomplex.input.predictions,
        input$variable.input.predictions)
    
    # Look up the actual file name instead of rebuilding it
    file <- pred_info$file[pred_info$fishery      == input$fishery.input.predictions &
                             pred_info$port.complex == input$portcomplex.input.predictions &
                             pred_info$variable     == input$variable.input.predictions][1]
    
    validate(need(!is.na(file), "No prediction map for this selection."))
    
    filename <- file.path("www", file)
    validate(need(file.exists(filename), paste("Image not found:", filename)))
    
    list(src = normalizePath(filename),
         contentType = "image/jpeg",
         width = "100%")
  }, deleteFile = FALSE)
  
  
  #adding in text
  output$prediction_title <- renderText({
    req(input$fishery.input.predictions,
        input$portcomplex.input.predictions,
        input$variable.input.predictions)
    
    variable_label <- c("SST" = "Sea surface temperature",
                        "exvessel" = "Ex-vessel price",
                        "fuel" = "Fuel")[input$variable.input.predictions]
    
    port_label <- pred_info$port.complex.label[pred_info$port.complex == input$portcomplex.input.predictions][1]
    fishery_label <- pred_info$fishery.label[pred_info$fishery == input$fishery.input.predictions][1]
    
    paste0(variable_label, " predictions for the ", fishery_label, " fishery at ", port_label)
  
    })
     # prediction caption ----
    output$prediction_caption <- renderText({
      "Predicted change in relative fishing effort under low and high conditions for the selected fishery and port complex. Predictions come from the full model, which combines the probability of fishing (presence/absence) with effort given presence. They are made for the last operable year in the month with the highest landings over the final five years of data. The selected variable is set to its 10th or 90th percentile while all other dynamic variables are held at their means. Values show the difference from a prediction with all variables at their means: red indicates more effort than average conditions, and blue indicates less. Black polygons show marine protected areas, and the red star marks the port complex."
   
    })
}

