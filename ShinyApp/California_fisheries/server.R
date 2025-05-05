# server function -----
server<-function(input,output){
  
  # vesels nad trips reactive dataframe ----
  vessels_trips_filtered <- reactive ({
    vessels_trips%>%
      filter(fishery %in% c(input$fishery.input)) %>%
      filter( port.complex %in% c(input$port.complex.input))
    
  }) #end reactive dataframe
  
  #trips scatterplot ----
  output$trips_scatterplot_output <-renderPlot({ 
    
        ggplot(vessels_trips_filtered(), aes(x=year, y=ntrips, color=port.complex))+
          geom_point()+
          labs(x="Year", y="Number of trips", color="Port complex")+
          geom_smooth(method="lm")+
          scale_color_brewer(palette = 'RdYlBu', direction=-1) +
          myCustomTheme()
    
  }) #END trips scatterplot
  
  #vesselscatterplot ----
  output$vessels_scatterplot_output <-renderPlot({ 
    
    ggplot(vessels_trips_filtered(), aes(x=year, y=nvessels, color=port.complex))+
      geom_point()+
      labs(x="Year", y="Number of vessels", color="Port complex")+
      geom_smooth(method="lm")+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      myCustomTheme()
    
  }) #dn vessels scatterplot
  

  # vdepth reactive dataframe ----
  depth_filtered <- reactive ({
    depths%>%
      filter(fishery %in% c(input$fishery.input)) %>%
      filter( port.complex %in% c(input$port.complex.input))
    
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
        myCustomTheme()
  }) #END latitude extent plot
  
  # fishing ground area graph ----
  output$area_output <-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, color=port.complex))+
      geom_line(aes(y=area_75_km2),size=.5)+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      labs(x="Year", y = bquote(atop("Fishing ground area", "(" ~ km^2 ~ ")")), color="Port complex")+
      myCustomTheme()
  }) #END fishing ground area plot
  
  
  # distance to shore graph ----
  output$shore_output<-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, y=distance_shore_km, color=port.complex))+
      geom_line(size=.5)+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      labs(x="Year", y = ('Distance from shore \n(km)'), color="Port complex")+
      myCustomTheme()
  }) #END distance to shore plot
  
  # distance to shore graph ----
  output$effort_mpa_output<-renderPlot({ 
    
    ggplot( KD_timeseries_filtered(), aes(x=year, y=percent_inmpa, color=port.complex))+
      geom_line(size=.5)+
      scale_color_brewer(palette = 'RdYlBu', direction=-1) +
      labs(x="Year", y = ('Effort in MPA \n(%)'), color="Port complex")+
      myCustomTheme()
  }) #END distance to shore plot
  
  
  } #END server