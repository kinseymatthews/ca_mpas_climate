# user interface ----
ui <- navbarPage(
  
  # app title ----
  title = "California Fishing Grounds",
  
  # (Page 1) intro tabPanel ----
  tabPanel(title="About this App",
           
           # intro text fluidRow ----
           fluidRow(
             
             # use columns to create white space on sides
             column(1),
             column(10, includeMarkdown("text/about.md")),
             column(1)
             
           ), # END intro text fluidRow
           
           hr(), # creates light gray horizontal line
           
           # footer text ----
           includeMarkdown("text/footer.md")
           
           
           
  ), #END page 1 intro tabpanel
  
  # (Page 2) data vis tab panel ----
  tabPanel(title = "Explore the Data",
           
           #tabsetPanel to contain tabs for data viz ----
           tabsetPanel( 
             
             #Exploratory analyses panel ----
             tabPanel(title="Exploratory Analysis",
                      
                      # exploratory sidebarLayout ----
                      sidebarLayout(
                        
                              # exploratory sidebarPanel ----
                              sidebarPanel(
                                
                                # fishery type pickerInput ----
                                pickerInput(inputId = "fishery.input", label = "Select fishery:",
                                            choices = unique(vessels_trips$fishery), # alternatively: choices = c("rapid", "cascade" ...)
                                            selected = c("CPFV"),
                                            multiple = FALSE,
                                            options = pickerOptions(actionsBox = FALSE) # creates "Select All / Deselect All" buttons
                                ), # END fishery  pickerInput
                                
                                
                                # port complex type pickerInput ----
                                pickerInput(inputId = "port.complex.input", label = "Select port complex(es):",
                                            choices = unique(vessels_trips$port.complex), # alternatively: choices = c("rapid", "cascade" ...)
                                            selected = c("Monterey"),
                                            multiple = TRUE,
                                            options = pickerOptions(actionsBox = TRUE) # creates "Select All / Deselect All" buttons
                                ) # END channel type pickerInput
                                
                              ), # END exploratory  sidebarPanel
                        
                              # exploratory  mainPanel ----
                              mainPanel(
                                
                                plotOutput(outputId = "trips_scatterplot_output"),
                                plotOutput(outputId = "vessels_scatterplot_output"),
                                plotOutput(outputId = "depth_output")
                              ) # END exploratory  mainPanel
                            
                      ) # END exploratory  sidebarLayout
                      
                      
             ), #END exploratory tabpanel
             
             # Kernel Density analyses panel ----
             tabPanel(title="Kernel Densities",
                      
                      # KD sidebarLayout ----
                      sidebarLayout(
                        
                              # KD sidebarPanel ----
                              sidebarPanel(
                                
                                # fishery type pickerInput ----
                                pickerInput(inputId = "fishery.input", label = "Select fishery:",
                                            choices = unique(KD_timeseries$fishery), # alternatively: choices = c("rapid", "cascade" ...)
                                            selected = c("CPFV"),
                                            multiple = FALSE,
                                            options = pickerOptions(actionsBox = FALSE) # creates "Select All / Deselect All" buttons
                                ), # END fishery  pickerInput
                                
                                
                                # port complex type pickerInput ----
                                pickerInput(inputId = "port.complex.input", label = "Select port complex(es):",
                                            choices = unique(KD_timeseries$port.complex), # alternatively: choices = c("rapid", "cascade" ...)
                                            selected = c("Monterey"),
                                            multiple = TRUE,
                                            options = pickerOptions(actionsBox = TRUE) # creates "Select All / Deselect All" buttons
                                ) # END channel type pickerInput
                                
                                
                              ), # END KD sidebarPanel
                        
                              # KD mainPanel ----
                              mainPanel(
                                plotOutput(outputId = "lat_extent_output"),
                                plotOutput(outputId = "area_output"),
                                plotOutput(outputId = "shore_output"),
                                plotOutput(outputId = "effort_mpa_output")
                              ) # END exploratory  mainPanel
                        
                      ) # END exploratory  sidebarLayout
                      
             )#END KD tabpanel
             
           )#END tabset panel
           
  ) #END tabpanel
  
) #END ui