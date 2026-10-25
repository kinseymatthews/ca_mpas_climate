
  #........................dashboardHeader.........................
header <- dashboardHeader(
  
  # title ----
  title = "California Fishing Grounds",
  titleWidth = 400
  
) # END dashboardHeader

#........................dashboardSidebar........................
sidebar <- dashboardSidebar(
  
  # sidebarMenu ----
  sidebarMenu(
    id = "sidebarid",
    #introduction
    menuItem(text = "Welcome", tabName = "welcome", icon = icon("house")),
    menuItem(text = "California Fishing Grounds", tabName = "welcome", icon = icon("fish"),
             # exploratory analyses
             menuSubItem(text = "Exploratory", tabName = "exploratory", icon = icon("magnifying-glass")) ,
             #kernel density maps
             menuSubItem(text = "Kernel density maps", tabName = "kdmaps", icon = icon("map")),
             #kernel density metrics
             menuSubItem(text = "Kernel density metrics", tabName = "kdmetrics", icon = icon("chart-line")),
            #spatial maps
             menuSubItem(text = "Spatial predictions", tabName = "predictions", icon = icon("globe"))
             
             ),
   
    conditionalPanel(
      'input.sidebarid == "exploratory"',
      # fishery type pickerInput ----
      pickerInput(inputId = "fishery.input.exploratory", label = "Select fishery:",
                  choices = unique(vessels_trips$fishery), # alternatively: choices = c("rapid", "cascade" ...)
                  selected = c("CPFV"),
                  multiple = FALSE,
                  options = pickerOptions(actionsBox = FALSE) # creates "Select All / Deselect All" buttons
      ), # END first picker input of fishery
      
      # port complex pickerInput ----
      pickerInput(inputId = "port.complex.input.exploratory", label = "Select port complex(es):",
                  choices = NULL, # alternatively: choices = c("rapid", "cascade" ...)
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE) # creates "Select All / Deselect All" buttons
      ) # END channel type pickerInput
      
    ), #END first conditional panel
    conditionalPanel(
      'input.sidebarid == "kdmetrics"',
      # fishery type pickerInput ----
      pickerInput(inputId = "fishery.input", label = "Select fishery:",
                  choices = unique(KD_timeseries$fishery), # alternatively: choices = c("rapid", "cascade" ...)
                  selected = c("CPFV"),
                  multiple = FALSE,
                  options = pickerOptions(actionsBox = FALSE) # creates "Select All / Deselect All" buttons
      ), # END first picker input of fishery
      
      # port complex pickerInput ----
      pickerInput(inputId = "port.complex.input", label = "Select port complex(es):",
                  choices = NULL, # alternatively: choices = c("rapid", "cascade" ...)
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE) # creates "Select All / Deselect All" buttons
      ) # END channel type pickerInput
      
    ),#END second conditional panel
    
    conditionalPanel(
      'input.sidebarid == "kdmaps"',
      # fishery type pickerInput ----
      pickerInput(inputId = "fishery.input.kdmaps", label = "Select fishery:",
                  choices = unique( KD_contours_sepyears$fishery), # alternatively: choices = c("rapid", "cascade" ...)
                  selected = c("CPFV"),
                  multiple = FALSE,
                  options = pickerOptions(actionsBox = FALSE) # creates "Select All / Deselect All" buttons
      ), # END first picker input of fishery
      
      # port complex pickerInput ----
      pickerInput(inputId = "port.complex.input.kdmaps", label = "Select port complex(es):",
                  choices = NULL, # alternatively: choices = c("rapid", "cascade" ...)
                  selected = NULL,
                  multiple = FALSE,
                  options = pickerOptions(actionsBox = FALSE) # creates "Select All / Deselect All" buttons
      ) # END channel type pickerInput
      
    ),#END third conditional panel

    conditionalPanel(
      'input.sidebarid == "predictions"',
      pickerInput(inputId = "variable.input.predictions",
                  label = "Select prediction type:",
                  choices = c("Sea surface temperature" = "SST",
                              "Ex-vessel price" = "exvessel",
                              "Fuel" = "fuel"),
                  selected = "SST",
                  multiple = FALSE,
                  options = pickerOptions(actionsBox = FALSE)
      ),
      pickerInput(inputId = "fishery.input.predictions",
                  label = "Select fishery:",
                  choices = NULL,
                  multiple = FALSE,
                  options = pickerOptions(actionsBox = FALSE)
      ),
      pickerInput(inputId = "portcomplex.input.predictions",
                  label = "Select port complex:",
                  choices = NULL,
                  multiple = FALSE,
                  options = pickerOptions(actionsBox = FALSE)
      )
    ) # END predictions conditional panel
  ) # END sidebarMenu
  
) # END dashboardSidebar

#..........................dashboardBody.........................
body <- dashboardBody(
  
  # tabItems ----
  tabItems(
    
    # welcome tabItem ----
    tabItem(tabName = "welcome",
            
            # first fluidRow — main content ----
            fluidRow(
              
              # left-hand column ----
              column(width = 8,
                     box(width = NULL,
                         includeMarkdown("text/background.md")
                     )
              ), # END left-hand column
              
              # right-hand column ----
              column(width = 4,
                     
                  
                     fluidRow(
                       box(width = NULL,
                           includeMarkdown("text/disclaimer.md")
                       )
                     ) # END second fluidRow
                     
              ) # END right-hand column
              
            ), # END first fluidRow
            
            # second fluidRow — acknowledgements ----
            fluidRow(
              column(width = 12,
                     box(width = NULL,
                         includeMarkdown("text/datacitation.md")
                     )
              )
            ) # END acknowledgements fluidRow
            
    ), # END welcome tabItem
    
    # exploratpry tabItem ----
    tabItem(tabName = "exploratory",
            
            
            # first fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 6,
                  
                  plotOutput(outputId = "vessels_plot_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("vessels_caption")
                  
              ), # END input box
              
              # leaflet box ----
              box(width = 6, 
                  
                  plotOutput(outputId = "trips_plot_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("trips_caption")
                  
              ) # END leaflet box
              
            ), # END fluidRow
            
            # second fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 12,
                  
                  plotOutput(outputId = "depth_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("depth_caption")
                  
              ) # END input box
              
              
              
            ) # END fluidRow
    ), # END exploratory tabItem
    
    # kdmetrics tabItem ----
    tabItem(tabName = "kdmetrics",
            
            
            # first fluidRow ----
            fluidRow(
              
              # extent box ----
              box(width = 6,
                  
                  plotOutput(outputId = "lat_extent_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("latitude_caption")
                  
              ), # END input box
              
              # leaflet box ----
              box(width = 6, 
                  
                  plotOutput(outputId = "area_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("area_caption")
                  
              ) # END leaflet box
              
            ), # END fluidRow
            
            
            #shared legend row ----
            fluidRow(
              style = "padding-bottom: 20px;",
              column(width = 12, align = "center",
                     plotOutput(outputId = "kd_legend_output", height = "55px")
              )
            ), # END legend fluidRow
            
            # second fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 6,
                  
                  plotOutput(outputId = "shore_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("shore_caption")
                  
              ), # END input box
              
              # input box ----
              box(width = 6,
                  
                  plotOutput(outputId = "effort_mpa_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("mpa_caption")
                  
              )
              
            ) # END fluidRow
    ), # END kdmetircs tabItem
    
    #start kdmaps tabitem
    tabItem(tabName = "kdmaps",
            # first fluidRow ----
            fluidRow(
              
              # all years KD box ----
              box(width = 6,
                  
                  plotOutput(outputId = "kd_allyears_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("kd_caption")
                  
                  
              ), # END all years KD box
              
              
              # COG box ----
              box(width = 6,
                  
                  plotOutput(outputId = "cog_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("cog_caption")
                  
                  
              ), #END COG box
              
              
              
            ), #END fluid row
            
            # second fluidRow ----
            fluidRow(
              
              #animation box
              box(
                width = 6,
                imageOutput("kd_animation_output") %>%
                  shinycssloaders::withSpinner(color = "#006792", type = 1),
                textOutput("kd_animation_caption")
              ), #END animation box
              
              
              # disclaimer box ----
              box(width = 6,
                  uiOutput("year_selector"),
                  
                  br(),
                  plotOutput(outputId = "kd_yearsseperate_output")%>% 
                    shinycssloaders::withSpinner(color = "#006792", type = 1),
                  textOutput("yearlykd_caption")
                  
              ) #end disclaimer
            ) # END second fluidrow
    ),

    
    tabItem(tabName = "predictions",
            fluidRow(
              box(width = 12,
                  h4(textOutput("prediction_title"), align = "center"),
                  imageOutput("prediction_plot", height = "auto"),   # "auto" so the caption isn't covered
                  textOutput("prediction_caption")
              )
            )
    ) # END predictions tabItem
  ) # END tabItems
  
) # END dashboardBody

#..................combine all in dashboardPage..................
ui <- secure_app(
  dashboardPage(header, sidebar, body)
)

