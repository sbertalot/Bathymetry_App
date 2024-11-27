library(shiny)
library(shinyWidgets)
library(terra)
library(tidyterra)
library(ggplot2)
library(leaflet)

jck <- rast("jl_rastStack.tif")
fgr <- rast(matrix(runif(100, 10, 20), 10, 10)) 
boy <- rast(matrix(runif(100, 20, 30), 10, 10))
outline <- vect("Jackson Lake/Jackson_Lake_Shape.shp")

# Define the UI
ui <- fluidPage(
  titlePanel("Lake Habitat Viewer"),
  
  # Buttons for lake selection
  fluidRow(
    column(12, 
           actionButton("jck", "Jackson Lake"),
           actionButton("fgr", "Flaming Gorge Reservoir"),
           actionButton("boy", "Boysen Reservoir")
    )
  ),
  
  # Layout with 3 columns (30% for plot1, 30% for plot2, and 40% for map + slider)
  fluidRow(
    # First column (for the first plot) - 30% of the screen width
    column(4, 
           plotOutput("raster_plot1", height = "90vh")
    ),
    
    # Second column (for the second plot) - 30% of the screen width
    column(4, 
           plotOutput("raster_plot2", height = "90vh")
    ),
    
    # Third column (for the map + slider) - 40% of the screen width
    column(4, 
           fluidRow(
             # Slider output (on left) - takes 20% of the space
             column(3, 
                    noUiSliderInput(
                      inputId = "lyr", 
                      label = "Drawdown (ft)
                      :",
                      min = 0,  # Slider starts from 0
                      max = 39,  # Slider ends at 39
                      step = 1,
                      value = 0,  # Default value is the first layer (index 1)
                      margin = 1,
                      orientation = "vertical",
                      width = "100px",
                      height = "35vh"
                    )
             ),
             
             # Map output (on right) - takes 80% of the space
             column(9, 
                    leafletOutput("lake_map", height = "40vh")
             )
           ),
           
           # Stats and Overview panels with tabset
           fluidRow(
             column(12,
                    style = "display: flex; justify-content: flex-end;",  # Align to the right
                    fluidRow(
                      # Panel with Tabsets for Overview and Stats
                      column(12, 
                             wellPanel(
                               tabsetPanel(
                                 id = "tabs",
                                 tabPanel("Stats", 
                                          # Add your default text for the Overview tab
                                          div(style = "height: 100%; overflow-y: auto;", 
                                              p("This is the default statistics for the selected lake. 
                                  You can replace this with dynamic or precomputed statistical data.")
                                          )
                                 ),
                                 tabPanel("Overview", 
                                          # Add your default stats or placeholder for the Stats tab
                                          div(style = "height: 100%; overflow-y: auto;", 
                                              p("This is the default overview text for the selected lake. 
You can replace this with detailed information about the lake, 
the raster data, and other relevant content")
                                          )
                                 )
                               )
                             )
                      )
                    )
             )
           )
    )
  )
)


server <- function(input, output, session) {
  # Create a reactive value to store the current raster
  current_raster <- reactiveVal(NULL)
  
  jackson_lake_coords <- c(-110.676795, 43.919902)  
  boysen_reservoir_coords <- c(-108.177955, 43.322812)  
  flaming_gorge_coords <- c(-109.534457, 41.103538)
  
  output$lake_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Set view to the center of Wyoming as the default view
      setView(lng = -107.29028, lat = 43.07597, zoom = 6.25)  # Wyoming's central coordinates
  })
  # Observe which button is pressed and update the raster stack dynamically
  observeEvent(input$jck, {
    # Load the specific raster stack for Jackson Lake (replace with actual raster)
    current_raster(jck)  # Replace with actual data loading
    #updateNoUiSliderInput(session, "lyr", min = 0, max = 39, value = 0)
    
    # Render the map for Jackson Lake
    output$lake_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = jackson_lake_coords[1], lat = jackson_lake_coords[2], 
                   zoom = 10)
    })
  })
  
  observeEvent(input$fgr, {
    # Load the specific raster stack for Flaming Gorge (replace with actual raster)
    current_raster(fgr)  # Replace with actual data loading
    #updateNoUiSliderInput(session, "lyr", min = 0, max = 39, value = 0)
    
    # Render the map for Flaming Gorge
    output$lake_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = flaming_gorge_coords[1], lat = flaming_gorge_coords[2], 
                   zoom = 10)
    })
  })
  
  observeEvent(input$boy, {
    # Load the specific raster stack for Boysen Reservoir (replace with actual raster)
    current_raster(boy)  # Replace with actual data loading
    #updateNoUiSliderInput(session, "lyr", min = 0, max = 39, value = 0)
    
    # Render the map for Boysen Reservoir
    output$lake_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = boysen_reservoir_coords[1], lat = boysen_reservoir_coords[2], 
                   zoom = 10)
    })
  })
  
  # Render the first plot of the raster
  output$raster_plot1 <- renderPlot({
    req(current_raster())  # Ensure a raster is selected
    ggplot() +
      geom_spatvector(data = outline) +
      geom_spatraster(data = current_raster()[[1]]) +
      scale_fill_viridis_c(na.value = "transparent") +
      theme_void() +
      theme(legend.position = "none")
  })
  
  # Render the second plot of the raster
  output$raster_plot2 <- renderPlot({
    req(current_raster())  # Ensure a raster is selected
    selected_layer <- input$lyr + 1  # Get the selected layer index from the slider
    ggplot() +
      geom_spatvector(data = outline) +
      geom_spatraster(data = current_raster()[[selected_layer]]) +
      scale_fill_viridis_c(na.value = "transparent") +
      theme_void() +
      theme(legend.position = "none")
  })

}

# Run the app
shinyApp(ui, server)
