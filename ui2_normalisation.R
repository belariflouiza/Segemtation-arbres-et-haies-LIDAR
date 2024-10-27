library(shiny)
library(lidR)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive LAS Decimation Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("lasfile", "Upload LAS File (.laz)"),
      selectInput("algorithm", "Select Algorithm", 
                  choices = c("None" = "", "random" = "random", "homogenize" = "homogenize", 
                              "highest" = "highest", "lowest" = "lowest", "random_per_voxel" = "random_per_voxel")),
      uiOutput("dynamicUI"),  # Dynamic UI for input parameters
      actionButton("updatePlot", "Update Plot")
    ),
    mainPanel(
      plotOutput("plotOutput")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  reactive_las <- reactive({
    req(input$lasfile)
    readLAS(input$lasfile$datapath, select = "xyz")
  })
  
  output$dynamicUI <- renderUI({
    if (input$algorithm == "random" || input$algorithm == "homogenize") {
      numericInput("param", "Parameter", value = if (input$algorithm == "random") 10 else 5)
    } else if (input$algorithm %in% c("highest", "lowest", "random_per_voxel")) {
      numericInput("param", "Parameter", value = 1)
    }
  })
  
  observeEvent(input$updatePlot, {
    req(reactive_las(), input$algorithm, input$param)
    las <- reactive_las()
    
    algorithm <- switch(input$algorithm,
                        "random" = random(input$param),
                        "homogenize" = homogenize(input$param),
                        "highest" = highest(input$param),
                        "lowest" = lowest(input$param),
                        "random_per_voxel" = random_per_voxel(input$param),
                        stop("Please select a valid algorithm"))
    
    decimated_las <- decimate_points(las, algorithm)
    
    output$plotOutput <- renderPlot({
      plot(decimated_las)
    })
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
