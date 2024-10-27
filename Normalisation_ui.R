library(shiny)
library(lidR)

# Define UI
ui <- fluidPage(
  titlePanel("LAS Decimation Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("lasfile", "Upload LAS File (.laz)"),
      selectInput("algorithm", "Select Algorithm", 
                  choices = c("random", "homogenize", "highest", "lowest", "random_per_voxel")),
      numericInput("param1", "Parameter 1", value = 1),
      numericInput("param2", "Parameter 2", value = 5),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Original LAS", plotOutput("original_plot")),
        tabPanel("Decimated LAS", plotOutput("decimated_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$submit, {
    req(input$lasfile)
    
    algorithm <- switch(input$algorithm,
                        "random" = random(input$param1),
                        "homogenize" = homogenize(input$param1, input$param2),
                        "highest" = highest(input$param1),
                        "lowest" = lowest(input$param1),
                        "random_per_voxel" = random_per_voxel(input$param1)
    )
    
    # Read LAS file
    las <- readLAS(input$lasfile$datapath, select = "xyz")
    
    # Decimate LAS
    decimated_las <- decimate_points(las, algorithm)
    
    # Plot original LAS
    output$original_plot <- renderPlot({
      plot(las)
    })
    
    # Plot decimated LAS
    output$decimated_plot <- renderPlot({
      plot(decimated_las)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
