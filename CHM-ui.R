library(shiny)
library(ade4)
library(terra)

# Define UI
ui <- fluidPage(
  titlePanel("CHM Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose LAS file"),
      selectInput("algorithm", "Choose Algorithm",
                  choices = c("p2r", "p2r(subcircle)", "dsmtin", "pitfree")),
      sliderInput("resolution", "Resolution (m)", min = 0.1, max = 1, value = 0.5),
      conditionalPanel(
        condition = "input.algorithm == 'p2r(subcircle)'",
        sliderInput("subcircle", "Subcircle Radius (m)", min = 0.05, max = 0.5, value = 0.15)
      ),
      conditionalPanel(
        condition = "input.algorithm == 'dsmtin' || input.algorithm == 'pitfree'",
        sliderInput("max_edge", "Max Edge Length", min = 0.5, max = 2.5, value = 1.5)
      )
    ),
    mainPanel(
      plotOutput("plotCHM")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  chm <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    # Load LAS file
    las <- lidR::readLAS(inFile$datapath)
    # Set projection
    lidR::projection(las) <- 2154
    # Generate CHM based on selected algorithm and parameters
    switch(input$algorithm,
           "p2r" = rasterize_canopy(las, res = input$resolution, algorithm = p2r()),
           "p2r(subcircle)" = rasterize_canopy(las, res = input$resolution, algorithm = p2r(subcircle = input$subcircle)),
           "dsmtin" = rasterize_canopy(las, res = input$resolution, algorithm = dsmtin(max_edge = input$max_edge)),
           "pitfree" = rasterize_canopy(las, res = input$resolution, algorithm = pitfree(max_edge = input$max_edge)))
  })
  
  observe({
    output$plotCHM <- renderPlot({
      plot(chm(), col = height.colors(25), main = paste("CHM - Algorithm:", input$algorithm))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
