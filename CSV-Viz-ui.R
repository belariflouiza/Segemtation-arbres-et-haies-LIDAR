library(shiny)
library(ade4)
library(lidR)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  titlePanel("Tree Inventory Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file"),
      tags$hr(),
      checkboxInput("tree_plot", "Show Tree Plot", value = TRUE),
      checkboxInput("height_hist", "Show Height Histogram", value = TRUE),
      checkboxInput("species_boxplot", "Show Species Boxplot", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("treePlot"),
      plotOutput("heightHist"),
      plotOutput("speciesBoxplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  # Tree plot
  output$treePlot <- renderPlot({
    req(input$file)
    req(input$tree_plot)
    if (input$tree_plot) {
      plot(data()$x, data()$y, xlab = "Position Est", ylab = "Position Nord",
           main = "Tree Positions", pch = 20, col = "blue")
    }
  })
  
  # Height histogram
  output$heightHist <- renderPlot({
    req(input$file)
    req(input$height_hist)
    if (input$height_hist) {
      hist(data()$h, xlab = "Height", main = "Height Histogram", col = "lightblue")
    }
  })
  
  # Species boxplot
  output$speciesBoxplot <- renderPlot({
    req(input$file)
    req(input$species_boxplot)
    if (input$species_boxplot) {
      ggplot(data(), aes(x = s, y = h)) +
        geom_boxplot(fill = "lightgreen") +
        labs(x = "Species", y = "Height", title = "Species Boxplot")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
