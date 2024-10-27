library(shiny)
library(lidaRtRee)
library(rgl)
library(terra)
library(sf)

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse des données LiDAR"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner le fichier LAS")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("CHM", plotOutput("chm_plot")),
        tabPanel("Segments", plotOutput("segments_plot")),
        tabPanel("Apices", plotOutput("apices_plot")),
        tabPanel("Projection 3D", rglwidgetOutput("rgl_widget"))
      )
    )
  )
)

# Définir la logique du serveur
server <- function(input, output) {
  observeEvent(input$file, {
    req(input$file)
    
    # Lire le fichier LAS
    las <- lidR::readLAS(input$file$datapath)
    
    # Calculer le CHM
    dtm <- lidR::rasterize_terrain(las, res = 0.5)
    dsm <- lidR::rasterize_terrain(las, res = 0.5)
    chm <- dsm - dtm
    
    # Calculer les segments
    segms <- tree_segmentation(chm)
    
    # Calculer les apices
    apices <- tree_extraction(segms)
    
    # Plot du CHM
    output$chm_plot <- renderPlot({
      plot(chm, main = "Modèle de hauteur du couvert")
    })
    
    # Plot des segments
    output$segments_plot <- renderPlot({
      plot(segms$segments_id, main = "Segments")
    })
    
    # Plot des apices
    output$apices_plot <- renderPlot({
      plot(apices$meanI, main = "Apices")
    })
    
    # Plot en 3D
    output$rgl_widget <- renderRglwidget({
      rgl::plot3d(las)
    })
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
