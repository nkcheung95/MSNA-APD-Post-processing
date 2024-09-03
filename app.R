library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Analysis Launcher"),
  
  # Centered button layout
  fluidRow(
    column(12, align = "center",
           actionButton("dbscan_btn", "Run DBSCAN Analysis", width = '300px', style = "font-size: 20px; margin: 20px;"),
           actionButton("isi_btn", "Run ISI Cluster Analysis", width = '300px', style = "font-size: 20px; margin: 20px;"),
           actionButton("arrhythmia_btn", "Run Arrhythmia Analysis", width = '300px', style = "font-size: 20px; margin: 20px;")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$dbscan_btn, {
    # Redirect to DBSCAN Analysis script hosted on GitHub
    browseURL("https://github.com/yourusername/yourrepo/blob/main/dbscan_script.R")
  })
  
  observeEvent(input$isi_btn, {
    # Redirect to ISI Cluster Analysis script hosted on GitHub
    browseURL("https://github.com/yourusername/yourrepo/blob/main/isi_script.R")
  })
  
  observeEvent(input$arrhythmia_btn, {
    # Redirect to Arrhythmia Analysis script hosted on GitHub
    browseURL("https://github.com/yourusername/yourrepo/blob/main/arrhythmia_script.R")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
