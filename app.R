# Define UI
ui <- fluidPage(
  titlePanel("APD Analysis Launcher"),
  
  # Centered button layout
  fluidRow(
    column(12, align = "center",
           actionButton("dbscan_btn", "Run DBSCAN Analysis", width = '300px', style = "font-size: 20px; margin: 20px;"),
           actionButton("isi_btn", "Run ISI Cluster Analysis", width = '300px', style = "font-size: 20px; margin: 20px;"),
           actionButton("arrhythmia_btn", "Run Arrhythmia Analysis", width = '300px', style = "font-size: 20px; margin: 20px;")
    )
  ),
  
  # Spinner below buttons
  fluidRow(
    column(12, align = "center",
      uiOutput("spinner")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$spinner <- renderUI({
    if (input$dbscan_btn > 0 || input$isi_btn > 0 || input$arrhythmia_btn > 0) {
      withSpinner(textOutput("loading_text"), type = 6)
    }
  })
  
  observeEvent(input$dbscan_btn, {
    output$loading_text <- renderText({ "Running DBSCAN Analysis..." })
    # Simulate a script run with a delay
    Sys.sleep(5)
    # Redirect to DBSCAN Analysis script
    source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/dbscan_script.R?raw=TRUE")
  })
  
  observeEvent(input$isi_btn, {
    output$loading_text <- renderText({ "Running ISI Cluster Analysis..." })
    # Simulate a script run with a delay
    Sys.sleep(5)
    # Redirect to ISI Cluster Analysis script
    source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/cluster_isi_script.R?raw=TRUE")
  })
  
  observeEvent(input$arrhythmia_btn, {
    output$loading_text <- renderText({ "Running Arrhythmia Analysis..." })
    # Simulate a script run with a delay
    Sys.sleep(5)
    # Redirect to Arrhythmia Analysis script
    source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/arrythmia_script.R?raw=TRUE")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
