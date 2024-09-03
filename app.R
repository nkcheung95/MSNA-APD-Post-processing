library(shiny)
library(shinycssloaders)

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
  
  # Reactive value to control spinner visibility
  busy <- reactiveVal(FALSE)
  
  # Update spinner UI based on button presses
  output$spinner <- renderUI({
    if (busy()) {
      tagList(
        withSpinner(textOutput("loading_text"), type = 6)
      )
    }
  })
  
  observeEvent(input$dbscan_btn, {
    busy(TRUE)
    output$loading_text <- renderText({ "Running DBSCAN Analysis..." })
    # Simulate a script run with a delay
    later::later(function() {
      # Redirect to DBSCAN Analysis script
      source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/dbscan_script.R?raw=TRUE")
      busy(FALSE)
    }, delay = 2)  # Adjust delay as needed
  })
  
  observeEvent(input$isi_btn, {
    busy(TRUE)
    output$loading_text <- renderText({ "Running ISI Cluster Analysis..." })
    # Simulate a script run with a delay
    later::later(function() {
      # Redirect to ISI Cluster Analysis script
      source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/cluster_isi_script.R?raw=TRUE")
      busy(FALSE)
    }, delay = 2)  # Adjust delay as needed
  })
  
  observeEvent(input$arrhythmia_btn, {
    busy(TRUE)
    output$loading_text <- renderText({ "Running Arrhythmia Analysis..." })
    # Simulate a script run with a delay
    later::later(function() {
      # Redirect to Arrhythmia Analysis script
      source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/arrythmia_script.R?raw=TRUE")
      busy(FALSE)
    }, delay = 2)  # Adjust delay as needed
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
