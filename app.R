library(shiny)
library(shinycssloaders)
library(later)

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
  
  # Spinner and status message below buttons
  fluidRow(
    column(12, align = "center",
           uiOutput("spinner"),
           uiOutput("status_message")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to control spinner visibility and status messages
  busy <- reactiveVal(FALSE)
  status <- reactiveVal(NULL)
  
  # Update spinner UI based on button presses
  output$spinner <- renderUI({
    if (busy()) {
      tagList(
        withSpinner(textOutput("loading_text"), type = 6)
      )
    }
  })
  
  # Update status message
  output$status_message <- renderUI({
    if (!is.null(status())) {
      tagList(
        h3(status(), style = "color: green;")
      )
    }
  })
  
  observeEvent(input$dbscan_btn, {
    busy(TRUE)
    status(NULL)
    output$loading_text <- renderText({ "Running DBSCAN Analysis..." })
    
    # Simulate a script run with a delay
    later::later(function() {
      # Source the DBSCAN Analysis script
      source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/dbscan_script.R?raw=TRUE")
      busy(FALSE)
      status("DBSCAN Analysis Completed!")
    }, delay = 2)  # Adjust delay as needed
  })
  
  observeEvent(input$isi_btn, {
    busy(TRUE)
    status(NULL)
    output$loading_text <- renderText({ "Running ISI Cluster Analysis..." })
    
    # Simulate a script run with a delay
    later::later(function() {
      # Source the ISI Cluster Analysis script
      source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/cluster_isi_script.R?raw=TRUE")
      busy(FALSE)
      status("ISI Cluster Analysis Completed!")
    }, delay = 2)  # Adjust delay as needed
  })
  
  observeEvent(input$arrhythmia_btn, {
    busy(TRUE)
    status(NULL)
    output$loading_text <- renderText({ "Running Arrhythmia Analysis..." })
    
    # Simulate a script run with a delay
    later::later(function() {
      # Source the Arrhythmia Analysis script
      source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/arrythmia_script.R?raw=TRUE")
      busy(FALSE)
      status("Arrhythmia Analysis Completed!")
    }, delay = 2)  # Adjust delay as needed
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
