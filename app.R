library(shiny)
library(shinycssloaders)
library(later)
library(tcltk)

# Define UI
ui <- fluidPage(
  titlePanel("APD Analysis Launcher"),
  
  # Centered button layout
  fluidRow(
    column(12, align = "center",
           actionButton("setwd_btn", "Set Working Directory", width = '300px', style = "font-size: 20px; margin: 20px;"),
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
  
  observeEvent(input$setwd_btn, {
    busy(TRUE)
    status(NULL)
    output$loading_text <- renderText({ "Selecting Directory and Checking Permissions..." })

    # Open directory selection dialog
    dir_path <- tryCatch({
      tk_choose.dir()
    }, error = function(e) {
      status(paste("Error selecting directory:", e$message))
      busy(FALSE)
      return(NULL)
    })

    if (is.null(dir_path) || dir_path == "") {
      status("No directory selected.")
      busy(FALSE)
      return()
    }

    # Create directory if it doesn't exist
    if (!dir.exists(dir_path)) {
      tryCatch({
        dir.create(dir_path, recursive = TRUE)
      }, error = function(e) {
        status(paste("Error creating directory:", e$message))
        busy(FALSE)
        return()
      })
    }

    # Try to set the working directory
    tryCatch({
      setwd(dir_path)
      can_write <- file.access(dir_path, 2) == 0
      if (can_write) {
        status(paste("Working directory set to:", dir_path, "and permissions are OK."))
      } else {
        status(paste("Working directory set to:", dir_path, "but permissions are not sufficient."))
      }
    }, error = function(e) {
      status(paste("Error setting working directory:", e$message))
    })

    busy(FALSE)
  })
  
  observeEvent(input$dbscan_btn, {
    busy(TRUE)
    status(NULL)
    output$loading_text <- renderText({ "Running DBSCAN Analysis..." })
    
    # Simulate a script run with a delay
    later::later(function() {
      # Try to source the DBSCAN Analysis script
      tryCatch({
        source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/dbscan_script.R?raw=TRUE")
        status("DBSCAN Analysis Completed!")
      }, error = function(e) {
        status(paste("Error during DBSCAN Analysis:", e$message))
      })
      busy(FALSE)
    }, delay = 2)  # Adjust delay as needed
  })
  
  observeEvent(input$isi_btn, {
    busy(TRUE)
    status(NULL)
    output$loading_text <- renderText({ "Running ISI Cluster Analysis..." })
    
    # Simulate a script run with a delay
    later::later(function() {
      # Try to source the ISI Cluster Analysis script
      tryCatch({
        source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/cluster_isi_script.R?raw=TRUE")
        status("ISI Cluster Analysis Completed!")
      }, error = function(e) {
        status(paste("Error during ISI Cluster Analysis:", e$message))
      })
      busy(FALSE)
    }, delay = 2)  # Adjust delay as needed
  })
  
  observeEvent(input$arrhythmia_btn, {
    busy(TRUE)
    status(NULL)
    output$loading_text <- renderText({ "Running Arrhythmia Analysis..." })
    
    # Simulate a script run with a delay
    later::later(function() {
      # Try to source the Arrhythmia Analysis script
      tryCatch({
        source("https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/arrythmia_script.R?raw=TRUE")
        status("Arrhythmia Analysis Completed!")
      }, error = function(e) {
        status(paste("Error during Arrhythmia Analysis:", e$message))
      })
      busy(FALSE)
    }, delay = 2)  # Adjust delay as needed
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
