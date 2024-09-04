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
    tt <- tktoplevel()  # Create a temporary Tk window
    tkwm.title(tt, "Select Directory")  # Set a title (optional)
    tkraise(tt)  # Raise the Tk window to the front
    tcl("wm", "attributes", tt, "-topmost", TRUE)  # Ensure it stays on top
    dir_path <- tk_choose.dir(parent = tt)  # Open the directory chooser with the Tk window as the parent
    tkdestroy(tt)  # Destroy the temporary window

    if (is.null(dir_path) || dir_path == "") {
        status("No directory selected.")
        busy(FALSE)
        return()
    }

    # Create directory if it doesn't exist
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
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
        status(paste("Error:", e$message))
    })

    busy(FALSE)
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
