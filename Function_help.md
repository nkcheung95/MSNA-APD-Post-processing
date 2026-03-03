
---

# Manual: Refactoring APD Analysis for Shiny

**Goal:** Transition from linear sourcing to a functional, modular architecture.

---

## 1. How to Create a Robust Function

A good function is a "black box": data goes in, logic happens, and a result comes out. It should not rely on any variables existing in your environment that aren't passed through the arguments.

### The Anatomy of an Analysis Function

```r
# Example: Refactoring a generic analysis script
run_analysis_task <- function(data_input, param_1 = 0.5) {
  
  # 1. Validation (Optional but recommended)
  if (!is.data.frame(data_input)) stop("Input must be a dataframe")
  
  # 2. Logic (The core of your old script)
  # Replace global names with 'data_input'
  processed_data <- data_input %>%
    filter(value > param_1) %>%
    mutate(status = "Analyzed")
  
  # 3. Return
  # Explicitly return the object you want to use in Shiny
  return(processed_data)
}

```

### Top 3 Conversion Rules:

1. **Input Everything:** If your script uses a file path or a threshold, make it an argument (e.g., `function(path, threshold)`).
2. **No `setwd()` inside functions:** Functions should be "location agnostic." Pass the path to the function instead of moving the R session to the path.
3. **The Return Rule:** A function can only return **one** object. If you need to return a plot AND a table, wrap them in a list: `return(list(my_plot = p1, my_table = df1))`.

---

## 2. Organizing Your GitHub Repository

To make your Shiny app call these functions efficiently, structure your files like this:

* **`functions/`** (Folder)
* `func_file_loader.R`
* `func_dbscan.R`
* `func_isi_cluster.R`


* **`app.R`** (Main Shiny file)

---

## 3. The New Shiny Calling Logic

Instead of `source()` inside the `observeEvent`, you will source all your logic **once** at the start. This makes the app much faster and prevents "URL-source lag" during analysis.

### The `app.R` Template

```r
library(shiny)
# ... other libraries ...

# --- 1. GLOBAL SCOPE ---
# Source all functions from GitHub at launch
# Note: Use a helper to source everything if you have many files
source("https://raw.githubusercontent.com/nkcheung95/MSNA-APD-Post-processing/main/functions/func_dbscan.R")
source("https://raw.githubusercontent.com/nkcheung95/MSNA-APD-Post-processing/main/functions/func_isi.R")

ui <- fluidPage(
  # ... your existing UI ...
)

server <- function(input, output, session) {
  
  # --- 2. DATA REACTIVITY ---
  # Use reactiveVal to hold data between steps
  raw_data <- reactiveVal(NULL)
  results  <- reactiveVal(NULL)

  # --- 3. THE CALLING LOGIC ---
  observeEvent(input$dbscan_btn, {
    req(raw_data()) # Ensure data is loaded first
    
    busy(TRUE)
    status("Running Analysis...")
    
    # NEW: Call the function instead of sourcing a script
    tryCatch({
      # We pass the reactive data into the function
      output_obj <- run_dbscan_analysis(raw_data(), eps = 0.5)
      
      # Store the result in a reactive variable for plotting/saving
      results(output_obj)
      status("DBSCAN Completed!")
    }, error = function(e) {
      status(paste("Error:", e$message))
    })
    
    busy(FALSE)
  })
}

```

---

## 4. Why This is Better for You

| Feature | Old Way (Scripting) | New Way (Functions) |
| --- | --- | --- |
| **Speed** | Downloads from GitHub on every click. | Downloads once at startup. |
| **Debugging** | Hard to find which script crashed. | Errors are localized to specific functions. |
| **Flexibility** | Logic is "hard-coded." | You can easily add UI sliders to change function arguments. |
| **Memory** | Global environment gets cluttered/messy. | Memory is cleared automatically after the function runs. |

---

## 5. Summary Checklist for your GitHub Files

For every script in your repo (DBSCAN, ISI, Arrhythmia):

1. Wrap the whole thing in `my_function_name <- function(data, ...) { ... }`.
2. Remove any hard-coded file paths like `"C:/Users/..."`.
3. Ensure the last line is `return(your_final_object)`.
4. Update the Shiny `app.R` to `source()` these files at the top.

**Would you like me to take your `dbscan_script.R` from the repo link and provide the exact refactored function code for it as a first template?**