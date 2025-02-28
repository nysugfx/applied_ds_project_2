# modules/data_loading.R
# Module for loading datasets from various sources

# UI Module
dataLoadingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Data Source",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        radioButtons(
          ns("data_source"),
          "Select data source:",
          choices = c("Upload File" = "file", "Use Built-in Dataset" = "builtin"),
          selected = "file",
          inline = TRUE
        )
      )
    ),
    
    # Conditional UI based on data source selection
    conditionalPanel(
      condition = sprintf("input['%s'] == 'file'", ns("data_source")),
      fluidRow(
        box(
          title = "Upload File",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          fileInput(
            ns("file_upload"),
            "Choose a file to upload:",
            multiple = FALSE,
            accept = c(
              ".csv", ".tsv", ".txt",
              ".xlsx", ".xls",
              ".json",
              ".rds"
            )
          ),
          uiOutput(ns("file_options"))
        )
      )
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'builtin'", ns("data_source")),
      fluidRow(
        box(
          title = "Built-in Datasets",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          selectInput(
            ns("builtin_dataset"),
            "Choose a dataset:",
            choices = sapply(built_in_datasets, function(x) x$name),
            selected = built_in_datasets$diamonds$name
          ),
          htmlOutput(ns("dataset_description"))
        )
      )
    ),
    
    # Preview data (shown for both upload and built-in)
    fluidRow(
      box(
        title = "Data Preview",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        withSpinner(
          DTOutput(ns("data_preview")),
          type = 4
        ),
        downloadButton(ns("download_current_data"), "Download Current Data")
      )
    ),
    
    # Data summary
    fluidRow(
      box(
        title = "Data Summary",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        verbatimTextOutput(ns("data_summary"))
      )
    )
  )
}

# Server Module
dataLoadingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store the dataset
    data_values <- reactiveValues(
      data = NULL,
      name = NULL,
      file_type = NULL
    )
    
    # Handle CSV import options based on file type
    output$file_options <- renderUI({
      ns <- session$ns
      if (is.null(input$file_upload)) return(NULL)
      
      file_ext <- tools::file_ext(input$file_upload$name)
      
      if (file_ext %in% c("csv", "tsv", "txt")) {
        tagList(
          checkboxInput(ns("header"), "First row is header", TRUE),
          radioButtons(
            ns("sep"),
            "Separator",
            choices = c(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t",
              "Space" = " "
            ),
            selected = ",",
            inline = TRUE
          ),
          radioButtons(
            ns("quote"),
            "Quote",
            choices = c(
              "Double quote" = '"',
              "Single quote" = "'",
              "None" = ""
            ),
            selected = '"',
            inline = TRUE
          ),
          radioButtons(
            ns("dec"),
            "Decimal",
            choices = c(
              "Period" = ".",
              "Comma" = ","
            ),
            selected = ".",
            inline = TRUE
          )
        )
      } else if (file_ext %in% c("xlsx", "xls")) {
        tagList(
          selectInput(
            ns("sheet"),
            "Select Sheet",
            choices = excel_sheets(input$file_upload$datapath)
          )
        )
      }
    })
    
    # Dataset description for built-in data
    output$dataset_description <- renderText({
      if (input$data_source == "builtin") {
        selected_name <- input$builtin_dataset
        selected_dataset <- names(which(sapply(built_in_datasets, function(x) x$name == selected_name)))
        
        if (length(selected_dataset) > 0) {
          return(paste("<div style='padding: 10px 0;'>", 
                 built_in_datasets[[selected_dataset]]$description, 
                 "</div>"))
        }
      }
      return("")
    })
    
    # Read the data based on user selection
    observe({
      # Clear data when source changes
      data_values$data <- NULL
      data_values$name <- NULL
      data_values$file_type <- NULL
      
      if (input$data_source == "file" && !is.null(input$file_upload)) {
        file <- input$file_upload
        ext <- tools::file_ext(file$name)
        
        tryCatch({
          if (ext %in% c("csv", "tsv", "txt")) {
            data_values$data <- read.csv(
              file$datapath,
              header = input$header,
              sep = input$sep,
              quote = input$quote,
              dec = input$dec
            )
            data_values$file_type <- "csv"
          } else if (ext %in% c("xls", "xlsx")) {
            if (!is.null(input$sheet)) {
              data_values$data <- read_excel(file$datapath, sheet = input$sheet)
              data_values$file_type <- "excel"
            }
          } else if (ext == "json") {
            data_values$data <- fromJSON(file$datapath)
            
            # Convert list to data frame if needed
            if (is.list(data_values$data) && !is.data.frame(data_values$data)) {
              data_values$data <- as.data.frame(data_values$data)
            }
            
            data_values$file_type <- "json"
          } else if (ext == "rds") {
            data_values$data <- readRDS(file$datapath)
            data_values$file_type <- "rds"
          }
          
          data_values$name <- file$name
        }, error = function(e) {
          showNotification(
            paste("Error loading file:", e$message),
            type = "error",
            duration = 10
          )
        })
      } else if (input$data_source == "builtin") {
        selected_name <- input$builtin_dataset
        selected_dataset <- names(which(sapply(built_in_datasets, function(x) x$name == selected_name)))
        
        if (length(selected_dataset) > 0) {
          tryCatch({
            data_values$data <- readRDS(built_in_datasets[[selected_dataset]]$path)
            data_values$name <- selected_name
            data_values$file_type <- "builtin"
          }, error = function(e) {
            showNotification(
              paste("Error loading built-in dataset:", e$message),
              type = "error",
              duration = 10
            )
          })
        }
      }
    })
    
    # Data preview
    output$data_preview <- renderDT({
      req(data_values$data)
      datatable(
        head(data_values$data, 100),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = FALSE
      )
    })
    
    # Data summary
    output$data_summary <- renderPrint({
      req(data_values$data)
      
      cat("Dataset:", data_values$name, "\n")
      cat("Observations:", nrow(data_values$data), "\n")
      cat("Variables:", ncol(data_values$data), "\n\n")
      
      # Show variable types and first few values
      summary_df <- data.frame(
        Variable = names(data_values$data),
        Type = sapply(data_values$data, class),
        Missing = sapply(data_values$data, function(x) sum(is.na(x))),
        "Missing %" = sapply(data_values$data, function(x) round(100 * sum(is.na(x)) / length(x), 2))
      )
      
      print(summary_df)
      
      # Show detailed summary
      cat("\nSummary Statistics:\n")
      print(summary(data_values$data))
    })
    
    # Download handler for current data
    output$download_current_data <- downloadHandler(
      filename = function() {
        if (!is.null(data_values$name)) {
          paste0("processed_", data_values$name, ".csv")
        } else {
          "processed_data.csv"
        }
      },
      content = function(file) {
        write.csv(data_values$data, file, row.names = FALSE)
      }
    )
    
    # Return reactive dataset for other modules to use
    return(reactive({
      req(data_values$data)
      data_values$data
    }))
  })
}
