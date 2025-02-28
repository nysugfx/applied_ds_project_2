# modules/data_cleaning.R
# Module for data cleaning and preprocessing

# UI Module
dataCleaningUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Select Variables",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("variable_selection")),
        hr(),
        actionButton(ns("apply_selection"), "Apply Selection", icon = icon("check"), 
                     class = "btn-primary"),
        actionButton(ns("reset_selection"), "Reset", icon = icon("refresh"), 
                     class = "btn-warning")
      )
    ),
    
    fluidRow(
      # Duplicate values
      box(
        title = "Duplicate Rows",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("duplicates_ui")),
        hr(),
        actionButton(ns("handle_duplicates"), "Remove Duplicates", icon = icon("trash-alt"),
                    class = "btn-primary")
      ),
      
      # Missing values
      box(
        title = "Missing Values",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        plotOutput(ns("missing_plot"), height = "200px"),
        hr(),
        uiOutput(ns("missing_values_ui"))
      )
    ),
    
    fluidRow(
      # Variable transformations
      box(
        title = "Data Transformations",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        
        # Tabs for different transformations
        tabsetPanel(
          id = ns("transform_tabs"),
          
          # Scaling tab
          tabPanel(
            "Scaling",
            br(),
            uiOutput(ns("scaling_ui")),
            actionButton(ns("apply_scaling"), "Apply Scaling", icon = icon("check"),
                        class = "btn-primary")
          ),
          
          # Encoding tab
          tabPanel(
            "Categorical Encoding",
            br(),
            uiOutput(ns("encoding_ui")),
            actionButton(ns("apply_encoding"), "Apply Encoding", icon = icon("check"),
                        class = "btn-primary")
          ),
          
          # Outlier tab
          tabPanel(
            "Outlier Handling",
            br(),
            uiOutput(ns("outlier_ui")),
            actionButton(ns("apply_outlier"), "Handle Outliers", icon = icon("check"),
                        class = "btn-primary")
          )
        )
      )
    ),
    
    fluidRow(
      # Results and comparison
      box(
        title = "Data Preview After Cleaning",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        withSpinner(
          DTOutput(ns("cleaned_data_preview")),
          type = 4
        ),
        hr(),
        downloadButton(ns("download_cleaned_data"), "Download Cleaned Data")
      )
    )
  )
}

# Server Module
dataCleaningServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store transformed data
    cleaned_data <- reactiveVal(NULL)
    original_data <- reactiveVal(NULL)
    selected_columns <- reactiveVal(NULL)
    
    # Initialize or update data when input dataset changes
    observe({
      req(dataset())
      original_data(dataset())
      cleaned_data(dataset())
      selected_columns(colnames(dataset()))
    })
    
    # Variable selection UI
    output$variable_selection <- renderUI({
      req(original_data())
      ns <- session$ns
      
      vars <- colnames(original_data())
      
      tagList(
        selectInput(
          ns("all_none"),
          "Quick select:",
          choices = c("Select All", "Select None"),
          selected = "Select All"
        ),
        checkboxGroupInput(
          ns("selected_vars"),
          "Choose variables to keep:",
          choices = vars,
          selected = if(input$all_none == "Select All" || is.null(input$all_none)) vars else NULL,
          inline = TRUE
        )
      )
    })
    
    # Update selections based on all/none dropdown
    observeEvent(input$all_none, {
      req(original_data())
      vars <- colnames(original_data())
      
      if (input$all_none == "Select All") {
        updateCheckboxGroupInput(session, "selected_vars", selected = vars)
      } else {
        updateCheckboxGroupInput(session, "selected_vars", selected = character(0))
      }
    })
    
    # Apply column selection
    observeEvent(input$apply_selection, {
      req(input$selected_vars, cleaned_data())
      
      if (length(input$selected_vars) > 0) {
        # Keep only selected columns
        new_data <- cleaned_data()[, input$selected_vars, drop = FALSE]
        cleaned_data(new_data)
        selected_columns(input$selected_vars)
        
        showNotification("Variable selection applied", type = "message")
      } else {
        showNotification("Please select at least one variable", type = "warning")
      }
    })
    
    # Reset selection
    observeEvent(input$reset_selection, {
      req(original_data())
      cleaned_data(original_data())
      selected_columns(colnames(original_data()))
      
      # Reset UI selections
      vars <- colnames(original_data())
      updateSelectInput(session, "all_none", selected = "Select All")
      updateCheckboxGroupInput(session, "selected_vars", selected = vars)
      
      showNotification("Data has been reset to original", type = "message")
    })
    
    # Duplicate rows UI
    output$duplicates_ui <- renderUI({
      req(cleaned_data())
      ns <- session$ns
      
      dup_count <- sum(duplicated(cleaned_data()))
      
      tagList(
        p(paste("Number of duplicate rows:", dup_count)),
        if (dup_count > 0) {
          numericInput(
            ns("duplicates_preview"),
            "Number of duplicates to preview:",
            min = 0,
            max = min(10, dup_count),
            value = min(5, dup_count)
          )
        }
      )
    })
    
    # Handle duplicates
    observeEvent(input$handle_duplicates, {
      req(cleaned_data())
      
      current_data <- cleaned_data()
      dup_count <- sum(duplicated(current_data))
      
      if (dup_count > 0) {
        new_data <- unique(current_data)
        cleaned_data(new_data)
        
        showNotification(
          paste("Removed", dup_count, "duplicate rows"),
          type = "message"
        )
      } else {
        showNotification("No duplicate rows found", type = "message")
      }
    })
    
    # Missing values plot
    output$missing_plot <- renderPlot({
      req(cleaned_data())
      
      # Calculate missing values by column
      df <- cleaned_data()
      missing_counts <- colSums(is.na(df))
      missing_pct <- round(100 * missing_counts / nrow(df), 1)
      
      missing_df <- data.frame(
        Variable = names(missing_counts),
        Count = missing_counts,
        Percentage = missing_pct
      )
      
      # Only show variables with missing values
      missing_df <- missing_df[missing_df$Count > 0, ]
      
      if (nrow(missing_df) > 0) {
        # Plot missing values
        ggplot(missing_df, aes(x = reorder(Variable, -Percentage), y = Percentage)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 3) +
          theme_minimal() +
          labs(x = "", y = "% Missing") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        # No missing values
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No missing values in the dataset") +
          theme_void() +
          xlim(0, 1) +
          ylim(0, 1)
      }
    })
    
    # Missing values UI
    output$missing_values_ui <- renderUI({
      req(cleaned_data())
      ns <- session$ns
      
      df <- cleaned_data()
      missing_counts <- colSums(is.na(df))
      vars_with_missing <- names(missing_counts[missing_counts > 0])
      
      if (length(vars_with_missing) == 0) {
        return(p("No missing values found in the dataset."))
      }
      
      tagList(
        selectInput(
          ns("missing_vars"),
          "Select variables with missing values:",
          choices = vars_with_missing,
          selected = vars_with_missing[1],
          multiple = TRUE
        ),
        
        radioButtons(
          ns("missing_strategy"),
          "Choose strategy:",
          choices = c(
            "Remove rows with missing values" = "remove_rows",
            "Fill with mean/mode" = "fill_mean_mode",
            "Fill with median" = "fill_median",
            "Fill with custom value" = "fill_custom"
          ),
          selected = "remove_rows"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'fill_custom'", ns("missing_strategy")),
          textInput(ns("custom_value"), "Enter custom value:")
        ),
        
        actionButton(
          ns("apply_missing"),
          "Handle Missing Values",
          icon = icon("check"),
          class = "btn-primary"
        )
      )
    })
    
    # Handle missing values
    observeEvent(input$apply_missing, {
      req(cleaned_data(), input$missing_vars, input$missing_strategy)
      
      current_data <- cleaned_data()
      strategy <- input$missing_strategy
      vars <- input$missing_vars
      
      if (length(vars) == 0) {
        showNotification("No variables selected", type = "warning")
        return()
      }
      
      # Apply strategy for each selected variable
      for (var in vars) {
        if (strategy == "remove_rows") {
          # Remove rows with missing values in selected variables
          current_data <- current_data[!is.na(current_data[[var]]), ]
        } else if (strategy == "fill_mean_mode") {
          # Fill with mean (numeric) or mode (categorical)
          if (is.numeric(current_data[[var]])) {
            # For numeric, use mean
            mean_val <- mean(current_data[[var]], na.rm = TRUE)
            current_data[[var]][is.na(current_data[[var]])] <- mean_val
          } else {
            # For categorical, use mode (most frequent value)
            mode_val <- names(sort(table(current_data[[var]]), decreasing = TRUE))[1]
            current_data[[var]][is.na(current_data[[var]])] <- mode_val
          }
        } else if (strategy == "fill_median") {
          # Fill with median (numeric only)
          if (is.numeric(current_data[[var]])) {
            median_val <- median(current_data[[var]], na.rm = TRUE)
            current_data[[var]][is.na(current_data[[var]])] <- median_val
          } else {
            # For non-numeric, just use mode as fallback
            mode_val <- names(sort(table(current_data[[var]]), decreasing = TRUE))[1]
            current_data[[var]][is.na(current_data[[var]])] <- mode_val
          }
        } else if (strategy == "fill_custom") {
          # Fill with custom value
          custom_val <- input$custom_value
          
          # Convert to appropriate type
          if (is.numeric(current_data[[var]])) {
            custom_val <- as.numeric(custom_val)
          } else if (is.factor(current_data[[var]])) {
            custom_val <- as.factor(custom_val)
          }
          
          current_data[[var]][is.na(current_data[[var]])] <- custom_val
        }
      }
      
      cleaned_data(current_data)
      
      showNotification(
        paste("Missing values handled using", 
              switch(strategy,
                     "remove_rows" = "row removal",
                     "fill_mean_mode" = "mean/mode imputation",
                     "fill_median" = "median imputation",
                     "fill_custom" = "custom value")),
        type = "message"
      )
    })
    
    # Scaling UI
    output$scaling_ui <- renderUI({
      req(cleaned_data())
      ns <- session$ns
      
      # Get numeric columns
      df <- cleaned_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if (length(numeric_cols) == 0) {
        return(p("No numeric variables available for scaling."))
      }
      
      tagList(
        selectInput(
          ns("scaling_vars"),
          "Select numeric variables to scale:",
          choices = numeric_cols,
          selected = numeric_cols,
          multiple = TRUE
        ),
        
        radioButtons(
          ns("scaling_method"),
          "Scaling method:",
          choices = c(
            "Min-Max Scaling (0-1)" = "minmax",
            "Standardization (Z-score)" = "zscore",
            "Robust Scaling (using median/IQR)" = "robust"
          ),
          selected = "minmax"
        )
      )
    })
    
    # Apply scaling
    observeEvent(input$apply_scaling, {
      req(cleaned_data(), input$scaling_vars, input$scaling_method)
      
      current_data <- cleaned_data()
      vars <- input$scaling_vars
      method <- input$scaling_method
      
      if (length(vars) == 0) {
        showNotification("No variables selected for scaling", type = "warning")
        return()
      }
      
      # Apply scaling method
      for (var in vars) {
        if (method == "minmax") {
          # Min-Max scaling
          min_val <- min(current_data[[var]], na.rm = TRUE)
          max_val <- max(current_data[[var]], na.rm = TRUE)
          
          if (max_val > min_val) {
            current_data[[var]] <- (current_data[[var]] - min_val) / (max_val - min_val)
          }
        } else if (method == "zscore") {
          # Z-score standardization
          mean_val <- mean(current_data[[var]], na.rm = TRUE)
          sd_val <- sd(current_data[[var]], na.rm = TRUE)
          
          if (sd_val > 0) {
            current_data[[var]] <- (current_data[[var]] - mean_val) / sd_val
          }
        } else if (method == "robust") {
          # Robust scaling using median and IQR
          median_val <- median(current_data[[var]], na.rm = TRUE)
          iqr_val <- IQR(current_data[[var]], na.rm = TRUE)
          
          if (iqr_val > 0) {
            current_data[[var]] <- (current_data[[var]] - median_val) / iqr_val
          }
        }
      }
      
      cleaned_data(current_data)
      
      showNotification(
        paste("Applied", 
              switch(method,
                     "minmax" = "min-max scaling",
                     "zscore" = "z-score standardization",
                     "robust" = "robust scaling"),
              "to", length(vars), "variables"),
        type = "message"
      )
    })
    
    # Encoding UI
    output$encoding_ui <- renderUI({
      req(cleaned_data())
      ns <- session$ns
      
      # Get categorical columns
      df <- cleaned_data()
      categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      
      if (length(categorical_cols) == 0) {
        return(p("No categorical variables available for encoding."))
      }
      
      tagList(
        selectInput(
          ns("encoding_vars"),
          "Select categorical variables to encode:",
          choices = categorical_cols,
          selected = categorical_cols[1],
          multiple = TRUE
        ),
        
        radioButtons(
          ns("encoding_method"),
          "Encoding method:",
          choices = c(
            "One-Hot Encoding" = "onehot",
            "Label Encoding" = "label"
          ),
          selected = "onehot"
        )
      )
    })
    
    # Apply encoding
    observeEvent(input$apply_encoding, {
      req(cleaned_data(), input$encoding_vars, input$encoding_method)
      
      current_data <- cleaned_data()
      vars <- input$encoding_vars
      method <- input$encoding_method
      
      if (length(vars) == 0) {
        showNotification("No variables selected for encoding", type = "warning")
        return()
      }
      
      # Apply encoding method
      if (method == "onehot") {
        # One-hot encoding
        for (var in vars) {
          # Skip if already numeric
          if (is.numeric(current_data[[var]])) next
          
          # Create dummy variables
          dummies <- model.matrix(~ 0 + get(var), data = current_data)
          colnames(dummies) <- paste0(var, "_", gsub("get\\(var\\)", "", colnames(dummies)))
          
          # Add dummy columns to dataframe
          current_data <- cbind(current_data, as.data.frame(dummies))
          
          # Remove original column
          current_data[[var]] <- NULL
        }
      } else if (method == "label") {
        # Label encoding
        for (var in vars) {
          # Skip if already numeric
          if (is.numeric(current_data[[var]])) next
          
          # Convert to factor and then to numeric
          current_data[[var]] <- as.numeric(as.factor(current_data[[var]])) - 1
        }
      }
      
      cleaned_data(current_data)
      
      showNotification(
        paste("Applied", 
              switch(method,
                     "onehot" = "one-hot encoding",
                     "label" = "label encoding"),
              "to", length(vars), "variables"),
        type = "message"
      )
    })
    
    # Outlier UI
    output$outlier_ui <- renderUI({
      req(cleaned_data())
      ns <- session$ns
      
      # Get numeric columns
      df <- cleaned_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if (length(numeric_cols) == 0) {
        return(p("No numeric variables available for outlier handling."))
      }
      
      tagList(
        selectInput(
          ns("outlier_vars"),
          "Select numeric variables to check for outliers:",
          choices = numeric_cols,
          selected = numeric_cols[1],
          multiple = TRUE
        ),
        
        radioButtons(
          ns("outlier_method"),
          "Outlier detection method:",
          choices = c(
            "IQR method (1.5 * IQR)" = "iqr",
            "Z-score method (|z| > 3)" = "zscore"
          ),
          selected = "iqr"
        ),
        
        radioButtons(
          ns("outlier_action"),
          "Action for outliers:",
          choices = c(
            "Cap outliers (winsorize)" = "cap",
            "Remove outlier rows" = "remove",
            "Replace with NA" = "na"
          ),
          selected = "cap"
        )
      )
    })
    
    # Apply outlier handling
    observeEvent(input$apply_outlier, {
      req(cleaned_data(), input$outlier_vars, input$outlier_method, input$outlier_action)
      
      current_data <- cleaned_data()
      vars <- input$outlier_vars
      method <- input$outlier_method
      action <- input$outlier_action
      
      if (length(vars) == 0) {
        showNotification("No variables selected for outlier handling", type = "warning")
        return()
      }
      
      # Count of detected outliers
      total_outliers <- 0
      affected_rows <- c()
      
      # Apply outlier detection and handling for each variable
      for (var in vars) {
        x <- current_data[[var]]
        outlier_indices <- c()
        
        # Detect outliers
        if (method == "iqr") {
          # IQR method
          q1 <- quantile(x, 0.25, na.rm = TRUE)
          q3 <- quantile(x, 0.75, na.rm = TRUE)
          iqr <- q3 - q1
          lower_bound <- q1 - 1.5 * iqr
          upper_bound <- q3 + 1.5 * iqr
          
          outlier_indices <- which(x < lower_bound | x > upper_bound)
        } else if (method == "zscore") {
          # Z-score method
          mean_val <- mean(x, na.rm = TRUE)
          sd_val <- sd(x, na.rm = TRUE)
          
          if (sd_val > 0) {
            z_scores <- abs((x - mean_val) / sd_val)
            outlier_indices <- which(z_scores > 3)
          }
        }
        
        total_outliers <- total_outliers + length(outlier_indices)
        affected_rows <- union(affected_rows, outlier_indices)
        
        # Handle outliers
        if (length(outlier_indices) > 0) {
          if (action == "cap") {
            # Winsorize (cap) outliers
            if (method == "iqr") {
              current_data[[var]][x < lower_bound] <- lower_bound
              current_data[[var]][x > upper_bound] <- upper_bound
            } else {
              # For z-score method
              mean_val <- mean(x, na.rm = TRUE)
              sd_val <- sd(x, na.rm = TRUE)
              lower_bound <- mean_val - 3 * sd_val
              upper_bound <- mean_val + 3 * sd_val
              
              current_data[[var]][x < lower_bound] <- lower_bound
              current_data[[var]][x > upper_bound] <- upper_bound
            }
          } else if (action == "na") {
            # Replace with NA
            current_data[[var]][outlier_indices] <- NA
          }
        }
      }
      
      # If action is remove, remove all rows with outliers
      if (action == "remove" && length(affected_rows) > 0) {
        current_data <- current_data[-affected_rows, ]
      }
      
      cleaned_data(current_data)
      
      showNotification(
        paste("Detected", total_outliers, "outliers in", length(affected_rows), "rows. Applied",
              switch(action,
                     "cap" = "capping (winsorizing)",
                     "remove" = "row removal",
                     "na" = "NA replacement")),
        type = "message"
      )
    })
    
    # Cleaned data preview
    output$cleaned_data_preview <- renderDT({
      req(cleaned_data())
      
      datatable(
        head(cleaned_data(), 100),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = FALSE
      )
    })
    
    # Download handler for cleaned data
    output$download_cleaned_data <- downloadHandler(
      filename = function() {
        "cleaned_data.csv"
      },
      content = function(file) {
        write.csv(cleaned_data(), file, row.names = FALSE)
      }
    )
    
    # Return cleaned data for other modules
    return(reactive({
      req(cleaned_data())
      cleaned_data()
    }))
  })
}
