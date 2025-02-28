# modules/feature_eng.R
# Module for feature engineering

# UI Module
featureEngUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Feature Engineering",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        
        tabsetPanel(
          id = ns("feature_tabs"),
          
          # Create new features tab
          tabPanel(
            "Create New Features",
            br(),
            uiOutput(ns("new_feature_ui")),
            actionButton(ns("create_feature"), "Create Feature", icon = icon("plus"),
                        class = "btn-primary")
          ),
          
          # Polynomial features tab
          tabPanel(
            "Polynomial Features",
            br(),
            uiOutput(ns("polynomial_ui")),
            actionButton(ns("create_polynomial"), "Create Polynomial Features", icon = icon("superscript"),
                        class = "btn-primary")
          ),
          
          # Interaction terms tab
          tabPanel(
            "Interaction Terms",
            br(),
            uiOutput(ns("interaction_ui")),
            actionButton(ns("create_interaction"), "Create Interaction", icon = icon("project-diagram"),
                        class = "btn-primary")
          ),
          
          # Binning tab
          tabPanel(
            "Binning",
            br(),
            uiOutput(ns("binning_ui")),
            actionButton(ns("apply_binning"), "Apply Binning", icon = icon("th-large"),
                        class = "btn-primary")
          ),
          
          # Date features tab
          tabPanel(
            "Date Features",
            br(),
            uiOutput(ns("date_ui")),
            actionButton(ns("extract_date"), "Extract Date Features", icon = icon("calendar"),
                        class = "btn-primary")
          ),
          
          # Text features tab
          tabPanel(
            "Text Features",
            br(),
            uiOutput(ns("text_ui")),
            actionButton(ns("extract_text"), "Extract Text Features", icon = icon("font"),
                        class = "btn-primary")
          )
        )
      )
    ),
    
    fluidRow(
      # Feature impact visualization
      box(
        title = "Feature Impact",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        plotOutput(ns("feature_impact_plot"), height = "300px")
      )
    ),
    
    fluidRow(
      # Data preview
      box(
        title = "Data Preview with Engineered Features",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        withSpinner(
          DTOutput(ns("engineered_data_preview")),
          type = 4
        ),
        hr(),
        downloadButton(ns("download_engineered_data"), "Download Data with Engineered Features")
      )
    )
  )
}

# Server Module
featureEngServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store engineered data
    engineered_data <- reactiveVal(NULL)
    
    # Initialize data when input dataset changes
    observe({
      req(dataset())
      engineered_data(dataset())
    })
    
    # Create new feature UI
    output$new_feature_ui <- renderUI({
      req(engineered_data())
      ns <- session$ns
      
      # Get numeric columns
      df <- engineered_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if (length(numeric_cols) == 0) {
        return(p("No numeric variables available for feature creation."))
      }
      
      tagList(
        textInput(
          ns("new_feature_name"),
          "New feature name:",
          value = "new_feature"
        ),
        
        selectInput(
          ns("operation_type"),
          "Operation type:",
          choices = c(
            "Arithmetic" = "arithmetic",
            "Math Function" = "math_function",
            "Custom Formula" = "custom"
          ),
          selected = "arithmetic"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'arithmetic'", ns("operation_type")),
          selectInput(
            ns("var1"),
            "First variable:",
            choices = numeric_cols,
            selected = numeric_cols[1]
          ),
          
          selectInput(
            ns("arithmetic_op"),
            "Operation:",
            choices = c(
              "Addition (+)" = "+",
              "Subtraction (-)" = "-",
              "Multiplication (*)" = "*",
              "Division (/)" = "/",
              "Power (^)" = "^",
              "Modulo (%%)" = "%%"
            ),
            selected = "+"
          ),
          
          selectInput(
            ns("var2"),
            "Second variable:",
            choices = c(numeric_cols, "Custom value"),
            selected = if (length(numeric_cols) > 1) numeric_cols[2] else numeric_cols[1]
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Custom value'", ns("var2")),
            numericInput(
              ns("custom_value"),
              "Enter custom value:",
              value = 1
            )
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'math_function'", ns("operation_type")),
          selectInput(
            ns("math_var"),
            "Select variable:",
            choices = numeric_cols,
            selected = numeric_cols[1]
          ),
          
          selectInput(
            ns("math_function"),
            "Math function:",
            choices = c(
              "Square" = "square",
              "Square Root" = "sqrt",
              "Log" = "log",
              "Log10" = "log10",
              "Exponential" = "exp",
              "Absolute" = "abs",
              "Sin" = "sin",
              "Cos" = "cos",
              "Tan" = "tan"
            ),
            selected = "square"
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("operation_type")),
          textAreaInput(
            ns("custom_formula"),
            "Enter R formula: (use column names as variables)",
            value = "col1 + col2 * 2",
            height = "100px"
          ),
          p("Example: log(price) + sqrt(carat * 2)")
        )
      )
    })
    
    # Create feature
    observeEvent(input$create_feature, {
      req(engineered_data(), input$new_feature_name)
      
      current_data <- engineered_data()
      new_feature_name <- input$new_feature_name
      
      # Check if name already exists
      if (new_feature_name %in% names(current_data)) {
        showNotification("Feature name already exists. Please choose a different name.", type = "warning")
        return()
      }
      
      # Create new feature based on operation type
      if (input$operation_type == "arithmetic") {
        req(input$var1, input$arithmetic_op)
        
        var1 <- current_data[[input$var1]]
        
        if (input$var2 == "Custom value") {
          var2 <- input$custom_value
        } else {
          var2 <- current_data[[input$var2]]
        }
        
        # Perform arithmetic operation
        result <- switch(input$arithmetic_op,
                         "+" = var1 + var2,
                         "-" = var1 - var2,
                         "*" = var1 * var2,
                         "/" = var1 / var2,
                         "^" = var1 ^ var2,
                         "%%" = var1 %% var2)
        
        current_data[[new_feature_name]] <- result
        
      } else if (input$operation_type == "math_function") {
        req(input$math_var, input$math_function)
        
        var <- current_data[[input$math_var]]
        
        # Apply math function
        result <- switch(input$math_function,
                         "square" = var ^ 2,
                         "sqrt" = sqrt(var),
                         "log" = log(var),
                         "log10" = log10(var),
                         "exp" = exp(var),
                         "abs" = abs(var),
                         "sin" = sin(var),
                         "cos" = cos(var),
                         "tan" = tan(var))
        
        current_data[[new_feature_name]] <- result
        
      } else if (input$operation_type == "custom") {
        req(input$custom_formula)
        
        # Create formula environment
        formula_env <- new.env()
        
        # Add all columns to environment
        for (col in names(current_data)) {
          formula_env[[col]] <- current_data[[col]]
        }
        
        # Try to evaluate formula
        tryCatch({
          result <- eval(parse(text = input$custom_formula), envir = formula_env)
          current_data[[new_feature_name]] <- result
        }, error = function(e) {
          showNotification(paste("Error in formula:", e$message), type = "error")
        })
      }
      
      engineered_data(current_data)
      
      showNotification(paste("Created new feature:", new_feature_name), type = "message")
    })
    
    # Polynomial features UI
    output$polynomial_ui <- renderUI({
      req(engineered_data())
      ns <- session$ns
      
      # Get numeric columns
      df <- engineered_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if (length(numeric_cols) == 0) {
        return(p("No numeric variables available for polynomial features."))
      }
      
      tagList(
        selectInput(
          ns("poly_vars"),
          "Select numeric variables:",
          choices = numeric_cols,
          selected = numeric_cols[1],
          multiple = TRUE
        ),
        
        sliderInput(
          ns("poly_degree"),
          "Polynomial degree:",
          min = 2,
          max = 5,
          value = 2,
          step = 1
        ),
        
        checkboxInput(
          ns("poly_include_original"),
          "Keep original features",
          value = TRUE
        )
      )
    })
    
    # Create polynomial features
    observeEvent(input$create_polynomial, {
      req(engineered_data(), input$poly_vars, input$poly_degree)
      
      current_data <- engineered_data()
      vars <- input$poly_vars
      degree <- input$poly_degree
      
      if (length(vars) == 0) {
        showNotification("No variables selected for polynomial features", type = "warning")
        return()
      }
      
      # Create polynomial features for each variable
      features_created <- 0
      
      for (var in vars) {
        for (d in 2:degree) {
          # Create feature name
          feat_name <- paste0(var, "_degree_", d)
          
          # Check if name already exists
          if (feat_name %in% names(current_data)) {
            # Append a suffix to make it unique
            i <- 1
            while (paste0(feat_name, "_", i) %in% names(current_data)) {
              i <- i + 1
            }
            feat_name <- paste0(feat_name, "_", i)
          }
          
          # Create polynomial feature
          current_data[[feat_name]] <- current_data[[var]] ^ d
          features_created <- features_created + 1
        }
      }
      
      # Remove original features if not needed
      if (!input$poly_include_original) {
        current_data <- current_data[, !(names(current_data) %in% vars), drop = FALSE]
      }
      
      engineered_data(current_data)
      
      showNotification(paste("Created", features_created, "polynomial features"), type = "message")
    })
    
    # Interaction terms UI
    output$interaction_ui <- renderUI({
      req(engineered_data())
      ns <- session$ns
      
      # Get numeric columns
      df <- engineered_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if (length(numeric_cols) < 2) {
        return(p("Need at least 2 numeric variables for interaction terms."))
      }
      
      tagList(
        selectInput(
          ns("interaction_var1"),
          "First variable:",
          choices = numeric_cols,
          selected = numeric_cols[1]
        ),
        
        selectInput(
          ns("interaction_var2"),
          "Second variable:",
          choices = numeric_cols,
          selected = if (length(numeric_cols) > 1) numeric_cols[2] else numeric_cols[1]
        ),
        
        textInput(
          ns("interaction_name"),
          "Interaction feature name:",
          value = ""
        ),
        
        p("Leave name blank to auto-generate as 'var1_x_var2'")
      )
    })
    
    # Create interaction
    observeEvent(input$create_interaction, {
      req(engineered_data(), input$interaction_var1, input$interaction_var2)
      
      current_data <- engineered_data()
      var1 <- input$interaction_var1
      var2 <- input$interaction_var2
      
      if (var1 == var2) {
        showNotification("Please select two different variables for interaction", type = "warning")
        return()
      }
      
      # Create feature name
      if (input$interaction_name == "") {
        feat_name <- paste0(var1, "_x_", var2)
      } else {
        feat_name <- input$interaction_name
      }
      
      # Check if name already exists
      if (feat_name %in% names(current_data)) {
        # Append a suffix to make it unique
        i <- 1
        while (paste0(feat_name, "_", i) %in% names(current_data)) {
          i <- i + 1
        }
        feat_name <- paste0(feat_name, "_", i)
      }
      
      # Create interaction feature
      current_data[[feat_name]] <- current_data[[var1]] * current_data[[var2]]
      
      engineered_data(current_data)
      
      showNotification(paste("Created interaction feature:", feat_name), type = "message")
    })
    
    # Binning UI
    output$binning_ui <- renderUI({
      req(engineered_data())
      ns <- session$ns
      
      # Get numeric columns
      df <- engineered_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if (length(numeric_cols) == 0) {
        return(p("No numeric variables available for binning."))
      }
      
      tagList(
        selectInput(
          ns("binning_var"),
          "Select variable to bin:",
          choices = numeric_cols,
          selected = numeric_cols[1]
        ),
        
        radioButtons(
          ns("binning_method"),
          "Binning method:",
          choices = c(
            "Equal width" = "equal_width",
            "Equal frequency" = "equal_freq",
            "Custom breaks" = "custom"
          ),
          selected = "equal_width"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] != 'custom'", ns("binning_method")),
          sliderInput(
            ns("bin_count"),
            "Number of bins:",
            min = 2,
            max = 20,
            value = 5,
            step = 1
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("binning_method")),
          textInput(
            ns("custom_breaks"),
            "Enter break points (comma separated):",
            value = ""
          ),
          p("Example: 0,10,20,50,100")
        ),
        
        textInput(
          ns("binned_name"),
          "New binned feature name:",
          value = ""
        ),
        
        checkboxInput(
          ns("binning_keep_original"),
          "Keep original numeric feature",
          value = TRUE
        )
      )
    })
    
    # Apply binning
    observeEvent(input$apply_binning, {
      req(engineered_data(), input$binning_var, input$binning_method)
      
      current_data <- engineered_data()
      var <- input$binning_var
      method <- input$binning_method
      
      # Create feature name
      if (input$binned_name == "") {
        feat_name <- paste0(var, "_binned")
      } else {
        feat_name <- input$binned_name
      }
      
      # Check if name already exists
      if (feat_name %in% names(current_data)) {
        # Append a suffix to make it unique
        i <- 1
        while (paste0(feat_name, "_", i) %in% names(current_data)) {
          i <- i + 1
        }
        feat_name <- paste0(feat_name, "_", i)
      }
      
      # Determine breaks
      if (method == "equal_width") {
        # Equal width bins
        breaks <- seq(
          min(current_data[[var]], na.rm = TRUE),
          max(current_data[[var]], na.rm = TRUE),
          length.out = input$bin_count + 1
        )
      } else if (method == "equal_freq") {
        # Equal frequency bins
        breaks <- quantile(
          current_data[[var]],
          probs = seq(0, 1, length.out = input$bin_count + 1),
          na.rm = TRUE
        )
      } else if (method == "custom") {
        # Custom breaks
        breaks_text <- strsplit(input$custom_breaks, ",")[[1]]
        
        # Convert to numeric
        breaks <- tryCatch({
          as.numeric(breaks_text)
        }, warning = function(w) {
          showNotification("Invalid break points. Please enter valid numbers.", type = "error")
          return(NULL)
        })
        
        if (is.null(breaks) || length(breaks) < 2) {
          showNotification("Need at least 2 valid break points for binning.", type = "warning")
          return()
        }
        
        # Sort breaks if not already sorted
        breaks <- sort(breaks)
      }
      
      # Create bin labels
      bin_labels <- paste0("Bin", 1:(length(breaks) - 1))
      
      # Create bins
      current_data[[feat_name]] <- cut(
        current_data[[var]],
        breaks = breaks,
        labels = bin_labels,
        include.lowest = TRUE
      )
      
      # Remove original feature if requested
      if (!input$binning_keep_original) {
        current_data[[var]] <- NULL
      }
      
      engineered_data(current_data)
      
      showNotification(paste("Created binned feature:", feat_name, "with", length(bin_labels), "bins"), type = "message")
    })
    
    # Date features UI
    output$date_ui <- renderUI({
      req(engineered_data())
      ns <- session$ns
      
      # Find columns that might be dates
      df <- engineered_data()
      date_cols <- c()
      
      for (col in names(df)) {
        # Check if column is already Date or POSIXct
        if (inherits(df[[col]], "Date") || inherits(df[[col]], "POSIXct")) {
          date_cols <- c(date_cols, col)
        } else if (is.character(df[[col]]) || is.factor(df[[col]])) {
          # Try to convert sample to date to check if it's a date column
          sample_value <- df[[col]][!is.na(df[[col]])][1]
          
          if (!is.na(sample_value)) {
            test_date <- tryCatch({
              as.Date(sample_value)
            }, error = function(e) {
              return(NA)
            })
            
            if (!is.na(test_date)) {
              date_cols <- c(date_cols, col)
            }
          }
        }
      }
      
      if (length(date_cols) == 0) {
        return(p("No date columns detected. Date columns must be in a recognized date format."))
      }
      
      tagList(
        selectInput(
          ns("date_var"),
          "Select date variable:",
          choices = date_cols,
          selected = date_cols[1]
        ),
        
        checkboxGroupInput(
          ns("date_components"),
          "Select components to extract:",
          choices = c(
            "Year" = "year",
            "Month" = "month",
            "Day" = "day",
            "Weekday" = "weekday",
            "Week of Year" = "week",
            "Quarter" = "quarter",
            "Is Weekend" = "is_weekend"
          ),
          selected = c("year", "month", "day")
        ),
        
        checkboxInput(
          ns("date_keep_original"),
          "Keep original date feature",
          value = TRUE
        )
      )
    })
    
    # Extract date features
    observeEvent(input$extract_date, {
      req(engineered_data(), input$date_var, input$date_components)
      
      current_data <- engineered_data()
      var <- input$date_var
      components <- input$date_components
      
      if (length(components) == 0) {
        showNotification("No date components selected for extraction", type = "warning")
        return()
      }
      
      # Convert to Date if not already
      if (!inherits(current_data[[var]], "Date") && !inherits(current_data[[var]], "POSIXct")) {
        tryCatch({
          current_data[[var]] <- as.Date(current_data[[var]])
        }, error = function(e) {
          showNotification(paste("Error converting", var, "to date:", e$message), type = "error")
          return()
        })
      }
      
      # Extract components
      if ("year" %in% components) {
        current_data[[paste0(var, "_year")]] <- lubridate::year(current_data[[var]])
      }
      
      if ("month" %in% components) {
        current_data[[paste0(var, "_month")]] <- lubridate::month(current_data[[var]])
      }
      
      if ("day" %in% components) {
        current_data[[paste0(var, "_day")]] <- lubridate::day(current_data[[var]])
      }
      
      if ("weekday" %in% components) {
        current_data[[paste0(var, "_weekday")]] <- lubridate::wday(current_data[[var]])
      }
      
      if ("week" %in% components) {
        current_data[[paste0(var, "_week")]] <- lubridate::week(current_data[[var]])
      }
      
      if ("quarter" %in% components) {
        current_data[[paste0(var, "_quarter")]] <- lubridate::quarter(current_data[[var]])
      }
      
      if ("is_weekend" %in% components) {
        current_data[[paste0(var, "_is_weekend")]] <- lubridate::wday(current_data[[var]]) %in% c(1, 7)
      }
      
      # Remove original date feature if requested
      if (!input$date_keep_original) {
        current_data[[var]] <- NULL
      }
      
      engineered_data(current_data)
      
      showNotification(
        paste("Extracted", length(components), "date components from", var),
        type = "message"
      )
    })
    
    # Text features UI
    output$text_ui <- renderUI({
      req(engineered_data())
      ns <- session$ns
      
      # Get character columns
      df <- engineered_data()
      text_cols <- names(df)[sapply(df, is.character)]
      
      if (length(text_cols) == 0) {
        return(p("No text variables available for feature extraction."))
      }
      
      tagList(
        selectInput(
          ns("text_var"),
          "Select text variable:",
          choices = text_cols,
          selected = text_cols[1]
        ),
        
        checkboxGroupInput(
          ns("text_features"),
          "Select text features to extract:",
          choices = c(
            "Character Count" = "char_count",
            "Word Count" = "word_count",
            "Uppercase Count" = "upper_count",
            "Lowercase Count" = "lower_count",
            "Digit Count" = "digit_count",
            "Special Character Count" = "special_count",
            "Contains Digits" = "has_digits",
            "Contains Special Characters" = "has_special"
          ),
          selected = c("char_count", "word_count")
        ),
        
        checkboxInput(
          ns("text_keep_original"),
          "Keep original text feature",
          value = TRUE
        )
      )
    })
    
    # Extract text features
    observeEvent(input$extract_text, {
      req(engineered_data(), input$text_var, input$text_features)
      
      current_data <- engineered_data()
      var <- input$text_var
      features <- input$text_features
      
      if (length(features) == 0) {
        showNotification("No text features selected for extraction", type = "warning")
        return()
      }
      
      # Convert column to character if not already
      if (!is.character(current_data[[var]])) {
        current_data[[var]] <- as.character(current_data[[var]])
      }
      
      # Extract features
      if ("char_count" %in% features) {
        current_data[[paste0(var, "_char_count")]] <- nchar(current_data[[var]])
      }
      
      if ("word_count" %in% features) {
        current_data[[paste0(var, "_word_count")]] <- sapply(
          strsplit(as.character(current_data[[var]]), "\\s+"),
          length
        )
      }
      
      if ("upper_count" %in% features) {
        current_data[[paste0(var, "_upper_count")]] <- sapply(
          current_data[[var]],
          function(x) sum(grepl("[A-Z]", strsplit(x, "")[[1]]))
        )
      }
      
      if ("lower_count" %in% features) {
        current_data[[paste0(var, "_lower_count")]] <- sapply(
          current_data[[var]],
          function(x) sum(grepl("[a-z]", strsplit(x, "")[[1]]))
        )
      }
      
      if ("digit_count" %in% features) {
        current_data[[paste0(var, "_digit_count")]] <- sapply(
          current_data[[var]],
          function(x) sum(grepl("[0-9]", strsplit(x, "")[[1]]))
        )
      }
      
      if ("special_count" %in% features) {
        current_data[[paste0(var, "_special_count")]] <- sapply(
          current_data[[var]],
          function(x) sum(grepl("[^a-zA-Z0-9\\s]", strsplit(x, "")[[1]]))
        )
      }
      
      if ("has_digits" %in% features) {
        current_data[[paste0(var, "_has_digits")]] <- grepl("[0-9]", current_data[[var]])
      }
      
      if ("has_special" %in% features) {
        current_data[[paste0(var, "_has_special")]] <- grepl("[^a-zA-Z0-9\\s]", current_data[[var]])
      }
      
      # Remove original text feature if requested
      if (!input$text_keep_original) {
        current_data[[var]] <- NULL
      }
      
      engineered_data(current_data)
      
      showNotification(
        paste("Extracted", length(features), "text features from", var),
        type = "message"
      )
    })
    
    # Feature impact plot
    output$feature_impact_plot <- renderPlot({
      req(engineered_data())
      
      # Get numeric columns
      df <- engineered_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if (length(numeric_cols) < 2) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                         label = "Not enough numeric variables for impact analysis") +
                 theme_void() +
                 xlim(0, 1) +
                 ylim(0, 1))
      }
      
      # Calculate variance for each numeric column
      var_data <- data.frame(
        Variable = numeric_cols,
        Variance = sapply(df[numeric_cols], var, na.rm = TRUE)
      )
      
      # Sort by variance
      var_data <- var_data[order(-var_data$Variance), ]
      
      # Take top 15 variables by variance
      if (nrow(var_data) > 15) {
        var_data <- var_data[1:15, ]
      }
      
      # Create plot
      ggplot(var_data, aes(x = reorder(Variable, Variance), y = Variance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Feature Importance (by Variance)",
          x = "",
          y = "Variance"
        )
    })
    
    # Engineered data preview
    output$engineered_data_preview <- renderDT({
      req(engineered_data())
      
      datatable(
        head(engineered_data(), 100),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = FALSE
      )
    })
    
    # Download handler for engineered data
    output$download_engineered_data <- downloadHandler(
      filename = function() {
        "engineered_data.csv"
      },
      content = function(file) {
        write.csv(engineered_data(), file, row.names = FALSE)
      }
    )
    
    # Return engineered data for other modules
    return(reactive({
      req(engineered_data())
      engineered_data()
    }))
  })
}
