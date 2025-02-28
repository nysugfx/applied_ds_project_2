# modules/eda.R
# Module for exploratory data analysis

# UI Module
edaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Data Exploration",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        
        tabsetPanel(
          id = ns("eda_tabs"),
          
          # Summary tab
          tabPanel(
            "Summary Statistics",
            br(),
            fluidRow(
              column(
                width = 3,
                selectInput(
                  ns("summary_var_type"),
                  "Variable Type:",
                  choices = c("All", "Numeric", "Categorical"),
                  selected = "All"
                )
              ),
              column(
                width = 9,
                uiOutput(ns("summary_var_select"))
              )
            ),
            withSpinner(
              verbatimTextOutput(ns("summary_output")),
              type = 4
            )
          ),
          
          # Distribution tab
          tabPanel(
            "Distribution Analysis",
            br(),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  ns("dist_var"),
                  "Select Variable:",
                  choices = NULL
                )
              ),
              column(
                width = 4,
                uiOutput(ns("dist_group_var"))
              ),
              column(
                width = 4,
                uiOutput(ns("dist_plot_type"))
              )
            ),
            withSpinner(
              plotlyOutput(ns("dist_plot"), height = "500px"),
              type = 4
            ),
            hr(),
            downloadButton(ns("download_dist_plot"), "Download Plot")
          ),
          
          # Correlation tab
          tabPanel(
            "Correlation Analysis",
            br(),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  ns("corr_method"),
                  "Correlation Method:",
                  choices = c("Pearson", "Spearman", "Kendall"),
                  selected = "Pearson"
                )
              ),
              column(
                width = 4,
                uiOutput(ns("corr_vars"))
              ),
              column(
                width = 4,
                sliderInput(
                  ns("corr_threshold"),
                  "Correlation Threshold:",
                  min = 0,
                  max = 1,
                  value = 0,
                  step = 0.1
                )
              )
            ),
            withSpinner(
              plotOutput(ns("corr_plot"), height = "600px"),
              type = 4
            ),
            hr(),
            downloadButton(ns("download_corr_plot"), "Download Plot")
          ),
          
          # Scatter plots tab
          tabPanel(
            "Scatter Analysis",
            br(),
            fluidRow(
              column(
                width = 3,
                selectInput(
                  ns("scatter_x"),
                  "X Variable:",
                  choices = NULL
                )
              ),
              column(
                width = 3,
                selectInput(
                  ns("scatter_y"),
                  "Y Variable:",
                  choices = NULL
                )
              ),
              column(
                width = 3,
                uiOutput(ns("scatter_color"))
              ),
              column(
                width = 3,
                checkboxInput(
                  ns("scatter_trend"),
                  "Add Trend Line",
                  value = TRUE
                )
              )
            ),
            withSpinner(
              plotlyOutput(ns("scatter_plot"), height = "500px"),
              type = 4
            ),
            hr(),
            downloadButton(ns("download_scatter_plot"), "Download Plot")
          ),
          
          # Multivariate tab
          tabPanel(
            "Multivariate Analysis",
            br(),
            fluidRow(
              column(
                width = 12,
                uiOutput(ns("multi_vars"))
              )
            ),
            withSpinner(
              plotOutput(ns("multi_plot"), height = "700px"),
              type = 4
            )
          ),
          
          # Custom Plot tab
          tabPanel(
            "Custom Visualization",
            br(),
            fluidRow(
              column(
                width = 3,
                selectInput(
                  ns("custom_plot_type"),
                  "Plot Type:",
                  choices = c(
                    "Bar Chart" = "bar",
                    "Line Chart" = "line",
                    "Box Plot" = "box",
                    "Violin Plot" = "violin",
                    "Density Plot" = "density",
                    "Scatter Plot" = "scatter"
                  ),
                  selected = "bar"
                )
              ),
              column(
                width = 3,
                uiOutput(ns("custom_x_var"))
              ),
              column(
                width = 3,
                uiOutput(ns("custom_y_var"))
              ),
              column(
                width = 3,
                uiOutput(ns("custom_group"))
              )
            ),
            fluidRow(
              column(
                width = 4,
                uiOutput(ns("custom_options"))
              ),
              column(
                width = 8,
                plotlyOutput(ns("custom_plot"), height = "500px")
              )
            ),
            hr(),
            downloadButton(ns("download_custom_plot"), "Download Plot")
          )
        )
      )
    ),
    
    fluidRow(
      # Advanced filtering
      box(
        title = "Data Filtering",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        
        fluidRow(
          column(
            width = 4,
            uiOutput(ns("filter_var_select"))
          ),
          column(
            width = 4,
            uiOutput(ns("filter_condition"))
          ),
          column(
            width = 4,
            uiOutput(ns("filter_value"))
          )
        ),
        hr(),
        actionButton(
          ns("add_filter"),
          "Add Filter",
          icon = icon("plus"),
          class = "btn-primary"
        ),
        actionButton(
          ns("clear_filters"),
          "Clear All Filters",
          icon = icon("trash"),
          class = "btn-warning"
        ),
        hr(),
        tags$div(
          id = ns("active_filters_container"),
          tags$h4("Active Filters:"),
          htmlOutput(ns("active_filters"))
        ),
        hr(),
        withSpinner(
          DTOutput(ns("filtered_data")),
          type = 4
        ),
        hr(),
        downloadButton(ns("download_filtered_data"), "Download Filtered Data")
      )
    )
  )
}

# Server Module
edaServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store filtered data and filters
    filtered_data <- reactiveVal(NULL)
    filters <- reactiveVal(list())
    
    # Initialize data when input dataset changes
    observe({
      req(dataset())
      filtered_data(dataset())
      filters(list())
      
      # Update variable selection inputs
      updateSelectInput(session, "dist_var", choices = names(dataset()))
      updateSelectInput(session, "scatter_x", choices = names(dataset()))
      updateSelectInput(session, "scatter_y", choices = names(dataset()))
    })
    
    # Helper function to get numeric and categorical columns
    get_var_types <- function(data) {
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      categorical_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x) || is.logical(x))]
      
      list(
        numeric = numeric_cols,
        categorical = categorical_cols
      )
    }
    
    # Summary variable selection
    output$summary_var_select <- renderUI({
      req(filtered_data())
      ns <- session$ns
      
      data <- filtered_data()
      var_types <- get_var_types(data)
      
      if (input$summary_var_type == "All") {
        choices <- names(data)
      } else if (input$summary_var_type == "Numeric") {
        choices <- var_types$numeric
      } else if (input$summary_var_type == "Categorical") {
        choices <- var_types$categorical
      }
      
      selectInput(
        ns("summary_vars"),
        "Select Variables:",
        choices = choices,
        selected = choices[1:min(5, length(choices))],
        multiple = TRUE
      )
    })
    
    # Summary output
    output$summary_output <- renderPrint({
      req(filtered_data(), input$summary_vars)
      
      data <- filtered_data()
      vars <- input$summary_vars
      
      if (length(vars) == 0) {
        return("No variables selected.")
      }
      
      cat("Dataset Summary\n")
      cat("---------------\n")
      cat("Rows:", nrow(data), "\n")
      cat("Columns:", ncol(data), "\n\n")
      
      # Summary for selected variables
      for (var in vars) {
        cat("\nVariable:", var, "\n")
        
        if (is.numeric(data[[var]])) {
          # Summary for numeric variables
          cat("Type: Numeric\n")
          cat("Missing values:", sum(is.na(data[[var]])), "\n")
          cat("Unique values:", length(unique(data[[var]])), "\n")
          
          # Summary statistics
          stats <- summary(data[[var]])
          print(stats)
          
          # Additional stats
          cat("Standard deviation:", sd(data[[var]], na.rm = TRUE), "\n")
          cat("Variance:", var(data[[var]], na.rm = TRUE), "\n")
          cat("Skewness:", moments::skewness(data[[var]], na.rm = TRUE), "\n")
          cat("Kurtosis:", moments::kurtosis(data[[var]], na.rm = TRUE), "\n")
        } else {
          # Summary for categorical variables
          cat("Type: Categorical\n")
          cat("Missing values:", sum(is.na(data[[var]])), "\n")
          cat("Unique values:", length(unique(data[[var]])), "\n")
          
          # Frequency table
          freq_table <- table(data[[var]], useNA = "ifany")
          print(freq_table)
          
          # Percentage table
          pct_table <- prop.table(freq_table) * 100
          cat("\nPercentages:\n")
          print(round(pct_table, 2))
        }
        
        cat("\n", rep("-", 50), "\n")
      }
    })
    
    # Distribution analysis - group variable selection
    output$dist_group_var <- renderUI({
      req(filtered_data(), input$dist_var)
      ns <- session$ns
      
      data <- filtered_data()
      var_types <- get_var_types(data)
      categorical_cols <- var_types$categorical
      
      # Don't include the selected variable as a grouping option
      categorical_cols <- setdiff(categorical_cols, input$dist_var)
      
      selectInput(
        ns("dist_group"),
        "Group By (optional):",
        choices = c("None" = "", categorical_cols),
        selected = ""
      )
    })
    
    # Distribution analysis - plot type selection
    output$dist_plot_type <- renderUI({
      req(filtered_data(), input$dist_var)
      ns <- session$ns
      
      data <- filtered_data()
      
      if (is.numeric(data[[input$dist_var]])) {
        # For numeric variables
        choices <- c(
          "Histogram" = "histogram",
          "Density Plot" = "density",
          "Box Plot" = "box",
          "Violin Plot" = "violin"
        )
      } else {
        # For categorical variables
        choices <- c(
          "Bar Chart" = "bar",
          "Pie Chart" = "pie"
        )
      }
      
      selectInput(
        ns("dist_plot_type"),
        "Plot Type:",
        choices = choices,
        selected = choices[1]
      )
    })
    
    # Distribution plot
    output$dist_plot <- renderPlotly({
      req(filtered_data(), input$dist_var, input$dist_plot_type)
      
      data <- filtered_data()
      var <- input$dist_var
      plot_type <- input$dist_plot_type
      group_var <- input$dist_group
      
      # Base plot
      p <- ggplot(data)
      
      if (is.numeric(data[[var]])) {
        # Plots for numeric variables
        if (plot_type == "histogram") {
          if (group_var == "") {
            p <- p + geom_histogram(aes_string(x = var), bins = 30, fill = "steelblue", color = "white")
          } else {
            p <- p + geom_histogram(aes_string(x = var, fill = group_var), bins = 30, alpha = 0.7, position = "identity")
          }
          p <- p + labs(title = paste("Histogram of", var), x = var, y = "Count")
        } else if (plot_type == "density") {
          if (group_var == "") {
            p <- p + geom_density(aes_string(x = var), fill = "steelblue", alpha = 0.5)
          } else {
            p <- p + geom_density(aes_string(x = var, fill = group_var), alpha = 0.5)
          }
          p <- p + labs(title = paste("Density Plot of", var), x = var, y = "Density")
        } else if (plot_type == "box") {
          if (group_var == "") {
            p <- p + geom_boxplot(aes_string(y = var, x = 1))
            p <- p + labs(title = paste("Box Plot of", var), y = var, x = "")
            p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
          } else {
            p <- p + geom_boxplot(aes_string(y = var, x = group_var, fill = group_var))
            p <- p + labs(title = paste("Box Plot of", var, "by", group_var), y = var, x = group_var)
          }
        } else if (plot_type == "violin") {
          if (group_var == "") {
            p <- p + geom_violin(aes_string(y = var, x = 1), fill = "steelblue")
            p <- p + labs(title = paste("Violin Plot of", var), y = var, x = "")
            p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
          } else {
            p <- p + geom_violin(aes_string(y = var, x = group_var, fill = group_var))
            p <- p + labs(title = paste("Violin Plot of", var, "by", group_var), y = var, x = group_var)
          }
        }
      } else {
        # Plots for categorical variables
        if (plot_type == "bar") {
          if (group_var == "") {
            # Count by single variable
            p <- p + geom_bar(aes_string(x = var), fill = "steelblue")
            p <- p + labs(title = paste("Bar Chart of", var), x = var, y = "Count")
          } else {
            # Stacked bar chart
            p <- p + geom_bar(aes_string(x = var, fill = group_var), position = "stack")
            p <- p + labs(title = paste("Stacked Bar Chart of", var, "by", group_var), x = var, y = "Count")
          }
        } else if (plot_type == "pie") {
          # Pie chart using ggplot2
          if (group_var == "") {
            count_data <- as.data.frame(table(data[[var]]))
            names(count_data) <- c("category", "count")
            
            p <- ggplot(count_data, aes(x = "", y = count, fill = category)) +
              geom_bar(stat = "identity", width = 1) +
              coord_polar("y", start = 0) +
              labs(title = paste("Pie Chart of", var), fill = var) +
              theme_void()
          } else {
            # Pie charts not supported for grouped data
            return(ggplotly(ggplot() + 
                      annotate("text", x = 0.5, y = 0.5, 
                             label = "Pie charts not supported for grouped data.\nPlease choose Bar Chart instead.") +
                      theme_void() +
                      xlim(0, 1) +
                      ylim(0, 1)))
          }
        }
      }
      
      # Apply common theme
      p <- p + theme_minimal()
      
      # Convert to plotly
      ggplotly(p)
    })
    
    # Download distribution plot
    output$download_dist_plot <- downloadHandler(
      filename = function() {
        paste("distribution_plot_", input$dist_var, ".png", sep = "")
      },
      content = function(file) {
        req(input$dist_var, input$dist_plot_type)
        
        data <- filtered_data()
        var <- input$dist_var
        plot_type <- input$dist_plot_type
        group_var <- input$dist_group
        
        # Create the plot
        p <- ggplot(data)
        
        if (is.numeric(data[[var]])) {
          # Plots for numeric variables
          if (plot_type == "histogram") {
            if (group_var == "") {
              p <- p + geom_histogram(aes_string(x = var), bins = 30, fill = "steelblue", color = "white")
            } else {
              p <- p + geom_histogram(aes_string(x = var, fill = group_var), bins = 30, alpha = 0.7, position = "identity")
            }
            p <- p + labs(title = paste("Histogram of", var), x = var, y = "Count")
          } else if (plot_type == "density") {
            if (group_var == "") {
              p <- p + geom_density(aes_string(x = var), fill = "steelblue", alpha = 0.5)
            } else {
              p <- p + geom_density(aes_string(x = var, fill = group_var), alpha = 0.5)
            }
            p <- p + labs(title = paste("Density Plot of", var), x = var, y = "Density")
          } else if (plot_type == "box") {
            if (group_var == "") {
              p <- p + geom_boxplot(aes_string(y = var, x = 1))
              p <- p + labs(title = paste("Box Plot of", var), y = var, x = "")
              p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            } else {
              p <- p + geom_boxplot(aes_string(y = var, x = group_var, fill = group_var))
              p <- p + labs(title = paste("Box Plot of", var, "by", group_var), y = var, x = group_var)
            }
          } else if (plot_type == "violin") {
            if (group_var == "") {
              p <- p + geom_violin(aes_string(y = var, x = 1), fill = "steelblue")
              p <- p + labs(title = paste("Violin Plot of", var), y = var, x = "")
              p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            } else {
              p <- p + geom_violin(aes_string(y = var, x = group_var, fill = group_var))
              p <- p + labs(title = paste("Violin Plot of", var, "by", group_var), y = var, x = group_var)
            }
          }
        } else {
          # Plots for categorical variables
          if (plot_type == "bar") {
            if (group_var == "") {
              p <- p + geom_bar(aes_string(x = var), fill = "steelblue")
              p <- p + labs(title = paste("Bar Chart of", var), x = var, y = "Count")
            } else {
              p <- p + geom_bar(aes_string(x = var, fill = group_var), position = "stack")
              p <- p + labs(title = paste("Stacked Bar Chart of", var, "by", group_var), x = var, y = "Count")
            }
          } else if (plot_type == "pie") {
            if (group_var == "") {
              count_data <- as.data.frame(table(data[[var]]))
              names(count_data) <- c("category", "count")
              
              p <- ggplot(count_data, aes(x = "", y = count, fill = category)) +
                geom_bar(stat = "identity", width = 1) +
                coord_polar("y", start = 0) +
                labs(title = paste("Pie Chart of", var), fill = var) +
                theme_void()
            } else {
              p <- ggplot() + 
                annotate("text", x = 0.5, y = 0.5, 
                        label = "Pie charts not supported for grouped data.\nPlease choose Bar Chart instead.") +
                theme_void() +
                xlim(0, 1) +
                ylim(0, 1)
            }
          }
        }
        
        # Apply common theme
        p <- p + theme_minimal()
        
        # Save the plot
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      }
    )
    
    # Correlation analysis - variable selection
    output$corr_vars <- renderUI({
      req(filtered_data())
      ns <- session$ns
      
      data <- filtered_data()
      var_types <- get_var_types(data)
      numeric_cols <- var_types$numeric
      
      if (length(numeric_cols) < 2) {
        return(p("Need at least 2 numeric variables for correlation analysis."))
      }
      
      selectInput(
        ns("corr_vars"),
        "Select Variables:",
        choices = numeric_cols,
        selected = numeric_cols[1:min(10, length(numeric_cols))],
        multiple = TRUE
      )
    })
    
    # Correlation plot
    output$corr_plot <- renderPlot({
      req(filtered_data(), input$corr_vars, input$corr_method)
      
      data <- filtered_data()
      vars <- input$corr_vars
      method <- tolower(input$corr_method)
      threshold <- input$corr_threshold
      
      if (length(vars) < 2) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                        label = "Select at least 2 variables for correlation analysis") +
                 theme_void() +
                 xlim(0, 1) +
                 ylim(0, 1))
      }
      
      # Compute correlation matrix
      corr_matrix <- cor(data[, vars], method = method, use = "pairwise.complete.obs")
      
      # Apply threshold if needed
      if (threshold > 0) {
        corr_matrix[abs(corr_matrix) < threshold] <- 0
      }
      
      # Create correlation plot
      corrplot(
        corr_matrix,
        method = "color",
        type = "upper",
        order = "hclust",
        tl.col = "black",
        tl.srt = 45,
        addCoef.col = "black",
        number.cex = 0.7,
        col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
        diag = FALSE,
        title = paste(input$corr_method, "Correlation Matrix"),
        mar = c(0, 0, 2, 0)
      )
    })
    
    # Download correlation plot
    output$download_corr_plot <- downloadHandler(
      filename = function() {
        paste("correlation_plot_", input$corr_method, ".png", sep = "")
      },
      content = function(file) {
        req(input$corr_vars, input$corr_method)
        
        data <- filtered_data()
        vars <- input$corr_vars
        method <- tolower(input$corr_method)
        threshold <- input$corr_threshold
        
        if (length(vars) < 2) {
          # Create an empty plot if not enough variables
          png(file, width = 800, height = 600)
          plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(0, 0, "Select at least 2 variables for correlation analysis")
          dev.off()
          return()
        }
        
        # Compute correlation matrix
        corr_matrix <- cor(data[, vars], method = method, use = "pairwise.complete.obs")
        
        # Apply threshold if needed
        if (threshold > 0) {
          corr_matrix[abs(corr_matrix) < threshold] <- 0
        }
        
        # Create and save correlation plot
        png(file, width = 800, height = 600, res = 100)
        corrplot(
          corr_matrix,
          method = "color",
          type = "upper",
          order = "hclust",
          tl.col = "black",
          tl.srt = 45,
          addCoef.col = "black",
          number.cex = 0.7,
          col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
          diag = FALSE,
          title = paste(input$corr_method, "Correlation Matrix"),
          mar = c(0, 0, 2, 0)
        )
        dev.off()
      }
    )
    
    # Scatter analysis - color variable selection
    output$scatter_color <- renderUI({
      req(filtered_data(), input$scatter_x, input$scatter_y)
      ns <- session$ns
      
      data <- filtered_data()
      var_types <- get_var_types(data)
      categorical_cols <- var_types$categorical
      numeric_cols <- var_types$numeric
      
      # All columns can be used for coloring
      all_cols <- c("None" = "", names(data))
      # Remove the x and y variables
      all_cols <- setdiff(all_cols, c(input$scatter_x, input$scatter_y))
      
      selectInput(
        ns("scatter_color_var"),
        "Color By (optional):",
        choices = all_cols,
        selected = ""
      )
    })
    
    # Scatter plot
    output$scatter_plot <- renderPlotly({
      req(filtered_data(), input$scatter_x, input$scatter_y)
      
      data <- filtered_data()
      x_var <- input$scatter_x
      y_var <- input$scatter_y
      color_var <- input$scatter_color_var
      add_trend <- input$scatter_trend
      
      # Base plot
      p <- ggplot(data, aes_string(x = x_var, y = y_var))
      
      # Add points with optional coloring
      if (color_var != "") {
        p <- p + geom_point(aes_string(color = color_var), alpha = 0.7)
      } else {
        p <- p + geom_point(color = "steelblue", alpha = 0.7)
      }
      
      # Add trend line if requested
      if (add_trend) {
        if (color_var != "" && (is.factor(data[[color_var]]) || is.character(data[[color_var]]))) {
          # Add trend line for each group
          p <- p + geom_smooth(aes_string(color = color_var), method = "lm", se = TRUE, alpha = 0.2)
        } else {
          # Add a single trend line
          p <- p + geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2)
        }
      }
      
      # Add labels and theme
      p <- p + labs(
        title = paste("Scatter Plot of", y_var, "vs", x_var),
        x = x_var,
        y = y_var
      ) + theme_minimal()
      
      # Convert to plotly
      ggplotly(p)
    })
    
    # Download scatter plot
    output$download_scatter_plot <- downloadHandler(
      filename = function() {
        paste("scatter_plot_", input$scatter_x, "_vs_", input$scatter_y, ".png", sep = "")
      },
      content = function(file) {
        req(input$scatter_x, input$scatter_y)
        
        data <- filtered_data()
        x_var <- input$scatter_x
        y_var <- input$scatter_y
        color_var <- input$scatter_color_var
        add_trend <- input$scatter_trend
        
        # Base plot
        p <- ggplot(data, aes_string(x = x_var, y = y_var))
        
        # Add points with optional coloring
        if (color_var != "") {
          p <- p + geom_point(aes_string(color = color_var), alpha = 0.7)
        } else {
          p <- p + geom_point(color = "steelblue", alpha = 0.7)
        }
        
        # Add trend line if requested
        if (add_trend) {
          if (color_var != "" && (is.factor(data[[color_var]]) || is.character(data[[color_var]]))) {
            # Add trend line for each group
            p <- p + geom_smooth(aes_string(color = color_var), method = "lm", se = TRUE, alpha = 0.2)
          } else {
            # Add a single trend line
            p <- p + geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2)
          }
        }
        
        # Add labels and theme
        p <- p + labs(
          title = paste("Scatter Plot of", y_var, "vs", x_var),
          x = x_var,
          y = y_var
        ) + theme_minimal()
        
        # Save the plot
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      }
    )
    
    # Multivariate analysis - variable selection
    output$multi_vars <- renderUI({
      req(filtered_data())
      ns <- session$ns
      
      data <- filtered_data()
      var_types <- get_var_types(data)
      numeric_cols <- var_types$numeric
      
      if (length(numeric_cols) < 3) {
        return(p("Need at least 3 numeric variables for multivariate analysis."))
      }
      
      selectInput(
        ns("multi_vars"),
        "Select Variables:",
        choices = numeric_cols,
        selected = numeric_cols[1:min(6, length(numeric_cols))],
        multiple = TRUE
      )
    })
    
    # Multivariate plot (pairs plot)
    output$multi_plot <- renderPlot({
      req(filtered_data(), input$multi_vars)
      
      data <- filtered_data()
      vars <- input$multi_vars
      
      if (length(vars) < 3) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                        label = "Select at least 3 variables for multivariate analysis") +
                 theme_void() +
                 xlim(0, 1) +
                 ylim(0, 1))
      }
      
      # Create pairs plot using GGally
      GGally::ggpairs(
        data[, vars],
        progress = FALSE,
        title = "Multivariate Analysis"
      )
    })
    
    # Custom plot - X variable selection
    output$custom_x_var <- renderUI({
      req(filtered_data(), input$custom_plot_type)
      ns <- session$ns
      
      data <- filtered_data()
      plot_type <- input$custom_plot_type
      var_types <- get_var_types(data)
      
      if (plot_type %in% c("bar")) {
        # For bar charts, x is typically categorical
        choices <- var_types$categorical
        if (length(choices) == 0) choices <- names(data)
      } else if (plot_type %in% c("box", "violin")) {
        # For box and violin plots, x is typically categorical
        choices <- c(var_types$categorical, "None" = "")
        if (length(choices) == 1) choices <- c("None" = "")  # Only "None" if no categorical vars
      } else {
        # For other plots, x can be any variable
        choices <- names(data)
      }
      
      selectInput(
        ns("custom_x"),
        "X Variable:",
        choices = choices,
        selected = if (length(choices) > 0) choices[1] else ""
      )
    })
    
    # Custom plot - Y variable selection
    output$custom_y_var <- renderUI({
      req(filtered_data(), input$custom_plot_type, input$custom_x)
      ns <- session$ns
      
      data <- filtered_data()
      plot_type <- input$custom_plot_type
      var_types <- get_var_types(data)
      
      if (plot_type %in% c("line", "scatter", "box", "violin")) {
        # For these plots, y should be numeric
        choices <- var_types$numeric
      } else if (plot_type == "bar") {
        # For bar charts, y can be optional (count) or numeric for values
        choices <- c("Count" = "", var_types$numeric)
      } else if (plot_type == "density") {
        # For density plots, y is determined by the density function
        choices <- c("Density" = "")
      } else {
        # For other plots, y can be any variable
        choices <- names(data)
      }
      
      # Remove x variable from choices
      choices <- setdiff(choices, input$custom_x)
      
      selectInput(
        ns("custom_y"),
        "Y Variable:",
        choices = choices,
        selected = if (length(choices) > 0) choices[1] else ""
      )
    })
    
    # Custom plot - Group variable selection
    output$custom_group <- renderUI({
      req(filtered_data(), input$custom_plot_type)
      ns <- session$ns
      
      data <- filtered_data()
      var_types <- get_var_types(data)
      categorical_cols <- var_types$categorical
      
      # For grouping, we typically use categorical variables
      choices <- c("None" = "", categorical_cols)
      
      # Remove variables already used for x and y
      if (!is.null(input$custom_x)) {
        choices <- setdiff(choices, input$custom_x)
      }
      if (!is.null(input$custom_y) && input$custom_y != "") {
        choices <- setdiff(choices, input$custom_y)
      }
      
      selectInput(
        ns("custom_group_var"),
        "Group By (optional):",
        choices = choices,
        selected = ""
      )
    })
    
    # Custom plot - Additional options
    output$custom_options <- renderUI({
      req(filtered_data(), input$custom_plot_type)
      ns <- session$ns
      
      plot_type <- input$custom_plot_type
      
      # Common options for all plot types
      common_options <- tagList(
        checkboxInput(
          ns("custom_title_auto"),
          "Auto Title",
          value = TRUE
        ),
        
        conditionalPanel(
          condition = sprintf("!input['%s']", ns("custom_title_auto")),
          textInput(
            ns("custom_title"),
            "Custom Title:",
            value = ""
          )
        )
      )
      
      # Plot-specific options
      specific_options <- switch(
        plot_type,
        "bar" = tagList(
          radioButtons(
            ns("bar_position"),
            "Bar Position:",
            choices = c(
              "Stack" = "stack",
              "Dodge (side-by-side)" = "dodge",
              "Fill (100%)" = "fill"
            ),
            selected = "stack",
            inline = TRUE
          )
        ),
        "line" = tagList(
          checkboxInput(
            ns("line_points"),
            "Show Points",
            value = TRUE
          ),
          checkboxInput(
            ns("line_smooth"),
            "Add Smoothing",
            value = FALSE
          )
        ),
        "scatter" = tagList(
          checkboxInput(
            ns("scatter_trendline"),
            "Add Trend Line",
            value = TRUE
          ),
          sliderInput(
            ns("scatter_alpha"),
            "Point Opacity:",
            min = 0.1,
            max = 1,
            value = 0.7,
            step = 0.1
          )
        ),
        "box" = tagList(
          checkboxInput(
            ns("box_notch"),
            "Notched Box Plot",
            value = FALSE
          ),
          checkboxInput(
            ns("box_points"),
            "Show Points",
            value = TRUE
          )
        ),
        "violin" = tagList(
          checkboxInput(
            ns("violin_box"),
            "Show Box Plot Inside",
            value = TRUE
          )
        ),
        "density" = tagList(
          sliderInput(
            ns("density_alpha"),
            "Fill Opacity:",
            min = 0.1,
            max = 1,
            value = 0.5,
            step = 0.1
          ),
          checkboxInput(
            ns("density_rug"),
            "Show Rug Plot",
            value = TRUE
          )
        ),
        NULL
      )
      
      # Combine common and specific options
      tagList(
        common_options,
        hr(),
        specific_options
      )
    })
    
    # Custom plot
    output$custom_plot <- renderPlotly({
      req(filtered_data(), input$custom_plot_type)
      
      data <- filtered_data()
      plot_type <- input$custom_plot_type
      
      # Get variables
      x_var <- input$custom_x
      y_var <- input$custom_y
      group_var <- input$custom_group_var
      
      # Skip if necessary variables are missing
      if (is.null(x_var) || (plot_type != "density" && plot_type != "bar" && (is.null(y_var) || y_var == ""))) {
        return(NULL)
      }
      
      # Create plot title
      if (input$custom_title_auto) {
        if (plot_type == "bar") {
          if (y_var == "") {
            title <- paste("Count of", x_var)
          } else {
            title <- paste(y_var, "by", x_var)
          }
        } else if (plot_type == "line" || plot_type == "scatter") {
          title <- paste(y_var, "vs", x_var)
        } else if (plot_type == "box" || plot_type == "violin") {
          if (x_var == "") {
            title <- paste(plot_type, "Plot of", y_var)
          } else {
            title <- paste(plot_type, "Plot of", y_var, "by", x_var)
          }
        } else if (plot_type == "density") {
          title <- paste("Density Plot of", x_var)
        }
      } else {
        title <- input$custom_title
      }
      
      # Base plot
      p <- ggplot(data)
      
      # Add layers based on plot type
      if (plot_type == "bar") {
        if (y_var == "") {
          # Count-based bar chart
          if (group_var == "") {
            p <- p + geom_bar(aes_string(x = x_var), fill = "steelblue")
          } else {
            p <- p + geom_bar(aes_string(x = x_var, fill = group_var), 
                           position = input$bar_position)
          }
          p <- p + labs(y = "Count")
        } else {
          # Value-based bar chart
          if (group_var == "") {
            p <- p + geom_bar(aes_string(x = x_var, y = y_var), 
                           stat = "identity", fill = "steelblue")
          } else {
            p <- p + geom_bar(aes_string(x = x_var, y = y_var, fill = group_var), 
                           stat = "identity", position = input$bar_position)
          }
        }
      } else if (plot_type == "line") {
        if (group_var == "") {
          p <- p + geom_line(aes_string(x = x_var, y = y_var), color = "steelblue")
          if (input$line_points) {
            p <- p + geom_point(aes_string(x = x_var, y = y_var), color = "steelblue")
          }
        } else {
          p <- p + geom_line(aes_string(x = x_var, y = y_var, color = group_var))
          if (input$line_points) {
            p <- p + geom_point(aes_string(x = x_var, y = y_var, color = group_var))
          }
        }
        
        if (input$line_smooth) {
          if (group_var == "") {
            p <- p + geom_smooth(aes_string(x = x_var, y = y_var), 
                              color = "red", se = FALSE)
          } else {
            p <- p + geom_smooth(aes_string(x = x_var, y = y_var, color = group_var), 
                              se = FALSE)
          }
        }
      } else if (plot_type == "scatter") {
        if (group_var == "") {
          p <- p + geom_point(aes_string(x = x_var, y = y_var), 
                           color = "steelblue", alpha = input$scatter_alpha)
        } else {
          p <- p + geom_point(aes_string(x = x_var, y = y_var, color = group_var), 
                           alpha = input$scatter_alpha)
        }
        
        if (input$scatter_trendline) {
          if (group_var == "") {
            p <- p + geom_smooth(aes_string(x = x_var, y = y_var), 
                              method = "lm", color = "red", se = TRUE)
          } else {
            p <- p + geom_smooth(aes_string(x = x_var, y = y_var, color = group_var), 
                              method = "lm", se = TRUE)
          }
        }
      } else if (plot_type == "box") {
        if (x_var == "") {
          # Single boxplot
          p <- p + geom_boxplot(aes_string(y = y_var, x = 1), 
                             notch = input$box_notch, fill = "steelblue")
          p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
          p <- p + labs(x = "")
        } else {
          # Multiple boxplots by category
          if (group_var == "") {
            p <- p + geom_boxplot(aes_string(x = x_var, y = y_var), 
                               notch = input$box_notch, fill = "steelblue")
          } else {
            p <- p + geom_boxplot(aes_string(x = x_var, y = y_var, fill = group_var), 
                               notch = input$box_notch)
          }
        }
        
        if (input$box_points) {
          if (x_var == "") {
            p <- p + geom_jitter(aes_string(y = y_var, x = 1), 
                              alpha = 0.5, width = 0.2)
          } else {
            if (group_var == "") {
              p <- p + geom_jitter(aes_string(x = x_var, y = y_var), 
                                alpha = 0.5, width = 0.2)
            } else {
              p <- p + geom_jitter(aes_string(x = x_var, y = y_var, color = group_var), 
                                alpha = 0.5, width = 0.2)
            }
          }
        }
      } else if (plot_type == "violin") {
        if (x_var == "") {
          # Single violin plot
          p <- p + geom_violin(aes_string(y = y_var, x = 1), fill = "steelblue")
          p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
          p <- p + labs(x = "")
        } else {
          # Multiple violin plots by category
          if (group_var == "") {
            p <- p + geom_violin(aes_string(x = x_var, y = y_var), fill = "steelblue")
          } else {
            p <- p + geom_violin(aes_string(x = x_var, y = y_var, fill = group_var))
          }
        }
        
        if (input$violin_box) {
          if (x_var == "") {
            p <- p + geom_boxplot(aes_string(y = y_var, x = 1), 
                               width = 0.1, alpha = 0.7)
          } else {
            p <- p + geom_boxplot(aes_string(x = x_var, y = y_var), 
                               width = 0.1, alpha = 0.7)
          }
        }
      } else if (plot_type == "density") {
        if (group_var == "") {
          p <- p + geom_density(aes_string(x = x_var), 
                             fill = "steelblue", alpha = input$density_alpha)
        } else {
          p <- p + geom_density(aes_string(x = x_var, fill = group_var), 
                             alpha = input$density_alpha)
        }
        
        if (input$density_rug) {
          if (group_var == "") {
            p <- p + geom_rug(aes_string(x = x_var), alpha = 0.5)
          } else {
            p <- p + geom_rug(aes_string(x = x_var, color = group_var), alpha = 0.5)
          }
        }
      }
      
      # Add title and theme
      p <- p + labs(title = title) + theme_minimal()
      
      # Convert to plotly
      ggplotly(p)
    })
    
    # Download custom plot
    output$download_custom_plot <- downloadHandler(
      filename = function() {
        paste("custom_plot_", input$custom_plot_type, ".png", sep = "")
      },
      content = function(file) {
        req(input$custom_plot_type)
        
        data <- filtered_data()
        plot_type <- input$custom_plot_type
        
        # Get variables
        x_var <- input$custom_x
        y_var <- input$custom_y
        group_var <- input$custom_group_var
        
        # Skip if necessary variables are missing
        if (is.null(x_var) || (plot_type != "density" && plot_type != "bar" && (is.null(y_var) || y_var == ""))) {
          # Create an empty plot
          png(file, width = 800, height = 600)
          plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(0, 0, "Missing required variables for this plot type")
          dev.off()
          return()
        }
        
        # Create plot title
        if (input$custom_title_auto) {
          if (plot_type == "bar") {
            if (y_var == "") {
              title <- paste("Count of", x_var)
            } else {
              title <- paste(y_var, "by", x_var)
            }
          } else if (plot_type == "line" || plot_type == "scatter") {
            title <- paste(y_var, "vs", x_var)
          } else if (plot_type == "box" || plot_type == "violin") {
            if (x_var == "") {
              title <- paste(plot_type, "Plot of", y_var)
            } else {
              title <- paste(plot_type, "Plot of", y_var, "by", x_var)
            }
          } else if (plot_type == "density") {
            title <- paste("Density Plot of", x_var)
          }
        } else {
          title <- input$custom_title
        }
        
        # Base plot
        p <- ggplot(data)
        
        # Add layers based on plot type
        if (plot_type == "bar") {
          if (y_var == "") {
            # Count-based bar chart
            if (group_var == "") {
              p <- p + geom_bar(aes_string(x = x_var), fill = "steelblue")
            } else {
              p <- p + geom_bar(aes_string(x = x_var, fill = group_var), 
                             position = input$bar_position)
            }
            p <- p + labs(y = "Count")
          } else {
            # Value-based bar chart
            if (group_var == "") {
              p <- p + geom_bar(aes_string(x = x_var, y = y_var), 
                             stat = "identity", fill = "steelblue")
            } else {
              p <- p + geom_bar(aes_string(x = x_var, y = y_var, fill = group_var), 
                             stat = "identity", position = input$bar_position)
            }
          }
        } else if (plot_type == "line") {
          if (group_var == "") {
            p <- p + geom_line(aes_string(x = x_var, y = y_var), color = "steelblue")
            if (input$line_points) {
              p <- p + geom_point(aes_string(x = x_var, y = y_var), color = "steelblue")
            }
          } else {
            p <- p + geom_line(aes_string(x = x_var, y = y_var, color = group_var))
            if (input$line_points) {
              p <- p + geom_point(aes_string(x = x_var, y = y_var, color = group_var))
            }
          }
          
          if (input$line_smooth) {
            if (group_var == "") {
              p <- p + geom_smooth(aes_string(x = x_var, y = y_var), 
                                color = "red", se = FALSE)
            } else {
              p <- p + geom_smooth(aes_string(x = x_var, y = y_var, color = group_var), 
                                se = FALSE)
            }
          }
        } else if (plot_type == "scatter") {
          if (group_var == "") {
            p <- p + geom_point(aes_string(x = x_var, y = y_var), 
                             color = "steelblue", alpha = input$scatter_alpha)
          } else {
            p <- p + geom_point(aes_string(x = x_var, y = y_var, color = group_var), 
                             alpha = input$scatter_alpha)
          }
          
          if (input$scatter_trendline) {
            if (group_var == "") {
              p <- p + geom_smooth(aes_string(x = x_var, y = y_var), 
                                method = "lm", color = "red", se = TRUE)
            } else {
              p <- p + geom_smooth(aes_string(x = x_var, y = y_var, color = group_var), 
                                method = "lm", se = TRUE)
            }
          }
        } else if (plot_type == "box") {
          if (x_var == "") {
            # Single boxplot
            p <- p + geom_boxplot(aes_string(y = y_var, x = 1), 
                               notch = input$box_notch, fill = "steelblue")
            p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            p <- p + labs(x = "")
          } else {
            # Multiple boxplots by category
            if (group_var == "") {
              p <- p + geom_boxplot(aes_string(x = x_var, y = y_var), 
                                 notch = input$box_notch, fill = "steelblue")
            } else {
              p <- p + geom_boxplot(aes_string(x = x_var, y = y_var, fill = group_var), 
                                 notch = input$box_notch)
            }
          }
          
          if (input$box_points) {
            if (x_var == "") {
              p <- p + geom_jitter(aes_string(y = y_var, x = 1), 
                                alpha = 0.5, width = 0.2)
            } else {
              if (group_var == "") {
                p <- p + geom_jitter(aes_string(x = x_var, y = y_var), 
                                  alpha = 0.5, width = 0.2)
              } else {
                p <- p + geom_jitter(aes_string(x = x_var, y = y_var, color = group_var), 
                                  alpha = 0.5, width = 0.2)
              }
            }
          }
        } else if (plot_type == "violin") {
          if (x_var == "") {
            # Single violin plot
            p <- p + geom_violin(aes_string(y = y_var, x = 1), fill = "steelblue")
            p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            p <- p + labs(x = "")
          } else {
            # Multiple violin plots by category
            if (group_var == "") {
              p <- p + geom_violin(aes_string(x = x_var, y = y_var), fill = "steelblue")
            } else {
              p <- p + geom_violin(aes_string(x = x_var, y = y_var, fill = group_var))
            }
          }
          
          if (input$violin_box) {
            if (x_var == "") {
              p <- p + geom_boxplot(aes_string(y = y_var, x = 1), 
                                 width = 0.1, alpha = 0.7)
            } else {
              p <- p + geom_boxplot(aes_string(x = x_var, y = y_var), 
                                 width = 0.1, alpha = 0.7)
            }
          }
        } else if (plot_type == "density") {
          if (group_var == "") {
            p <- p + geom_density(aes_string(x = x_var), 
                               fill = "steelblue", alpha = input$density_alpha)
          } else {
            p <- p + geom_density(aes_string(x = x_var, fill = group_var), 
                               alpha = input$density_alpha)
          }
          
          if (input$density_rug) {
            if (group_var == "") {
              p <- p + geom_rug(aes_string(x = x_var), alpha = 0.5)
            } else {
              p <- p + geom_rug(aes_string(x = x_var, color = group_var), alpha = 0.5)
            }
          }
        }
        
        # Add title and theme
        p <- p + labs(title = title) + theme_minimal()
        
        # Save the plot
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      }
    )
    
    # Filter variable selection
    output$filter_var_select <- renderUI({
      req(filtered_data())
      ns <- session$ns
      
      data <- filtered_data()
      
      selectInput(
        ns("filter_var"),
        "Select Variable to Filter:",
        choices = names(data),
        selected = names(data)[1]
      )
    })
    
    # Filter condition based on variable type
    output$filter_condition <- renderUI({
      req(filtered_data(), input$filter_var)
      ns <- session$ns
      
      data <- filtered_data()
      var <- input$filter_var
      
      if (is.numeric(data[[var]])) {
        # Numeric variable conditions
        selectInput(
          ns("filter_condition"),
          "Condition:",
          choices = c(
            "Equal to (=)" = "eq",
            "Not equal to (!=)" = "neq",
            "Greater than (>)" = "gt",
            "Greater than or equal to (>=)" = "gte",
            "Less than (<)" = "lt",
            "Less than or equal to (<=)" = "lte",
            "Between" = "between",
            "Is NA" = "is_na",
            "Is not NA" = "not_na"
          ),
          selected = "gt"
        )
      } else {
        # Categorical variable conditions
        selectInput(
          ns("filter_condition"),
          "Condition:",
          choices = c(
            "Equal to (=)" = "eq",
            "Not equal to (!=)" = "neq",
            "Contains" = "contains",
            "Starts with" = "starts_with",
            "Ends with" = "ends_with",
            "Is NA" = "is_na",
            "Is not NA" = "not_na"
          ),
          selected = "eq"
        )
      }
    })
    
    # Filter value based on variable type and condition
    output$filter_value <- renderUI({
      req(filtered_data(), input$filter_var, input$filter_condition)
      ns <- session$ns
      
      data <- filtered_data()
      var <- input$filter_var
      condition <- input$filter_condition
      
      if (condition %in% c("is_na", "not_na")) {
        # No value needed for these conditions
        return(NULL)
      }
      
      if (is.numeric(data[[var]])) {
        # For numeric variables
        if (condition == "between") {
          # Range input for "between" condition
          min_val <- min(data[[var]], na.rm = TRUE)
          max_val <- max(data[[var]], na.rm = TRUE)
          
          tagList(
            sliderInput(
              ns("filter_range"),
              "Range:",
              min = min_val,
              max = max_val,
              value = c(min_val + (max_val - min_val) * 0.25, 
                        min_val + (max_val - min_val) * 0.75),
              step = (max_val - min_val) / 100
            )
          )
        } else {
          # Single value for other conditions
          numericInput(
            ns("filter_value"),
            "Value:",
            value = median(data[[var]], na.rm = TRUE)
          )
        }
      } else {
        # For categorical variables
        if (is.factor(data[[var]]) || length(unique(data[[var]])) <= 10) {
          # For factors or variables with few unique values, use select input
          selectInput(
            ns("filter_value"),
            "Value:",
            choices = sort(unique(data[[var]][!is.na(data[[var]])])),
            selected = unique(data[[var]][!is.na(data[[var]])])[1]
          )
        } else {
          # For other variables, use text input
          textInput(
            ns("filter_value"),
            "Value:",
            value = ""
          )
        }
      }
    })
    
    # Add filter
    observeEvent(input$add_filter, {
      req(filtered_data(), input$filter_var, input$filter_condition)
      
      data <- filtered_data()
      var <- input$filter_var
      condition <- input$filter_condition
      
      # Get current filters
      current_filters <- filters()
      
      # Create new filter
      new_filter <- list(
        var = var,
        condition = condition
      )
      
      # Add value if needed
      if (!condition %in% c("is_na", "not_na")) {
        if (condition == "between") {
          new_filter$value <- input$filter_range
        } else {
          new_filter$value <- input$filter_value
        }
      }
      
      # Add unique ID for the filter
      new_filter$id <- paste0("filter_", length(current_filters) + 1)
      
      # Add to filters list
      current_filters[[length(current_filters) + 1]] <- new_filter
      filters(current_filters)
      
      # Apply filters to data
      apply_filters()
    })
    
    # Clear all filters
    observeEvent(input$clear_filters, {
      filters(list())
      filtered_data(dataset())
    })
    
    # Apply filters function
    apply_filters <- function() {
      req(dataset())
      
      data <- dataset()
      current_filters <- filters()
      
      if (length(current_filters) == 0) {
        filtered_data(data)
        return()
      }
      
      # Apply each filter
      for (filter in current_filters) {
        var <- filter$var
        condition <- filter$condition
        
        if (!(var %in% names(data))) {
          next  # Skip if variable not in data
        }
        
        if (condition == "eq") {
          # Equal to
          data <- data[data[[var]] == filter$value, ]
        } else if (condition == "neq") {
          # Not equal to
          data <- data[data[[var]] != filter$value, ]
        } else if (condition == "gt") {
          # Greater than
          data <- data[data[[var]] > as.numeric(filter$value), ]
        } else if (condition == "gte") {
          # Greater than or equal to
          data <- data[data[[var]] >= as.numeric(filter$value), ]
        } else if (condition == "lt") {
          # Less than
          data <- data[data[[var]] < as.numeric(filter$value), ]
        } else if (condition == "lte") {
          # Less than or equal to
          data <- data[data[[var]] <= as.numeric(filter$value), ]
        } else if (condition == "between") {
          # Between
          data <- data[data[[var]] >= filter$value[1] & data[[var]] <= filter$value[2], ]
        } else if (condition == "contains") {
          # Contains (for character/factor)
          data <- data[grepl(filter$value, data[[var]], fixed = TRUE), ]
        } else if (condition == "starts_with") {
          # Starts with (for character/factor)
          data <- data[grepl(paste0("^", filter$value), data[[var]]), ]
        } else if (condition == "ends_with") {
          # Ends with (for character/factor)
          data <- data[grepl(paste0(filter$value, "$"), data[[var]]), ]
        } else if (condition == "is_na") {
          # Is NA
          data <- data[is.na(data[[var]]), ]
        } else if (condition == "not_na") {
          # Is not NA
          data <- data[!is.na(data[[var]]), ]
        }
      }
      
      filtered_data(data)
    }
    
    # Active filters display
    output$active_filters <- renderUI({
      current_filters <- filters()
      
      if (length(current_filters) == 0) {
        return(tags$p("No active filters."))
      }
      
      # Create list of filter descriptions
      filter_items <- lapply(seq_along(current_filters), function(i) {
        filter <- current_filters[[i]]
        
        # Create readable description of the filter
        condition_text <- switch(
          filter$condition,
          "eq" = "=",
          "neq" = "!=",
          "gt" = ">",
          "gte" = ">=",
          "lt" = "<",
          "lte" = "<=",
          "between" = "between",
          "contains" = "contains",
          "starts_with" = "starts with",
          "ends_with" = "ends with",
          "is_na" = "is NA",
          "not_na" = "is not NA"
        )
        
        if (filter$condition == "between") {
          value_text <- paste(filter$value[1], "and", filter$value[2])
        } else if (filter$condition %in% c("is_na", "not_na")) {
          value_text <- ""
        } else {
          value_text <- as.character(filter$value)
        }
        
        description <- paste0(
          filter$var, " ", 
          condition_text, 
          if (value_text != "") paste0(" ", value_text)
        )
        
        # Create list item with description and remove button
        tags$div(
          class = "filter-item",
          style = "margin-bottom: 5px; padding: 5px; background-color: #f8f9fa; border-radius: 3px;",
          tags$span(description),
          tags$button(
            class = "btn btn-xs btn-danger",
            style = "margin-left: 10px;",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});", 
                             session$ns("remove_filter"), filter$id),
            icon("times"),
            "Remove"
          )
        )
      })
      
      # Return all filter items
      do.call(tagList, filter_items)
    })
    
    # Remove filter
    observeEvent(input$remove_filter, {
      filter_id <- input$remove_filter
      current_filters <- filters()
      
      # Find and remove the filter with matching ID
      new_filters <- list()
      for (filter in current_filters) {
        if (filter$id != filter_id) {
          new_filters[[length(new_filters) + 1]] <- filter
        }
      }
      
      filters(new_filters)
      
      # Apply updated filters
      apply_filters()
    })
    
    # Filtered data display
    output$filtered_data <- renderDT({
      req(filtered_data())
      
      datatable(
        head(filtered_data(), 100),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = FALSE
      )
    })
    
    # Download filtered data
    output$download_filtered_data <- downloadHandler(
      filename = function() {
        "filtered_data.csv"
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
    
    # Return filtered data for other modules
    return(reactive({
      req(filtered_data())
      filtered_data()
    }))
  })
}
