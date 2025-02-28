# modules/help.R
# Module for application help and user guide

# UI Module
helpUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "User Guide",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        
        tabsetPanel(
          id = ns("help_tabs"),
          
          # Overview tab
          tabPanel(
            "Overview",
            tags$div(
              class = "help-section",
              tags$h3("Data Science Toolkit", class = "help-title"),
              tags$p(
                "Welcome to the Data Science Toolkit, an interactive web application for data exploration, 
                preprocessing, and analysis. This tool allows you to upload your own datasets, clean and 
                transform data, engineer new features, and create interactive visualizations to gain insights."
              ),
              tags$p(
                "Use the tabs in the sidebar to navigate between different functionalities of the application:"
              ),
              tags$ul(
                tags$li(tags$strong("Data Loading:"), "Upload your own dataset or use a built-in dataset"),
                tags$li(tags$strong("Data Cleaning:"), "Clean and preprocess your data, handle missing values and outliers"),
                tags$li(tags$strong("Feature Engineering:"), "Create new features and transform existing ones"),
                tags$li(tags$strong("Exploratory Data Analysis:"), "Visualize and explore your data through charts and statistics"),
                tags$li(tags$strong("Help:"), "Access this user guide")
              ),
              tags$p(
                "Each module is designed to work in sequence, but you can also use them independently. 
                Changes made in one module will be carried forward to the next."
              )
            )
          ),
          
          # Data Loading tab
          tabPanel(
            "Data Loading",
            tags$div(
              class = "help-section",
              tags$h3("Data Loading Module", class = "help-title"),
              
              tags$h4("Uploading Your Data"),
              tags$p("You can upload your own dataset in multiple formats:"),
              tags$ul(
                tags$li(tags$strong("CSV:"), "Comma-separated values file (.csv)"),
                tags$li(tags$strong("Excel:"), "Microsoft Excel files (.xlsx, .xls)"),
                tags$li(tags$strong("JSON:"), "JavaScript Object Notation file (.json)"),
                tags$li(tags$strong("RDS:"), "R serialized data file (.rds)")
              ),
              
              tags$h4("Using Built-in Datasets"),
              tags$p(
                "If you don't have your own dataset, you can use one of the built-in datasets to explore 
                the functionality of the application:"
              ),
              tags$ul(
                tags$li(tags$strong("Diamonds:"), "A dataset containing prices and other attributes of nearly 54,000 diamonds"),
                tags$li(tags$strong("Iris:"), "A classic dataset containing measurements for 150 iris flowers from three species")
              ),
              
              tags$h4("Data Preview and Summary"),
              tags$p(
                "After loading a dataset, you can view a preview of the data and a statistical summary. 
                This helps you understand the structure and content of your dataset before proceeding to 
                the data cleaning and analysis stages."
              ),
              
              tags$div(
                class = "help-tips",
                tags$h4("Tips:"),
                tags$ul(
                  tags$li("Make sure your CSV files have proper column headers"),
                  tags$li("Check the preview to ensure data was loaded correctly"),
                  tags$li("Review the data summary to identify potential issues like missing values"),
                  tags$li("You can download the current dataset at any point using the 'Download Current Data' button")
                )
              )
            )
          ),
          
          # Data Cleaning tab
          tabPanel(
            "Data Cleaning",
            tags$div(
              class = "help-section",
              tags$h3("Data Cleaning Module", class = "help-title"),
              
              tags$h4("Variable Selection"),
              tags$p(
                "Select which variables you want to keep in your dataset. This is useful for removing 
                irrelevant or redundant variables."
              ),
              
              tags$h4("Handling Duplicate Rows"),
              tags$p(
                "Identify and remove duplicate rows in your dataset. The module shows the number of 
                duplicates found and allows you to preview them before removal."
              ),
              
              tags$h4("Managing Missing Values"),
              tags$p("Several strategies are available for handling missing values:"),
              tags$ul(
                tags$li(tags$strong("Remove rows:"), "Delete rows containing missing values"),
                tags$li(tags$strong("Fill with mean/mode:"), "Replace missing values with the mean (for numeric variables) or mode (for categorical variables)"),
                tags$li(tags$strong("Fill with median:"), "Replace missing values with the median (for numeric variables)"),
                tags$li(tags$strong("Fill with custom value:"), "Replace missing values with a user-specified value")
              ),
              
              tags$h4("Data Transformations"),
              tags$p("Apply various transformations to improve your data quality:"),
              tags$ul(
                tags$li(
                  tags$strong("Scaling:"), "Normalize numeric variables using methods like Min-Max scaling, 
                  Z-score standardization, or robust scaling"
                ),
                tags$li(
                  tags$strong("Categorical Encoding:"), "Convert categorical variables into numeric form using 
                  one-hot encoding or label encoding"
                ),
                tags$li(
                  tags$strong("Outlier Handling:"), "Detect and manage outliers using methods like IQR or Z-score, 
                  with options to cap, remove, or replace outliers"
                )
              ),
              
              tags$div(
                class = "help-tips",
                tags$h4("Tips:"),
                tags$ul(
                  tags$li("Start with variable selection to reduce complexity"),
                  tags$li("Handle missing values before applying transformations"),
                  tags$li("Consider the impact of removing rows with missing data on your sample size"),
                  tags$li("Choose scaling methods based on your data distribution and analysis goals"),
                  tags$li("Review the data preview after each transformation to ensure expected results")
                )
              )
            )
          ),
          
          # Feature Engineering tab
          tabPanel(
            "Feature Engineering",
            tags$div(
              class = "help-section",
              tags$h3("Feature Engineering Module", class = "help-title"),
              
              tags$h4("Creating New Features"),
              tags$p(
                "Create new variables derived from existing ones through various operations:"
              ),
              tags$ul(
                tags$li(tags$strong("Arithmetic Operations:"), "Combine variables using addition, subtraction, multiplication, or division"),
                tags$li(tags$strong("Math Functions:"), "Apply mathematical functions like square, square root, logarithm, etc."),
                tags$li(tags$strong("Custom Formulas:"), "Create complex features using custom R expressions")
              ),
              
              tags$h4("Polynomial Features"),
              tags$p(
                "Generate polynomial terms from existing numeric variables (e.g., squared, cubed terms) 
                to capture non-linear relationships."
              ),
              
              tags$h4("Interaction Terms"),
              tags$p(
                "Create interaction features by multiplying two variables together, useful for 
                capturing combined effects."
              ),
              
              tags$h4("Binning"),
              tags$p(
                "Transform continuous variables into categorical bins using methods like:"
              ),
              tags$ul(
                tags$li(tags$strong("Equal Width:"), "Divide the range into equal intervals"),
                tags$li(tags$strong("Equal Frequency:"), "Create bins with approximately equal number of observations"),
                tags$li(tags$strong("Custom Breaks:"), "Define your own breakpoints")
              ),
              
              tags$h4("Date Features"),
              tags$p(
                "Extract components from date variables such as year, month, day, weekday, etc."
              ),
              
              tags$h4("Text Features"),
              tags$p(
                "Extract features from text columns like character count, word count, presence of 
                special characters, etc."
              ),
              
              tags$div(
                class = "help-tips",
                tags$h4("Tips:"),
                tags$ul(
                  tags$li("Feature engineering should be guided by domain knowledge and the analysis goals"),
                  tags$li("Check the 'Feature Impact' visualization to see which features might be most important"),
                  tags$li("Be cautious about creating too many features, which can lead to overfitting"),
                  tags$li("Document your feature engineering steps for reproducibility"),
                  tags$li("Use meaningful names for your created features")
                )
              )
            )
          ),
          
          # EDA tab
          tabPanel(
            "Exploratory Data Analysis",
            tags$div(
              class = "help-section",
              tags$h3("Exploratory Data Analysis (EDA) Module", class = "help-title"),
              
              tags$h4("Summary Statistics"),
              tags$p(
                "View detailed statistical summaries of your variables, including measures of central 
                tendency, dispersion, and distribution characteristics."
              ),
              
              tags$h4("Distribution Analysis"),
              tags$p(
                "Visualize the distribution of individual variables using various plot types:"
              ),
              tags$ul(
                tags$li(tags$strong("For numeric variables:"), "Histograms, density plots, box plots, violin plots"),
                tags$li(tags$strong("For categorical variables:"), "Bar charts, pie charts")
              ),
              tags$p(
                "You can also group these visualizations by a categorical variable to compare distributions 
                across different groups."
              ),
              
              tags$h4("Correlation Analysis"),
              tags$p(
                "Explore relationships between numeric variables through correlation matrices, with 
                options for different correlation methods (Pearson, Spearman, Kendall) and thresholds."
              ),
              
              tags$h4("Scatter Analysis"),
              tags$p(
                "Create scatter plots to examine relationships between two numeric variables, with 
                options to color points by a third variable and add trend lines."
              ),
              
              tags$h4("Multivariate Analysis"),
              tags$p(
                "Use pairs plots to visualize relationships among multiple variables simultaneously."
              ),
              
              tags$h4("Custom Visualization"),
              tags$p(
                "Build custom plots with various options, allowing you to tailor visualizations to 
                your specific analysis needs."
              ),
              
              tags$h4("Data Filtering"),
              tags$p(
                "Apply filters to focus your analysis on specific subsets of your data, with options 
                for multiple filter conditions and easy management of active filters."
              ),
              
              tags$div(
                class = "help-tips",
                tags$h4("Tips:"),
                tags$ul(
                  tags$li("Begin with univariate analysis (distribution analysis) before exploring relationships"),
                  tags$li("Use correlation analysis to identify potential predictive relationships"),
                  tags$li("Apply filters to examine how patterns change across different subsets"),
                  tags$li("Download plots for reports or presentations using the download buttons"),
                  tags$li("Combine different visualization types to get a comprehensive understanding of your data")
                )
              )
            )
          ),
          
          # Advanced Tips tab
          tabPanel(
            "Advanced Tips",
            tags$div(
              class = "help-section",
              tags$h3("Advanced Tips and Best Practices", class = "help-title"),
              
              tags$h4("Workflow Optimization"),
              tags$ul(
                tags$li(
                  "Follow a logical sequence: Load → Clean → Engineer Features → Analyze. 
                  This ensures each step builds on properly prepared data."
                ),
                tags$li(
                  "Save intermediate datasets at key points using the download buttons to maintain 
                  a record of your data processing steps."
                ),
                tags$li(
                  "Experiment with different cleaning and transformation approaches to see which 
                  yields the most insight."
                )
              ),
              
              tags$h4("Performance Considerations"),
              tags$ul(
                tags$li("Large datasets (>100,000 rows) may cause performance slowdowns."),
                tags$li(
                  "If working with large data, consider sampling your data first or focusing on 
                  specific subsets using the filtering functionality."
                ),
                tags$li(
                  "Complex visualizations with many variables or groupings may take longer to render."
                )
              ),
              
              tags$h4("Data Analysis Strategy"),
              tags$ul(
                tags$li(
                  "Start with simple univariate analysis to understand individual variables before 
                  exploring relationships."
                ),
                tags$li(
                  "Use the correlation analysis to prioritize which relationships to investigate 
                  further with detailed visualizations."
                ),
                tags$li(
                  "When creating new features, focus on those that have theoretical justification or 
                  clear potential value."
                ),
                tags$li(
                  "Document your analysis steps and findings for reproducibility and sharing."
                )
              ),
              
              tags$h4("Common Issues and Solutions"),
              tags$ul(
                tags$li(
                  tags$strong("Handling date formats:"), "If dates aren't properly recognized, try 
                  different formats or convert them manually using feature engineering."
                ),
                tags$li(
                  tags$strong("Memory limitations:"), "If you encounter memory errors, try reducing 
                  the number of variables or observations."
                ),
                tags$li(
                  tags$strong("Categorical variables with many levels:"), "Consider grouping less 
                  frequent categories using the feature engineering module."
                ),
                tags$li(
                  tags$strong("Visualization clarity:"), "If plots are cluttered, use filters to focus 
                  on key subsets of data or simplify by selecting fewer variables."
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server Module (minimal since this is mostly a static module)
helpServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # This module is primarily informational with static content,
    # so the server-side logic is minimal.
    
    # We could add reactive elements here if needed in the future,
    # such as context-sensitive help or tutorials.
  })
}
