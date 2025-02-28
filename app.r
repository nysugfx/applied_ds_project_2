# app.R - Main application file
# Integrates all modules for the Data Science Toolkit

# Source modules after loading global.R (which loads packages)
source("global.R")

# Source module files
source("modules/data_loading.R")
source("modules/data_cleaning.R")
source("modules/feature_eng.R")
source("modules/eda.R")
source("modules/help.R")

# UI Definition
ui <- dashboardPage(
  # Application header
  dashboardHeader(
    title = app_title,
    titleWidth = 300
  ),
  
  # Sidebar with navigation menu
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Loading", tabName = "data_loading", icon = icon("upload")),
      menuItem("Data Cleaning", tabName = "data_cleaning", icon = icon("broom")),
      menuItem("Feature Engineering", tabName = "feature_eng", icon = icon("cogs")),
      menuItem("Exploratory Analysis", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      
      # Version and info at bottom of sidebar
      tags$div(
        class = "sidebar-footer",
        style = "position: absolute; bottom: 0; padding: 10px; width: 100%; text-align: center; color: #b8c7ce;",
        paste("Version", app_version),
        tags$br(),
        "© 2025 Data Science Toolkit"
      )
    )
  ),
  
  # Main content area
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # Use shinyjs for advanced JavaScript operations
    useShinyjs(),
    
    # Tabs correspond to menu items
    tabItems(
      # Data Loading tab
      tabItem(
        tabName = "data_loading",
        h2("Data Loading and Preview"),
        dataLoadingUI("dataLoading")
      ),
      
      # Data Cleaning tab
      tabItem(
        tabName = "data_cleaning",
        h2("Data Cleaning and Preprocessing"),
        dataCleaningUI("dataCleaning")
      ),
      
      # Feature Engineering tab
      tabItem(
        tabName = "feature_eng",
        h2("Feature Engineering"),
        featureEngUI("featureEng")
      ),
      
      # Exploratory Data Analysis tab
      tabItem(
        tabName = "eda",
        h2("Exploratory Data Analysis"),
        edaUI("eda")
      ),
      
      # Help tab
      tabItem(
        tabName = "help",
        h2("Help and Documentation"),
        helpUI("help")
      )
    )
  ),
  
  # Set skin color
  skin = "blue"
)

# Server Definition
server <- function(input, output, session) {
  # Initialize the data flow between modules
  
  # Data Loading Module
  loaded_data <- dataLoadingServer("dataLoading")
  
  # Data Cleaning Module (depends on loaded_data)
  cleaned_data <- dataCleaningServer("dataCleaning", loaded_data)
  
  # Feature Engineering Module (depends on cleaned_data)
  engineered_data <- featureEngServer("featureEng", cleaned_data)
  
  # EDA Module (depends on engineered_data)
  eda_data <- edaServer("eda", engineered_data)
  
  # Help Module (mostly static content)
  helpServer("help")
  
  # Show welcome modal on startup
  observe({
    # Only show once per session
    session$userData$welcome_shown <- session$userData$welcome_shown || FALSE
    
    if (!session$userData$welcome_shown) {
      showModal(
        modalDialog(
          title = "Welcome to the Data Science Toolkit",
          tags$div(
            tags$p(
              "This application allows you to explore, clean, transform, and visualize your data interactively."
            ),
            tags$p(
              "Start by loading a dataset in the 'Data Loading' tab, or navigate to the 'Help' section for detailed instructions."
            ),
            tags$p(
              "Each module builds on the previous one, so follow the tabs from left to right for a complete workflow."
            )
          ),
          footer = tagList(
            actionButton("welcome_ok", "Get Started", class = "btn-primary")
          ),
          easyClose = TRUE,
          size = "m"
        )
      )
      session$userData$welcome_shown <- TRUE
    }
  })
  
  # Close welcome modal when button is clicked
  observeEvent(input$welcome_ok, {
    removeModal()
  })
  
  # Handle sidebar navigation
  observeEvent(input$sidebar, {
    # Logic to ensure proper flow between tabs
    current_tab <- input$sidebar
    
    if (current_tab == "data_cleaning" && is.null(loaded_data())) {
      showNotification(
        "Please load a dataset first",
        type = "warning"
      )
      updateTabItems(session, "sidebar", "data_loading")
    } else if (current_tab == "feature_eng" && is.null(cleaned_data())) {
      showNotification(
        "Please complete data cleaning first",
        type = "warning"
      )
      updateTabItems(session, "sidebar", "data_cleaning")
    } else if (current_tab == "eda" && is.null(engineered_data())) {
      showNotification(
        "Please complete feature engineering first",
        type = "warning"
      )
      updateTabItems(session, "sidebar", "feature_eng")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
