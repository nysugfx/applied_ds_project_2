# global.R - Load packages and define global variables

# Load required packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(readxl)
library(jsonlite)
library(data.table)
library(corrplot)
library(caret)
library(shinyjs)
library(shinyWidgets)

# Global variables
app_title <- "Data Science Toolkit"
app_version <- "1.0"

# Define available built-in datasets with descriptions
built_in_datasets <- list(
  "diamonds" = list(
    name = "Diamonds Dataset",
    description = "A dataset containing the prices and other attributes of almost 54,000 diamonds.",
    path = "data/diamonds.rds"
  ),
  "iris" = list(
    name = "Iris Dataset",
    description = "Famous dataset containing measurements for 150 flowers from three species of Iris.",
    path = "data/iris.rds"
  )
)

# Function to prepare built-in datasets
prepare_built_in_datasets <- function() {
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Save diamonds dataset if it doesn't exist
  if (!file.exists("data/diamonds.rds")) {
    data(diamonds, package = "ggplot2")
    saveRDS(diamonds, "data/diamonds.rds")
  }
  
  # Save iris dataset if it doesn't exist
  if (!file.exists("data/iris.rds")) {
    data(iris)
    saveRDS(iris, "data/iris.rds")
  }
}

# Call the function to prepare datasets
prepare_built_in_datasets()
