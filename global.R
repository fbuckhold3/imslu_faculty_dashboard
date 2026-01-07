library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

source("R/utils/data_processing.R")

# Load cached test data (faster during development)
cat("Loading data...\n")
test_data <- load_test_data()
faculty_redcap_data <- test_data$faculty
rdm_redcap_data <- test_data$rdm
cat("✓ Data loaded!\n")