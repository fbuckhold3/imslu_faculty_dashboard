library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

source("R/utils/data_processing.R")
source("R/utils/calculations.R")
source("R/utils/plot_functions.R")

# Load data from REDCap APIs
cat("Loading data from REDCap...\n")
faculty_redcap_data <- download_faculty_data()
rdm_redcap_data <- download_rdm_focused(faculty_data = faculty_redcap_data)
cat("✓ Data loaded successfully!\n")