library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bs4Dash)
library(tidyverse)
library(DT)
library(plotly)
library(fresh)  # For custom theming

source("R/utils/data_processing.R")
source("R/utils/calculations.R")
source("R/utils/plot_functions.R")

# Load data from REDCap APIs
cat("Loading data from REDCap...\n")
faculty_redcap_data <- download_faculty_data()
rdm_redcap_data <- download_rdm_focused()
cat("✓ Data loaded successfully!\n")
