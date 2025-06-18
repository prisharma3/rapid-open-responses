# build-site.R - Main processing script for RAPID Open Responses
library(here)
library(haven)
library(tidyverse)
library(zoo)
library(DT)
library(lubridate)
library(yaml)
library(rmarkdown)

cat("Starting RAPID Open Responses processing...\n")

# Source utility functions
source("R/functions.R")

# 1. Load and process data
cat("Loading data files...\n")
master_data <- load_master_files()

# 3. Process household (EC) data
cat("Processing household data...\n")
ec_response_table <- process_ec_data(master_data$ec_master)

# 4. Process provider (CC) data  
cat("Processing provider data...\n")
cc_response_table <- process_cc_data(master_data$cc_master)

cat("Data processing completed successfully!\n")