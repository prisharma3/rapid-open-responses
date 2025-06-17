# build-site.R - Main processing script for RAPID Open Responses
# This script processes survey data and builds the website automatically

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

# 2. Process zipcode data
cat("Processing geocoding data...\n")
zipcode_data <- get_zipcode_data()

# 3. Process household (EC) data
cat("Processing household data...\n")
ec_response_table <- process_ec_data(master_data$ec_master, zipcode_data)

# 4. Process provider (CC) data  
cat("Processing provider data...\n")
cc_response_table <- process_cc_data(master_data$cc_master, zipcode_data)

# 5. Get question lists
ec_questions <- get_ec_questions(master_data$ec_master)
cc_questions <- get_cc_questions(master_data$cc_master)

# 6. Generate site.yml automatically
cat("Generating site navigation...\n")
generate_site_yml(ec_questions, cc_questions)

# 7. Generate response tables
cat("Generating household response tables...\n")
generate_response_reports("ec", ec_questions, ec_response_table)

cat("Generating provider response tables...\n")
generate_response_reports("cc", cc_questions, cc_response_table)

# 8. Build the website
cat("Building website...\n")
rmarkdown::render_site()

cat("RAPID Open Responses website built successfully!\n")