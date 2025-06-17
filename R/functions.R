# functions.R - Helper functions for RAPID Open Responses processing

# Load master data files
load_master_files <- function() {
    cat("Loading master files...\n")
    
    # Find all EC master files (2020-2024)
    ec_files <- list.files("data", pattern = "MasterFile_groupings_\\d{4}\\.sav$", full.names = TRUE)
    
    # Find all CC master files (2021-2024) 
    cc_files <- list.files("data", pattern = "CC\\.MasterFile_groupings_\\d{4}\\.sav$", full.names = TRUE)
    
    # Load EC files
    ec_master <- NULL
    if (length(ec_files) > 0) {
        ec_data_list <- map(ec_files, ~{
            cat("Loading:", .x, "\n")
            read_sav(.x)
        })
        ec_master <- reduce(ec_data_list, full_join)
    }
    
    # Load CC files
    cc_master <- NULL
    if (length(cc_files) > 0) {
        cc_data_list <- map(cc_files, ~{
            cat("Loading:", .x, "\n") 
            read_sav(.x)
        })
        cc_master <- reduce(cc_data_list, full_join)
    }
    
    list(ec_master = ec_master, cc_master = cc_master)
}

# Get zipcode data for geocoding
get_zipcode_data <- function() {
    cat("Downloading zipcode data...\n")
    
    zipcode_url <- "http://download.geonames.org/export/zip/US.zip"
    temp <- tempfile()
    
    tryCatch({
        download.file(zipcode_url, temp)
        zipcodes <- read.table(unz(temp, "US.txt"), sep = "\t")
        names(zipcodes) <- c("CountryCode", "zip", "PlaceName", 
                             "AdminName1", "State", "AdminName2", "AdminCode2", 
                             "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")
        
        zipcodes %>%
            mutate(zip = case_when(
                nchar(as.character(zip)) == 5 ~ as.character(zip),
                nchar(as.character(zip)) == 4 ~ paste0("0", zip),
                nchar(as.character(zip)) == 3 ~ paste0("00", zip),
                TRUE ~ as.character(zip)
            ))
    }, finally = {
        unlink(temp)
    })
}

# Process household (EC) data
process_ec_data <- function(ec_master, zipcode_data) {
    cat("Processing EC response data...\n")
    
    if (is.null(ec_master)) {
        cat("No EC data found\n")
        return(NULL)
    }
    
    ec_master %>%
        mutate(
            zip = as.character(DEMO.001),
            # Convert Week to Month/Year (adjust base date as needed)
            date = as.Date("2020-03-01") + weeks(Week - 1),
            month_year = format(date, "%B %Y"),
            # Enhanced FPL categories
            fpl_category = case_when(
                FPL.2019.150 <= 2.0 ~ "≤ 200% FPL",
                FPL.2019.150 > 2.0 & FPL.2019.150 <= 4.0 ~ "200-400% FPL", 
                FPL.2019.150 > 4.0 ~ "> 400% FPL",
                TRUE ~ "Unknown"
            ),
            child_age03 = case_when(
                DEMO.004.a.2 >= 1 ~ "Yes",
                !is.na(DEMO.004.a.2) ~ "No",
                TRUE ~ NA_character_
            ),
            Language = case_when(
                UserLanguage == "EN" ~ "English", 
                UserLanguage == "SPA" ~ "Spanish",
                TRUE ~ "Other"
            )
        ) %>%
        left_join(select(zipcode_data, zip, State), by = "zip") %>%
        select(CaregiverID, Week, month_year, starts_with("OPEN"), 
               State, fpl_category, zip, Language, RaceGroup, child_age03) %>%
        filter(OPEN.006 == 1) %>%
        arrange(Week) %>%
        group_by(CaregiverID) %>%
        mutate_if(is.labelled, as_factor, levels = "labels") %>%
        mutate_at(vars(State, fpl_category, RaceGroup, child_age03, zip, Language, month_year), na.locf0) %>%
        ungroup() %>%
        select(-OPEN.006) %>%
        rename(`Child 0-3` = child_age03,
               `FPL Category` = fpl_category,
               `Month/Year` = month_year) %>%
        gather("Question", "Response", starts_with("OPEN")) %>%
        filter(Response != "" & !is.na(Response))
}

# Process provider (CC) data  
process_cc_data <- function(cc_master, zipcode_data) {
    cat("Processing CC response data...\n")
    
    if (is.null(cc_master)) {
        cat("No CC data found\n")
        return(NULL)
    }
    
    cc_master %>%
        mutate(
            zip = as.character(CC.DEMO.001),
            date = as.Date("2021-03-01") + weeks(Week - 1),
            month_year = format(date, "%B %Y"),
            fpl_category = case_when(
                FPL.2019.150 <= 2.0 ~ "≤ 200% FPL",
                FPL.2019.150 > 2.0 & FPL.2019.150 <= 4.0 ~ "200-400% FPL",
                FPL.2019.150 > 4.0 ~ "> 400% FPL", 
                TRUE ~ "Unknown"
            ),
            provider_type = case_when(
                CC.DEMO.013_1 == 1 ~ "Center teacher",
                CC.DEMO.013_2 == 1 ~ "Center director", 
                CC.DEMO.013_3 == 1 | CC.DEMO.013_4 == 1 | CC.DEMO.013_8 == 1 | 
                    CC.DEMO.013_12 == 1 | CC.DEMO.013_9 == 1 | CC.DEMO.013_13 == 1 ~ "Home-based provider",
                CC.DEMO.013_5 == 1 ~ "FFN",
                CC.DEMO.013_6 == 1 | CC.DEMO.013_10 == 1 | CC.DEMO.013_11 == 1 ~ "Babysitter/nanny",
                TRUE ~ "Other"
            ),
            Language = case_when(
                UserLanguage == "EN" ~ "English",
                UserLanguage == "SPA" ~ "Spanish", 
                TRUE ~ "Other"
            )
        ) %>%
        left_join(select(zipcode_data, zip, State), by = "zip") %>%
        select(ProviderID, Week, month_year, starts_with("CC.OPEN"),
               State, fpl_category, zip, RaceGroup, provider_type, Language) %>%
        filter(CC.OPEN.007 == 1) %>%
        arrange(Week) %>%
        group_by(ProviderID) %>%
        mutate_if(is.labelled, as_factor, levels = "labels") %>%
        mutate_at(vars(State, fpl_category, zip, RaceGroup, provider_type, Language, month_year), na.locf0) %>%
        ungroup() %>%
        select(-CC.OPEN.007) %>%
        rename(`FPL Category` = fpl_category,
               `Month/Year` = month_year,
               `Provider Type` = provider_type) %>%
        gather("Question", "Response", starts_with("CC.OPEN")) %>%
        filter(Response != "" & !is.na(Response))
}

# Get EC question list
get_ec_questions <- function(ec_master) {
    if (is.null(ec_master)) return(list(names = character(0), text = character(0), numbers = integer(0)))
    
    ec_questions <- ec_master %>% 
        select(starts_with("OPEN")) %>%
        select(-OPEN.006)  # Remove the filter variable
    
    q_text <- sjlabelled::get_label(ec_questions)
    q_names <- names(ec_questions)
    q_numbers <- seq_along(q_names)
    
    list(names = q_names, text = q_text, numbers = q_numbers)
}

# Get CC question list  
get_cc_questions <- function(cc_master) {
    if (is.null(cc_master)) return(list(names = character(0), text = character(0), numbers = integer(0)))
    
    cc_questions <- cc_master %>%
        select(starts_with("CC.OPEN")) %>%
        select(-CC.OPEN.007)  # Remove the filter variable
    
    q_text <- sjlabelled::get_label(cc_questions)
    q_names <- names(cc_questions)
    q_numbers <- seq_along(q_names)
    
    list(names = q_names, text = q_text, numbers = q_numbers)
}

# Generate _site.yml automatically
generate_site_yml <- function(ec_questions, cc_questions) {
    cat("Generating _site.yml file...\n")
    
    # Create household menu items
    household_items <- map2(ec_questions$numbers, ec_questions$text, ~{
        list(
            text = paste0("OPEN.", sprintf("%03d", .x), ": ", str_trunc(.y, 60)),
            href = paste0("ec_responses_", .x, ".html")
        )
    })
    
    # Create provider menu items
    provider_items <- map2(cc_questions$numbers, cc_questions$text, ~{
        list(
            text = paste0("CC.OPEN.", sprintf("%03d", .x), ": ", str_trunc(.y, 60)), 
            href = paste0("cc_responses_", .x, ".html")
        )
    })
    
    # Create site structure
    site_yml <- list(
        name = "RAPID Open-ended Questions",
        navbar = list(
            title = "Open-ended Questions",
            left = list(
                list(
                    text = "Household Response Tables",
                    menu = household_items
                ),
                list(
                    text = "Provider Response Tables", 
                    menu = provider_items
                )
            ),
            right = list(
                list(
                    icon = "fa-github",
                    href = "https://github.com/prisharma3/rapid-open-responses"
                )
            )
        ),
        output_dir = "_site"
    )
    
    # Write to _site.yml
    write_yaml(site_yml, "_site.yml")
}

# Generate response reports
generate_response_reports <- function(data_type, questions, response_data) {
    if (is.null(response_data) || length(questions$numbers) == 0) {
        cat("No data or questions found for", data_type, "\n")
        return()
    }
    
    template_file <- paste0("_template_responses_", toupper(data_type), ".Rmd")
    
    for (i in seq_along(questions$numbers)) {
        output_file <- paste0(data_type, "_responses_", questions$numbers[i], ".html")
        
        cat("Generating:", output_file, "\n")
        
        rmarkdown::render(
            input = template_file,
            output_file = output_file,
            params = list(
                title = paste("Question", questions$numbers[i]),
                question = questions$text[i],
                variable = questions$names[i],
                data = response_data
            ),
            quiet = TRUE
        )
    }
}