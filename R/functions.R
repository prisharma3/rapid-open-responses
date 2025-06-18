# functions.R - Helper functions for RAPID Open Responses processing

# Helper functions
moe.p = function(p, n){
    q = 1-p
    moe = 1.96*sqrt((p*q)/n)
    return(moe)
}

moe.m = function(sd,n){
    moe = 1.96*sd/sqrt(n)
    return(moe)
}

combine.cat = function(x, cols, id, newvar.name){
    subset = x[,c(id, "Week", cols)]
    names(subset) = c("id", "Week", paste0("X",1:length(cols)))
    subset = suppressWarnings(gather(subset, "key", "value", -id, -Week))
    subset = filter(subset, !is.na(value))
    subset$key = gsub("X", "", subset$key)
    subset = group_by(subset, id, Week)
    subset = summarise(subset, newvar = paste(key, collapse = ",")) %>% ungroup()
    names(subset) = c(id,"Week", newvar.name)
    x = suppressMessages(full_join(x, subset))
    return(x)
}

find_items = function(string, data2){
    items = names(data2)
    locations = which(grepl(string, items))
    final = items[locations]
    return(final)
}

my.max = function(x) ifelse (!all(is.na(x)), max(x,na.rm = TRUE), NA)
my.min = function(x) ifelse (!all(is.na(x)), min(x,na.rm = TRUE), NA)

monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

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


# Process household (EC) data with proper FPL calculation
process_ec_data <- function(ec_master) {
    cat("Processing EC response data...\n")
    
    if (is.null(ec_master)) {
        cat("No EC data found\n")
        return(NULL)
    }
    
    # Load FPL line data
    FPL_line <- read.csv("data/FPL_line.csv")
    
    # Calculate poverty status
    poverty <- ec_master %>%
        select(CaregiverID, Week, StartDate, allyearly2019, allyearly2020, allyearly2021, 
               Annual2022Income, Annual2023Income, Annual2024Income, Annual2025Income, 
               JOB.002, STATE_CODED) %>%
        group_by(CaregiverID) %>%
        summarise(allyearly2019 = my.max(allyearly2019),
                  allyearly2020 = my.max(allyearly2020),
                  allyearly2021 = my.max(allyearly2021),
                  allyearly2022 = my.max(Annual2022Income),
                  allyearly2023 = my.max(Annual2023Income),
                  allyearly2024 = my.max(Annual2024Income),
                  allyearly2025 = my.max(Annual2025Income),
                  nhousehold = my.max(JOB.002),
                  STATE_CODED = my.max(STATE_CODED)) %>%
        mutate(nhousehold = ifelse(nhousehold < 2, 2, nhousehold))
    
    poverty <- merge(x = poverty, y = FPL_line, by = c("nhousehold", "STATE_CODED"), all.x = T)
    
    poverty <- poverty %>%
        mutate(FPL_pct19 = allyearly2019/FPL_line_2019,
               FPL_pct20 = allyearly2020/FPL_line_2020,
               FPL_pct21 = allyearly2021/FPL_line_2021, 
               FPL_pct22 = allyearly2022/FPL_line_2022,
               FPL_pct23 = allyearly2023/FPL_line_2023,
               FPL_pct24 = allyearly2024/FPL_line_2024,
               FPL_pct25 = allyearly2025/FPL_line_2025,
               FPL_pct_merge = case_when(is.na(FPL_pct25) == F ~ FPL_pct25,
                                         is.na(FPL_pct25) == T & is.na(FPL_pct24) == F ~ FPL_pct24,
                                         is.na(FPL_pct25) == T & is.na(FPL_pct24) == T & is.na(FPL_pct23) == F ~ FPL_pct23,
                                         is.na(FPL_pct25) == T & is.na(FPL_pct24) == T & is.na(FPL_pct23) == T & is.na(FPL_pct22) == F ~ FPL_pct22,
                                         is.na(FPL_pct25) == T & is.na(FPL_pct24) == T & is.na(FPL_pct23) == T & is.na(FPL_pct22) == T & 
                                             is.na(FPL_pct21) == F ~ FPL_pct21,
                                         is.na(FPL_pct25) == T & is.na(FPL_pct24) == T & is.na(FPL_pct23) == T & is.na(FPL_pct22) == T & is.na(FPL_pct21) == T & 
                                             is.na(FPL_pct20) == F ~ FPL_pct20,
                                         is.na(FPL_pct25) == T & is.na(FPL_pct24) == T & is.na(FPL_pct23) == T & is.na(FPL_pct22) == T & is.na(FPL_pct21) == T & 
                                             is.na(FPL_pct20) == T & is.na(FPL_pct19) == F ~ FPL_pct19)) %>%
        mutate(poverty = case_when(FPL_pct_merge < 2 ~ "≤ 200% FPL",
                                   FPL_pct_merge >= 2 & FPL_pct_merge < 4 ~ "200-400% FPL", 
                                   FPL_pct_merge >= 4 ~ "> 400% FPL",
                                   TRUE ~ "Unknown")) %>%
        select(CaregiverID, poverty)
    
    # Process disability status
    ec_master <- combine.cat(x = ec_master, 
                             cols = find_items("HEALTH.005_[0-9]{1}$", ec_master), 
                             id = "CaregiverID",
                             newvar.name = "HEALTH.005_cat")
    
    ec_master <- ec_master %>%
        mutate(disability = case_when(HEALTH.005_1 == 1 ~ 1,
                                      HEALTH.005_2 == 1 ~ 1,
                                      HEALTH.005_3 == 1 ~ 1,
                                      HEALTH.005_4 == 1 ~ 1,
                                      HEALTH.005_5 == 1 ~ 0,
                                      HEALTH.005_997 == 1 ~ 1,
                                      HEALTH.005.2 == 1 ~ 1,
                                      HEALTH.005.2 == 0 ~ 0,
                                      TRUE ~ NA_real_))
    
    # Process family structure
    ec_master <- ec_master %>%
        mutate(single = case_when(Week < 9 & DEMO.002 %in% c(3,4,5,7,8) ~ 1, 
                                  Week < 9 & DEMO.002 %in% c(1, 2) ~ 0, 
                                  Week >= 9 & Week < 78 & DEMO.011 %in% c(2,4) ~ 1,
                                  Week >= 9 & Week < 78 & DEMO.011 %in% c(1) ~ 0,
                                  Week >= 78 & (DEMO.011.2_2 == 1 | DEMO.011.2_3 == 1 | DEMO.011.2_4 == 1 |
                                                    DEMO.011.2_5 == 1 | DEMO.011.2_6 == 1 | DEMO.011.2_7 == 1) ~ 1,
                                  Week >= 78 & DEMO.011.2_1 == 1 ~ 0,
                                  TRUE ~ NA_real_))
}

# Create demographic summary
master_dem <- ec_master %>%
    group_by(CaregiverID) %>%
    summarise(disability = my.max(disability),
              single = my.max(single)) %>%
    mutate(disability = case_when(disability == 1 ~ "With disability", 
                                  disability == 0 ~ "Without disability", 
                                  TRUE ~ NA_character_),
           single = case_when(single == 1 ~ "Non-dual parent", 
                              single == 0 ~ "Dual parents", 
                              TRUE ~ NA_character_))

# Main processing
ec_master %>%
    mutate(
        month_year = format(as.Date(StartDate), "%m/%Y"),
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
    left_join(poverty, by = "CaregiverID") %>%
    left_join(master_dem, by = "CaregiverID") %>%
    select(CaregiverID, Week, month_year, starts_with("OPEN"), 
           poverty, Language, RaceGroup, child_age03, disability, single) %>%
    filter(OPEN.006 == 1) %>%
    arrange(Week) %>%
    group_by(CaregiverID) %>%
    mutate_if(is.labelled, as_factor, levels = "labels") %>%
    mutate_at(vars(poverty, RaceGroup, child_age03, Language, month_year, disability, single), na.locf0) %>%
    ungroup() %>%
    select(-OPEN.006) %>%
    rename(`Child 0-3` = child_age03,
           `FPL Category` = poverty,
           `Month/Year` = month_year,
           `Child Disability` = disability,
           `Family Structure` = single) %>%
    gather("Question", "Response", starts_with("OPEN")) %>%
    filter(Response != "" & !is.na(Response))


# Process provider (CC) data  
process_cc_data <- function(cc_master) {
    cat("Processing CC response data...\n")
    
    if (is.null(cc_master)) {
        cat("No CC data found\n")
        return(NULL)
    }
    
    # Load FPL line data
    FPL_line <- read.csv("data/FPL_line.csv")
    
    # Calculate poverty status for CC
    poverty <- cc_master %>%
        select(ProviderID, Week, StartDate, Annual2020Income, Annual2021Income, Annual2022Income, 
               Annual2023Income, Annual2024Income, Annual2025Income, CC.JOB.002.b, CC.JOB.003.b, STATE_CODED) %>%
        mutate(nhousehold = case_when(is.na(CC.JOB.003.b) == F ~ CC.JOB.003.b,
                                      is.na(CC.JOB.003.b) == T & is.na(CC.JOB.002.b) == F ~ CC.JOB.002.b)) %>%
        group_by(ProviderID) %>%
        summarise(Annual2020Income = my.max(Annual2020Income),
                  Annual2021Income = my.max(Annual2021Income),
                  Annual2022Income = my.max(Annual2022Income),
                  Annual2023Income = my.max(Annual2023Income),
                  Annual2024Income = my.max(Annual2024Income),
                  Annual2025Income = my.max(Annual2025Income),
                  nhousehold = my.max(nhousehold),
                  STATE_CODED = my.max(STATE_CODED))
    
    poverty <- merge(x = poverty, y = FPL_line, by = c("nhousehold", "STATE_CODED"), all.x = T)
    
    poverty <- poverty %>%
        mutate(FPL_pct20 = Annual2020Income/FPL_line_2020,
               FPL_pct21 = Annual2021Income/FPL_line_2021,
               FPL_pct22 = Annual2022Income/FPL_line_2022,
               FPL_pct23 = Annual2023Income/FPL_line_2023,
               FPL_pct24 = Annual2024Income/FPL_line_2024,
               FPL_pct25 = Annual2025Income/FPL_line_2025,
               FPL_pct_merge = case_when(is.na(FPL_pct25) == F ~ FPL_pct25,
                                         is.na(FPL_pct25) == T & is.na(FPL_pct24) == F ~ FPL_pct24,
                                         is.na(FPL_pct24) == T & is.na(FPL_pct23) == F ~ FPL_pct23,
                                         is.na(FPL_pct24) == T & is.na(FPL_pct23) == T & is.na(FPL_pct22) == F ~ FPL_pct22,
                                         is.na(FPL_pct24) == T & is.na(FPL_pct23) == T & is.na(FPL_pct22) == T & is.na(FPL_pct21) == F ~ FPL_pct21,
                                         is.na(FPL_pct24) == T & is.na(FPL_pct23) == T & is.na(FPL_pct22) == T & is.na(FPL_pct21) == T & is.na(FPL_pct20) == F ~ FPL_pct20)) %>%
        mutate(poverty = case_when(FPL_pct_merge < 2 ~ "≤ 200% FPL",
                                   FPL_pct_merge >= 2 & FPL_pct_merge < 4 ~ "200-400% FPL",
                                   FPL_pct_merge >= 4 ~ "> 400% FPL",
                                   TRUE ~ "Unknown")) %>%
        select(ProviderID, poverty)
}
    
    