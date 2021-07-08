# TFC Data Wrangling R script
# File author: John Lee 
# Purpose: To clean/process the data from ActBlue so that it can be used in a Shiny app


# Part 1: Load packages, data ---------------------------------------------------------------------


# Load packages
library(janitor) # for data cleaning
library(lubridate) # for date-related functions
library(skimr) # to summarize data
library(stringr) # for NLP
library(gender) # for the function to estimate gender based on 1st name
library(genderdata) # for the underlying historical data 
library(tidyverse)



# Set the ActBlue data download date (manually update this each time the dataset is pulled from AB)
AB_download_date <- "09-11-2019"

# Load the ActBlue data (CSV --> to tibble)
actblue_data_tbl <- read_csv(file = paste0("data/", AB_download_date, 
                                      "/FILE_NAME_HERE.csv")) %>%
  clean_names()


# Print var names
names(actblue_data_tbl)



# Part 2: Clean, manipulate data -------------------------------------------------------

# Filter in the relevant variables
actblue_data_tbl <- actblue_data_tbl %>%
  dplyr::select(
    # info about the donation 
    payment_date, reference_code, amount, 
    # donor info (donor 1st name is used to estimate gender)
    donor_first_name, donor_last_name, donor_occupation, donor_employer, donor_email,
    # info about the donor location
    donor_addr1, donor_city, donor_state, donor_zip, donor_country)

# Look up the first few lines
actblue_data_tbl %>% head()

# summary statistics
actblue_data_tbl %>% skim


# Impute date (actual date must be extracted from payment_date field)
actblue_data_tbl <- actblue_data_tbl %>% 
  # Just extract the date (ignore the time)
  mutate(date = payment_date %>% 
           as_date()) %>% 
  # Add a field for the full address
  mutate(full_address = paste(donor_addr1, donor_city, donor_state, 
                               donor_zip, donor_country, sep = ", ")) %>%
  # Add a field for the full name, 
  mutate(full_name = paste(donor_first_name, donor_last_name, sep = " ")) %>%
  # Add estimated birth yr range, which is needed to estimate gender (assume most donors are btwn 20-70)
  mutate(min_birthyr = 2019-70,
         max_birthyr = 2019-20) %>%
  # This field will be used later; for now, set as 1 for each row 
  mutate(donation_counter = 1) %>%
  # Use the title case (1st letter capitalized) b/c it helps with aggregation later
  mutate(full_name = tools::toTitleCase(full_name),
         donor_addr1 = tools::toTitleCase(donor_addr1),
         full_address = tools::toTitleCase(full_address),
         donor_occupation = tools::toTitleCase(donor_occupation), 
         donor_employer = tools::toTitleCase(donor_employer)) %>%
  # Remove the pound sign (#) from the street addresses; it causes a problem with the TAMU geocoding service
  mutate(donor_addr1 = gsub(pattern = "[#,]", replacement = "", x = donor_addr1))

# Estimate the gender based on 1st name 
est_gender_tbl <- actblue_data_tbl %>% 
  gender_df(name_col = "donor_first_name", 
            year_col = c("min_birthyr", "max_birthyr"), method = "ssa") %>%
  mutate(donor_first_name = name) %>%
  # drop the vars we don't need
  dplyr::select(donor_first_name, gender)

# Add the estimated gender back to the original tbl 
actblue_data_tbl <- actblue_data_tbl %>% 
  left_join(x = ., y = est_gender_tbl, by = "donor_first_name") %>%
  # Drop the obs where the gender couldn't be estimated (about ~X% of the original dataset)
  dplyr::filter(!is.na(gender))

# Let's check the gender estimates (I confirmed that it worked)
actblue_data_tbl %>% dplyr::select(donor_first_name, gender) %>% head(n = 15)


# Objective: create a separate tbl for the donors. 
# Problem: we need a unique identifier. We can use email, but ~X entries don't have an associated email
# Solution: create a unique identifier by finding all of the unique combos of full name and street address


# Create the donor tbl - each row is a unique donor 
donor_tbl <- actblue_data_tbl %>%
  group_by(full_name, donor_addr1) %>%
  summarize(donation_count = sum(donation_counter), 
            donation_sum = sum(amount)) %>%
  ungroup()

# Create the unique identifier in the donor tbl
donor_tbl <- donor_tbl %>%
  mutate(id_text = paste(full_name, donor_addr1, sep = ", "))

# Add the covariates into the donor tibble (when a unique donor in the donor tbl has multiple matches in
# the contributor tbl, just pull in the data from the 1st match; otherwise, this will create more duplicates
# in the donor tbl)
donor_tbl <- donor_tbl %>%
  left_join(x = ., y = distinct(actblue_data_tbl %>% 
                                  # Just filter in the donor-level vars of interest
                                dplyr::select(full_name, full_address,
                                       # Each element of the address is req for geocoding thru TAMU
                                       donor_addr1, donor_city, donor_state, donor_zip,
                                       gender, donor_occupation, donor_employer) %>%
                                  # Create the unique identifier in the contributor tbl 
                                  mutate(id_text = paste(full_name, donor_addr1, sep = ", ")),
            # The matching is done using id_text (exact matching rule, 1st time)
            id_text, .keep_all = T)
              ) %>%
  # Drop the unique identifier b/c it's no longer needed
  dplyr::select(-id_text)


# Display top donors: by sum
donor_tbl %>%
  arrange(desc(donation_sum))

# Display top donors: by count
donor_tbl %>%
  arrange(desc(donation_count))


# Add a unique donor ID (row number) -- this is so that I can split up the donor tibble into smaller chunks
# for the batch geocoding 
donor_tbl <- donor_tbl %>%
  mutate(donor_id = row_number())



