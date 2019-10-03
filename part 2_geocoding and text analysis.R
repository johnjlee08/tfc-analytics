# TFC Geocoding and Text Analysis R script
# File author: John Lee 
# Purpose: Geocode the addresses by sending requests to the TAMU API

library(ggmap) # for geocoding 
library(XML) # For working with the TAMU API
library(RCurl) # For working with the TAMU API
library(raster) # for interactive maps 
library(leaflet) # for interactive maps 
library(tidyverse)



# Part 3: Geocoding and Spatial Analysis ------------------------------------------------------------------

# Next Step: geo-code all of the addresses using the TAMU geocoding service (it's free)
# To do this, I'll need to use the service's API 
# My API key (note that this is unique for each user of the TAMU geocoding service)
my_API_key <- "place_yours_here"

# This is the main geocoding function
# Input: tibble that contains the address elements in separate columns; output: same tibble but two new 
# columns: for lat and lon
tamu_geocode <- function(donor_addresses_tbl, API_key){
  
  # Creates a new tibble that adds the geocoded lat/lon to the base/input tibble
  geocoded_results_tbl <- donor_addresses_tbl %>% 
    # Add the lat/lon vars; initially set values as blanks -- they'll be filled in via the loop below 
    mutate(latitude = "", longitude = "")
  
  # Loop through each row (donor) in the tibble
  for (i in 1:nrow(geocoded_results_tbl)) {
    
    # Task #1: for each address, create a unique URL that will be used in the API call
    
    # Extract the address elements for each row (i) in the tibble
    d_str_address <- geocoded_results_tbl[i,] %>% pull(donor_addr1) %>% gsub(" ", "%20", x = .)
    d_city <- geocoded_results_tbl[i,] %>% pull(donor_city) %>% gsub(" ", "%20", x = .)
    d_state <- geocoded_results_tbl[i,] %>% pull(donor_state) %>% gsub(" ", "%20", x = .)
    d_zip <- geocoded_results_tbl[i,] %>% pull(donor_zip) %>% gsub(" ", "%20", x = .)
    
    # Set the base and end urls (i.e., the 1st/last parts of each unique URL are the same)
    base_url <- "geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?"
    end_url <- "&format=XML&census=true&censusYear=2000|2010&notStore=false&version=4.01"
    
    # Create the unique URL for each address -- for each API call 
    full_url <- paste0(base_url, "streetAddress=", d_str_address, "&city=", d_city, "&state", d_state,
                       "&zip=", d_zip, "&apikey=", API_key, end_url)
    
    # Try submitting the API call for the address; if it works, then store the lat and lon; 
    # if it fails, (e.g., bad address), then set the lat/lon as NA and move onto the next address (i+1)
    if (
      try({
        
        # Submits the API call w/ the URL specific to each request (i.e., each address); stores the result
        xml_request <- RCurl::getURL(full_url)
        
        # Removes 4 unnecessary chars at the begining
        xml_aslist <- substr(xml_request, 4, nchar(xml_request)) %>%
          # Converts the XML object into a list
          XML::xmlToList(node = .) 
        
        # Drill down to the correct place in the list hierarchy (see XML example)
        # Then extract and store the lat and lon
        lat <- as.numeric(xml_aslist$OutputGeocodes$OutputGeocode$Latitude)
        lon <- as.numeric(xml_aslist$OutputGeocodes$OutputGeocode$Longitude) 
        
      } # end of R expression that is attempted 
      
      ) %>% class() == "try-error" # Checks to see if an error results
    ) {
      # If the API call for an address fails (i.e., class = "try-error"), set the lat and lon as NA
      geocoded_results_tbl[i, "latitude"] <- NA_real_
      geocoded_results_tbl[i, "longitude"] <- NA_real_
      
      # If there's no error, then store the results of the API call 
    } else {
      
      geocoded_results_tbl[i, "latitude"] <- lat
      geocoded_results_tbl[i, "longitude"] <- lon
    }
    
    # Before submitting the API call for the next address, pause for 1/10 of a sec 
    # This is to avoid overwhelming the server 
    Sys.sleep(time = 0.1)
    
    # end of the for loop here (i.e., after looping through each row/addr in the tibble)
  }
  
  # After looping through all of the addresses in the tibble, return the original tibble w/
  # the geocoded lat/lon
  geocoded_results_tbl <- geocoded_results_tbl %>%
    mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
  
  return(geocoded_results_tbl)
  
  # end of the function here
}


# Now, split up the addresses into smaller chunks of 2.5k addresses 
# (this will require 4 chunks for 9.1k addresses)
# Note: do not run the code chunks below, b/c they can take ~40 minutes each. Instead, just load the 
# data that have already been geocoded and saved 

# Chunk #1: donor ID 1-2500
address_set_1 <- donor_tbl %>% dplyr::filter(donor_id < 2501)
set_1_results <- tamu_geocode(address_set_1, my_API_key)
write_rds(x = set_1_results, path = "set_1_results.rds")

# Chunk #2: donor ID 2501-5000
address_set_2 <- donor_tbl %>% dplyr::filter(between(x = donor_id, 2501, 5000))
set_2_results <- tamu_geocode(address_set_2, my_API_key)
write_rds(x = set_2_results, path = "set_2_results.rds")

# Chunk #4: donor ID 5001-7500
address_set_3 <- donor_tbl %>% dplyr::filter(between(x = donor_id, 5001, 7500))
set_3_results <- tamu_geocode(address_set_3, my_API_key)
write_rds(x = set_3_results, path = "set_3_results.rds")

# Chunk #4: donor ID 7501-9085
address_set_4 <- donor_tbl %>% dplyr::filter(donor_id > 7500)
set_4_results <- tamu_geocode(address_set_4, my_API_key)
write_rds(x = set_4_results, path = "set_4_results.rds")


# Load the geocoded data (geocoded on 09/11-12) ----------------------
set_1_results <- read_rds("set_1_results.rds")
set_2_results <- read_rds("set_2_results.rds")
set_3_results <- read_rds("set_3_results.rds")
set_4_results <- read_rds("set_4_results.rds")

# Combine the chunks
combined_file <- base::rbind(set_1_results, set_2_results, set_3_results, set_4_results) %>% 
  as_tibble()

# Remove the obs when lat/lon is = 0 (e.g., non-US address) or NA 
combined_file <- combined_file %>%
  dplyr::filter(latitude != 0, longitude != 0) %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude))

# Add random noise to lat/lon to preserve anonymity (update on 09/17: don't add jitter; 
# it produces some markers that incorrectly appear to be in the water)
combined_file <- combined_file %>%
  # Drop the personally identifiable vars (e.g., name, address)
  dplyr::select(-full_name, -donor_addr1, -full_address)


# Save the edited combined file -- which will be used in the Shiny app 
write_rds(x = combined_file, path = "combined_file.rds")



# Mapping w/ leaflet starts here ------------------------------

leaflet(combined_file) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude)

leaflet(data = combined_file, 
        options = leafletOptions(maxZoom = 11)) %>% # Set the max zoom level to preserve donor anonymity
  #addCircles(lng = ~longitude, lat = ~latitude, weight = 5, radius = ~donation_sum) %>%
  addMarkers(lng = ~lon_rn,
             lat = ~lat_rn,
             popup = ~as.character(donation_sum), label = ~as.character(donation_sum), 
             clusterOptions = markerClusterOptions()) #%>%
#addPolygons(label = ~community)


leaflet(data = combined_file, 
        # Set max zoom level to preserve donor anonymity (no neighborhood-level viewing)
        options = leafletOptions(maxZoom = 11)) %>% 
  addTiles() %>%
  addCircles(lng = ~lon_rn, lat = ~lat_rn, 
             weight = 5, 
             # Weight each point by 1/5 of the donation sum
             radius = ~donation_sum/5, 
             #color = ~ifelse(freq_donor == "yes", "navy", "blue"),
             popup = ~as.character(donation_sum), label = ~as.character(donation_sum),
             fillOpacity = 0.5) 

# When deleting the obs with missing lat/lon, also look up and remove obs when lat/lon = 0 (e.g., non-US)



# Text Analytics -----------------------------------------------------

# Next: text analysis -- of job titles and employers 

# Read in the processed/geocoded donor file
combined_file <- read_rds("combined_file.rds")

# Look at the top occupations
combined_file %>% count(donor_occupation) %>% arrange(desc(n)) %>% head(n = 20)

# Look at the top employers
combined_file %>% count(donor_employer) %>% arrange(desc(n)) %>% head(n = 20)

# It looks like some of the different categories can be combined (e.g., Attorney and Lawyer)

# Combine categories for donor occuptation
combined_file <- combined_file %>%
  mutate(donor_occupation = ifelse(donor_occupation == "None", "Not Employed", 
                                   ifelse(donor_occupation == "Lawyer", "Attorney",
                                          ifelse(donor_occupation == "Self Employed", "Self-Employed",
                                                 donor_occupation)))
  )

# Combine categories for donor employer
combined_file <- combined_file %>%
  mutate(donor_employer = ifelse(donor_employer == "None", "Not Employed", 
                                 ifelse(donor_employer %in% c("Self", "Self Employed", "SELF"), "Self-Employed", 
                                        ifelse(donor_employer == "Commonwealth of Virginia", "Virginia",
                                               ifelse(donor_employer == "Virginia Commonwealth University", "VCU", 
                                                      donor_employer))))
  )

# Save the edited combined file -- which will be used in the Shiny app 
write_rds(x = combined_file, path = "combined_file.rds")

# Save the donor occupation and employer counts 
d_occupation <- combined_file %>% count(donor_occupation)
write_rds(x = d_occupation, path = "d_occupation.rds")

d_employer <- combined_file %>% count(donor_employer)
write_rds(x = d_employer, path = "d_employer.rds")


# Create a word cloud -----------------------------------

# Load packages
library(tm)
library(wordcloud)
library(memoise)

# Load data
d_occupation <- read_rds(path = "d_occupation.rds")
d_employer <- read_rds(path = "d_employer.rds")

d_employer %>% arrange(desc(n))

png("wordcloud_packages.png", width=1280, height=800 , res = 400)

# Create the word cloud for donor occupation 
wordcloud(words = d_occupation$donor_occupation, 
          # Remember to log the freq, to correct for skew
          freq = log(d_occupation$n),
          # Controls the diff between the largest and smallest font
          scale = c(1,0.2),
          # Min freq of the word to be plotted (this param doesn't seem to work properly)
          min.freq = 10,
          # Max number of words to display; least frequent terms dropped
          max.words = 50, 
          random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))

# Create the word cloud for donor employer 
wordcloud(words = d_employer$donor_employer, 
          # Remember to log the freq, to correct for skew
          freq = log(d_employer$n),
          # Controls the diff between the largest and smallest font
          scale = c(1,0.2),
          # Min freq of the word to be plotted (this param doesn't seem to work properly)
          min.freq = 10,
          # Max number of words to display; least frequent terms dropped
          max.words = 50, 
          random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))

