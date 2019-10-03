# Shiny App for TFC 

# Load libraries
library(shiny)
library(raster) # for interactive maps 
library(leaflet) # for interactive maps 
library(tidyverse)
library(wordcloud)



# Part 1: User Interface

ui <- fluidPage(
  
  # App title ----
  titlePanel("SVV Campaign Donor (2017-2019) Analytics"),
  
  # Sidebar layout #1: Spatial Analysis of Donor Location ---------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs #1
    sidebarPanel(
      
      h4("(1) Spatial Analysis: Donor Locations"),
      # Input: Select map type (w/ circle markers or clustered markers)
      selectInput(inputId = "map_type",
                  label = "Map Type:",
                  choices = c("With Circle Markers" = "cmarkers", # FYI: label = var name;
                              "With Clustered Markers" = "clusmarkers"),
                  selected = "cmarkers", # Set the default option to circle markers
                  multiple = FALSE), # Don't allow the selection of multiple items
      
      # Add explanatory text below the input widget
      helpText("Notes:", br(), 
               "(i) Only includes donor data available on ActBlue between Jan. 2017 - Sept. 2019.", br(),
               "(ii) Default map: plots a blue circle marker for each donor",
               "that is weighted by (the natural log of) donation sum.", 
               "Marker labels indicate donation sums.", br(),
               "(iii) The clustered markers map provides an estimate of the",
               "number of donors in a given geographical area.", br(),
               "(iv) The max level of zoom is set to protect donor anonymity",
               "(i.e., no neighborhood-level viewing).")
      
      ),
    
    # Main panel for displaying the geocoded map (#1)
    mainPanel(
      
      # Output: Interactive Map w/ leafletOutput (not plotOutput)
      leafletOutput(outputId = "donor_map")
      
    )
    
  ), # End of sidebar layout #1
  
  br(), br(), br(), # Add a few spaces before the next set of widgets
  
  
  
  # Sidebar layout #2: Word Cloud of Donor Characteristics ---------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs #2
    sidebarPanel(
      
      # Set section title #2
      h4("(2) Word Cloud: Donor Occupation, Employer"),
      
      # Widget 1: Allows users to choose the characteristic: d. occupation or employer
      selectInput(inputId = "d_info",
                  label = "Donor Characteristic:",
                  choices = c("Occupation" = "occupation", # FYI: label = var name;
                              "Employer" = "employer"),
                  selected = "occupation", # Set the default option 
                  multiple = FALSE), # Don't allow the selection of multiple items
      
      hr(), # Add a subtle divider between the two widgets
      
      # Widget 2: Allows users to choose the max number of words to display 
      # (e.g., top 50 words by frequency of appearance)
      sliderInput(inputId = "word_max",
                  label = "Maximum Number of Words to Display:",
                  min = 10,  max = 100,  value = 50) # Set the default value at 50 
      
    ), # End of the sidepanel for inputs
    
    
    # Main panel for displaying outputs #2
    mainPanel(
      
      # Output: Interactive Map w/ leafletOutput (not plotOutput)
      plotOutput(outputId = "word_cloud"),
     
      br(), br(), br() # Add a few extra spaces below the word cloud
       
    ) # End of the main panel for outputs
    
  ) # End of sidebar layout #2 (word cloud) 
  
) # End of fluid page function




# Part 2: Server (backend) -----------------------------------------------------------
server <- function(input, output, session) {

# Part 2a: Server code for the geocoded map ------
  
  # Read in the processed/geocoded donor file
  combined_file <- read_rds("data/combined_file.rds")
  
  # By default, display the map w/ circle markers 
  output$donor_map <- renderLeaflet({ # Note that renderLeaflet is used, not renderPlot
    
    leaflet(data = combined_file, 
            # Set max zoom level to 11 to preserve donor anonymity 
            # (no neighborhood-level viewing)
            options = leafletOptions(minZoom = 3, maxZoom = 11)) %>% 
      addTiles() %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                 weight = 1, 
                 # Weight each point by the nat log of the donation sum
                 radius = ~log(donation_sum), 
                 # Marker labels = total donated by the donor
                 popup = ~as.character(donation_sum), label = ~as.character(donation_sum),
                 fillOpacity = 0.5) 
    
  }) # End of renderLeaflet
  
  
  # observe: reruns this code chunk every time input$map_type changes (due to user clicks)
  observeEvent(input$map_type, {
    
    # Check to see if the user selected the clustered markers option; if so, generate
    # a map with clustered markers
    if(input$map_type == "clusmarkers"){
      
      leafletProxy(mapId = "donor_map", data = combined_file) %>%
        # Clear the old (Circle) markers 
        clearMarkers() %>%
        addMarkers(lng = ~longitude, lat = ~latitude, 
                   popup = ~as.character(donation_sum), 
                   label = ~as.character(donation_sum), 
                   clusterOptions = markerClusterOptions()) 
      
    } else { # End of if statement 
    
      # The else statement here allows the user to click on the 1st map type again
      # If user selects circle map again, then marker type is switched back to circles
      leafletProxy(mapId = "donor_map", data = combined_file) %>%
        clearMarkerClusters() %>% # Note that a diff clear func is req here
        addCircleMarkers(lng = ~longitude, lat = ~latitude,  
                         weight = 1, 
                         radius = ~log(donation_sum), 
                         popup = ~as.character(donation_sum), label = ~as.character(donation_sum),
                         fillOpacity = 0.5) 
      
    }
  }) # End of observe func
  
  
  
# Part 2b: Server code for the word cloud ------
  
  # Read in the tibbles: char, freq
  d_occupation <- read_rds("data/d_occupation.rds") %>%
    mutate(title = donor_occupation) # Use a consistent var name (title)
  d_employer <- read_rds("data/d_employer.rds")  %>%
    mutate(title = donor_employer)
  
  # Reactive expression updates the terms tibble based on the user's selection
  terms_tbl <- reactive({
    
    # If the user selects employer, then return the employer tibble
    if(input$d_info == "employer") {
      
      d_employer
      
    } else { # Otherwise, by default, return the occupation tibble
      
      d_occupation
    }
  }) # End of the reactive function for terms
  
  # Create the word cloud based on user inputs
  output$word_cloud <- renderImage({
    
    terms <- terms_tbl()
    
    # A temp file to save the output. 
    # It will be deleted after it's displayed
    temp_image <- tempfile(fileext = '.png')
    
    # Generate a png object (we can adjust the size of the image as necessary here)
    png(temp_image, width = 400, height = 400, res = 200)
    
    # Create the word cloud plot, which is temporarily saved as a png
    wordcloud(words = terms$title, 
              # Log the freq/count, to correct for skew
              freq = log(terms$n),
              # Controls the diff between the largest and smallest font
              scale = c(1,0.2),
              # Min freq of the word to be plotted 
              # (this isn't dynamic b/c since n is logged, the param doesn't work properly)
              min.freq = 10,
              # Max number of words to display; based on user input
              max.words = input$word_max, 
              random.order = FALSE, rot.per=0.2, 
              colors = brewer.pal(8, "Dark2"))
    
    dev.off() 
    
    # Return a list w/ the image
    list(src = temp_image)
    
  }, deleteFile = TRUE) # After the word cloud image is displayed, delete it
    
    
    
  #}) # End of renderPlot for the word cloud
  
  
} # End of the server

shinyApp(ui, server)
