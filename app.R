library(shiny)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(httr)

# Google Authentication
# Define authentication function to be called when app starts
setup_google_auth <- function() {
  # Google Sheets Authentication
  # Checks if service account credentials exist, otherwise uses OAuth
  if (file.exists("JSON_Key/service-account.json")) {
    gs4_auth(path = "JSON_Key/service-account.json")
    drive_auth(path = "JSON_Key/service-account.json")
  } else {
    gs4_auth(email = "arzt@jorge-torres.de")
    drive_auth(email = "arzt@jorge-torres.de")
  }
}

# Load data from Google Sheets
load_google_data <- function() {
  # Replace these URLs with your actual Google Sheet URLs
  main_sheet_url <- "https://docs.google.com/spreadsheets/d/13yP8iQ-z_DTFiGE0IYX1C3zMWrZ3_UqyarV7mOkJYSQ"
  zusatz_sheet_url <- "https://docs.google.com/spreadsheets/d/12EKmeD--_JrhAsRL63Y8Sah15dDY8IcI9V9CslZQZlE"
  
  # Read data from Google Sheets
  main_data <- read_sheet(main_sheet_url)
  zusatz_data <- read_sheet(zusatz_sheet_url)
  
  # Convert to data.frames to ensure compatibility with existing code
  main_data <- as.data.frame(main_data)
  zusatz_data <- as.data.frame(zusatz_data)
  
  # Return both datasets
  return(list(main_data = main_data, zusatz_data = zusatz_data))
}

# Function to get image from Google Drive
get_drive_image <- function(tumor, nstadium) {
  # Construct image name
  image_name <- paste0(tumor, "_", nstadium, ".png")
  
  # Search for the image in Google Drive
  image_file <- drive_find(
    pattern = image_name,
    type = "image/png"
  )
  
  if (nrow(image_file) > 0) {
    # Create temp directory if it doesn't exist
    temp_dir <- "temp_images"
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir)
    }
    
    # Create temp file path
    temp_file <- file.path(temp_dir, image_name)
    
    # Download the file
    drive_download(
      file = image_file$id[1],
      path = temp_file,
      overwrite = TRUE
    )
    
    return(temp_file)
  } else {
    return(NULL)
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Elektive Lymphknotenlevels"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tumorlokalisation", "Tumorlokalisation:",
                  choices = NULL,  # Will be populated after data loads
                  selected = NULL),
      
      selectInput("konzept", "Konzept:",
                  choices = NULL,  # Will be populated after data loads
                  selected = NULL),
      
      selectInput("nstadium", "N-Stadium:",
                  choices = NULL),  # Dynamically updated
      
      uiOutput("kondition_checkboxes"),  # Dynamic checkboxes for Kondition
      
      actionButton("submit", "Elektive Levels"),
      
      # Add refresh button to reload data from Google Sheets
      actionButton("refresh_data", "Refresh Data")
    ),
    
    mainPanel(
      h3("RT bis 50,4 Gy:"),
      verbatimTextOutput("ipsilateralLevel"),
      verbatimTextOutput("kontralateralLevel"),
      h3("Zusätzliche Lymphknoten Levels:"),
      verbatimTextOutput("konditionLevel"),
      h3("Tumorlokalisation & N-Stadium Image:"),
      imageOutput("outputImage")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Initialize authentication
  setup_google_auth()
  
  # Create reactive values to store the data
  data_store <- reactiveVal(list(
    main_data = data.frame(),
    zusatz_data = data.frame()
  ))
  
  # Load data on startup and when refresh button is clicked
  observe({
    # Initialize or refresh data
    tryCatch({
      new_data <- load_google_data()
      data_store(new_data)
      
      # Update UI selections once data is loaded
      updateSelectInput(session, "tumorlokalisation", 
                        choices = unique(new_data$main_data$Tumorlokalisation))
      updateSelectInput(session, "konzept", 
                        choices = unique(new_data$main_data$Konzept))
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  }) %>% bindEvent(input$refresh_data, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Access data from the reactive
  main_data <- reactive({
    data_store()$main_data
  })
  
  zusatz_data <- reactive({
    data_store()$zusatz_data
  })
  
  # Dynamically update N-Stadium based on Konzept
  observeEvent(input$konzept, {
    req(main_data())
    
    filtered_nstadium <- if (input$konzept == "definitiv") {
      main_data()$NStadium[grep("^c", main_data()$NStadium)]
    } else if (input$konzept == "adjuvant") {
      main_data()$NStadium[grep("^p", main_data()$NStadium)]
    } else {
      NULL
    }
    
    updateSelectInput(session, "nstadium",
                      choices = unique(filtered_nstadium),
                      selected = NULL)
    
    # Create checkboxes dynamically for Kondition without ja/nein
    relevant_kondition <- zusatz_data() %>%
      filter(Konzept == input$konzept) %>%
      select(Kondition) %>%
      distinct()
    
    output$kondition_checkboxes <- renderUI({
      checkboxGroupInput(
        "kondition",
        "Sonderbedingungen:",
        choices = relevant_kondition$Kondition,
        selected = NULL
      )
    })
  })
  
  # Filter data for ipsilateral
  filteredIpsilateral <- reactive({
    req(input$tumorlokalisation, input$nstadium, input$konzept, main_data())
    main_data() %>%
      filter(
        Tumorlokalisation == input$tumorlokalisation,
        Seite == "ipsilateral",
        NStadium == input$nstadium,
        Konzept == input$konzept
      )
  })
  
  # Filter data for kontralateral
  filteredKontralateral <- reactive({
    req(input$tumorlokalisation, input$nstadium, input$konzept, main_data())
    main_data() %>%
      filter(
        Tumorlokalisation == input$tumorlokalisation,
        Seite == "kontralateral",
        NStadium == input$nstadium,
        Konzept == input$konzept
      )
  })
  
  # Display Elektives Level for ipsilateral
  output$ipsilateralLevel <- renderText({
    req(filteredIpsilateral())
    result <- filteredIpsilateral()$Elektiveslevel
    if (nrow(filteredIpsilateral()) == 0) {
      "Ipsilateral: No matching Elektives Level found."
    } else {
      paste("Ipsilateral:", paste(result, collapse = ", "))
    }
  })
  
  # Display Elektives Level for kontralateral
  output$kontralateralLevel <- renderText({
    req(filteredKontralateral())
    result <- filteredKontralateral()$Elektiveslevel
    if (nrow(filteredKontralateral()) == 0) {
      "Kontralateral: No matching Elektives Level found."
    } else {
      paste("Kontralateral:", paste(result, collapse = ", "))
    }
  })
  
  # Display additional levels based on Kondition selections
  output$konditionLevel <- renderText({
    req(input$kondition, zusatz_data())
    
    # Filter Zusatz data based on checked conditions (ja)
    result <- zusatz_data() %>%
      filter(Kondition %in% input$kondition & ja_nein == "ja") %>%
      select(level) %>%
      distinct()
    
    if (nrow(result) == 0) {
      "No additional levels found based on Kondition selections."
    } else {
      paste(paste(result$level, collapse = ", "))
    }
  })
  
  # Keep track of current image file
  current_image <- reactiveVal(NULL)
  
  # Display image from Google Drive
  output$outputImage <- renderImage({
    req(input$tumorlokalisation, input$nstadium)
    
    # Get the image from Google Drive
    image_path <- get_drive_image(input$tumorlokalisation, input$nstadium)
    current_image(image_path)
    
    if (!is.null(image_path) && file.exists(image_path)) {
      list(src = image_path, 
           alt = "Tumor and N-stadium visualization", 
           width = "100%")
    } else {
      # Return a placeholder or default image
      list(src = "www/images/placeholder.png", 
           alt = "No image available for the selected parameters", 
           width = "100%")
    }
  }, deleteFile = FALSE)
  
  # Clean up temporary files when the session ends
  session$onSessionEnded(function() {
    # Clean up temp files
    temp_dir <- "temp_images"
    if (dir.exists(temp_dir)) {
      file_list <- list.files(temp_dir, full.names = TRUE)
      unlink(file_list)
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)