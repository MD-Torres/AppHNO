library(shiny)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(httr)
library(DT)

# Authentication setup for shinyapps.io deployment
# This approach uses gargle's caching mechanism which works better in deployed environments
setup_google_auth <- function() {
  # Try-catch to prevent app from crashing if authentication fails
  tryCatch({
    # Setting auth method based on environment
    if (Sys.getenv("SHINY_SERVER") == "") {
      # Local development - can use interactive auth
      gs4_auth(email = "arzt@jorge-torres.de", cache = TRUE)
    } else {
      # On shinyapps.io - need service account
      # IMPORTANT: You must upload your service-account.json when deploying
      # And set proper path in setwd() below
      
      # Set working directory to the app directory on shinyapps.io
      setwd("/srv/connect/apps/elektive_lymphknotenlevels")
      
      if (file.exists("service-account.json")) {
        gs4_auth(path = "service-account.json")
      } else {
        # Fallback that will likely trigger authentication challenge
        gs4_auth(email = "arzt@jorge-torres.de", cache = TRUE)
      }
    }
    return(TRUE)
  }, error = function(e) {
    # Return error message for debugging
    return(paste("Authentication error:", e$message))
  })
}

# Load data from Google Sheets with error handling
load_google_data <- function() {
  tryCatch({
    main_sheet_url <- "https://docs.google.com/spreadsheets/d/13yP8iQ-z_DTFiGE0IYX1C3zMWrZ3_UqyarV7mOkJYSQ"
    zusatz_sheet_url <- "https://docs.google.com/spreadsheets/d/12EKmeD--_JrhAsRL63Y8Sah15dDY8IcI9V9CslZQZlE"
    icd_sheet_url <- "https://docs.google.com/spreadsheets/d/1NmGszAw0drH1woUIt6elRp_SiExIt-ybl-Pbe1CNjGU"
    
    # Read data from Google Sheets
    main_data <- read_sheet(main_sheet_url)
    zusatz_data <- read_sheet(zusatz_sheet_url)
    icd_data <- read_sheet(icd_sheet_url) 
    
    # Convert to data.frames
    main_data <- as.data.frame(main_data)
    zusatz_data <- as.data.frame(zusatz_data)
    icd_data <- as.data.frame(icd_data)
    
    # Return all datasets
    return(list(
      main_data = main_data, 
      zusatz_data = zusatz_data,
      icd_data = icd_data
    ))
  }, error = function(e) {
    # Return empty dataframes with error message
    return(list(
      main_data = data.frame(Error = paste("Error loading data:", e$message)),
      zusatz_data = data.frame(),
      icd_data = data.frame()
    ))
  })
}

# Define UI with additional status elements
ui <- fluidPage(
  titlePanel("Elektive Lymphknotenlevels"),
  
  # Add status message area for debugging
  conditionalPanel(
    condition = "output.authStatus !== 'Authentication successful'",
    div(style = "background-color: #ffeeee; padding: 10px; margin-bottom: 15px;",
        h4("Authentication Status:"),
        textOutput("authStatus")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tumorlokalisation", "Tumorlokalisation:",
                  choices = c("Loading data..." = ""),
                  selected = ""),
      
      selectInput("konzept", "Konzept:",
                  choices = c("Loading data..." = ""),
                  selected = ""),
      
      selectInput("nstadium", "N-Stadium:",
                  choices = c("Select Konzept first" = "")),
      
      uiOutput("kondition_checkboxes"),
      
      actionButton("submit", "Elektive Levels"),
      
      # Add refresh button with clearer label
      actionButton("refresh_data", "Refresh Google Data")
    ),
    
    mainPanel(
      h3("RT bis 50,4 Gy:"),
      verbatimTextOutput("ipsilateralLevel"),
      verbatimTextOutput("kontralateralLevel"),
      h3("Zusätzliche Lymphknoten Levels:"),
      verbatimTextOutput("konditionLevel"),
      h3("ICD-10 Codes:"),
      DTOutput("icdTable")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Initialize values for auth status
  auth_status <- reactiveVal("Initializing...")
  
  # Initialize authentication on startup
  observe({
    auth_result <- setup_google_auth()
    if (is.logical(auth_result) && auth_result) {
      auth_status("Authentication successful")
    } else {
      auth_status(auth_result)
    }
  })
  
  # Display authentication status
  output$authStatus <- renderText({
    auth_status()
  })
  
  # Create reactive values to store the data
  data_store <- reactiveVal(list(
    main_data = data.frame(),
    zusatz_data = data.frame(),
    icd_data = data.frame()
  ))
  
  # Load data on startup and when refresh button is clicked
  observe({
    # Show loading indicator
    show_modal_spinner(
      spin = "circle",
      text = "Loading data from Google Sheets..."
    )
    
    # Initialize or refresh data
    new_data <- load_google_data()
    data_store(new_data)
    
    # Check if main data loaded successfully
    if (ncol(new_data$main_data) > 1) {
      # Update UI selections once data is loaded
      updateSelectInput(session, "tumorlokalisation", 
                        choices = unique(new_data$main_data$Tumorlokalisation))
      updateSelectInput(session, "konzept", 
                        choices = unique(new_data$main_data$Konzept))
      
      # Success notification
      showNotification("Data loaded successfully", type = "message")
    } else {
      # Error message - display error from the dataframe
      showNotification(new_data$main_data$Error, type = "error")
    }
    
    # Remove loading indicator
    remove_modal_spinner()
  }) %>% bindEvent(input$refresh_data, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Access data from the reactive
  main_data <- reactive({
    data_store()$main_data
  })
  
  zusatz_data <- reactive({
    data_store()$zusatz_data
  })
  
  icd_data <- reactive({
    data_store()$icd_data
  })
  
  # Dynamically update N-Stadium based on Konzept
  observeEvent(input$konzept, {
    req(main_data())
    # Check if main_data has expected structure
    if (!"NStadium" %in% colnames(main_data())) return()
    
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
  })
  
  # Update condition checkboxes based on both Tumorlokalisation and Konzept
  observeEvent(c(input$tumorlokalisation, input$konzept), {
    req(input$tumorlokalisation, input$konzept, zusatz_data())
    
    # Check if zusatz_data has expected structure
    if (!all(c("Lokalisation", "Konzept", "Kondition") %in% colnames(zusatz_data()))) return()
    
    # Filter conditions based on both location and concept
    relevant_kondition <- zusatz_data() %>%
      filter(
        Lokalisation == input$tumorlokalisation,
        Konzept == input$konzept
      ) %>%
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
    
    # Check if main_data has expected structure
    if (!all(c("Tumorlokalisation", "Seite", "NStadium", "Konzept", "Elektiveslevel") %in% colnames(main_data()))) {
      return(data.frame())
    }
    
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
    
    # Check if main_data has expected structure
    if (!all(c("Tumorlokalisation", "Seite", "NStadium", "Konzept", "Elektiveslevel") %in% colnames(main_data()))) {
      return(data.frame())
    }
    
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
    if (nrow(filteredIpsilateral()) == 0) {
      "Ipsilateral: No matching Elektives Level found."
    } else {
      paste("Ipsilateral:", paste(filteredIpsilateral()$Elektiveslevel, collapse = ", "))
    }
  })
  
  # Display Elektives Level for kontralateral
  output$kontralateralLevel <- renderText({
    req(filteredKontralateral())
    if (nrow(filteredKontralateral()) == 0) {
      "Kontralateral: No matching Elektives Level found."
    } else {
      paste("Kontralateral:", paste(filteredKontralateral()$Elektiveslevel, collapse = ", "))
    }
  })
  
  # Display additional levels based on Kondition selections
  output$konditionLevel <- renderText({
    req(input$kondition, zusatz_data())
    
    # Check if zusatz_data has expected structure
    if (!all(c("Lokalisation", "Konzept", "Kondition", "ja_nein", "level") %in% colnames(zusatz_data()))) {
      return("Error: Data structure issue with additional levels.")
    }
    
    # Filter Zusatz data based on checked conditions, location, and concept
    result <- zusatz_data() %>%
      filter(
        Lokalisation == input$tumorlokalisation,
        Konzept == input$konzept,
        Kondition %in% input$kondition,
        ja_nein == "ja"
      ) %>%
      select(level) %>%
      distinct()
    
    if (nrow(result) == 0) {
      "No additional levels found based on Kondition selections."
    } else {
      paste(paste(result$level, collapse = ", "))
    }
  })
  
  # Display ICD-10 table filtered by selected Tumorlokalisation
  output$icdTable <- renderDT({
    req(input$tumorlokalisation, icd_data())
    
    # Check if icd_data has expected structure
    if (!all(c("Lokalisation", "Bezeichnung", "ICD-10") %in% colnames(icd_data()))) {
      return(datatable(
        data.frame(Message = "Error: Data structure issue with ICD codes."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Filter ICD data based on selected tumorlokalisation
    filtered_icd <- icd_data() %>%
      filter(Lokalisation == input$tumorlokalisation)
    
    # Return formatted table
    if (nrow(filtered_icd) == 0) {
      return(datatable(
        data.frame(Message = "No ICD-10 codes found for the selected tumor location."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    } else {
      # Return only the needed columns and format them
      filtered_icd %>%
        select(Bezeichnung, `ICD-10`) %>%
        datatable(
          options = list(
            pageLength = 5,
            dom = 'tp',  # Only show table and pagination controls
            ordering = TRUE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    }
  })
}

# Adding missing dependency for loading spinner
show_modal_spinner <- function(spin = "circle", text = NULL, session = getDefaultReactiveDomain()) {
  showNotification(text, type = "message", duration = NULL, id = "loading_data")
}

remove_modal_spinner <- function(session = getDefaultReactiveDomain()) {
  removeNotification(id = "loading_data")
}

# Run the App
shinyApp(ui = ui, server = server)