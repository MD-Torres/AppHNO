library(shiny)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(httr)
library(DT)

# New App

# Google Authentication
# Define authentication function to be called when app starts
setup_google_auth <- function() {
  # Google Sheets Authentication
  # Checks if service account credentials exist, otherwise uses OAuth
  if (file.exists("JSON_Key/service-account.json")) {
    gs4_auth(path = "JSON_Key/service-account.json")
    #drive_auth(path = "JSON_Key/service-account.json")
  } else {
    gs4_auth(email = "arzt@jorge-torres.de")
    #drive_auth(email = "arzt@jorge-torres.de")
  }
}

# Load data from Google Sheets
load_google_data <- function() {
  main_sheet_url <- "https://docs.google.com/spreadsheets/d/13yP8iQ-z_DTFiGE0IYX1C3zMWrZ3_UqyarV7mOkJYSQ"
  zusatz_sheet_url <- "https://docs.google.com/spreadsheets/d/12EKmeD--_JrhAsRL63Y8Sah15dDY8IcI9V9CslZQZlE"
  icd_sheet_url <- "https://docs.google.com/spreadsheets/d/1NmGszAw0drH1woUIt6elRp_SiExIt-ybl-Pbe1CNjGU"

  # Read data from Google Sheets
  main_data <- read_sheet(main_sheet_url)
  zusatz_data <- read_sheet(zusatz_sheet_url)
  icd_data <- read_sheet(icd_sheet_url) 
  
  # Convert to data.frames to ensure compatibility with existing code
  main_data <- as.data.frame(main_data)
  zusatz_data <- as.data.frame(zusatz_data)
  icd_data <- as.data.frame(icd_data)
  
  # Return all datasets
  return(list(
    main_data = main_data, 
    zusatz_data = zusatz_data,
    icd_data = icd_data
  ))
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
      h3("ICD-10 Codes:"),
      DTOutput("icdTable")  # Display ICD-10 codes as a table
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
    zusatz_data = data.frame(),
    icd_data = data.frame()
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
  
  icd_data <- reactive({
    data_store()$icd_data
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
  })
  
  # Update condition checkboxes based on both Tumorlokalisation and Konzept
  observeEvent(c(input$tumorlokalisation, input$konzept), {
    req(input$tumorlokalisation, input$konzept, zusatz_data())
    
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
    
    # Filter ICD data based on selected tumorlokalisation
    filtered_icd <- icd_data() %>%
      filter(Lokalisation == input$tumorlokalisation)
    
    # Return formatted table
    if (nrow(filtered_icd) == 0) {
      return(data.frame(
        Message = "No ICD-10 codes found for the selected tumor location."
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

# Run the App
shinyApp(ui = ui, server = server)