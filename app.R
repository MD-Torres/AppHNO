library(shiny)
library(dplyr)

# Load Data
main_data <- read.csv("data/HNO_Konzepte.csv")
zusatz_data <- read.csv("data/HNO_Konzepte_Zusatz.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Elektive Lymphknotenlevels"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tumorlokalisation", "Tumorlokalisation:",
                  choices = unique(main_data$Tumorlokalisation),
                  selected = NULL),
      
      selectInput("konzept", "Konzept:",
                  choices = unique(main_data$Konzept),
                  selected = NULL),
      
      selectInput("nstadium", "N-Stadium:",
                  choices = NULL),  # Dynamically updated
      
      uiOutput("kondition_checkboxes"),  # Dynamic checkboxes for Kondition
      
      # actionButton("submit", "Elektive Levels")
    ),
    
    mainPanel(
      h3("RT bis 50,4 Gy:"),
      verbatimTextOutput("ipsilateralLevel"),
      verbatimTextOutput("kontralateralLevel"),
      h3("Zusätzliche Lymphknoten Levels:"),
      verbatimTextOutput("konditionLevel"),
      # h3("Tumorlokalisation & N-Stadium Image:"),
      # imageOutput("outputImage")
    ),
    
    mainPanel(
      h3("RT bis 50,4 Gy:"),
      verbatimTextOutput("ipsilateralLevel"),
      verbatimTextOutput("kontralateralLevel"),
      h3("Zusätzliche Lymphknoten Levels:"),
      verbatimTextOutput("konditionLevel")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Dynamically update N-Stadium based on Konzept
  observeEvent(input$konzept, {
    filtered_nstadium <- if (input$konzept == "definitiv") {
      main_data$NStadium[grep("^c", main_data$NStadium)]
    } else if (input$konzept == "adjuvant") {
      main_data$NStadium[grep("^p", main_data$NStadium)]
    } else {
      NULL
    }
    
    updateSelectInput(session, "nstadium",
                      choices = unique(filtered_nstadium),
                      selected = NULL)
    
    # Create checkboxes dynamically for Kondition without ja/nein
    relevant_kondition <- zusatz_data %>%
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
    req(input$tumorlokalisation, input$nstadium, input$konzept)
    main_data %>%
      filter(
        Tumorlokalisation == input$tumorlokalisation,
        Seite == "ipsilateral",
        NStadium == input$nstadium,
        Konzept == input$konzept
      )
  })
  
  # Filter data for kontralateral
  filteredKontralateral <- reactive({
    req(input$tumorlokalisation, input$nstadium, input$konzept)
    main_data %>%
      filter(
        Tumorlokalisation == input$tumorlokalisation,
        Seite == "kontralateral",
        NStadium == input$nstadium,
        Konzept == input$konzept
      )
  })
  
  # Display Elektives Level for ipsilateral
  output$ipsilateralLevel <- renderText({
    result <- filteredIpsilateral()$Elektiveslevel
    if (nrow(filteredIpsilateral()) == 0) {
      "Ipsilateral: No matching Elektives Level found."
    } else {
      paste("Ipsilateral:", paste(result, collapse = ", "))
    }
  })
  
  # Display Elektives Level for kontralateral
  output$kontralateralLevel <- renderText({
    result <- filteredKontralateral()$Elektiveslevel
    if (nrow(filteredKontralateral()) == 0) {
      "Kontralateral: No matching Elektives Level found."
    } else {
      paste("Kontralateral:", paste(result, collapse = ", "))
    }
  })
  
  # Display additional levels based on Kondition selections
  output$konditionLevel <- renderText({
    req(input$kondition)
    
    # Filter Zusatz data based on checked conditions (ja)
    result <- zusatz_data %>%
      filter(Kondition %in% input$kondition & ja_nein == "ja") %>%
      select(level) %>%
      distinct()
    
    if (nrow(result) == 0) {
      "No additional levels found based on Kondition selections."
    } else {
      paste( paste(result$level, collapse = ", "))
    }
  })
  
  
  # output$outputImage <- renderImage({
  #   req(input$tumorlokalisation, input$nstadium)
  #   
  #   # Construct the image path
  #   image_path <- paste0("www/images/", input$tumorlokalisation, "_", input$nstadium, ".png")
  #   
  #   # Check if the file exists
  #   if (file.exists(image_path)) {
  #     list(src = image_path, alt = "Image not available", width = "100%")
  #   } else {
  #     list(src = NULL, alt = "No image found for the selected parameters.")
  #   }
  # }, deleteFile = FALSE)
  # 
  # 
  
  
  
  
}

# Run the App
shinyApp(ui = ui, server = server)


