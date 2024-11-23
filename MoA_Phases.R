library(shiny)
library(dplyr)
library(readr)

# Install ggplot2 if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load ggplot2
library(ggplot2)

# Load data
data <- read_csv("C:/Users/alwer/OneDrive/Desktop/MoA/current_data_fake.csv")

# Process data: Assign participants to only one phase based on progression
data <- data %>%
  mutate(
    current_phase = case_when(
      # 12 Month Follow-up
      !is.na(date_12mo_end) ~ "12 Month Follow-up",
      
      # 1 Month Follow-up
      !is.na(date_1mo_end) & is.na(date_12mo_end) ~ "1 Month Follow-up",
      
      # Training Phase
      !is.na(date_training_start) & 
        !is.na(date_training_end) & 
        Sys.Date() <= as.Date(date_training_end) &
        is.na(date_1mo_end) & is.na(date_12mo_end) ~ "Training Phase",
      
      # Calibration Phase
      !is.na(date_calibration_end) & 
        is.na(date_training_start) & 
        Sys.Date() <= as.Date(date_calibration_end) + 21 &
        is.na(date_1mo_end) & is.na(date_12mo_end) ~ "Calibration Phase",
      
      # Inactive Calibration Phase
      !is.na(date_calibration_end) & 
        Sys.Date() > as.Date(date_calibration_end) + 21 &
        is.na(date_training_start) ~ "Inactive",
      
      # Consent
      !is.na(date_consent_completed) & 
        is.na(date_training_start) & 
        is.na(date_1mo_end) & 
        is.na(date_12mo_end) ~ "Consent",
      
      # Screened
      !is.na(screened_date) & 
        is.na(date_calibration_end) & 
        is.na(date_training_start) ~ "Screened",
      
      # Default: Not assigned to any phase
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(current_phase) & current_phase != "Inactive")

# Add cumulative counts for Screened and Consented
cumulative_data <- data %>%
  summarize(
    cumulative_screened = sum(!is.na(screened_date)),
    cumulative_consented = sum(!is.na(date_consent_completed))
  )

# UI
ui <- fluidPage(
  titlePanel("Participant Tracking Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("coord_filter", "Filter by Coordinator Email:", 
                  choices = c("All", unique(tolower(data$coord_email))), selected = "All")
    ),
    mainPanel(
      uiOutput("cumulative_totals"),
      tableOutput("phase_summary"),
      plotOutput("phase_plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered data (exclude Screened and normalize email case)
  filtered_data <- reactive({
    filtered <- data %>%
      filter(current_phase != "Screened") %>% # Exclude "Screened" phase
      mutate(coord_email = tolower(coord_email)) # Normalize emails to lowercase
    
    if (input$coord_filter == "All") {
      filtered
    } else {
      filtered %>% filter(coord_email == tolower(input$coord_filter))
    }
  })
  
  # Summary table
  output$phase_summary <- renderTable({
    filtered_data() %>%
      count(current_phase) %>%
      rename(Phase = current_phase, Count = n) %>%
      arrange(Phase)
  })
  
  # Cumulative totals for Screened and Consented
  output$cumulative_totals <- renderUI({
    cumulative_data <- data %>%
      summarize(
        cumulative_screened = sum(!is.na(screened_date)),
        cumulative_consented = sum(!is.na(date_consent_completed))
      )
    
    HTML(paste0(
      "Total Screened: ", cumulative_data$cumulative_screened, "<br>",
      "Total Consented: ", cumulative_data$cumulative_consented
    ))
  })
  
  # Plot
  output$phase_plot <- renderPlot({
    filtered_data() %>%
      count(current_phase) %>%
      ggplot(aes(x = reorder(current_phase, n), y = n, fill = current_phase)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Participant Counts by Phase", x = "Phase", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
