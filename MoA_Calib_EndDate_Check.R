#install.packages(c("shiny", "dplyr", "lubridate", "DT"))


# Load necessary libraries
library(shiny)
library(dplyr)
library(lubridate)
library(DT)

setwd("C:/Users/alwer/OneDrive/Desktop/MoA")


# Load the dataset
data <- read.csv("current_data_fake.csv")

# Convert date columns to Date format
data <- data %>%
  mutate(across(contains("date"), ~ as.Date(., format = "%Y-%m-%d")))

# Define the UI
ui <- fluidPage(
  titlePanel("Calibration Phase Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("current_date", "Select Current Date:", value = Sys.Date()),
      selectInput("coord_filter", "Filter by Coordinator Email:",
                  choices = c("All" = "All", unique(tolower(data$coord_email))),
                  selected = "All"),
      helpText("Adjust the current date or filter by coordinator to see dynamic changes.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Calibration Progress", 
                 h4("Participants in Calibration Phase"),
                 DTOutput("calibration_table")),
        tabPanel("Cumulative Counts",
                 h4("Cumulative Statistics"),
                 verbatimTextOutput("cumulative_counts")),
        tabPanel("Special Cases",
                 h4("Consented but Not Started Calibration"),
                 DTOutput("not_started_calibration")),
        tabPanel("Randomization",
                 h4("Randomization Summary"),
                 verbatimTextOutput("randomization_summary"),
                 h4("Randomized Participants"),
                 DTOutput("randomized_table")),
        tabPanel("Eligible but Not Randomized",
                 h4("Training Eligible but Missing Randomized Group"),
                 DTOutput("not_randomized_table"))
      )
    )
  )
)

# Define the Server
server <- function(input, output) {
  
  # Reactive filter for coordinator email
  filtered_data <- reactive({
    if (input$coord_filter == "All") {
      data
    } else {
      data %>% filter(tolower(coord_email) == input$coord_filter)
    }
  })
  
  # Filter and calculate participants in the Calibration Phase
  calibration_data <- reactive({
    current_date <- input$current_date
    
    filtered_data() %>%
      filter(
        is.na(training_eligible), # Not training eligible
        !is.na(date_calibration_start_1) # Started Calibration
      ) %>%
      mutate(
        days_remaining = ifelse(
          !is.na(date_calibration_end) & date_calibration_end >= current_date, 
          as.numeric(date_calibration_end - current_date), 
          NA
        ),
        days_since_ended = ifelse(
          !is.na(date_calibration_end) & date_calibration_end < current_date, 
          as.numeric(current_date - date_calibration_end), 
          NA
        )
      ) %>%
      select(record_id_godin, date_calibration_start_1, date_calibration_end, 
             days_remaining, days_since_ended)
  })
  
  # Output the Calibration Phase table
  output$calibration_table <- renderDT({
    datatable(calibration_data(), options = list(pageLength = 10))
  })
  
  # Calculate cumulative statistics
  output$cumulative_counts <- renderText({
    total_screened <- sum(!is.na(filtered_data()$screened_date))
    total_consented <- sum(!is.na(filtered_data()$date_consent_completed))
    total_training_eligible <- sum(filtered_data()$training_eligible == 1, na.rm = TRUE)
    
    paste(
      "Total Screened: ", total_screened, "\n",
      "Total Consented: ", total_consented, "\n",
      "Total Training Eligible: ", total_training_eligible
    )
  })
  
  # Filter participants who have consented but not started Calibration
  not_started_data <- reactive({
    filtered_data() %>%
      filter(!is.na(date_consent_completed), is.na(date_calibration_start_1)) %>%
      select(record_id_godin, date_consent_completed)
  })
  
  # Output the list of consented but not started Calibration
  output$not_started_calibration <- renderDT({
    datatable(not_started_data(), options = list(pageLength = 10))
  })
  
  # Randomization summary and table
  randomization_data <- reactive({
    filtered_data() %>%
      filter(!is.na(randomized_group)) %>%
      select(record_id_godin, randomized_group)
  })
  
  output$randomization_summary <- renderText({
    random_data <- randomization_data()
    
    total_randomized <- nrow(random_data)
    control_count <- sum(random_data$randomized_group == 1, na.rm = TRUE)
    intervention_count <- sum(random_data$randomized_group == 2, na.rm = TRUE)
    
    paste(
      "Total Randomized: ", total_randomized, "\n",
      "Control Group (1): ", control_count, "\n",
      "Intervention Group (2): ", intervention_count
    )
  })
  
  output$randomized_table <- renderDT({
    datatable(randomization_data(), options = list(pageLength = 10))
  })
  
  # Filter participants who are training eligible but not randomized
  not_randomized_data <- reactive({
    filtered_data() %>%
      filter(training_eligible == 1, is.na(randomized_group)) %>%
      select(record_id_godin, training_eligible, randomized_group)
  })
  
  output$not_randomized_table <- renderDT({
    datatable(not_randomized_data(), options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui = ui, server = server)