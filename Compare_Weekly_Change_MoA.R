# Load necessary libraries
library(shiny)
library(dplyr)
library(lubridate)
library(DT)

#setwd("C:/Users/alwer/OneDrive/Desktop/MoA")


# Load the dataset
#current_file <- read.csv("current_file.csv")
#prior_file <- read.csv("prior_file.csv")


# Define file paths for the datasets
current_file_path <- "C:/Users/alwer/OneDrive/Desktop/MoA/current_data_fake.csv"
prior_file_path <- "C:/Users/alwer/OneDrive/Desktop/MoA/prior_data_fake.csv"

# Load and preprocess the datasets
load_data <- function(file_path) {
  read.csv(file_path)
}

current_data <- load_data(current_file_path)
prior_data <- load_data(prior_file_path)



current_data <- current_data %>%
  mutate(
    date_calibration_start_1 = as.Date(date_calibration_start_1, format = "%Y-%m-%d"),
    date_calibration_end = as.Date(date_calibration_end, format = "%Y-%m-%d"),
    date_consent_completed = as.Date(date_consent_completed, format = "%Y-%m-%d"),
    screened_date = as.Date(screened_date, format = "%Y-%m-%d")
  )

prior_data <- prior_data %>%
  mutate(
    date_calibration_start_1 = as.Date(date_calibration_start_1, format = "%Y-%m-%d"),
    date_calibration_end = as.Date(date_calibration_end, format = "%Y-%m-%d"),
    date_consent_completed = as.Date(date_consent_completed, format = "%Y-%m-%d"),
    screened_date = as.Date(screened_date, format = "%Y-%m-%d")
  )

# Define the UI
ui <- fluidPage(
  titlePanel("Calibration Phase Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("current_date", "Select Current Date:", value = Sys.Date()),
      selectInput("coord_filter", "Filter by Coordinator Email:",
                  choices = c("All" = "All", unique(tolower(current_data$coord_email))),
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
                 DTOutput("not_randomized_table")),
        tabPanel("Weekly Changes",
                 h4("Changes From Prior Week"),
                 verbatimTextOutput("change_summary"),
                 DTOutput("change_table"))
      )
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  
  # Check the structure of the data (Debugging Step 1)
  observe({
    print("Current Data:")
    print(str(current_data))  # Preview the structure of current_data
    
    print("Prior Data:")
    print(str(prior_data))    # Preview the structure of prior_data
  })
  
  # Reactive filter for coordinator email
  filtered_data <- reactive({
    print("Applying Coordinator Filter:")
    if (input$coord_filter == "All") {
      print("No filter applied")
      return(current_data)
    } else {
      filtered <- current_data %>% 
        filter(tolower(coord_email) == tolower(input$coord_filter))  # Case-insensitive filtering
      print("Filtered Data Preview:")
      print(head(filtered))  # Preview the filtered data
      return(filtered)
    }
  })
  
  # Weekly Changes Comparison
  changes_data <- reactive({
    req(prior_data)
    
    # Identify changes
    added <- anti_join(current_data, prior_data, by = "record_id_godin") %>% mutate(change_type = "Added")
    removed <- anti_join(prior_data, current_data, by = "record_id_godin") %>% mutate(change_type = "Removed")
    updated <- inner_join(current_data, prior_data, by = "record_id_godin", suffix = c("_current", "_prior")) %>%
      filter(
        training_eligible_current != training_eligible_prior |
          randomized_group_current != randomized_group_prior |
          date_calibration_start_1_current != date_calibration_start_1_prior |
          date_calibration_end_current != date_calibration_end_prior
      ) %>%
      mutate(change_type = "Updated")
    
    # Combine changes
    bind_rows(added, removed, updated)
  })
  
  # Summary of changes
  output$change_summary <- renderText({
    changes <- changes_data()
    paste(
      "Added Participants: ", sum(changes$change_type == "Added"), "\n",
      "Removed Participants: ", sum(changes$change_type == "Removed"), "\n",
      "Updated Participants: ", sum(changes$change_type == "Updated")
    )
  })
  
  # Table of changes
  output$change_table <- renderDT({
    datatable(changes_data(), options = list(pageLength = 10))
  })
  
  # Calibration data
  calibration_data <- reactive({
    current_date <- input$current_date
    print(paste("Current Date:", current_date))  # Debugging step to check the date being used
    
    filtered_data() %>%
      filter(is.na(training_eligible), !is.na(date_calibration_start_1)) %>%
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
             days_remaining, days_since_ended) %>%
      {print(head(.)); .}  # Show the first few rows of the resulting data
  })
  
  output$calibration_table <- renderDT({
    data <- calibration_data()
    print("Rendering Calibration Table:")
    print(head(data))  # Show the first few rows of the data before rendering the table
    datatable(data, options = list(pageLength = 10))
  })
  
  # Cumulative counts
  cumulative_counts <- reactive({
    data <- current_data %>%
      mutate(
        training_eligible = ifelse(is.na(training_eligible), 0, training_eligible),
        randomized_group = ifelse(is.na(randomized_group), 0, randomized_group),
        consented = ifelse(!is.na(date_consent_completed), 1, 0)
      )
    
    total_training_eligible <- sum(data$training_eligible == 1)
    total_screened <- sum(!is.na(data$screened_date))
    total_consented <- sum(data$consented == 1)
    
    paste(
      "Training Eligible: ", total_training_eligible, "\n",
      "Screened: ", total_screened, "\n",
      "Consented: ", total_consented
    )
  })
  
  output$cumulative_counts <- renderText({
    cumulative_counts()
  })
  
  # Not Started Calibration
  not_started_calibration <- reactive({
    filtered_data() %>%
      filter(!is.na(date_consent_completed) & is.na(date_calibration_start_1))
  })
  
  output$not_started_calibration <- renderDT({
    datatable(not_started_calibration(), options = list(pageLength = 10))
  })
  
  # Randomized summary
  randomization_summary <- reactive({
    data <- current_data %>%
      filter(!is.na(randomized_group)) %>%
      group_by(randomized_group) %>%
      summarise(count = n())
    
    paste(
      "Control Group (1): ", sum(data$randomized_group == 1), "\n",
      "Intervention Group (2): ", sum(data$randomized_group == 2)
    )
  })
  
  output$randomization_summary <- renderText({
    randomization_summary()
  })
  
  # Randomized participants
  randomized_table <- reactive({
    filtered_data() %>%
      filter(!is.na(randomized_group))
  })
  
  output$randomized_table <- renderDT({
    datatable(randomized_table(), options = list(pageLength = 10))
  })
  
  # Eligible but not randomized
  not_randomized_table <- reactive({
    filtered_data() %>%
      filter(training_eligible == 1 & is.na(randomized_group))
  })
  
  output$not_randomized_table <- renderDT({
    datatable(not_randomized_table(), options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui = ui, server = server)


server <- function(input, output, session) {
  
  # Check if the data has been loaded successfully
  print("Current Data (Preview):")
  print(head(current_data))  # Preview the first few rows of current_data
  
  print("Prior Data (Preview):")
  print(head(prior_data))    # Preview the first few rows of prior_data
  
  # Other server logic goes here...
}
