# MoA Clinical Trial Weekly Check for Upcoming Go No-Go Decisions, Randomization, and Enrollment

This repository contains R code and data for tracking and analyzing research participants' progress through various phases of a study. The focus is on tracking participants in the **Calibration Phase**, calculating time remaining in this phase, and tracking screening, consent, and training eligibility.

## Purpose

This project aims to provide a dashboard for research coordinators to:

- Track participants in the **Calibration Phase** and calculate time remaining.
- Monitor training eligibility and provide cumulative counts of training-eligible participants.
- Track participants who have been screened and consented.
- Filter data by **coordinator email** and display relevant information for each coordinator.
- Identify participants who have consented but have not started the Calibration Phase.
- Track **randomized groups** (Control or Intervention).

## Key Features

- **Participant Tracking**: Keep track of participants through phases such as Calibration, Screening, Consent, and Training.
- **Date Calculations**: Calculate how long participants have been in each phase and identify overdue actions.
- **Randomization Tracking**: View participants' randomization groups (Control vs. Intervention).
- **Data Filtering**: Filter by coordinator email to view data for specific coordinators.
- **Cumulative Counts**: Display cumulative counts of screened, consented, and training-eligible participants.

## Files in This Repository

- **R Code**: Scripts for data processing, calculating time in phases, and generating summary statistics.
- **Data Files**: Example `.csv` files containing participant tracking data.
- **Output**: Interactive dashboard or reports using R Shiny or R Markdown for real-time analysis.

## Setup Instructions

1. **Clone the Repository**:  
   Clone this repository to your local machine using:
   ```bash
   git clone https://github.com/your-username/participant-tracking-dashboard.git

2. **Install the Required R Packages**:
  install.packages(c("dplyr", "lubridate", "shinydashboard", "ggplot2", "tidyverse"))

3. **Source the R Script and Load Your Data to Generate the Dashboard**:
  source("participant_tracking_script.R")

4. **Customize the Code**
   adjust column names or filtering options as necessary for your dataset

## Example Output

The interactive dashboard will display the following:

- A list of participants in the **Calibration Phase** with time remaining.
- **Cumulative counts** of:
  - Screened participants.
  - Consented participants.
  - Training-eligible participants.
- A filtered view based on **coordinator email**.
- A summary of participants' **randomization status** (Control vs. Intervention).

Contributing
If you have suggestions, bug fixes, or improvements, feel free to open an issue or submit a pull request. We welcome contributions from other researchers and developers.

Notes
Date Format: The project assumes date columns are in the format YYYY-MM-DD. If your data uses a different format, make sure to adjust the mutate functions accordingly.
Data Security: Always be mindful of participant data security and ensure that any data shared does not contain personally identifiable information (PII).
