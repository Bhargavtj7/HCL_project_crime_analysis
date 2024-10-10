# My data set contains majorly 3 types :
#           1) OUTCOMES
#           2) STOP AND SEARCH
#           3) STREET

# I'm taking 2 major cities london and sussex
# I'm taking 3 month data of the same year,i.e from April to June 


# Loading all my neccesary packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(leaflet)
library(plotly)
library(scales)
library(forcats)
library(caret)
library(readr)
library(janitor)
library(sf)
library(ggspatial)

# setting up the directory
setwd("E:/Bhargav/R Project")

# Showing The Files Present In The Directory
list.files()

# Define the main directory path
main_directory <- "E:/Bhargav/R Project"

# Listing  all files in all sub directories with a specific pattern (e.g., CSV files)
all_files <- list.files(path = main_directory, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
all_files

# Read all the files into a list of dataframes
dataframes <- lapply(all_files, read_csv)
dataframes[[1]]

# To name the list elements based on the file names
names(dataframes) <- tools::file_path_sans_ext(basename(all_files))

# Print the names of the files read
print(names(dataframes))



# Load all the necessary dataframes
l_out_04 <- dataframes$`2024-04-city-of-london-outcomes`
l_out_05 <- dataframes$`2024-05-city-of-london-outcomes`
l_out_06 <- dataframes$`2024-06-city-of-london-outcomes`

s_out_04 <- dataframes$`2024-04-sussex-outcomes`
s_out_05 <- dataframes$`2024-05-sussex-outcomes`
s_out_06 <- dataframes$`2024-06-sussex-outcomes`

a_sov_out_04 <- dataframes$`2024-04-avon-and-somerset-outcomes`
a_sov_out_05 <- dataframes$`2024-05-avon-and-somerset-outcomes`
a_sov_out_06 <- dataframes$`2024-06-avon-and-somerset-outcomes`

bed_out_04 <- dataframes$`2024-04-bedfordshire-outcomes`
bed_out_05 <- dataframes$`2024-05-bedfordshire-outcomes`
bed_out_06 <- dataframes$`2024-06-bedfordshire-outcomes`

cam_out_04 <- dataframes$`2024-04-cambridgeshire-outcomes`
cam_out_05 <- dataframes$`2024-05-cambridgeshire-outcomes`
cam_out_06 <- dataframes$`2024-06-cambridgeshire-outcomes`

clev_out_04 <- dataframes$`2024-04-cleveland-outcomes`
clev_out_05 <- dataframes$`2024-05-cleveland-outcomes`
clev_out_06 <- dataframes$`2024-06-cleveland-outcomes`

cumb_out_04 <- dataframes$`2024-04-cumbria-outcomes`
cumb_out_05 <- dataframes$`2024-05-cumbria-outcomes`
cumb_out_06 <- dataframes$`2024-06-cumbria-outcomes`

# Since all the 'outcomes' df have same columns.Let's combine them 
outcomes_df <- bind_rows(l_out_04,l_out_05,l_out_06,
                         s_out_04,s_out_05,s_out_06,
                         a_sov_out_04,a_sov_out_05,a_sov_out_06,
                         bed_out_04,bed_out_05,bed_out_06,
                         cam_out_04,cam_out_05,cam_out_06,
                         cumb_out_04,cumb_out_05,cumb_out_06)


#                         DATA CLEANING   

# cleaning the column names
outcomes_df <- outcomes_df %>%
  clean_names()

#Since 'month' column is character data type and is in the format of 'yyyy-mm'.
#Let's get the mm part alone.
outcomes_df$month <- substr(outcomes_df$month, 6, 7)

# Convert the character data type into numeric data type
outcomes_df$month <- as.numeric(outcomes_df$month)

# Create a function to get the mode alone
get_mode <- function(x) {
  x %>%                 # column name
    na.omit() %>%       # removes all the null values in 'x',to ignore their frequency
    table() %>%         #converts 'x' into vectors and their frequency
    which.max() %>%     # returns the highest frequency (mode)
    names() %>%         # name of the vector having highest frequency
    as.numeric()        # converts it into numeric value
}
# Apply the function to the 'latitude' and 'longitude' columns
outcomes_df <- outcomes_df %>%
  mutate(latitude = ifelse(is.na(latitude),get_mode(latitude),latitude),
         longitude = ifelse(is.na(longitude),get_mode(longitude),longitude))


# To get the rows having at least one NULL value
null_outcomes <- outcomes_df %>%
  filter(rowSums(is.na(.)) > 0)

# Removing the NULL values from the entire data frame
outcomes_df <- outcomes_df %>%
  filter(rowSums(is.na(.)) == 0)



# Load all the necessary dataframes

l_street_04 <- dataframes$`2024-04-city-of-london-street`
l_street_05 <- dataframes$`2024-05-city-of-london-street`
l_street_06 <- dataframes$`2024-06-city-of-london-street`

s_street_04 <- dataframes$`2024-04-sussex-street`
s_street_05 <- dataframes$`2024-05-sussex-street`
s_street_06 <- dataframes$`2024-05-sussex-street`

a_street_04 <- dataframes$`2024-04-avon-and-somerset-street`
a_street_05 <- dataframes$`2024-05-avon-and-somerset-street`
a_street_06 <- dataframes$`2024-06-avon-and-somerset-street`

bed_street_04 <- dataframes$`2024-04-bedfordshire-street`
bed_street_05 <- dataframes$`2024-05-bedfordshire-street`
bed_street_06 <- dataframes$`2024-06-bedfordshire-street`

cam_street_04 <- dataframes$`2024-04-cambridgeshire-street`
cam_street_05 <- dataframes$`2024-05-cambridgeshire-street`
cam_street_06 <- dataframes$`2024-06-cambridgeshire-street`

clev_street_04 <- dataframes$`2024-04-cleveland-street`
clev_street_05 <- dataframes$`2024-05-cleveland-street`
clev_street_06 <- dataframes$`2024-06-cleveland-street`

cumb_street_04 <- dataframes$`2024-04-cumbria-street`
cumb_street_05 <- dataframes$`2024-05-cumbria-street`
cumb_street_06 <- dataframes$`2024-06-cumbria-street`


# Since all the 'street' df have same columns.Let's combine them 
street_df <- bind_rows(l_street_04,l_street_05,l_street_06,
                       s_street_04,s_street_05,s_street_06,
                       a_street_04,a_street_05,a_street_06,
                       bed_street_04,bed_street_05,bed_street_06,
                       cam_street_04,cam_street_05,cam_street_06,
                       clev_street_04,clev_street_05,clev_street_06,
                       cumb_street_04,cumb_street_05,cumb_street_06)

#                         DATA CLEANING   

# cleaning the column names
street_df <- street_df %>%
  clean_names()

#Since the context column have only NULL values.Remove it
street_df <- street_df %>%
  select(-context)

#Since 'month' column is character data type and is in the format of 'yyyy-mm'.
#Let's get the mm part alone.
street_df$month <- substr(street_df$month, 6, 7)

# Convert the character data type into numeric data type
street_df$month <- as.numeric(street_df$month)


#To check the NULL values in each row
NULL_street <- street_df %>%
  filter(rowSums(is.na(.)) > 0)

#Replacing NULL values in 'LOCATION' column with 'No Location'
street_df <- street_df %>%
  mutate(location = ifelse(location =='','No Location',location ))

#Removing all the NULL values
street_df <- street_df %>%
  filter(rowSums(is.na(.)) == 0)



# Load all the necessary dataframes
l_sas_04 <- dataframes$`2024-04-city-of-london-stop-and-search`
l_sas_05 <- dataframes$`2024-05-city-of-london-stop-and-search`
l_sas_06 <- dataframes$`2024-06-city-of-london-stop-and-search`

s_sas_04 <- dataframes$`2024-04-sussex-stop-and-search`
s_sas_05 <- dataframes$`2024-05-sussex-stop-and-search`
s_sas_06 <- dataframes$`2024-06-sussex-stop-and-search`

a_sov_sas_04 <- dataframes$`2024-04-avon-and-somerset-stop-and-search`
a_sov_sas_05 <- dataframes$`2024-05-avon-and-somerset-stop-and-search`
a_sov_sas_06 <- dataframes$`2024-06-avon-and-somerset-stop-and-search`

bed_sas_04 <- dataframes$`2024-04-bedfordshire-stop-and-search`
bed_sas_05 <- dataframes$`2024-05-bedfordshire-stop-and-search`
bed_sas_06 <- dataframes$`2024-06-bedfordshire-stop-and-search`

cam_sas_04 <- dataframes$`2024-04-cambridgeshire-stop-and-search`
cam_sas_05 <- dataframes$`2024-05-cambridgeshire-stop-and-search`
cam_sas_06 <- dataframes$`2024-06-cambridgeshire-stop-and-search`

clev_sas_04 <- dataframes$`2024-04-cleveland-stop-and-search`
clev_sas_05 <- dataframes$`2024-05-cleveland-stop-and-search`
clev_sas_06 <- dataframes$`2024-06-cleveland-stop-and-search`

cumb_sas_04 <- dataframes$`2024-04-cumbria-stop-and-search`
cumb_sas_05 <- dataframes$`2024-05-cumbria-stop-and-search`
cumb_sas_06 <- dataframes$`2024-06-cumbria-stop-and-search`

# Since all the 'outcomes' df have same columns.Let's combine them 
sas_df <- bind_rows(l_sas_04,l_sas_05,l_sas_06,
                    s_sas_04,s_sas_05,s_sas_06,
                    a_sov_sas_04,a_sov_sas_05,a_sov_sas_06,
                    bed_sas_04,bed_sas_05,bed_sas_06,
                    cam_sas_04,cam_sas_05,cam_sas_06,
                    cumb_sas_04,cumb_sas_05,cumb_sas_06)


#                         DATA CLEANING   

# cleaning the column names
sas_df <- sas_df %>%
  clean_names()

# Fill missing values for Part of a Policing Operation
sas_df$part_of_a_policing_operation[is.na(sas_df$part_of_a_policing_operation)] <- "Unknown"

# Fill missing values for Policing Operation
sas_df$policing_operation[is.na(sas_df$policing_operation)] <- "Not specified"

#
sas_df <- sas_df %>%
  filter(!is.na(date))

# Ensure the date column is in the correct format
sas_df <- sas_df %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"))

# Split the date column into Date and Time
sas_df <- sas_df %>%
  mutate(date_only = as.Date(date),
         time = format(date, "%H:%M:%S"))

sas_df <- sas_df %>%
  select(- date)

sas_df <- sas_df %>%
  select(date_only,time,everything())





# Create a function to get the mode alone
get_mode <- function(x) {
  x %>%                 # column name
    na.omit() %>%       # removes all the null values in 'x',to ignore their frequency
    table() %>%         #converts 'x' into vectors and their frequency
    which.max() %>%     # returns the highest frequency (mode)
    names() %>%         # name of the vector having highest frequency
    as.numeric()        # converts it into numeric value
}
# Apply the function to the 'latitude' and 'longitude' columns
sas_df <- sas_df %>%
  mutate(latitude = ifelse(is.na(latitude),get_mode(latitude),latitude),
         longitude = ifelse(is.na(longitude),get_mode(longitude),longitude))


# Fill missing values for categorical columns

sas_df$gender[is.na(sas_df$gender)] <- "Unknown"

sas_df$age_range[is.na(sas_df$age_range)] <- "Not provided"

sas_df$self_defined_ethnicity[is.na(sas_df$self_defined_ethnicity)] <- "Not provided"

sas_df$officer_defined_ethnicity[is.na(sas_df$officer_defined_ethnicity)] <- "Not provided"

sas_df$outcome[is.na(sas_df$outcome)] <- "Unknown"

sas_df$outcome_linked_to_object_of_search[is.na(sas_df$outcome_linked_to_object_of_search)] <- "Unknown"


# *************************************
# Load required libraries
library(shiny)
library(ggplot2)
library(janitor)
library(dplyr)
library(forcats)
library(sf)
library(ggmap)

# Load datasets
# Make sure to replace this with actual loading code or paths to datasets
sas_df <- sas_df  # Assuming sas_df is preloaded
outcomes_df <- outcomes_df # Assuming outcomes_df is preloaded
street_df <- street_df  # Assuming street_df is preloaded

# Prepare the data for plotting (example preparation, adjust as needed)
street_crime_counts <- street_df %>%
  group_by(crime_type) %>%
  summarise(n = n()) %>%
  ungroup()

street_top_locations <- street_df %>%
  group_by(location) %>%
  summarise(n = n()) %>%
  top_n(10) %>%
  ungroup()

crime_month <- street_df %>%
  group_by(month) %>%
  summarise(Count = n()) %>%
  ungroup()

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f0f2f5;
      }
      .form-container {
        width: 400px;  
        padding: 30px; 
        margin: 100px auto;
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
      }
      .form-title {
        text-align: center;
        margin-bottom: 20px;
        font-size: 28px; 
        color: #333;
      }
      .form-control {
        margin-bottom: 15px;
        width: 100%;
        padding: 12px;  
        border: 1px solid #ced4da;
        border-radius: 5px;
      }
      .btn-submit {
        width: 100%;
        background-color: #007bff;
        color: white;
        border: none;
        padding: 12px; 
        font-size: 16px;
        border-radius: 5px;
      }
      .btn-submit:hover {
        background-color: #0056b3;
      }
      .link {
        text-align: center;
        margin-top: 10px;
      }
      .link a {
        color: #007bff;
        text-decoration: none;
      }
      .dashboard-container {
        margin: 50px auto;
        padding: 20px;
        width: 80%;
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
      }
      .box {
        padding: 20px;
        border: 1px solid #007bff;
        border-radius: 5px;
        margin: 15px;
        text-align: center;
        font-size: 20px;
      }
    "))
  ),
  
  # Main UI components
  uiOutput("main_ui")
)

# Server
server <- function(input, output, session) {
  current_view <- reactiveVal("login")
  
  output$main_ui <- renderUI({
    if (current_view() == "login") {
      div(class = "form-container",
          h2(class = "form-title", "Login"),
          tags$input(type = "text", id = "username", placeholder = "Enter your username", class = "form-control"),
          tags$input(type = "password", id = "password", placeholder = "Enter your password", class = "form-control"),
          actionButton("login_button", "Login", class = "btn-submit"),
          div(class = "link",
              "Don't have an account? ",
              actionLink("register_link", "Create one")
          )
      )
    } else if (current_view() == "signup") {
      div(class = "form-container",
          h2(class = "form-title", "Create Account"),
          tags$input(type = "text", id = "new_username", placeholder = "Choose a username", class = "form-control"),
          tags$input(type = "password", id = "new_password", placeholder = "Choose a password", class = "form-control"),
          actionButton("signup_button", "Sign Up", class = "btn-submit"),
          div(class = "link",
              "Already have an account? ",
              actionLink("login_link", "Login")
          )
      )
    } else if (current_view() == "dashboard") {
      div(class = "dashboard-container",
          h2("Dashboard"),
          actionButton("outcomes_button", "Outcomes", class = "box"),
          actionButton("street_button", "Street Crimes", class = "box"),
          actionButton("stop_search_button", "Stop and Search", class = "box")
      )
    } else if (current_view() == "outcomes") {
      div(class = "dashboard-container",
          h2("Outcomes Options"),
          actionButton("outcome_summary_button", "Outcome Summary", class = "box"),
          actionButton("police_force_button", "Total Crimes by Police Force", class = "box"),
          actionButton("total_crimes_month_button", "Total Crimes by Month", class = "box"),
          actionButton("month_top_outcomes_button", "Top 3 Outcomes by Month", class = "box"),
          actionButton("back_dashboard", "Back to Dashboard")
      )
    } else if (current_view() == "street") {
      div(class = "dashboard-container",
          h2("Street Crimes Options"),
          actionButton("street_option_1", "Street Crime Option 1", class = "box"),
          actionButton("street_option_2", "Street Crime Option 2", class = "box"),
          actionButton("back_dashboard", "Back to Dashboard")
      )
    } else if (current_view() == "stop_search") {
      div(class = "dashboard-container",
          h2("Stop and Search Options"),
          actionButton("age_button", "By Age", class = "box"),
          actionButton("gender_button", "By Gender", class = "box"),
          actionButton("ethnicity_button", "By Ethnicity", class = "box"),
          actionButton("outcome_button", "By Outcome", class = "box"),
          actionButton("back_dashboard", "Back to Dashboard")
      )
    } else if (current_view() == "outcome_summary") {
      div(class = "dashboard-container",
          h2("Outcome Summary"),
          plotOutput("outcomeTypePlot"),
          actionButton("back_outcomes", "Back to Outcomes")
      )
    } else if (current_view() == "police_force") {
      div(class = "dashboard-container",
          h2("Total Crimes by Police Force"),
          plotOutput("policeForcePlot"),
          actionButton("back_outcomes", "Back to Outcomes")
      )
    } else if (current_view() == "total_crimes_month") {
      div(class = "dashboard-container",
          h2("Total Crimes by Month"),
          plotOutput("totalCrimesByMonthPlot"),
          actionButton("back_outcomes", "Back to Outcomes")
      )
    } else if (current_view() == "month_top_outcomes") {
      div(class = "dashboard-container",
          h2("Top 3 Outcomes by Month"),
          plotOutput("monthTopOutcomesPlot"),
          actionButton("back_outcomes", "Back to Outcomes")
      )
    } else if (current_view() == "age") {
      div(class = "dashboard-container",
          h2("Stop and Search by Age Group"),
          plotOutput("stopSearchAgePlot"),
          actionButton("back_stop_search", "Back to Stop and Search")
      )
    } else if (current_view() == "gender") {
      div(class = "dashboard-container",
          h2("Stop and Search by Gender"),
          plotOutput("stopSearchGenderPlot"),
          actionButton("back_stop_search", "Back to Stop and Search")
      )
    } else if (current_view() == "ethnicity") {
      div(class = "dashboard-container",
          h2("Ethnicity Distribution of Stop and Search"),
          plotOutput("ethnicityPlot"),
          actionButton("back_stop_search", "Back to Stop and Search")
      )
    } else if (current_view() == "outcome") {
      div(class = "dashboard-container",
          h2("Outcome of Stop and Search"),
          plotOutput("outcomePlot"),
          actionButton("back_stop_search", "Back to Stop and Search")
      )
    } else if (current_view() == "street_crime") {
      div(class = "dashboard-container",
          h2("Street Crime Analysis"),
          plotOutput("crimeTypeBarPlot"),
          plotOutput("crimeTypePiePlot"),
          plotOutput("topLocationsBarPlot"),
          plotOutput("topLocationsPiePlot"),
          plotOutput("monthlyCrimePlot"),
          plotOutput("mapPlot"),
          actionButton("back_street", "Back to Street Options")
      )
    }
  })
  
  observeEvent(input$login_button, {
    showNotification("Login successful! Redirecting to dashboard.", type = "message")
    current_view("dashboard")
  })
  
  observeEvent(input$signup_button, {
    showNotification("Account created! Redirecting to login.", type = "message")
    current_view("login")
  })
  
  observeEvent(input$register_link, {
    current_view("signup")
  })
  
  observeEvent(input$login_link, {
    current_view("login")
  })
  
  observeEvent(input$outcomes_button, {
    current_view("outcomes")
  })
  
  observeEvent(input$street_button, {
    current_view("street")
  })
  
  observeEvent(input$stop_search_button, {
    current_view("stop_search")
  })
  
  # Outcomes Button Actions
  observeEvent(input$outcome_summary_button, {
    current_view("outcome_summary")
  })
  
  observeEvent(input$police_force_button, {
    current_view("police_force")
  })
  
  observeEvent(input$total_crimes_month_button, {
    current_view("total_crimes_month")
  })
  
  observeEvent(input$month_top_outcomes_button, {
    current_view("month_top_outcomes")
  })
  
  # Stop and Search Button Actions
  observeEvent(input$age_button, {
    current_view("age")
  })
  
  observeEvent(input$gender_button, {
    current_view("gender")
  })
  
  observeEvent(input$ethnicity_button, {
    current_view("ethnicity")
  })
  
  observeEvent(input$outcome_button, {
    current_view("outcome")
  })
  
  # Street Button Actions
  observeEvent(input$street_option_1, {
    current_view("street_crime")
  })
  
  observeEvent(input$street_option_2, {
    current_view("street_crime")
  })
  
  # Plots
  output$outcomeTypePlot <- renderPlot({
    ggplot(outcomes_df, aes(x = outcome_type)) +
      geom_bar(fill = "steelblue") +
      labs(title = "Outcome Type Distribution", x = "Outcome Type", y = "Count")
  })
  
  output$policeForcePlot <- renderPlot({
    ggplot(outcomes_df, aes(x = police_force, y = total_crimes)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(title = "Total Crimes by Police Force", x = "Police Force", y = "Total Crimes")
  })
  
  output$totalCrimesByMonthPlot <- renderPlot({
    ggplot(outcomes_df, aes(x = factor(month), y = total_crimes)) +
      geom_line() +
      geom_point() +
      labs(title = "Total Crimes by Month", x = "Month", y = "Total Crimes")
  })
  
  output$monthTopOutcomesPlot <- renderPlot({
    top_outcomes <- outcomes_df %>%
      group_by(month, outcome_type) %>%
      summarise(total = n()) %>%
      top_n(3, total)
    
    ggplot(top_outcomes, aes(x = month, y = total, color = outcome_type)) +
      geom_line() +
      labs(title = "Top 3 Outcomes by Month", x = "Month", y = "Total")
  })
  
  output$stopSearchAgePlot <- renderPlot({
    ggplot(sas_df, aes(x = age_group)) +
      geom_bar(fill = "purple") +
      labs(title = "Stop and Search by Age Group", x = "Age Group", y = "Count")
  })
  
  output$stopSearchGenderPlot <- renderPlot({
    ggplot(sas_df, aes(x = gender)) +
      geom_bar(fill = "green") +
      labs(title = "Stop and Search by Gender", x = "Gender", y = "Count")
  })
  
  output$ethnicityPlot <- renderPlot({
    ggplot(sas_df, aes(x = ethnicity)) +
      geom_bar(fill = "blue") +
      labs(title = "Stop and Search by Ethnicity", x = "Ethnicity", y = "Count")
  })
  
  output$outcomePlot <- renderPlot({
    ggplot(sas_df, aes(x = outcome)) +
      geom_bar(fill = "red") +
      labs(title = "Stop and Search Outcomes", x = "Outcome", y = "Count")
  })
  
  output$crimeTypeBarPlot <- renderPlot({
    ggplot(street_crime_counts, aes(x = crime_type, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Street Crime Types", x = "Crime Type", y = "Count")
  })
  
  output$crimeTypePiePlot <- renderPlot({
    ggplot(street_crime_counts, aes(x = "", y = n, fill = crime_type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = "Distribution of Street Crimes by Type")
  })
  
  output$topLocationsBarPlot <- renderPlot({
    ggplot(street_top_locations, aes(x = reorder(location, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Locations for Street Crimes", x = "Location", y = "Number of Crimes")
  })
  
  output$topLocationsPiePlot <- renderPlot({
    ggplot(street_top_locations, aes(x = "", y = n, fill = location)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = "Distribution of Street Crimes by Location")
  })
  
  output$monthlyCrimePlot <- renderPlot({
    ggplot(crime_month, aes(x = factor(month), y = Count)) +
      geom_line() +
      geom_point() +
      labs(title = "Monthly Crime Count", x = "Month", y = "Crime Count")
  })
  
  output$mapPlot <- renderPlot({
    # Assuming street_df has longitude and latitude columns
    crime_locations <- st_as_sf(street_df, coords = c("longitude", "latitude"), crs = 4326)
    crime_map <- get_map(location = 'Chennai', zoom = 12) # Adjust location and zoom as necessary
    
    ggmap(crime_map) +
      geom_sf(data = crime_locations, aes(color = crime_type), size = 3) +
      labs(title = "Crime Locations in Chennai", x = "Longitude", y = "Latitude")
  })
  
  # Back button functionality for plots
  observeEvent(input$back_outcomes, {
    current_view("outcomes")
  })
  
  observeEvent(input$back_stop_search, {
    current_view("stop_search")
  })
  
  observeEvent(input$back_street, {
    current_view("street")
  })
  
  # Optional: Handle the back button in the street crime dashboard
  observeEvent(input$street_option_1, {
    current_view("street_crime")
  })
  
  observeEvent(input$street_option_2, {
    current_view("street_crime")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
