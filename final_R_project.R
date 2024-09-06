# I'm Using R language for the analysis purposes
# I've done the coding using R studio
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


#***********************    OUTCOMES  ********************************

# Load all the necessary dataframes
l_out_04 <- dataframes$`2024-04-city-of-london-outcomes`
l_out_05 <- dataframes$`2024-05-city-of-london-outcomes`
l_out_06 <- dataframes$`2024-06-city-of-london-outcomes`
s_out_04 <- dataframes$`2024-04-sussex-outcomes`
s_out_05 <- dataframes$`2024-05-sussex-outcomes`
s_out_06 <- dataframes$`2024-06-sussex-outcomes`

# Since all the 'outcomes' df have same columns.Let's combine them 
outcomes_df <- bind_rows(l_out_04,l_out_05,l_out_06,s_out_04,s_out_05,s_out_06)


#*********************    DATA CLEANING   *******************************

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



#                              DATA UNDERSTANDING

#Finding the count of each outcome in the data frame
outcome_summary <- outcomes_df %>%
  group_by(outcome_type) %>%
  summarise(count_type = n()) %>%
  arrange(desc(count_type))

# we can say,Investigation complete; no suspect identified, has the most number
#of counts

#Finding the count of each reported by in the data frame
crime_count_by_force <- outcomes_df %>%
  group_by(reported_by) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes))

# We can say that Sussex Police have registered more cases 


# Finding the month which has more no. of outcomes
crime_month <- outcomes_df %>%
  group_by(month) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes))
# Therefore,we can say 4th month i.e April month has more no.of outcomes


# Finding the outcome type by location analysis
outcome_by_location <- outcomes_df %>%
  group_by(location, outcome_type) %>%
  summarise(crime_Count = n()) %>%
  arrange(desc(crime_Count))

# Detailed analysis for top crime outcomes
top_outcomes <- outcomes_df %>%
  group_by(outcome_type) %>%
  summarise(crime_Count = n()) %>%
  arrange(desc(crime_Count))

# Filter for the top 3 outcomes and group by Month
month_top_outcomes <- outcomes_df %>%
  filter(outcome_type %in% top_outcomes$outcome_type[1:3]) %>%
  group_by(month, outcome_type) %>%
  summarise(Crime_Count = n()) %>%
  arrange(month)

# Focus on the top 3 outcomes
top_outcomes_summary <- outcomes_df %>%
  filter(outcome_type %in% top_outcomes$outcome_type[1:3]) %>%
  group_by(location, outcome_type) %>%
  summarise(Crime_Count = n())


# contingency table for locations and outcome types
location_outcome_corr <- top_outcomes_summary %>%
  spread(outcome_type,Crime_Count,fill = 0)


#                       DATA VISUALIZATION

# Plotting 'outcome type' VS 'count'
ggplot(outcome_summary, aes(x = reorder(outcome_type, count_type), y = count_type)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Summary of Outcomes", x = "Outcome Type", y = "Count") +
  theme_minimal() +
  coord_flip()

# Plotting 'Police Force' VS 'Total Crimes'
ggplot(crime_count_by_force, aes(x = reorder(reported_by, Total_Crimes), y = Total_Crimes)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Total Crimes by Reporting Force", x = "Police Force", y = "Total Crimes") +
  theme_minimal() +
  coord_flip()


# Plotting 'month' VS 'Total Crimes'
ggplot(crime_month, aes(x = reorder(month, Total_Crimes), y = Total_Crimes)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Total Crimes by Month", x = "Month", y = "Total Crimes") +
  theme_minimal() +
  coord_flip()

# Plotting 'month' VS 'Total Crimes' for the top 3 outcomes
ggplot(month_top_outcomes, aes(x = month, y = Crime_Count, color = outcome_type)) +
  geom_line(size = 1) +
  labs(title = "Crime Trends Over Time for Top 3 Outcomes", 
       x = "Month", 
       y = "Number of Crimes", 
       color = "Outcome Type") +
  theme_minimal()

# Define a color palette for outcome types
pal <- colorFactor(
  palette = c("red", "blue", "green", "orange", "purple", "yellow", "pink", "brown", "gray", "cyan", "magenta"),
  domain = outcomes_df$outcome_type
)

# Create the map with circle markers
leaflet(outcomes_df) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude, 
    radius = 3, 
    color = ~pal(outcome_type),  # Use the color palette for coloring based on outcome_type
    popup = ~paste0("Location: ", location, "<br>Outcome: ", outcome_type),
    label = ~paste0("Crime ID: ", crime_id)
  ) %>%
  setView(lng = mean(outcomes_df$longitude), lat = mean(outcomes_df$latitude), zoom = 10)



#************************ STOP AND SEARCH **********************************

