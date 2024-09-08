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



#************************ STREET **********************************

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

#               UNDERSTANDING OF THE DATASET

#summarizing the data set
summary(street_df)

#Table for crime type & last outcome category
table(street_df$crime_type)
table(street_df$last_outcome_category)

#Getting the count of crimes
street_crime_counts <- street_df %>%
  count(crime_type, sort = TRUE)

#Top 10 most dangerous locations
street_top_locations <- street_df %>%
  count(location,sort = TRUE) %>%
  top_n(10)
#
crime_month <- street_df %>%
  group_by(month) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))




#                     DATA VISULIZATION

# Bar chart for 'Crime Type' vs 'crime count'
ggplot(street_crime_counts, aes(x = fct_reorder(crime_type, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Crimes by Type",
       x = "Crime Type", y = "Crime Count")

#pie chart for 'Crime Type' vs 'crime count'
ggplot(street_crime_counts, aes(x = "", y = n, fill = crime_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Crimes by Type",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),    
        panel.grid = element_blank())    

# Bar chart for  'Top 10 Crime Locations' vs 'Number of Crimes'
ggplot(street_top_locations, aes(x = fct_reorder(location, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Crime Locations",
       x = "Location", y = "Number of Crimes")


# pie chart for  'Top 10 Crime Locations' vs 'Number of Crimes'
ggplot(street_top_locations, aes(x = "", y = n, fill = location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Top 10 Crime Locations",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(),   
        axis.ticks = element_blank(),    
        panel.grid = element_blank())    

# Bar plot for 'month' VS 'crime counts'
ggplot(crime_month, aes(x = reorder(month, Count), y = Count)) +
geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Crimes by Month (Descending Order)", 
       x = "Month", 
       y = "Crime Count") +
  theme_minimal() +
  coord_flip()




#Interactive Plotting the crime on real map
street_df$crime_type <- as.factor(street_df$crime_type)

# Create a color palette for crime types
pal <- colorFactor(topo.colors(length(unique(london_outcomes_04$crime_type))), domain = london_outcomes_04$crime_type)

leaflet(street_df) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, color = ~pal(crime_type),
                   popup = ~paste("Type:", crime_type, "<br>",
                                  "Outcome:", last_outcome_category),
                   radius = 4) %>%
  addLegend("bottomright", pal = pal,
            values = ~crime_type, title = "Crime Type")


#                         PREDICTIONS

# Since these columns are characters convert them into factors
street_df$crime_type <- as.factor(street_df$crime_type)
street_df$last_outcome_category <- as.factor(street_df$last_outcome_category)
street_df$location <- as.factor(street_df$location)

memory.limit(size = 16000)
# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(street_df$last_outcome_category, p = 0.8, list = FALSE)

train_data <- street_df[sample(nrow(train_data), 10000), ]
test_data <- street_df[-train_index, ]

# Train a Random Forest model to predict crime outcomes
#model <- train(last_outcome_category ~ crime_type + location + longitude + latitude,
#               data = train_data, 
#              method = "rf", 
#             trControl = trainControl(method = "cv", number = 5), 
#            importance = TRUE)

# Predict outcomes on the test set
#predictions <- predict(model, test_data)

# Confusion matrix
#confusionMatrix(predictions, test_data$last_outcome_category)












