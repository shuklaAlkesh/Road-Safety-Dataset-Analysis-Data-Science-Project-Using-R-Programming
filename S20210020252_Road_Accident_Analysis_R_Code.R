# Road_Safety_Data_Accidents_2019

setwd("C://Users//abc//OneDrive//Documents//R programming//Road_safety_Project")
stores<-read.csv("Road_Safety_Data _Accidents_2019.csv");  # Corrected file name
print(ncol(stores))  # 32
print(nrow(stores))  # 117536

# Print All the column names
column_names <- colnames(stores)
print(column_names)
####################################################################################
# Cleaning the data
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("mice", quietly = TRUE)) {
  install.packages("mice")
}
library(dplyr)
library(mice)

# Create a copy of the dataset for cleaning
cleaned_data <- stores

# Finding NaN values in each column
for (col in column_names) {
  print(col)
  print(sum(is.na(cleaned_data[[col]])))
}

# Remove rows with missing values
cleaned_data <- na.omit(cleaned_data)

print(ncol(cleaned_data))  # 32
print(nrow(cleaned_data))  # 117508

# Re-check missing values after imputation/removal
for (col in column_names) {
  print(col)
  print(sum(is.na(cleaned_data[[col]])))
}

head(cleaned_data)

# Summary statistics after cleaning
summary_stats_cleaned <- summary(cleaned_data)
print(summary_stats_cleaned)

# Identify categorical and numerical columns
categorical_columns <- sapply(cleaned_data, is.factor)
numerical_columns <- sapply(cleaned_data, is.numeric)

# Print the results
cat("Categorical Columns:\n")
print(names(cleaned_data[categorical_columns]))

cat("\nNumerical Columns:\n")
print(names(cleaned_data[numerical_columns]))

############################################################################################
# Data set Analysis of distribution of accidents over time patterns in accidents throughout the year,month,day.

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)

# Convert the 'Date' column to a Date type
cleaned_data$Date <- as.Date(cleaned_data$Date, format = "%d/%m/%Y")

# Create a time series plot
library(ggplot2)
ggplot(cleaned_data, aes(x = factor(month(Date, label = TRUE)), fill = factor(Accident_Severity))) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Accidents Over Months",
       x = "Month",
       y = "Number of Accidents",
       fill = "Severity") +
  scale_x_discrete(name = "Month") +
  scale_fill_discrete(name = "Severity",
                      labels = c("Slight", "Serious", "Fatal"))
########################################################################
# Analize the Accident happen on the map 
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}
library(leaflet)

# Create a leaflet map
accident_map <- leaflet(data = cleaned_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = 1,
    color = "red",
    fillOpacity = 0.7,
    popup = paste("Severity: ", cleaned_data$Accident_Severity)
  )

# Display the map
accident_map

#################################################################
# Check the structure of the severity_data data frame


# Load ggplot2
library(ggplot2)

# Calculate the counts for each severity level
severity_counts <- table(cleaned_data$Accident_Severity)

# Rename the columns
severity_data <- data.frame(Severity = factor(names(severity_counts)), Count = as.integer(severity_counts))

# Create a pie chart for the distribution of accident severity
ggplot(severity_data, aes(x = "", y = Count, fill = Severity)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Accident Severity", fill = "Severity") +
  theme_minimal() +
  scale_fill_discrete(name = "Severity", labels = c("Slight", "Serious", "Fatal"))


#####################################################################
## Speed Limit Analysis by Accident Severity

# Create a box plot for the distribution of speed limits by accident severity
ggplot(cleaned_data, aes(x = factor(Accident_Severity), y = Speed_limit)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(
    title = "Speed Limit Analysis by Accident Severity",
    x = "Accident Severity",
    y = "Speed Limit"
  ) +
  scale_x_discrete(labels = c("Fatal", "Serious", "Slight")) +  # Add severity labels
  theme_minimal()
#####################################################################
# Road type Analyisis.
# Create a bar chart for the distribution of accidents based on road types
road_type_counts <- table(cleaned_data$Road_Type)
road_type_data <- data.frame(Road_Type = factor(names(road_type_counts)), Count = as.integer(road_type_counts))

# Plot the bar chart with labels
library(ggplot2)

ggplot(road_type_data, aes(x = Road_Type, y = Count, fill = Road_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +  # Add labels on top of the bars
  labs(
    title = "Distribution of Accidents Based on Road Types",
    x = "Road Type",
    y = "Number of Accidents",
    fill = "Road Type",
    caption = "Source: Your Dataset"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

####################################################
# Convert the 'Date' column to a Date type
cleaned_data$Date <- as.Date(cleaned_data$Date, format = "%d/%m/%Y")

# Create a new column for the day of the week
cleaned_data$Day_of_Week <- factor(weekdays(cleaned_data$Date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Create a bar chart for the distribution of accidents over days of the week
day_of_week_counts <- table(cleaned_data$Day_of_Week)
day_of_week_data <- data.frame(Day_of_Week = factor(names(day_of_week_counts), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), 
                               Count = as.integer(day_of_week_counts))

# Ensure Count is numeric
day_of_week_data$Count <- as.numeric(day_of_week_data$Count)

# Plot the bar chart
library(ggplot2)

ggplot(day_of_week_data, aes(x = Day_of_Week, y = Count, fill = Day_of_Week)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +  # Add labels for counts
  labs(title = "Distribution of Accidents Over Days of the Week", x = "Day of the Week", y = "Number of Accidents", fill = "Day of the Week") +
  theme_minimal()

#####################################################
#### corrleation analysis
# Select only numerical columns for correlation analysis
numerical_columns <- sapply(cleaned_data, is.numeric)
correlation_data <- cleaned_data[, numerical_columns]

# Compute correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Round the correlation matrix to 2 digits
correlation_matrix <- round(correlation_matrix, digits = 2)

# Load the reshape2 package
library(reshape2)

# Prepare data for ggplot2
melted_cormat <- melt(correlation_matrix)

# Plot the correlation matrix as a heatmap
library(ggplot2)

# Plot the correlation matrix as a heatmap with adjustments
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Add white borders to tiles for better separation
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Change color palette
  labs(title = "Correlation Matrix for Your Dataset") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8, color = "black"),  # Adjust text size and color
    axis.text.y = element_text(size = 8, color = "black"),  # Adjust text size and color
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_blank(),  # Remove panel borders
    axis.ticks = element_blank(),  # Remove axis ticks
    legend.position = "right"  # Move legend to the right
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 0.5)  # Add diagonal line




