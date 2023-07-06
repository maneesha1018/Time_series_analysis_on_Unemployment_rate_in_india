#####USING MAD METHOD######

# Load the necessary libraries
library(dplyr)
library(zoo)

# Load the data into a data frame
data <- read.csv('E:\\Main project Journals\\Unemployment_rate_in_kerala\\df.csv')

# Convert the data into a time series object
ts_data <- ts(data$Value)

# Detect outliers using the median absolute deviation (MAD) method
med <- median(ts_data, na.rm = TRUE)
mad <- median(abs(ts_data - med), na.rm = TRUE)
threshold <- 3 * mad
outliers <- which(abs(ts_data - med) > threshold)

# Remove the outliers from the time series data
ts_data_clean <- ts_data[-outliers]

# Convert the cleaned time series object back to a data frame
data_clean <- data.frame(Date = time(ts_data_clean), Value = coredata(ts_data_clean))

# Save the cleaned data to a CSV file
write.csv(data_clean, "daily_data_clean.csv", row.names = FALSE)

####
