# ==========================================
# AirSense - Week 1
# Data Cleaning and Preliminary Analysis
# ==========================================

library(tidyverse)


# Load dataset
air_quality <- read_csv("Data/air_quality_dataset.csv")

# Check dimensions
dim(air_quality)

# Column names
names(air_quality)

# Structure
str(air_quality)

# Summary statistics
summary(air_quality)

# Missing values in each column
colSums(is.na(air_quality))

# ==========================================
# Remove Duplicate Records
# ==========================================

# Check number of rows before removing duplicates
nrow(air_quality)

# Remove duplicate rows
air_quality_clean <- air_quality %>%
  distinct()

# Check number of rows after removing duplicates
nrow(air_quality_clean)

# Number of duplicates removed
nrow(air_quality) - nrow(air_quality_clean)
library(tidyverse)
# ==========================================
# Step 2: Missing Value Analysis
# ==========================================

missing_clean <- data.frame(
  Variable = names(air_quality_clean),
  Missing_Count = colSums(is.na(air_quality_clean)),
  Missing_Percentage = round(
    colSums(is.na(air_quality_clean)) /
      nrow(air_quality_clean) * 100, 2
  )
)

missing_clean
# ==========================================
# Step 3: Handle Missing Values
# Median Imputation for Numerical Variables
# ==========================================

numeric_vars <- c(
  "AQI", "PM2.5", "PM10", "Ozone",
  "NO2", "CO", "SO2", "Temperature", "Humidity"
)

for (var in numeric_vars) {
  air_quality_clean[[var]][is.na(air_quality_clean[[var]])] <-
    median(air_quality_clean[[var]], na.rm = TRUE)
}

# Check missing values after imputation
colSums(is.na(air_quality_clean))
# ==========================================
# Step 4: Outlier Analysis using IQR Method
# ==========================================

outlier_summary_clean <- data.frame(
  Variable = numeric_vars,
  Outlier_Count = sapply(air_quality_clean[numeric_vars], function(x) {
    
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    sum(
      x < lower_bound | x > upper_bound,
      na.rm = TRUE
    )
  })
)

outlier_summary_clean
# ==========================================
# Step 5: Final Data Validation
# ==========================================

# Check dimensions
dim(air_quality_clean)

# Check missing values
colSums(is.na(air_quality_clean))

# Check duplicate rows
sum(duplicated(air_quality_clean))

# Check negative pollutant values
air_quality_clean %>%
  summarise(
    AQI_negative = sum(AQI < 0),
    PM25_negative = sum(`PM2.5` < 0),
    PM10_negative = sum(PM10 < 0),
    Ozone_negative = sum(Ozone < 0),
    NO2_negative = sum(NO2 < 0),
    CO_negative = sum(CO < 0),
    SO2_negative = sum(SO2 < 0)
  )
