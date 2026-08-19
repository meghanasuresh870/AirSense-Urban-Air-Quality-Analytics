# ==========================================
# AirSense - Week 2
# Data Visualization & Exploratory Analysis
# ==========================================

library(tidyverse)

# Use cleaned dataset from Week 1
air_quality <- air_quality_clean

# Check cleaned data
dim(air_quality)
summary(air_quality)
# AQI Distribution

ggplot(air_quality, aes(x = AQI)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Air Quality Index (AQI)",
    x = "AQI",
    y = "Number of Observations"
  ) +
  theme_minimal()
# City-wise Average AQI

city_aqi <- air_quality %>%
  group_by(City) %>%
  summarise(
    Average_AQI = mean(AQI, na.rm = TRUE)
  ) %>%
  arrange(desc(Average_AQI))

city_aqi
# City-wise Average AQI Visualization

ggplot(city_aqi, aes(x = reorder(City, Average_AQI),
                     y = Average_AQI)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average AQI by City",
    x = "City",
    y = "Average AQI"
  ) +
  theme_minimal()
# Pollutant Distribution

pollutant_data <- air_quality %>%
  select(`PM2.5`, PM10, Ozone, NO2, CO, SO2) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Pollutant",
    values_to = "Concentration"
  )

ggplot(pollutant_data, aes(x = Pollutant, y = Concentration)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Major Air Pollutants",
    x = "Pollutant",
    y = "Concentration"
  ) +
  theme_minimal()
# AQI vs PM2.5

ggplot(air_quality, aes(x = `PM2.5`, y = AQI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between PM2.5 and AQI",
    x = "PM2.5 Concentration",
    y = "AQI"
  ) +
  theme_minimal()
# Correlation between AQI and PM2.5

cor(
  air_quality$AQI,
  air_quality$`PM2.5`,
  use = "complete.obs"
)
# Monthly AQI Trend

monthly_aqi <- air_quality %>%
  mutate(
    Month = floor_date(Date, "month")
  ) %>%
  group_by(Month) %>%
  summarise(
    Average_AQI = mean(AQI, na.rm = TRUE)
  )

ggplot(monthly_aqi, aes(x = Month, y = Average_AQI)) +
  geom_line() +
  labs(
    title = "Monthly Average AQI Trend",
    x = "Month",
    y = "Average AQI"
  ) +
  theme_minimal()
# ==========================================
# Step 7: Pollutant Correlation Analysis
# ==========================================

correlation_data <- air_quality %>%
  select(AQI, `PM2.5`, PM10, Ozone, NO2, CO, SO2,
         Temperature, Humidity)

correlation_matrix <- cor(
  correlation_data,
  use = "complete.obs"
)

round(correlation_matrix, 2)
# Convert correlation matrix to long format

correlation_long <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(
    -Variable1,
    names_to = "Variable2",
    values_to = "Correlation"
  )

# Correlation Heatmap

ggplot(correlation_long,
       aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  labs(
    title = "Correlation Matrix of Air Quality Variables",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# ==========================================
# Step 9: AQI Category Analysis
# ==========================================

air_quality <- air_quality %>%
  mutate(
    AQI_Category = case_when(
      AQI <= 50 ~ "Good",
      AQI <= 100 ~ "Moderate",
      AQI <= 150 ~ "Unhealthy for Sensitive Groups",
      AQI <= 200 ~ "Unhealthy",
      AQI <= 300 ~ "Very Unhealthy",
      AQI > 300 ~ "Hazardous"
    )
  )

# Count observations in each AQI category

aqi_categories <- air_quality %>%
  count(AQI_Category)

aqi_categories
# ==========================================
# Step 10: AQI Category Distribution
# ==========================================

ggplot(aqi_categories,
       aes(x = reorder(AQI_Category, n),
           y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribution of AQI Categories",
    x = "AQI Category",
    y = "Number of Observations"
  ) +
  theme_minimal()
# ==========================================
# Step 11: AQI Category by City
# ==========================================

city_category <- air_quality %>%
  count(City, AQI_Category)

city_category
ggplot(city_category,
       aes(x = reorder(City, n),
           y = n,
           fill = AQI_Category)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "AQI Category Distribution by City",
    x = "City",
    y = "Number of Observations",
    fill = "AQI Category"
  ) +
  theme_minimal()
# ==========================================
# Step 12: Monthly AQI Trend
# ==========================================

monthly_aqi <- air_quality %>%
  mutate(
    Month = floor_date(Date, "month")
  ) %>%
  group_by(Month) %>%
  summarise(
    Average_AQI = mean(AQI, na.rm = TRUE)
  )

monthly_aqi
ggplot(monthly_aqi,
       aes(x = Month, y = Average_AQI)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Average AQI Trend",
    x = "Month",
    y = "Average AQI"
  ) +
  theme_minimal()
# ==========================================
# Step 13: Average PM2.5 by City
# ==========================================

city_pm25 <- air_quality %>%
  group_by(City) %>%
  summarise(
    Average_PM25 = mean(`PM2.5`, na.rm = TRUE)
  ) %>%
  arrange(desc(Average_PM25))

city_pm25
ggplot(city_pm25,
       aes(x = reorder(City, Average_PM25),
           y = Average_PM25)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average PM2.5 Concentration by City",
    x = "City",
    y = "Average PM2.5"
  ) +
  theme_minimal()
# ==========================================
# Step 14: Weather Factors vs AQI
# ==========================================

# AQI vs Temperature

ggplot(air_quality,
       aes(x = Temperature, y = AQI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQI vs Temperature",
    x = "Temperature",
    y = "AQI"
  ) +
  theme_minimal()
# AQI vs Humidity

ggplot(air_quality,
       aes(x = Humidity, y = AQI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQI vs Humidity",
    x = "Humidity",
    y = "AQI"
  ) +
  theme_minimal()
# AQI vs PM2.5

ggplot(air_quality, aes(x = `PM2.5`, y = AQI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQI vs PM2.5",
    x = "PM2.5",
    y = "AQI"
  ) +
  theme_minimal()
# AQI vs PM10

ggplot(air_quality, aes(x = PM10, y = AQI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQI vs PM10",
    x = "PM10",
    y = "AQI"
  ) +
  theme_minimal()
# AQI vs Ozone

ggplot(air_quality, aes(x = Ozone, y = AQI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AQI vs Ozone",
    x = "Ozone",
    y = "AQI"
  ) +
  theme_minimal()
