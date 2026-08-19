# ==========================================
# AIRSENSE - WEEK 4
# FINAL DATA ANALYSIS & REPORTING
# ==========================================

# Load packages
library(tidyverse)
library(lubridate)
library(caret)

# ==========================================
# 1. USE CLEANED DATASET
# ==========================================

air_quality <- air_quality_clean

# Check dataset
dim(air_quality)
str(air_quality)
summary(air_quality)


# ==========================================
# 2. DATASET OVERVIEW
# ==========================================

dataset_overview <- data.frame(
  Metric = c(
    "Number of Observations",
    "Number of Variables",
    "Number of Cities",
    "Start Date",
    "End Date"
  ),
  Value = c(
    nrow(air_quality),
    ncol(air_quality),
    n_distinct(air_quality$City),
    as.character(min(air_quality$Date)),
    as.character(max(air_quality$Date))
  )
)

dataset_overview


# ==========================================
# 3. FINAL AQI SUMMARY
# ==========================================

aqi_summary <- air_quality %>%
  summarise(
    Mean_AQI = mean(AQI, na.rm = TRUE),
    Median_AQI = median(AQI, na.rm = TRUE),
    SD_AQI = sd(AQI, na.rm = TRUE),
    Min_AQI = min(AQI, na.rm = TRUE),
    Max_AQI = max(AQI, na.rm = TRUE)
  )

aqi_summary


# ==========================================
# 4. CITY-WISE AQI ANALYSIS
# ==========================================

city_summary <- air_quality %>%
  group_by(City) %>%
  summarise(
    Mean_AQI = mean(AQI, na.rm = TRUE),
    Median_AQI = median(AQI, na.rm = TRUE),
    SD_AQI = sd(AQI, na.rm = TRUE),
    Observations = n()
  ) %>%
  arrange(desc(Mean_AQI))

city_summary


# ==========================================
# 5. AVERAGE AQI BY CITY
# ==========================================

p_city <- ggplot(
  city_summary,
  aes(
    x = reorder(City, Mean_AQI),
    y = Mean_AQI
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average AQI by City",
    x = "City",
    y = "Average AQI"
  ) +
  theme_minimal()

p_city


# ==========================================
# 6. AQI DISTRIBUTION
# ==========================================

p_distribution <- ggplot(
  air_quality,
  aes(x = AQI)
) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Air Quality Index",
    x = "AQI",
    y = "Frequency"
  ) +
  theme_minimal()

p_distribution


# ==========================================
# 7. MONTHLY AQI TREND
# ==========================================

monthly_aqi <- air_quality %>%
  mutate(
    Month = floor_date(Date, "month")
  ) %>%
  group_by(Month) %>%
  summarise(
    Mean_AQI = mean(AQI, na.rm = TRUE)
  )

monthly_aqi


p_trend <- ggplot(
  monthly_aqi,
  aes(x = Month, y = Mean_AQI)
) +
  geom_line() +
  labs(
    title = "Monthly AQI Trend",
    x = "Month",
    y = "Average AQI"
  ) +
  theme_minimal()

p_trend


# ==========================================
# 8. POLLUTANT ANALYSIS
# ==========================================

pollutant_summary <- air_quality %>%
  summarise(
    PM2.5 = mean(`PM2.5`, na.rm = TRUE),
    PM10 = mean(PM10, na.rm = TRUE),
    Ozone = mean(Ozone, na.rm = TRUE),
    NO2 = mean(NO2, na.rm = TRUE),
    CO = mean(CO, na.rm = TRUE),
    SO2 = mean(SO2, na.rm = TRUE)
  )

pollutant_summary


# ==========================================
# 9. CORRELATION SUMMARY
# ==========================================

correlation_summary <- data.frame(
  Variable = c(
    "PM2.5",
    "PM10",
    "Ozone",
    "NO2",
    "CO",
    "SO2",
    "Temperature",
    "Humidity"
  ),
  Correlation = c(
    cor(air_quality$AQI, air_quality$`PM2.5`),
    cor(air_quality$AQI, air_quality$PM10),
    cor(air_quality$AQI, air_quality$Ozone),
    cor(air_quality$AQI, air_quality$NO2),
    cor(air_quality$AQI, air_quality$CO),
    cor(air_quality$AQI, air_quality$SO2),
    cor(air_quality$AQI, air_quality$Temperature),
    cor(air_quality$AQI, air_quality$Humidity)
  )
)

correlation_summary


# ==========================================
# 10. PREPARE MODEL DATA
# ==========================================

model_data <- air_quality %>%
  select(
    AQI,
    `PM2.5`,
    PM10,
    Ozone,
    NO2,
    CO,
    SO2,
    Temperature,
    Humidity
  )

dim(model_data)
summary(model_data)


# ==========================================
# 11. TRAIN-TEST SPLIT
# ==========================================

set.seed(123)

train_index <- createDataPartition(
  model_data$AQI,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

dim(train_data)
dim(test_data)


# ==========================================
# 12. FINAL LINEAR REGRESSION MODEL
# ==========================================

aqi_model <- lm(
  AQI ~ `PM2.5` + PM10 + Ozone + NO2 + CO + SO2 +
    Temperature + Humidity,
  data = train_data
)

summary(aqi_model)


# ==========================================
# 13. MODEL PREDICTION
# ==========================================

test_predictions <- predict(
  aqi_model,
  newdata = test_data
)

prediction_results <- data.frame(
  Actual_AQI = test_data$AQI,
  Predicted_AQI = test_predictions
)

head(prediction_results)


# ==========================================
# 14. MODEL EVALUATION
# ==========================================

RMSE <- sqrt(
  mean(
    (prediction_results$Actual_AQI -
       prediction_results$Predicted_AQI)^2
  )
)

MAE <- mean(
  abs(
    prediction_results$Actual_AQI -
      prediction_results$Predicted_AQI
  )
)

R_squared <- cor(
  prediction_results$Actual_AQI,
  prediction_results$Predicted_AQI
)^2

final_model_performance <- data.frame(
  RMSE = RMSE,
  MAE = MAE,
  R_Squared = R_squared
)

final_model_performance


# ==========================================
# 15. ACTUAL VS PREDICTED AQI
# ==========================================

p_actual <- ggplot(
  prediction_results,
  aes(
    x = Actual_AQI,
    y = Predicted_AQI
  )
) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  labs(
    title = "Actual vs Predicted AQI",
    x = "Actual AQI",
    y = "Predicted AQI"
  ) +
  theme_minimal()

p_actual


# ==========================================
# 16. RESIDUAL DIAGNOSTICS
# ==========================================

model_residuals <- data.frame(
  Fitted = fitted(aqi_model),
  Residuals = residuals(aqi_model)
)

p_residuals <- ggplot(
  model_residuals,
  aes(
    x = Fitted,
    y = Residuals
  )
) +
  geom_point(alpha = 0.5) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted AQI",
    y = "Residuals"
  ) +
  theme_minimal()

p_residuals


# ==========================================
# 17. NORMAL Q-Q PLOT
# ==========================================

p_qq <- ggplot(
  model_residuals,
  aes(sample = Residuals)
) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Residuals"
  ) +
  theme_minimal()

p_qq


# ==========================================
# 18. 5-FOLD CROSS VALIDATION
# ==========================================

cv_control <- trainControl(
  method = "cv",
  number = 5
)

cv_model <- train(
  AQI ~ `PM2.5` + PM10 + Ozone + NO2 + CO + SO2 +
    Temperature + Humidity,
  data = model_data,
  method = "lm",
  trControl = cv_control
)

cv_model

cv_model$results


# ==========================================
# 19. FINAL INSIGHTS
# ==========================================

top_city <- city_summary %>%
  slice_max(Mean_AQI, n = 1)

bottom_city <- city_summary %>%
  slice_min(Mean_AQI, n = 1)

top_city
bottom_city

cat(
  "Highest average AQI city:", top_city$City, "\n",
  "Lowest average AQI city:", bottom_city$City, "\n",
  "Mean AQI:", round(mean(air_quality$AQI), 2), "\n",
  "Test RMSE:", round(RMSE, 2), "\n",
  "Test MAE:", round(MAE, 2), "\n",
  "Test R-squared:", round(R_squared, 4), "\n"
)


# ==========================================
# 20. CREATE OUTPUTS FOLDER
# ==========================================

dir.create(
  "Outputs",
  showWarnings = FALSE
)


# ==========================================
# 21. SAVE FINAL TABLES
# ==========================================

write.csv(
  dataset_overview,
  "Outputs/dataset_overview.csv",
  row.names = FALSE
)

write.csv(
  aqi_summary,
  "Outputs/aqi_summary.csv",
  row.names = FALSE
)

write.csv(
  city_summary,
  "Outputs/city_aqi_summary.csv",
  row.names = FALSE
)

write.csv(
  pollutant_summary,
  "Outputs/pollutant_summary.csv",
  row.names = FALSE
)

write.csv(
  correlation_summary,
  "Outputs/correlation_summary.csv",
  row.names = FALSE
)

write.csv(
  final_model_performance,
  "Outputs/final_model_performance.csv",
  row.names = FALSE
)

write.csv(
  cv_model$results,
  "Outputs/cross_validation_results.csv",
  row.names = FALSE
)


# ==========================================
# 22. SAVE FINAL MODEL
# ==========================================

saveRDS(
  aqi_model,
  "Outputs/aqi_model.rds"
)


# ==========================================
# 23. SAVE FINAL PLOTS
# ==========================================

ggsave(
  "Outputs/average_aqi_by_city.png",
  plot = p_city,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "Outputs/aqi_distribution.png",
  plot = p_distribution,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "Outputs/monthly_aqi_trend.png",
  plot = p_trend,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "Outputs/actual_vs_predicted_aqi.png",
  plot = p_actual,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "Outputs/residuals_vs_fitted.png",
  plot = p_residuals,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "Outputs/normal_qq_plot.png",
  plot = p_qq,
  width = 8,
  height = 6,
  dpi = 300
)


# ==========================================
# 24. SAVE PREDICTIONS
# ==========================================

write.csv(
  prediction_results,
  "Outputs/aqi_predictions.csv",
  row.names = FALSE
)


# ==========================================
# 25. SAVE FINAL INSIGHTS
# ==========================================

final_insights <- data.frame(
  Metric = c(
    "Highest Average AQI City",
    "Lowest Average AQI City",
    "Overall Mean AQI",
    "Test RMSE",
    "Test MAE",
    "Test R-Squared"
  ),
  Value = c(
    top_city$City,
    bottom_city$City,
    round(mean(air_quality$AQI), 2),
    round(RMSE, 2),
    round(MAE, 2),
    round(R_squared, 4)
  )
)

write.csv(
  final_insights,
  "Outputs/final_insights.csv",
  row.names = FALSE
)

print(final_insights)

cat("\n==========================================\n")
cat("WEEK 4 ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("All final outputs saved in the Outputs folder.\n")
cat("==========================================\n")
