library(tidyverse)
library(lubridate)
library(caret)

# Use cleaned dataset from Week 1
air_quality <- air_quality_clean

# Check data
dim(air_quality)
str(air_quality)
summary(air_quality)
stat_summary <- air_quality %>%
  summarise(
    Mean_AQI = mean(AQI, na.rm = TRUE),
    Median_AQI = median(AQI, na.rm = TRUE),
    SD_AQI = sd(AQI, na.rm = TRUE),
    Min_AQI = min(AQI, na.rm = TRUE),
    Max_AQI = max(AQI, na.rm = TRUE)
  )

stat_summary
aqi_anova <- aov(AQI ~ City, data = air_quality)

summary(aqi_anova)
ggplot(air_quality, aes(x = City, y = AQI)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "AQI Distribution Across Cities",
    x = "City",
    y = "AQI"
  ) +
  theme_minimal()
tukey_result <- TukeyHSD(aqi_anova)

tukey_result
cor_test_pm10 <- cor.test(
  air_quality$AQI,
  air_quality$PM10,
  method = "pearson"
)

cor_test_pm10
cor_test_pm25 <- cor.test(
  air_quality$AQI,
  air_quality$`PM2.5`,
  method = "pearson"
)

cor_test_pm25
# Correlation Analysis

# AQI vs Ozone
cor_test_ozone <- cor.test(
  air_quality$AQI,
  air_quality$Ozone,
  method = "pearson"
)

cor_test_ozone


# AQI vs NO2
cor_test_no2 <- cor.test(
  air_quality$AQI,
  air_quality$NO2,
  method = "pearson"
)

cor_test_no2


# AQI vs CO
cor_test_co <- cor.test(
  air_quality$AQI,
  air_quality$CO,
  method = "pearson"
)

cor_test_co


# AQI vs SO2
cor_test_so2 <- cor.test(
  air_quality$AQI,
  air_quality$SO2,
  method = "pearson"
)

cor_test_so2


# AQI vs Temperature
cor_test_temp <- cor.test(
  air_quality$AQI,
  air_quality$Temperature,
  method = "pearson"
)

cor_test_temp


# AQI vs Humidity
cor_test_humidity <- cor.test(
  air_quality$AQI,
  air_quality$Humidity,
  method = "pearson"
)

cor_test_humidity
#  Multiple Linear Regression

aqi_model <- lm(
  AQI ~ `PM2.5` + PM10 + Ozone + NO2 + CO + SO2 +
    Temperature + Humidity,
  data = air_quality
)

# Model summary
summary(aqi_model)
# ==========================================
# MULTIPLE LINEAR REGRESSION MODEL
# ==========================================

set.seed(123)

# Select variables for prediction
model_data <- air_quality %>%
  select(
    AQI,
    PM2.5,
    PM10,
    Ozone,
    NO2,
    CO,
    SO2,
    Temperature,
    Humidity
  )

# Check model data
dim(model_data)
summary(model_data)

# ------------------------------------------
# Train-Test Split (80% Train, 20% Test)
# ------------------------------------------

train_index <- createDataPartition(
  model_data$AQI,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

dim(train_data)
dim(test_data)

# ------------------------------------------
# Build Multiple Linear Regression Model
# ------------------------------------------

aqi_model <- lm(
  AQI ~ PM2.5 + PM10 + Ozone + NO2 + CO + SO2 +
    Temperature + Humidity,
  data = train_data
)

# Model summary
summary(aqi_model)

# ------------------------------------------
# Predictions on Test Data
# ------------------------------------------

test_predictions <- predict(
  aqi_model,
  newdata = test_data
)

# Actual vs Predicted
prediction_results <- data.frame(
  Actual_AQI = test_data$AQI,
  Predicted_AQI = test_predictions
)

head(prediction_results)

# ------------------------------------------
# Model Performance Metrics
# ------------------------------------------

RMSE <- sqrt(
  mean((prediction_results$Actual_AQI -
          prediction_results$Predicted_AQI)^2)
)

MAE <- mean(
  abs(prediction_results$Actual_AQI -
        prediction_results$Predicted_AQI)
)

R_squared <- cor(
  prediction_results$Actual_AQI,
  prediction_results$Predicted_AQI
)^2

model_performance <- data.frame(
  RMSE = RMSE,
  MAE = MAE,
  R_Squared = R_squared
)

model_performance

# ------------------------------------------
# Actual vs Predicted Plot
# ------------------------------------------

ggplot(
  prediction_results,
  aes(x = Actual_AQI, y = Predicted_AQI)
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

# ------------------------------------------
# Residual Plot
# ------------------------------------------

model_residuals <- data.frame(
  Fitted = fitted(aqi_model),
  Residuals = residuals(aqi_model)
)

ggplot(
  model_residuals,
  aes(x = Fitted, y = Residuals)
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

# ------------------------------------------
# Normal Q-Q Plot
# ------------------------------------------

ggplot(
  model_residuals,
  aes(sample = Residuals)
) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Residuals"
  ) +
  theme_minimal()

# ------------------------------------------
# Cross-Validation
# ------------------------------------------
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

# Final Actual vs Predicted AQI Plot

ggplot(
  prediction_results,
  aes(x = Actual_AQI, y = Predicted_AQI)
) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs Predicted AQI",
    x = "Actual AQI",
    y = "Predicted AQI"
  ) +
  theme_minimal()
write.csv(
  model_performance,
  "Outputs/model_performance.csv",
  row.names = FALSE
)
write.csv(
  cv_model$results,
  "Outputs/cross_validation_results.csv",
  row.names = FALSE
)
write.csv(
  prediction_results,
  "Outputs/aqi_predictions.csv",
  row.names = FALSE
)
capture.output(
  summary(aqi_model),
  file = "Outputs/linear_regression_summary.txt"
)
