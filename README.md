# AirSense-Urban-Air-Quality-Analytics
Urban air quality analytics, data cleaning, visualization, statistical analysis and predictive modeling using R.

## Project Overview

AirSense is a data analytics and predictive modeling project focused on analyzing urban air quality using AQI, air pollutants, and environmental factors.

The project uses R and RStudio to perform data cleaning, exploratory data analysis, statistical analysis, data visualization, and predictive modeling.

The main objective is to understand air-quality patterns, identify variables associated with AQI, compare air quality across cities, and build a regression model to predict AQI.

---

## Objectives

- Clean and prepare the air-quality dataset.
- Explore AQI and environmental variables.
- Analyze air-quality patterns across different cities.
- Study relationships between AQI and pollutants.
- Perform statistical hypothesis and correlation analysis.
- Build a multiple linear regression model for AQI prediction.
- Evaluate model performance using RMSE, MAE, and R-squared.
- Validate the model using 5-fold cross-validation.
- Present final insights through visualizations and a comprehensive report.

---

## Dataset

The project uses an air-quality dataset containing measurements related to:

- Date
- City
- AQI
- PM2.5
- PM10
- Ozone
- NO2
- CO
- SO2
- Temperature
- Humidity

The dataset contains air-quality observations collected across multiple cities and dates.

---

## Technologies Used

- R
- RStudio
- tidyverse
- ggplot2
- lubridate
- caret
- Multiple Linear Regression
- GitHub

---

## Project Workflow

### Week 1 – Data Cleaning

The first week focused on understanding and preparing the dataset.

Activities included:

- Loading the dataset into R.
- Inspecting the structure and dimensions.
- Checking data types.
- Identifying missing values.
- Cleaning and preparing the dataset.
- Creating a cleaned dataset for further analysis.

---

### Week 2 – Data Visualization

The second week focused on exploratory data analysis and visualization.

Activities included:

- AQI distribution analysis.
- City-wise AQI analysis.
- Pollutant analysis.
- Environmental variable analysis.
- Creating charts using ggplot2.
- Identifying important patterns and trends in the dataset.

---

### Week 3 – Statistical Analysis and Predictive Modeling

The third week focused on statistical analysis and predictive modeling.

Pearson correlation tests were performed to study the relationship between AQI and:

- PM2.5
- PM10
- Ozone
- NO2
- CO
- SO2
- Temperature
- Humidity

A multiple linear regression model was developed using AQI as the dependent variable.

The dataset was divided into:

- 80% training data
- 20% testing data

The model was evaluated using:

- RMSE
- MAE
- R-squared

Model diagnostics were also performed using:

- Residuals vs Fitted plot
- Normal Q-Q plot
- Actual vs Predicted AQI plot

Finally, 5-fold cross-validation was performed using the caret package.

---

### Week 4 – Final Data Analysis and Reporting

The final week focused on consolidating the complete project analysis.

Activities included:

- Creating a final dataset overview.
- Summarizing overall AQI statistics.
- Performing city-wise AQI analysis.
- Analyzing pollutant levels.
- Creating AQI distribution visualizations.
- Creating monthly AQI trend visualizations.
- Preparing correlation summaries.
- Building and evaluating the final regression model.
- Performing cross-validation.
- Generating final insights.
- Exporting final analysis tables and visualizations.
- Preparing the final project report.

---

## Key Findings

The correlation analysis showed meaningful relationships between AQI and several environmental variables.

PM10 showed the strongest positive correlation with AQI among the analyzed variables, followed by Ozone, Temperature, and PM2.5.

Humidity showed a negative correlation with AQI.

The multiple linear regression model showed that several pollutant and environmental variables contributed significantly to AQI prediction.

Model performance was evaluated on unseen test data using RMSE, MAE, and R-squared.

Five-fold cross-validation was also used to assess the consistency of model performance.

---

## Model Performance

The final regression model was evaluated using the following metrics:

- RMSE: approximately 18.68
- MAE: approximately 11.11
- R-squared: approximately 0.429

The cross-validation results were also analyzed to assess model stability.

---

## Visualizations

The project includes visualizations such as:

- AQI distribution
- City-wise AQI comparison
- Monthly AQI trend
- Actual vs Predicted AQI
- Residuals vs Fitted Values
- Normal Q-Q Plot
- Pollutant and correlation analysis

All generated outputs are available in the `outputs` folder.

---
Developed by Meghana kolla
├── Week3_Statistical_Analysis.R
├── Week4_final_report.R
│
├── air_quality_dataset.csv
├── AirSense_Final_Report.docx
├── README.md
└── .gitignore
