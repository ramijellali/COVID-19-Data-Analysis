# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(e1071)
library(magrittr)
library(car)
library(missRanger)  # For median imputation
library(viridis)  # For color palettes
library(xgboost)     # For XGBoost
library(randomForest)  # For Random Forest
library(caret) 

# Step 1: Load and Document Data
cat("Step 1: Loading Data\n")
covid_india <- read.csv("/Users/macbook/Desktop/Statistics/covid_19_india.csv")
covid_cases <- read.csv("/Users/macbook/Desktop/Statistics/COVID-19 Cases(22-04-2021).csv")

cat("Dataset: covid_india\n")
print(str(covid_india))
cat("\nDataset: covid_cases\n")
print(str(covid_cases))

# Step 2: Handle Missing Data with Imputation
cat("\nStep 2: Handling Missing Data\n")
covid_india <- missRanger(covid_india, pmm.k = 5)
covid_cases <- missRanger(covid_cases, pmm.k = 5)
cat("Missing data imputation completed for both datasets.\n")

# Step 3: Convert Date Column and Merge Datasets
covid_india$Date <- as.Date(covid_india$Date, format = "%Y-%m-%d")
covid_cases$Date <- as.Date(covid_cases$Date, format = "%d/%m/%Y")

merged_data <- merge(
  covid_india[, c("Date", "State.UnionTerritory", "Cured", "Deaths", "Confirmed")],
  covid_cases[, c("Date", "Region", "Confirmed.Cases", "Active.Cases", "Cured.Discharged", "Death")],
  by = "Date", all = TRUE
)

cat("Step 3: Merged Datasets by Date\n")
cat("Dataset structure after merge:\n")
print(str(merged_data))

# Step 4: Add 'Week' Column and Calculate CFR
merged_data <- merged_data %>%
  mutate(Week = as.numeric(format(Date, "%U"))) %>%
  mutate(CFR = ifelse(Confirmed > 0, (Deaths / Confirmed) * 100, 0))

cat("\nStep 4: Case Fatality Rate (CFR) Added\n")
print(head(merged_data[, c("Date", "Deaths", "Confirmed", "CFR")]))

# Step 5: Check and Remove Zero Variance Columns
covid_india_numeric <- covid_india %>% select(where(is.numeric))
covid_cases_numeric <- covid_cases %>% select(where(is.numeric))

zero_variance_columns_india <- sapply(covid_india_numeric, function(x) sd(x, na.rm = TRUE) == 0)
zero_variance_columns_cases <- sapply(covid_cases_numeric, function(x) sd(x, na.rm = TRUE) == 0)

cat("\nStep 5: Columns with Zero Variance\n")
cat("Zero variance columns in covid_india: ", names(zero_variance_columns_india[zero_variance_columns_india]), "\n")
cat("Zero variance columns in covid_cases: ", names(zero_variance_columns_cases[zero_variance_columns_cases]), "\n")

covid_india_no_zero_variance <- covid_india_numeric[, !zero_variance_columns_india]
covid_cases_no_zero_variance <- covid_cases_numeric[, !zero_variance_columns_cases]

# Step 6: Perform Correlation Analysis
cat("\nStep 6: Correlation Analysis\n")
correlation_matrix_india <- cor(covid_india_no_zero_variance, use = "complete.obs")
correlation_matrix_cases <- cor(covid_cases_no_zero_variance, use = "complete.obs")

cat("Correlation Matrix for covid_india:\n")
print(correlation_matrix_india)

cat("\nCorrelation Matrix for covid_cases:\n")
print(correlation_matrix_cases)

# Visualize the correlation matrices
corrplot(correlation_matrix_india, method = "color", addCoef.col = "black", tl.cex = 0.8, title = "Covid India Correlation Matrix")
corrplot(correlation_matrix_cases, method = "color", addCoef.col = "black", tl.cex = 0.8, title = "Covid Cases Correlation Matrix")

# Step 7: Handle Outliers
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  upper_limit <- Q3 + 1.5 * IQR
  lower_limit <- Q1 - 1.5 * IQR
  pmin(pmax(x, lower_limit), upper_limit)
}

covid_india$Confirmed <- cap_outliers(covid_india$Confirmed)
covid_cases$Confirmed.Cases <- cap_outliers(covid_cases$Confirmed.Cases)

cat("\nStep 7: Outliers Capped\n")

# Step 8: Extract Week-Specific Data
extract_week_data <- function(data, week_num) {
  week_data <- filter(data, Week == week_num) %>% filter(complete.cases(.))
  return(week_data)
}

week_4_data <- extract_week_data(merged_data, 4)
week_5_data <- extract_week_data(merged_data, 5)
week_6_data <- extract_week_data(merged_data, 6)

# Align data across weeks
min_rows <- min(nrow(week_4_data), nrow(week_5_data), nrow(week_6_data))
week_4_data <- week_4_data[1:min_rows, ]
week_5_data <- week_5_data[1:min_rows, ]
week_6_data <- week_6_data[1:min_rows, ]

# Step 9: Multivariate Linear Regression for Week 5 Deaths Prediction
regression_data_week_5 <- week_4_data %>%
  mutate(Week_5_Deaths = week_5_data$Deaths)

# Multivariate Linear Regression model including multiple predictors
lm_week_5_multivariate <- lm(Week_5_Deaths ~ Confirmed + Active.Cases + Cured + CFR + 
                               Confirmed.Cases + Active.Cases, data = regression_data_week_5)

summary(lm_week_5_multivariate)

# Step 10: Predictions for Week 5 with Confidence Intervals
predictions_week_5 <- predict(lm_week_5_multivariate, interval = "confidence")
regression_data_week_5$Predicted_Week_5_Deaths <- predictions_week_5[, "fit"]
regression_data_week_5$CI_Lower_Week_5_Deaths <- predictions_week_5[, "lwr"]
regression_data_week_5$CI_Upper_Week_5_Deaths <- predictions_week_5[, "upr"]

cat("\nStep 10: Predictions for Week 5 Completed\n")

# Step 11: Auto-Regression for Week 6 Deaths Prediction
cat("\nStep 11: Auto-Regression for Week 6 Deaths Prediction\n")
regression_data_week_6 <- regression_data_week_5 %>%
  mutate(Week_6_Deaths = week_6_data$Deaths)

auto_regression <- lm(Week_6_Deaths ~ Week_5_Deaths, data = regression_data_week_6)
summary(auto_regression)

# Step 12: Predictions for Week 6 with Confidence Intervals
predictions_week_6 <- predict(auto_regression, interval = "confidence")
regression_data_week_6$Predicted_Week_6_Deaths <- predictions_week_6[, "fit"]
regression_data_week_6$CI_Lower_Week_6_Deaths <- predictions_week_6[, "lwr"]
regression_data_week_6$CI_Upper_Week_6_Deaths <- predictions_week_6[, "upr"]

cat("\nStep 12: Predictions for Week 6 Completed\n")

# Step 13: Kruskal-Wallis Test
kruskal_test <- kruskal.test(Deaths ~ Region, data = merged_data)
cat("\nStep 13: Kruskal-Wallis Test Results\n")
print(kruskal_test)

# Step 14: Model Evaluation
cat("\nStep 14: Model Evaluation\n")
cat("Week 5 Linear Regression Model Summary:\n")
print(summary(lm_week_5_multivariate))

cat("\nWeek 6 Auto-Regression Model Summary:\n")
print(summary(auto_regression))

# Data Cleaning for Plotting
cat("\nStep 15: Data Cleaning for Plotting\n")
regression_data_week_5 <- regression_data_week_5 %>%
  filter(!is.na(Week_5_Deaths), !is.na(Predicted_Week_5_Deaths), 
         is.finite(Week_5_Deaths), is.finite(Predicted_Week_5_Deaths))

regression_data_week_6 <- regression_data_week_6 %>%
  filter(!is.na(Week_6_Deaths), !is.na(Predicted_Week_6_Deaths), 
         is.finite(Week_6_Deaths), is.finite(Predicted_Week_6_Deaths))

# Geographical Visualization (Bar Plot) for Total Deaths by Region
cat("\nStep 16: Geographical Visualization for Total Deaths by Region\n")
top_regions <- merged_data %>%
  group_by(Region) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(desc(Total_Deaths)) %>%
  top_n(12)

ggplot(top_regions, aes(x = reorder(Region, Total_Deaths), y = Total_Deaths, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Deaths by Region", x = "Region", y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal()

# Visualize Predictions vs Actuals for Week 5
ggplot(regression_data_week_5, aes(x = Week_5_Deaths, y = Predicted_Week_5_Deaths)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Week 5: Predicted vs Actual Deaths", x = "Actual Deaths", y = "Predicted Deaths")

# Visualize Predictions vs Actuals for Week 6
ggplot(regression_data_week_6, aes(x = Week_6_Deaths, y = Predicted_Week_6_Deaths)) +
  geom_point(color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Week 6: Predicted vs Actual Deaths", x = "Actual Deaths", y = "Predicted Deaths")

# Residual Plot for Week 5 Regression
regression_data_week_5$residuals <- regression_data_week_5$Week_5_Deaths - regression_data_week_5$Predicted_Week_5_Deaths
ggplot(regression_data_week_5, aes(x = Predicted_Week_5_Deaths, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals for Week 5 Prediction", x = "Predicted Deaths", y = "Residuals") +
  theme_minimal()

# Cumulative Deaths Over Time
ggplot(merged_data, aes(x = Date, y = Deaths)) +
  geom_line(color = "red") +
  labs(title = "Cumulative Deaths Over Time", x = "Date", y = "Cumulative Deaths")

# Prediction Error Histogram
ggplot(regression_data_week_5, aes(x = Week_5_Deaths - Predicted_Week_5_Deaths)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Prediction Error Histogram for Week 5", x = "Prediction Error", y = "Frequency")

# Confidence Intervals for Predictions
ggplot(regression_data_week_5, aes(x = Week_5_Deaths, y = Predicted_Week_5_Deaths)) +
  geom_ribbon(aes(ymin = CI_Lower_Week_5_Deaths, ymax = CI_Upper_Week_5_Deaths), fill = "blue", alpha = 0.2) +
  geom_point(color = "red") +
  labs(title = "Predicted Deaths with Confidence Intervals (Week 5)", x = "Actual Deaths", y = "Predicted Deaths")


# Step 11: Visualizations for Analysis

cat("\nStep 11: Visualizations\n")

# 1. **Visualize Total Deaths by Region (Bar Plot)**
ggplot(merged_data %>%
         group_by(Region) %>%
         summarize(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>%
         arrange(desc(Total_Deaths)), 
       aes(x = reorder(Region, Total_Deaths), y = Total_Deaths, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Deaths by Region", x = "Region", y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + # Adjust text size
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal()

# 2. **Visualize Relationship Between Confirmed Cases and Deaths**
ggplot(merged_data, aes(x = Confirmed, y = Deaths)) +
  geom_point(color = "blue") +
  labs(title = "Deaths vs Confirmed Cases", x = "Confirmed Cases", y = "Deaths") +
  theme_minimal()

# 3. **Visualize Relationship Between Active Cases and Deaths (with NA removal)**
ggplot(merged_data %>%
         filter(!is.na(Active.Cases) & !is.na(Deaths)), 
       aes(x = Active.Cases, y = Deaths)) +
  geom_point(color = "green") +
  labs(title = "Deaths vs Active Cases", x = "Active Cases", y = "Deaths") +
  theme_minimal()

# 4. **Create Percentage of Cured, Active, and Deceased Cases Over Time**
merged_data <- merged_data %>%
  filter(!is.na(Confirmed.Cases), !is.na(Deaths), !is.na(Cured.Discharged), !is.na(Active.Cases)) %>%
  mutate(Total_Cases = Confirmed.Cases + Deaths + Cured.Discharged) %>%
  mutate(Percentage_Cured = (Cured.Discharged / Total_Cases) * 100,
         Percentage_Active = (Active.Cases / Total_Cases) * 100,
         Percentage_Deaths = (Deaths / Total_Cases) * 100) %>%
  filter(is.finite(Percentage_Cured), is.finite(Percentage_Active), is.finite(Percentage_Deaths), is.finite(Total_Cases))

# Create the plot
ggplot(merged_data) +
  geom_area(aes(x = Date, y = Percentage_Cured, fill = "Cured"), alpha = 0.5) +
  geom_area(aes(x = Date, y = Percentage_Active, fill = "Active"), alpha = 0.5) +
  geom_area(aes(x = Date, y = Percentage_Deaths, fill = "Deaths"), alpha = 0.5) +
  labs(title = "Percentage of Cured, Active, and Deceased Cases Over Time", x = "Date", y = "Percentage (%)") +
  scale_fill_manual(values = c("Cured" = "green", "Active" = "blue", "Deaths" = "red")) +
  theme_minimal()

# 5. **Step 12: Cumulative Deaths Over Time (Trend Analysis)**
ggplot(merged_data, aes(x = Date, y = Deaths)) +
  geom_line(color = "red") +
  labs(title = "Cumulative Deaths Over Time", x = "Date", y = "Cumulative Deaths") +
  theme_minimal()

# 6. **Heatmap of Deaths Over Time by Region (Visualization)**
deaths_by_region <- merged_data %>%
  group_by(Date, Region) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE))

ggplot(deaths_by_region, aes(x = Date, y = Region, fill = Total_Deaths)) +
  geom_tile() +
  labs(title = "Heatmap of Deaths Over Time by Region", x = "Date", y = "Region") +
  scale_fill_viridis() +
  theme_minimal()

# 7. **CFR by Region (Boxplot)**
ggplot(merged_data, aes(x = Region, y = CFR)) +
  geom_boxplot() +
  labs(title = "CFR by Region", x = "Region", y = "Case Fatality Rate (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal()


# Boxplot for Confirmed Cases (Before Outlier Capping)
boxplot_confirmed_before <- ggplot(merged_data, aes(y = Confirmed_Original)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(title = "Confirmed Cases (Before Outlier Capping)", y = "Confirmed Cases") +
  theme_minimal()

print(boxplot_confirmed_before)

# Boxplot for Confirmed Cases (After Outlier Capping)
boxplot_confirmed_after <- ggplot(merged_data, aes(y = Confirmed)) +
  geom_boxplot(fill = "green", alpha = 0.6) +
  labs(title = "Confirmed Cases (After Outlier Capping)", y = "Confirmed Cases") +
  theme_minimal()

print(boxplot_confirmed_after)




# Scatter plot: Predicted vs. Actual Week 5 Deaths
scatter_week5 <- ggplot(regression_data_week_5, aes(x = Week_5_Deaths, y = Predicted_Week_5_Deaths)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Week 5 Deaths",
       x = "Actual Week 5 Deaths",
       y = "Predicted Week 5 Deaths") +
  theme_minimal()

print(scatter_week5)

# Residual plot: Residuals vs. Predicted Week 5 Deaths
regression_data_week_5$residuals <- regression_data_week_5$Week_5_Deaths - regression_data_week_5$Predicted_Week_5_Deaths

residual_plot_week5 <- ggplot(regression_data_week_5, aes(x = Predicted_Week_5_Deaths, y = residuals)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Predicted Week 5 Deaths",
       x = "Predicted Week 5 Deaths",
       y = "Residuals") +
  theme_minimal()

print(residual_plot_week5)

# Scatter plot: Predicted vs. Actual Week 6 Deaths
scatter_week6 <- ggplot(regression_data_week_6, aes(x = Week_6_Deaths, y = Predicted_Week_6_Deaths)) +
  geom_point(color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Week 6 Deaths",
       x = "Actual Week 6 Deaths",
       y = "Predicted Week 6 Deaths") +
  theme_minimal()

print(scatter_week6)






