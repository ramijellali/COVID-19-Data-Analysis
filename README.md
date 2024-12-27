# COVID-19 Data Analysis in India: Insights, Trends, and Recommendations

## Table of Contents
1. [Introduction](#introduction)
2. [Data Overview and Methodology](#data-overview-and-methodology)
3. [Data Cleaning and Preprocessing](#data-cleaning-and-preprocessing)
4. [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
5. [Correlation Analysis](#correlation-analysis)
6. [Statistical Tests & Outlier Detection](#statistical-tests-and-outlier-detection)
7. [Regression Analysis](#regression-analysis)
8. [Recommendations](#recommendations)
9. [Conclusion](#conclusion)

---

## Introduction
### Key Points:
- **COVID-19 Impact in India**: India faced one of the largest outbreaks globally, with over 30 million cases and 400,000 deaths.
- **Objective**: To identify factors affecting COVID-19 fatalities, analyze trends, and highlight regional variations.
- **Approach**: 
  - Data preprocessing and exploratory analysis.
  - Statistical modeling for insights.

![Introduction Summary](assets/page4_image.png)

---

## Data Overview and Methodology
### Datasets:
- **Sources**: Datasets on confirmed cases, deaths, recoveries, and active cases across India.
- **Variables**: 
  - Case counts, deaths, and recoveries over time.

### Methodology:
- Data Cleaning:
  - Missing values handled with `missRanger`.
  - Datasets merged by date.
- Feature Engineering:
  - Weekly data aggregation.
  - Case Fatality Rate (CFR) calculated as deaths/confirmed cases.
- Statistical Analysis:
  - Correlation and regional comparisons using Kruskal-Wallis tests.
  - Regression models (multivariate and auto-regression).

![Dataset Overview](assets/page6_image.png)

---

## Data Cleaning and Preprocessing
- **Missing Data Handling**: Imputed missing values for critical metrics like deaths and cases.
- **Merging**: Unified datasets using consistent date formatting.
- **Feature Engineering**:
  - CFR calculated as a percentage.
  - Weekly data extraction.
- **Zero Variance Removal**: Eliminated non-informative variables.

![Data Cleaning Code Example](assets/page12_image.png)

---

## Exploratory Data Analysis (EDA)
1. **Descriptive Statistics**:
   - Minimum, maximum, median, and mean values computed for deaths and confirmed cases.

2. **Key Visualizations**:
   - **Scatter Plot**: Deaths vs. Confirmed Cases
     - Positive correlation observed.
     - ![Scatter Plot](assets/page17_image.png)
   - **Bar Plot**: Total Deaths by Region
     - Highlights regions with severe impacts.
     - ![Bar Plot](assets/page18_image.png)
   - **Line Plot**: Cumulative Deaths Over Time
     - Tracks the progression of fatalities.
     - ![Line Plot](assets/page19_image.png)
   - **Heatmap**: Deaths by Region Over Time
     - Identifies temporal and regional death trends.
     - ![Heatmap](assets/page23_image.png)
   - **Boxplot**: CFR by Region
     - Analyzes variations in fatality rates.
     - ![Boxplot](assets/page24_image.png)

---

## Correlation Analysis
### Findings:
- **Confirmed vs. Deaths**:
  - Strong positive correlation, emphasizing the proportionality of confirmed cases to deaths.
- **Active Cases vs. Deaths**:
  - Indicates ongoing infections influence fatality rates.

![Correlation Matrix](assets/page27_image.png)

---

## Statistical Tests and Outlier Detection
1. **Outlier Detection**:
   - Used Interquartile Range (IQR) to cap extreme values.
   - Improved consistency by adjusting values.

   ![Before and After Outlier Detection](assets/page32_image.png)

2. **Kruskal-Wallis Test**:
   - Analyzed regional differences in death counts.
   - Results revealed significant disparities among regions.

---

## Regression Analysis
1. **Multivariate Linear Regression**:
   - Variables: Confirmed Cases, Active Cases, Cured Cases, CFR.
   - Predicted vs. actual deaths showed underestimation for extreme values (>30,000).

   ![Multivariate Regression](assets/page35_image.png)

2. **Auto-Regression**:
   - Time-series modeling for Week 6 predictions.
   - Struggled with underprediction for deaths exceeding 10,000.

   ![Auto-Regression Plot](assets/page37_image.png)

---

## Recommendations
1. **Model Improvements**:
   - Add predictors (e.g., population density).
   - Test non-linear models like Random Forest or XGBoost.
2. **Data Quality**:
   - Standardize data collection and reporting.
   - Regularly update datasets to reduce inaccuracies.

![Recommendations](assets/page39_image.png)

---

## Conclusion
- **Summary**: The study effectively analyzed COVID-19 death trends, revealing key drivers and disparities across regions. However, predictive models require refinements to handle extreme cases effectively.
- **Impact**: Provides actionable insights for policymakers and healthcare strategists.

![Conclusion](assets/page41_image.png)

---