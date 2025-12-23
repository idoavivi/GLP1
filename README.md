# GLP-1 Therapy and Physical Activity Analysis

## Research Question
Do patients starting injectable GLP-1 therapy (semaglutide, tirzepatide) change their physical activity patterns, as measured by Fitbit devices?

## Data Source
All of Us Research Program Database (Registered Tier Dataset v8)

## Study Population
- Patients with Fitbit data
- Patients who initiated injectable GLP-1 therapy (semaglutide or tirzepatide)

## Data Processing Pipeline

### 1. Data Loading
The script loads the following data from BigQuery:
- **Person Demographics**: Basic demographic information
- **Drug Exposures**: GLP-1 medication prescriptions
- **Measurements**: Weight, height, and BMI
- **Fitbit Activity Summary**: Daily activity metrics
- **Fitbit Heart Rate Summary**: Heart rate zone data

### 2. Data Filtering

#### Fitbit Activity Filters
- Steps: 100-25,000 steps per day
- Total activity minutes: ≤ 1,440 minutes per day (24 hours)

#### Anthropometric Filters
- Weight: 30-300 kg
- Height: 100-220 cm
- BMI: 0-80 kg/m²

#### Medication Filters
- Only injectable formulations of semaglutide and tirzepatide
- Routes: Subcutaneous injection

### 3. Unit Conversions
- **Weight**: Converted to kilograms (kg)
- **Height**: Converted to centimeters (cm)
- **BMI**: Calculated as kg/m² when missing

### 4. Missing Data Imputation
For each patient:
- Most recent height is carried forward/backward
- If weight and height available: BMI is calculated
- If BMI and height available: Weight is calculated

### 5. GLP-1 Initiation Date
- First GLP-1 prescription date is identified for each patient
- All data is divided into "before" and "after" periods

## Key Output Files

1. **glp1_processed_data.RData**: Complete R workspace with all processed data
2. **glp1_initiation_dates.csv**: Patient-level GLP-1 initiation information
3. **activity_by_glp1_period.csv**: Daily Fitbit activity by period (before/after)
4. **weight_by_glp1_period.csv**: Weight measurements by period (before/after)

## Data Structure

### Activity Data (activity_with_glp1)
Key variables:
- `person_id`: Patient identifier
- `date`: Activity date
- `steps`: Daily step count
- `very_active_minutes`: Minutes in vigorous activity
- `fairly_active_minutes`: Minutes in moderate activity
- `lightly_active_minutes`: Minutes in light activity
- `sedentary_minutes`: Minutes sedentary
- `glp1_initiation_date`: Date of first GLP-1 prescription
- `days_from_initiation`: Days relative to GLP-1 start
- `period`: "before" or "after" GLP-1 initiation

### Weight Data (weight_with_glp1)
Key variables:
- `person_id`: Patient identifier
- `measurement_date`: Date of measurement
- `weight_kg`: Weight in kilograms
- `height_cm`: Height in centimeters
- `bmi`: Body mass index
- `glp1_initiation_date`: Date of first GLP-1 prescription
- `days_from_initiation`: Days relative to GLP-1 start
- `period`: "before" or "after" GLP-1 initiation

## Usage

### Running the Analysis
```r
# In All of Us Workbench R/RStudio environment
source("glp1_activity_analysis.R")

# Or run interactively in RStudio
```

### Loading Processed Data
```r
# Load the processed data
load("glp1_processed_data.RData")

# View available objects
ls()

# Quick exploration
summary(activity_with_glp1)
summary(weight_with_glp1)
```

## Suggested Next Steps

### 1. Descriptive Statistics
- Patient characteristics at baseline
- Activity patterns before GLP-1 initiation
- Weight trajectories

### 2. Primary Analysis
Compare activity metrics before vs. after GLP-1 initiation:
- Mean daily steps
- Active minutes (very, fairly, lightly active)
- Sedentary time

### 3. Statistical Methods
Consider:
- **Paired t-tests** or **Wilcoxon signed-rank tests** for before/after comparison
- **Mixed-effects models** to account for repeated measures
- **Interrupted time series analysis** to assess trends
- **Propensity score matching** if comparing to control group

### 4. Visualizations
- Step count trajectories aligned to GLP-1 initiation
- Spaghetti plots of individual patient trajectories
- Before/after boxplots
- Heat maps of activity patterns

### 5. Stratified Analyses
Consider stratifying by:
- Sex/gender
- Age groups
- Baseline BMI categories
- Comorbidities (diabetes, hypertension, etc.)
- Medication type (semaglutide vs. tirzepatide)

### 6. Sensitivity Analyses
- Varying time windows (e.g., 3, 6, 12 months before/after)
- Excluding patients with minimal follow-up
- Different activity thresholds

## Example Analysis Code

```r
# Load processed data
load("glp1_processed_data.RData")

library(tidyverse)
library(lme4)
library(ggplot2)

# Compare steps before vs after (per patient)
patient_comparison <- activity_with_glp1 %>%
  group_by(person_id, period) %>%
  summarize(
    mean_steps = mean(steps, na.rm = TRUE),
    n_days = n()
  ) %>%
  filter(n_days >= 30) %>%  # At least 30 days in each period
  pivot_wider(
    names_from = period,
    values_from = c(mean_steps, n_days)
  ) %>%
  mutate(
    step_change = mean_steps_after - mean_steps_before,
    pct_change = 100 * step_change / mean_steps_before
  )

# Summary statistics
summary(patient_comparison$step_change)

# Paired t-test
t.test(patient_comparison$mean_steps_after,
       patient_comparison$mean_steps_before,
       paired = TRUE)

# Visualization
ggplot(activity_with_glp1, aes(x = days_from_initiation, y = steps, group = person_id)) +
  geom_line(alpha = 0.2) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Physical Activity Patterns Around GLP-1 Initiation",
    x = "Days from GLP-1 Initiation",
    y = "Daily Steps",
    caption = "Red line: smoothed average; Blue line: GLP-1 initiation"
  ) +
  theme_minimal()
```

## Notes and Considerations

### Data Quality
- Fitbit data may have gaps due to device non-wear
- Consider requiring minimum wear time per period
- Outliers already filtered, but inspect distributions

### Confounding
- Weight loss itself may drive activity changes
- Seasonal variations in activity
- Concurrent lifestyle interventions
- Medication adherence

### Missing Data
- Not all patients have both before and after data
- Varying follow-up durations
- Consider time-to-event or survival methods

### Multiple Testing
- If conducting many comparisons, adjust p-values (Bonferroni, FDR)

## Contact
For questions about this analysis, please refer to the All of Us Research Program documentation.

## Version History
- v1.0 (2024): Initial data processing pipeline
