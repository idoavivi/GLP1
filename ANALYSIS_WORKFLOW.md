# GLP-1 Obesity Cohort Analysis Workflow

Complete workflow for analyzing GLP-1 activity data from All of Us. Created for offline analysis.

---

## Overview

This workflow creates Table 1 (baseline characteristics) and Table 2 (activity changes over time) for a GLP-1 obesity cohort using All of Us data.

**Baseline Cohort Definition (N=304)**:
- BMI ≥30 OR obesity diagnosis
- On injectable GLP-1 therapy (semaglutide/tirzepatide)
- ≥3 valid baseline activity days (-180 to 0 days from initiation)
- ≥3 valid 1-30d follow-up activity days

---

## Files Created

### 1. **test_normality.R**
Tests whether activity parameters follow normal distribution.

**What it does**:
- Tests Shapiro-Wilk normality for all activity parameters at each time period
- Calculates skewness and kurtosis
- Generates recommendation: use mean ± SD or median (IQR) for Table 2
- Saves results to CSV files

**How to run**:
```r
# In RStudio or All of Us Workbench
source("test_normality.R")
```

**Outputs**:
- `normality_test_results.csv` - Detailed Shapiro-Wilk test results by period and parameter
- `normality_summary_by_parameter.csv` - Summary of normality across all periods
- `normality_recommendation.csv` - Final recommendation (mean_sd or median_iqr)

**When to run**:
- Run ONCE after loading fresh data from All of Us
- Re-run if you change inclusion criteria or data filters

---

### 2. **table2_activity_adaptive.R**
Creates Table 2 with activity changes over time. Automatically adapts to use mean ± SD or median (IQR) based on normality testing.

**What it does**:
- Loads normality recommendation from `normality_recommendation.csv`
- Calculates either mean ± SD or median (IQR) for all activity parameters
- Uses mixed effects models for p-values (robust to non-normality)
- Compares each follow-up period to baseline
- Creates wide-format table with periods as columns

**How to run**:
```r
# FIRST: Run test_normality.R (only once)
source("test_normality.R")

# THEN: Run table2_activity_adaptive.R
source("table2_activity_adaptive.R")
```

**Outputs**:
- `table2_activity_adaptive.csv` - Data table
- `table2_activity_adaptive.html` - Publication-ready HTML table

**Activity parameters included**:
- Steps per day
- MVPA (moderate-to-vigorous physical activity, min/day)
- Sedentary time (min/day)
- Activity calories (kcal/day)

**Time periods**:
- Baseline (-180 to 0 days)
- 1-30 days
- 31-90 days
- 91-180 days
- 181-365 days

---

### 3. **table1_baseline_characteristics.R**
Creates Table 1 with baseline characteristics of the cohort.

**What it does**:
- Defines baseline cohort using exact same logic as Table 2
- Calculates baseline anthropometrics (weight, BMI)
- Calculates baseline activity metrics
- Summarizes obesity inclusion criteria
- Notes missing demographics (age, sex, race/ethnicity)

**How to run**:
```r
source("table1_baseline_characteristics.R")
```

**Outputs**:
- `table1_baseline_characteristics.csv` - Data table
- `table1_baseline_characteristics.html` - Publication-ready HTML table
- `baseline_data_patient_level.csv` - Patient-level baseline data (for additional analysis)
- `table1_missing_data_summary.csv` - Summary of missing data

**What's included**:
- Total N
- Obesity inclusion criteria (BMI ≥30 vs. obesity diagnosis)
- Baseline weight and BMI (mean ± SD)
- BMI categories (Class I, II, III)
- Baseline activity metrics (steps, MVPA, sedentary, calories)
- Number of valid activity days

**What's NOT included** (needs to be added from All of Us):
- Age
- Sex/Gender
- Race/Ethnicity
- Comorbidities (diabetes, hypertension, etc.)

To add demographics, run `pull_demographics_diagnoses.R` in All of Us Workbench.

---

## Complete Workflow

### Step 1: Data Preparation (in All of Us Workbench)

```r
# Run comprehensive data cleaning
source("comprehensive_data_cleaning.R")

# This creates:
# - glp1_cleaned_data.RData
# - comprehensive_cleaning_summary.csv
```

**Download from All of Us**:
- `glp1_cleaned_data.RData`
- `comprehensive_cleaning_summary.csv`

---

### Step 2: Test Normality (in RStudio or All of Us)

```r
# Load data
load("glp1_cleaned_data.RData")

# Test normality
source("test_normality.R")

# Review recommendation
normality_rec <- read.csv("normality_recommendation.csv")
print(normality_rec)
```

**Expected output**:
```
recommendation: median_iqr (if data is non-normal)
  OR
recommendation: mean_sd (if data is normal)
```

---

### Step 3: Generate Table 2 (in RStudio or All of Us)

```r
# Generate adaptive Table 2
source("table2_activity_adaptive.R")

# Review output
table2 <- read.csv("table2_activity_adaptive.csv")
print(table2)
```

**Table 2 will automatically use**:
- Median (IQR) if data is non-normal
- Mean ± SD if data is normal
- Mixed effects p-values in both cases

---

### Step 4: Generate Table 1 (in RStudio or All of Us)

```r
# Generate Table 1
source("table1_baseline_characteristics.R")

# Review output
table1 <- read.csv("table1_baseline_characteristics.csv")
print(table1)
```

---

## Understanding the Output

### Table 1 Format
```
Characteristic                    | Value (Mean ± SD or N (%))
----------------------------------|---------------------------
Total N                          | 304
Obesity Inclusion Criteria       |
  BMI ≥ 30                       | 280 (92.1%)
  Obesity diagnosis              | 24 (7.9%)
Anthropometrics                  |
  Weight (kg)                    | 102.3 ± 18.5
  BMI (kg/m²)                    | 36.2 ± 5.8
...
```

### Table 2 Format (if using median)
```
Parameter              | Baseline      | 1-30 days     | 31-90 days    | ...
-----------------------|---------------|---------------|---------------|----
Steps per day          | 304 (5709, 3450) | 304 (5650, 3200) | 280 (5580, 3100) | ...
  P-value              | —             | 0.3241        | 0.1852        | ...
MVPA (min/day)         | 304 (45, 35)  | 304 (43, 32)  | 280 (42, 30)  | ...
  P-value              | —             | 0.0823        | 0.0451        | ...
...
```
*Format: N (Median, IQR)*

### Table 2 Format (if using mean)
```
Parameter              | Baseline         | 1-30 days        | 31-90 days       | ...
-----------------------|------------------|------------------|------------------|----
Steps per day          | 304 (5709 ± 3450)| 304 (5650 ± 3200)| 280 (5580 ± 3100)| ...
  P-value              | —                | 0.3241           | 0.1852           | ...
...
```
*Format: N (Mean ± SD)*

---

## Key Notes

### About Normality Testing

- **Mixed effects models are robust** to non-normality with sufficient sample size (N>30 per group)
- P-values remain valid even when data is non-normal
- Median (IQR) is more appropriate for **descriptive statistics** when data is skewed
- The adaptive script ensures the table uses the most appropriate summary statistic

### About the Baseline Cohort

**CRITICAL**: The baseline cohort is FIXED at N=304 (or current ground truth)

This means:
- All patients in the cohort have ≥3 valid baseline days
- All patients in the cohort have ≥3 valid 1-30d follow-up days
- The N for baseline and 1-30d should be IDENTICAL
- Follow-up periods may have lower N due to attrition

### About P-values

P-values are from **linear mixed effects models** comparing each period to baseline:
- Model: `outcome ~ period + (1 | person_id)`
- Random intercept accounts for within-person correlation
- Categorical period variable allows comparison of each period to baseline
- Baseline is the reference category (p-value = "—")

---

## Troubleshooting

### Issue: "Object 'activity_cleaned' not found"
**Solution**: Your RData file uses `_final` suffix. The scripts handle this automatically, but if you get this error, check that you're loading `glp1_cleaned_data.RData` (not the old version).

### Issue: N doesn't match expected (e.g., N=190 instead of N=304)
**Solution**: Re-run `comprehensive_data_cleaning.R` in All of Us to get fresh data with the correct obesity_cohort definition (BMI ≥30 OR obesity diagnosis).

### Issue: "Normality recommendation not found"
**Solution**: Run `test_normality.R` first. The adaptive Table 2 script will default to mean ± SD if no recommendation exists.

### Issue: Weight is increasing instead of decreasing
**Solution**: This is a data quality issue. Focus on activity-only analysis first. Weight analysis requires funnel design (only patients with measurements at ALL previous consecutive periods).

---

## Next Steps (Future Analysis)

1. **Add demographics to Table 1**
   - Run `pull_demographics_diagnoses.R` in All of Us
   - Merge age, sex, race/ethnicity with baseline_cohort
   - Update Table 1 script

2. **Add weight analysis to Table 2**
   - Implement funnel design for weight
   - Ensure weight decreases (not increases) on GLP-1
   - May need additional data quality filters

3. **Sensitivity analyses**
   - By baseline BMI class: change in steps and activity calories
   - By baseline step count: change in nadir weight
   - Generate publication-ready visualizations

4. **Methods section**
   - Already created: `METHODS.md`
   - Update with final N and any changes to inclusion criteria

---

## Questions?

If you encounter issues:
1. Check the diagnostic output in the R console
2. Review the comprehensive_cleaning_summary.csv for data quality metrics
3. Check table1_missing_data_summary.csv to see data completeness

---

**Last Updated**: 2026-01-13
**Data Version**: glp1_cleaned_data.RData (from comprehensive_data_cleaning.R)
**Ground Truth N**: 304 patients
