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

## Analysis Scripts

### 1. **glp1_activity_analysis.R** - Initial Data Processing
Main data loading and preprocessing pipeline. Run this first to create the base datasets.

### 2. **analysis_examples.R** - Basic Statistical Analyses
Simple before/after comparisons with paired t-tests, correlation analyses, and visualizations.

### 3. **windowed_analysis.R** - Windowed Time-Based Analysis (RECOMMENDED)
Sophisticated longitudinal analysis with specific time windows:
- **Baseline Window Selection**: Compares multiple pre-GLP1 windows (-30d to 0, -60d to 0, -90d to 0, -180d to 0) with different minimum Fitbit data requirements (3, 5, 7, 10 days) to select optimal baseline period
- **Follow-up Timepoints**: Analyzes activity at 30, 60, 90, 120, 180, and 360 days post-initiation
- **Active Treatment Requirement**: Only includes patients with GLP-1 prescription within 90 days of each timepoint
- **Nadir Weight Analysis**: Identifies lowest weight during treatment and associated activity levels
- **Measurement Windows**: Uses ±15 day windows around each timepoint with minimum 3 days of Fitbit data

### 4. **windowed_visualizations.R** - Windowed Analysis Figures
Creates publication-quality visualizations for the windowed analysis results.

### 5. **statistical_comparison_table.R** - Publication Table with P-values
Creates comprehensive statistical comparison table showing:
- Baseline vs each follow-up timepoint (30, 60, 90, 120, 180, 360 days + nadir)
- Paired t-tests for all comparisons
- All metrics: weight, steps, sedentary/light/fairly/very active minutes, activity calories
- P-values with significance stars (*** p<0.001, ** p<0.01, * p<0.05)
- Mean ± SD for all measurements
- Change scores and percent change

### 6. **optimized_window_analysis.R** - Optimized Baseline and Follow-up Analysis
Implements exact specifications for baseline window selection and follow-up analysis:
- **Baseline Selection**: Tests 5 pre-GLP1 windows (30d, 60d, 90d, 120d, 180d) with specific minimum activity requirements
- **Selection Criteria**: Chooses window with most patients, highest weight, most activity
- **Follow-up Timepoints**: Days 30, 60, 90, 120, 180, 365 plus nadir
- **Weight Measurement**: Lowest weight within ±15 day window
- **Activity Measurement**: Average activity with minimum 3 days of data within ±15 day window
- **Active Treatment**: Only includes patients with GLP-1 prescription within 90 days of timepoint

### 7. **period_analysis.R** - Period-Based Analysis with Period-Specific Baselines and Paired T-Tests
Alternative analysis approach using time periods with rigorous paired comparison methodology:
- **Time Periods**: 1-30d, 31-60d, 61-90d, 1-90d, 91-180d, 181-365d, 1-45d, 46-90d post-initiation
- **Period-Specific Baselines**: Each period has its own baseline calculated ONLY from patients who appear in that period, ensuring proper paired comparison (N_baseline = N_period = N_paired)
- **Baseline Window**: Days -180 to 0 before GLP-1 initiation
  - **Weight**: HIGHEST weight in baseline window (true starting weight)
  - **Activity**: AVERAGE activity metrics (minimum 3 days required)
  - **Wear Time Adjustment**: Calculates percentage of wear time for sedentary, light, and MVPA
  - **MVPA Diagnostics**: Tracks days with MVPA > 0 to identify zero-inflation
- **Follow-up Measurement**:
  - **Weight**: LOWEST weight during period
  - **Activity**: AVERAGE activity metrics (minimum 3 days required)
  - **Active Treatment**: Only includes patients with ≥2 prescription fills AND prescription within 90 days of period midpoint
- **Statistical Methods**:
  - **Paired t-tests**: Each period vs its specific baseline (same patients)
  - **Random effects models**: Linear trends across all periods (lme4/lmerTest)
- **Output**: Comprehensive table with baseline values, period values, changes (Δ), and p-values for ALL metrics including weight
- **HTML Output**: Formatted HTML table for easy viewing

### 8. **sensitivity_analysis.R** - Stratification by Weight Loss and Step Change
Sensitivity analyses to explore heterogeneity in treatment response:

**Analysis 1 - Activity Changes by Weight Loss Category**:
- Stratifies patients by weight loss magnitude: < 5%, 5-10%, > 10% loss
- Alternative categorization: < 7.5% vs ≥ 7.5% loss
- Compares step changes and calorie changes across weight loss groups
- ANOVA and t-tests to test for significant differences
- Answers: Do patients who lose more weight also increase activity more?

**Analysis 2 - Weight Changes by Step Change Category**:
- Stratifies patients by step change: Decrease > 5%, No change (-5% to +5%), Increase > 5%
- Compares weight changes across activity behavior groups
- ANOVA with post-hoc Tukey HSD tests
- Answers: Do patients who increase steps lose more weight?

**Target Periods**: Analyzes three key aggregated periods (1-90d, 91-180d, 181-365d)
**Output**: CSV files for each analysis and period, plus comprehensive RData file

### 9. **period_visualizations.R** - Period Analysis Figures
Creates publication-quality visualizations for the period-based analysis:
- **Figure 1**: Weight and steps trajectories across periods with error bars
- **Figure 2**: Activity composition stacked bar chart (sedentary/light/fairly/very active)
- **Figure 3**: 4-panel change from baseline (weight, steps, sedentary, very active)
- **Figure 4**: Sample sizes and retention rates across periods
- **Figure 5**: Percent change heatmap for all metrics

### 10. **period_analysis_optimized.R** - Optimized Period Analysis with Baseline Selection and Nadir ⭐ RECOMMENDED
Streamlined period-based analysis with smart baseline selection:

**Cohort Definition** (CRITICAL):
- First identifies final 1-90d cohort (with active treatment requirement)
- Baseline calculated ONLY for patients in this final cohort
- Ensures baseline N = 1-90d N = N_paired for valid comparisons

**Baseline Selection Algorithm**:
- Tests 3 candidate windows: -30 to 0d, -90 to 0d, -180 to 0d
- Selects window with: **HIGHEST weight** + most steps/activity
- Single baseline used for ALL comparisons

**Follow-up Periods**:
- **1-90d**: First 3 months
- **91-180d**: 3-6 months
- **181-365d**: 6-12 months
- **Nadir**: Lowest on-treatment weight (mean ± SD days to nadir reported)

**Nadir Analysis**:
- Finds lowest weight during treatment for each patient
- Activity = mean of ±30 days around nadir
- Requires active treatment at nadir (≥2 fills, Rx within 90 days)

**Statistical Methods**:
- Paired t-tests for each timepoint vs baseline
- Same baseline cohort for all comparisons

**Output**:
- CSV table: Baseline + 3 periods + Nadir with N, N_paired, mean ± SD, Δ, and p-values
- HTML table: Formatted version for easy viewing in browser

### 11. **sensitivity_visualizations.R** - Sensitivity Analysis Figures (for Optimized Periods)
Creates visualizations using the optimized period analysis timepoints:

**Categorization**: Patients grouped by baseline-to-nadir weight loss (< 5%, 5-10%, > 10%)

**Figure 1 - Weight Trajectories**:
- All 5 timepoints: Baseline, 1-90d, 91-180d, 181-365d, Nadir
- Sample sizes (n) displayed at each timepoint
- Mean ± SE with error bars

**Figure 2 - Steps Trajectories**:
- Daily steps across all 5 timepoints
- Sample sizes at each timepoint

**Figure 3 - Calories Trajectories**:
- Activity calories across all 5 timepoints
- Sample sizes at each timepoint

**Figure 4 - Combined 3-Panel**:
- Weight, Steps, Calories in one view
- All timepoints shown with shared legend

**Figures 5 & 6 - Spaghetti Plots**:
- Individual patient trajectories (up to 30 per category)
- Weight and steps over time
- Bold mean line with SE ribbon

Color scheme: Red (< 5% loss), Orange (5-10% loss), Green (> 10% loss)
All figures: 300 dpi, publication-ready

## Windowed Analysis Methodology

The windowed analysis provides a rigorous, time-based approach to assess activity changes:

### Eligibility Criteria
Patients must meet **both** criteria:
1. **GLP-1 Therapy**: Initiated injectable semaglutide or tirzepatide
2. **BMI Criteria** (at baseline, before or at GLP-1 initiation):
   - BMI ≥ 30, **OR**
   - BMI ≥ 27 with documented obesity diagnosis

### Baseline Selection Algorithm
1. Tests four pre-GLP1 windows: -30 to 0 days, -60 to 0, -90 to 0, -180 to 0
2. For each window, requires 3, 5, 7, or 10 minimum Fitbit days
3. Calculates activity score = (total active minutes) - (sedentary minutes / 10)
4. Recommends window with highest activity engagement

### Follow-up Assessment
At each timepoint (30, 60, 90, 120, 180, 360 days):
1. **Eligibility**: Patient must have GLP-1 prescription within 90 days of timepoint
2. **Measurement Window**: ±15 days around timepoint
3. **Minimum Data**: At least 3 days of Fitbit data in the window
4. **Weight**: Lowest weight within the ±15 day window
5. **Activity**: Mean activity metrics within the ±15 day window

### Nadir Analysis
1. Identifies lowest weight during treatment for each patient
2. Requires active GLP-1 at time of nadir (prescription within 90 days)
3. Calculates activity metrics in ±15 day window around nadir
4. Reports time from initiation to nadir (mean ± SD)

This approach ensures:
- Only patients on active treatment are analyzed
- Sufficient data density for reliable estimates
- Consistent measurement windows across patients
- Captures both scheduled timepoints and individualized nadir

## Period-Based Analysis Methodology

The period-based analysis provides an alternative approach using consecutive time periods and mixed effects models:

### Eligibility Criteria
Same as windowed analysis:
1. **GLP-1 Therapy**: Initiated injectable semaglutide or tirzepatide
2. **BMI Criteria**: BMI ≥ 30 OR BMI ≥ 27 with documented obesity diagnosis

### Time Periods
Seven post-initiation periods are analyzed:
1. **1-30 days**: First month
2. **31-60 days**: Second month
3. **61-90 days**: Third month
4. **91-180 days**: 3-6 months
5. **181-365 days**: 6-12 months
6. **1-45 days**: First 1.5 months (alternative)
7. **46-90 days**: 1.5-3 months (alternative)

### Baseline
Uses the optimal baseline window selected from windowed analysis (typically 30-180 days pre-initiation)

### Measurement Approach
For each time period:
1. **Weight**: Lowest weight recorded during the period
2. **Activity**: Average of all activity metrics during the period
3. **Minimum Data**: At least 3 days of Fitbit data required to be included
4. **Active Treatment**: Patient must have GLP-1 prescription within 90 days of period midpoint (same as windowed analysis)

### Statistical Analysis
**Random Effects Models** (lme4/lmerTest):
- Accounts for repeated measures within patients
- Handles missing data and dropout
- Models temporal trends across periods
- Provides p-values for change over time
- Random intercepts for each patient

This approach:
- Captures gradual changes over longer periods
- Maximizes data utilization within each period
- Accounts for varying follow-up durations
- Provides robust inference with dropout

## Key Output Files

### Basic Processing Outputs
1. **glp1_processed_data.RData**: Complete R workspace with all processed data
2. **glp1_initiation_dates.csv**: Patient-level GLP-1 initiation information
3. **activity_by_glp1_period.csv**: Daily Fitbit activity by period (before/after)
4. **weight_by_glp1_period.csv**: Weight measurements by period (before/after)

### Windowed Analysis Outputs
5. **windowed_analysis_results.RData**: Complete windowed analysis results
6. **summary_table.csv**: Formatted summary table (mean ± SD) for all timepoints
7. **summary_table_raw.csv**: Raw summary with separate mean and SD columns
8. **followup_individual_data.csv**: Patient-level data at each follow-up timepoint
9. **nadir_individual_data.csv**: Patient-level nadir weight and activity data

### Visualization Outputs
10. **figure_trajectories.png**: Weight and steps trajectories over time
11. **figure_activity_composition.png**: Stacked area chart of activity levels
12. **figure_change_from_baseline.png**: 4-panel change from baseline analysis
13. **figure_nadir_analysis.png**: Nadir weight distribution and associations
14. **figure_sample_sizes.png**: Sample size at each timepoint
15. **figure_percent_change.png**: Percent change for all metrics

### Statistical Comparison Outputs
16. **statistical_comparisons_table.csv**: Publication-ready table with p-values
17. **statistical_comparisons_detailed.csv**: Detailed statistical results
18. **key_findings_summary.csv**: Brief summary of key findings

### Period Analysis Outputs (with Period-Specific Baselines)
19. **period_analysis_results.RData**: Complete period-based analysis results with period-specific baselines
20. **period_analysis_comprehensive_table.csv**: Main publication table with baseline, period values, Δ, and p-values
21. **period_analysis_comprehensive_table.html**: Formatted HTML version of comprehensive table (easy viewing)
22. **period_analysis_descriptive.csv**: Descriptive statistics only (Mean ± SD)
23. **period_analysis_changes.csv**: Changes from baseline only
24. **period_analysis_paired_tests.csv**: Detailed paired t-test results
25. **period_analysis_summary_with_diagnostics.csv**: Wear time percentages and MVPA diagnostics
26. **period_analysis_raw_stats.csv**: Raw statistics for all periods
27. **period_analysis_model_pvalues.csv**: Random effects model p-values (linear trend)
28. **period_analysis_model_coefficients.csv**: Full random effects model results
29. **period_analysis_long_data.csv**: Long format data for custom analyses

### Sensitivity Analysis Outputs
30. **sensitivity_weight_loss_3cat_[period].csv**: Activity changes by 3-category weight loss (< 5%, 5-10%, > 10%)
31. **sensitivity_weight_loss_2cat_[period].csv**: Activity changes by 2-category weight loss (< 7.5%, ≥ 7.5%)
32. **sensitivity_step_change_[period].csv**: Weight changes by step change category
33. **sensitivity_analysis_results.RData**: Complete sensitivity analysis results and patient-level data

### Period Visualization Outputs
34. **period_figure1_trajectories.png**: Weight and steps trajectories with error bars
35. **period_figure2_activity_composition.png**: Stacked bar chart of activity intensity
36. **period_figure3_change_from_baseline.png**: 4-panel change analysis
37. **period_figure4_sample_sizes.png**: Sample sizes and retention rates
38. **period_figure5_percent_change.png**: Percent change heatmap

### Optimized Period Analysis Outputs (with Baseline Selection and Nadir)
39. **period_analysis_optimized_table.csv**: Comprehensive table with Baseline + 3 periods + Nadir
40. **period_analysis_optimized_table.html**: Formatted HTML version of comprehensive table
41. **period_analysis_optimized_detailed.csv**: Detailed results with all calculated values
42. **period_analysis_optimized_results.RData**: All data for further analysis and visualization

### Sensitivity Visualization Outputs (Optimized Timepoints Version)
43. **sensitivity_figure1_weight_trajectories_by_category.png**: Weight across all 5 timepoints by weight loss category
44. **sensitivity_figure2_steps_trajectories_by_category.png**: Steps across all 5 timepoints by weight loss category
45. **sensitivity_figure3_calories_trajectories_by_category.png**: Calories across all 5 timepoints by weight loss category
46. **sensitivity_figure4_combined_trajectories.png**: 3-panel figure (Weight, Steps, Calories)
47. **sensitivity_figure5_spaghetti_weight.png**: Individual weight trajectories by weight loss category
48. **sensitivity_figure6_spaghetti_steps.png**: Individual steps trajectories by weight loss category

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

### Step-by-Step Analysis Workflow

#### Step 1: Initial Data Processing
```r
# In All of Us Workbench R/RStudio environment
source("glp1_activity_analysis.R")
# This creates: glp1_processed_data.RData
```

#### Step 2: Windowed Analysis (RECOMMENDED)
```r
# Run the windowed analysis
source("windowed_analysis.R")
# This creates: windowed_analysis_results.RData and CSV files
```

#### Step 3: Statistical Comparison Table (RECOMMENDED)
```r
# Create publication table with p-values
source("statistical_comparison_table.R")
# This creates: statistical_comparisons_table.csv
```

#### Step 4: Create Visualizations
```r
# Generate all figures
source("windowed_visualizations.R")
# This creates: PNG figures in current directory
```

#### Step 5 (RECOMMENDED): Optimized Period Analysis with Baseline Selection and Nadir
```r
# Run optimized period-based analysis (RECOMMENDED approach)
source("period_analysis_optimized.R")
# This creates: period_analysis_optimized_table.csv and period_analysis_optimized_results.RData
# Features:
# - Smart baseline selection (tests -30d, -90d, -180d windows)
# - 3 key periods: 1-90d, 91-180d, 181-365d
# - Nadir analysis with activity data (±30 days)
# - Single comprehensive table with all comparisons
```

#### Step 5 (Alternative): Period-Based Analysis with Period-Specific Baselines
```r
# Run period-based analysis with period-specific baselines (alternative approach)
source("period_analysis.R")
# This creates: period_analysis_results.RData, comprehensive_table.csv/html, and diagnostic CSVs
# Uses period-specific baselines for proper paired comparison
# Includes paired t-tests AND random effects models
# More periods (8 total) with individual baselines for each
```

#### Step 6: Sensitivity Analyses - Stratification by Weight Loss and Step Change
```r
# Run sensitivity analyses (requires period_analysis_results.RData from Step 5)
source("sensitivity_analysis.R")
# This creates: sensitivity_*.csv files for each analysis and period
# Analysis 1: Activity changes by weight loss category
# Analysis 2: Weight changes by step change category
```

#### Step 7: Period Analysis Visualizations
```r
# Generate figures for period-based analysis (run after Step 5)
source("period_visualizations.R")
# This creates: period_figure*.png files
# Requires period_analysis_results.RData
```

#### Step 8: Sensitivity Analysis Visualizations (for Optimized Periods)
```r
# Generate figures using optimized period timepoints (run after Step 5 RECOMMENDED)
source("sensitivity_visualizations.R")
# This creates: 6 sensitivity_figure*.png files
# Requires period_analysis_optimized_results.RData from Step 5 (RECOMMENDED)
# Shows ALL timepoints: Baseline, 1-90d, 91-180d, 181-365d, Nadir
# Categorizes patients by baseline-to-nadir weight loss
# Sample sizes displayed at each timepoint
```

#### Optional: Optimized Window Analysis
```r
# For customized baseline selection and follow-up
source("optimized_window_analysis.R")
```

#### Optional: Basic Analyses
```r
# For simple before/after comparisons
source("analysis_examples.R")
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

# Load windowed analysis results
load("windowed_analysis_results.RData")
View(windowed_analysis_results$combined_summary)
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

### 6. Sensitivity Analyses ✓ IMPLEMENTED
**Now available via sensitivity_analysis.R**:
- **Weight loss stratification**: Activity changes by weight loss magnitude (< 5%, 5-10%, > 10% or < 7.5%, ≥ 7.5%)
- **Step change stratification**: Weight changes by activity behavior (decrease > 5%, no change, increase > 5%)
- **Statistical testing**: ANOVA and t-tests to compare groups

**Additional sensitivity analyses to consider**:
- Varying time windows (e.g., 3, 6, 12 months before/after)
- Excluding patients with minimal follow-up
- Different activity thresholds
- Varying minimum Fitbit wear time requirements

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
- **v1.0** (2024): Initial data processing pipeline
- **v2.0** (2024): Added windowed analysis with baseline selection, follow-up timepoints, and nadir analysis
- **v3.0** (2025): Major update to period-based analysis and sensitivity analyses:
  - Implemented period-specific baselines for proper paired comparisons
  - Fixed weight p-values in comprehensive table
  - Added wear time adjustments and MVPA diagnostics
  - Created sensitivity_analysis.R for weight loss and step change stratification
  - HTML output for comprehensive tables
  - Enhanced documentation and interpretation guides
- **v3.1** (2025): Optimized period analysis with smart baseline selection:
  - Created period_analysis_optimized.R (RECOMMENDED approach)
  - Automated baseline selection algorithm (tests -30d, -90d, -180d)
  - Focused on 3 key periods: 1-90d, 91-180d, 181-365d
  - Added nadir analysis with surrounding activity data (±30 days)
  - Single baseline for all comparisons ensures consistency
  - Streamlined output: one comprehensive table with all timepoints
- **v3.2** (2025): Critical fix for baseline cohort matching:
  - Fixed baseline cohort to match 1-90d period exactly
  - Now first identifies final 1-90d cohort (with active treatment)
  - Then calculates baseline ONLY for those patients
  - Ensures baseline N = 1-90d N = N_paired for valid paired comparisons
  - Improved handling of missing activity data (NA instead of NaN)
  - Enhanced documentation with important notes about cohort matching
- **v3.3** (2025): Complete rewrite of cohort matching algorithm:
  - CRITICAL FIX: Baseline N now EXACTLY matches 1-90d N (not just subset)
  - New strategy: Find patients with BOTH baseline AND 1-90d data first
  - Then select best baseline window from those matched patients only
  - Fixed nadir to ONLY use on-treatment weights (days > 0, not baseline)
  - Fixed missing activity data by ensuring matched cohort throughout
  - All periods now use exact same matched cohort (no more mismatches)
  - Updated all documentation to emphasize matched cohort requirement
