# =============================================================================
# NORMALITY TESTING FOR ACTIVITY PARAMETERS
# =============================================================================
# Tests whether activity metrics follow normal distribution at each time period
# Informs whether to use mean ± SD or median (IQR) for Table 2
# =============================================================================

library(tidyverse)
library(gridExtra)
library(grid)

cat("\n##################################################\n")
cat("NORMALITY TESTING FOR ACTIVITY PARAMETERS\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading cleaned data...\n")
load("glp1_cleaned_data.RData")

# Object name mapping (if needed for compatibility)
if (!exists("activity_cleaned") && exists("activity_final")) {
  activity_cleaned <- activity_final
}
if (!exists("obesity_cohort")) {
  stop("Error: obesity_cohort not found in RData file")
}

cat(sprintf("Loaded: %d patients in obesity cohort\n", nrow(obesity_cohort)))
cat(sprintf("        %d activity records\n\n", nrow(activity_cleaned)))

# =============================================================================
# DEFINE BASELINE COHORT (EXACT TABLE 1 LOGIC)
# =============================================================================

cat("Defining baseline cohort...\n")
cat("Criteria: ≥3 valid baseline days AND ≥3 valid 1-30d follow-up days\n\n")

# Patients with ≥3 valid baseline days
baseline_activity_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_baseline_days = n(), .groups = "drop")

# Patients with ≥3 valid 1-30d follow-up days
followup_1_30d_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 30,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_followup_days = n(), .groups = "drop")

# Intersection: both baseline AND 1-30d follow-up
baseline_cohort_fixed <- baseline_activity_patients %>%
  inner_join(followup_1_30d_patients, by = "person_id")

cat(sprintf("Baseline cohort: N=%d\n\n", nrow(baseline_cohort_fixed)))

# =============================================================================
# DEFINE TIME PERIODS AND FILTER ACTIVITY DATA
# =============================================================================

cat("Defining time periods...\n")

# Period assignments
period_data <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort_fixed$person_id,
         is_valid_day == TRUE) %>%
  mutate(
    period = case_when(
      days_from_initiation >= -180 & days_from_initiation <= 0 ~ "Baseline",
      days_from_initiation >= 1 & days_from_initiation <= 30 ~ "1-30d",
      days_from_initiation >= 31 & days_from_initiation <= 90 ~ "31-90d",
      days_from_initiation >= 91 & days_from_initiation <= 180 ~ "91-180d",
      days_from_initiation >= 181 & days_from_initiation <= 365 ~ "181-365d",
      days_from_initiation > 84 ~ "Nadir",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

cat(sprintf("Total valid activity days: %d\n\n", nrow(period_data)))

# =============================================================================
# AGGREGATE TO PERSON-PERIOD LEVEL
# =============================================================================

cat("Aggregating to person-period means...\n")

# Calculate mean per person per period
person_period_means <- period_data %>%
  group_by(person_id, period) %>%
  summarize(
    n_days = n(),
    steps = mean(steps, na.rm = TRUE),
    activity_calories = mean(activity_calories, na.rm = TRUE),
    total_wear_minutes = mean(total_wear_minutes, na.rm = TRUE),
    pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
    pct_light = mean(pct_light, na.rm = TRUE),
    pct_fairly = mean(pct_fairly, na.rm = TRUE),
    pct_very = mean(pct_very, na.rm = TRUE),
    pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_days >= 3)  # Require ≥3 valid days per period

cat(sprintf("Person-period observations: %d\n\n", nrow(person_period_means)))

# =============================================================================
# NORMALITY TESTING FUNCTION
# =============================================================================

test_normality <- function(data, var_name, period_name) {
  x <- data %>% pull(!!sym(var_name))
  n <- length(x)

  # Skip if too few observations
  if (n < 3) {
    return(tibble(
      period = period_name,
      parameter = var_name,
      n = n,
      shapiro_W = NA,
      shapiro_p = NA,
      normal = NA,
      skewness = NA,
      kurtosis = NA
    ))
  }

  # Shapiro-Wilk test (only works for n = 3 to 5000)
  if (n >= 3 && n <= 5000) {
    shapiro_result <- shapiro.test(x)
    shapiro_W <- shapiro_result$statistic
    shapiro_p <- shapiro_result$p.value
  } else {
    shapiro_W <- NA
    shapiro_p <- NA
  }

  # Calculate skewness and kurtosis
  x_centered <- x - mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  if (sd_x > 0) {
    skewness <- mean((x_centered / sd_x)^3, na.rm = TRUE)
    kurtosis <- mean((x_centered / sd_x)^4, na.rm = TRUE) - 3  # Excess kurtosis
  } else {
    skewness <- NA
    kurtosis <- NA
  }

  # Decision: normal if p > 0.05 (don't reject null hypothesis of normality)
  normal <- ifelse(is.na(shapiro_p), NA, shapiro_p > 0.05)

  tibble(
    period = period_name,
    parameter = var_name,
    n = n,
    shapiro_W = shapiro_W,
    shapiro_p = shapiro_p,
    normal = normal,
    skewness = skewness,
    kurtosis = kurtosis
  )
}

# =============================================================================
# TEST ALL PARAMETERS ACROSS ALL PERIODS
# =============================================================================

cat("Testing normality for all parameters...\n\n")

# Parameters to test
activity_params <- c(
  "steps",
  "activity_calories",
  "total_wear_minutes",
  "pct_sedentary",
  "pct_light",
  "pct_fairly",
  "pct_very",
  "pct_MVPA"
)

# Periods to test
periods <- c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d", "Nadir")

# Run normality tests
normality_results <- expand_grid(
  period = periods,
  parameter = activity_params
) %>%
  rowwise() %>%
  mutate(
    test_result = list(test_normality(
      person_period_means %>% filter(period == !!period),
      parameter,
      period
    ))
  ) %>%
  ungroup() %>%
  select(test_result) %>%
  unnest(test_result)

# =============================================================================
# GENERATE REPORT
# =============================================================================

cat("========================================\n")
cat("NORMALITY TEST RESULTS\n")
cat("========================================\n\n")

cat("Interpretation:\n")
cat("  - Shapiro-Wilk p > 0.05: Data consistent with normal distribution\n")
cat("  - Shapiro-Wilk p ≤ 0.05: Data NOT normally distributed\n")
cat("  - Skewness: |value| < 1 = fairly symmetric, |value| > 1 = skewed\n")
cat("  - Kurtosis: |value| < 1 = tails similar to normal, |value| > 1 = heavy/light tails\n\n")

# Summary by parameter (across all periods)
cat("SUMMARY BY PARAMETER:\n")
cat("--------------------\n\n")

param_summary <- normality_results %>%
  group_by(parameter) %>%
  summarize(
    periods_tested = sum(!is.na(shapiro_p)),
    periods_normal = sum(normal, na.rm = TRUE),
    pct_normal = 100 * periods_normal / periods_tested,
    min_p = min(shapiro_p, na.rm = TRUE),
    max_p = max(shapiro_p, na.rm = TRUE),
    mean_skewness = mean(abs(skewness), na.rm = TRUE),
    mean_kurtosis = mean(abs(kurtosis), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pct_normal)

print(param_summary)
cat("\n")

# Detailed results table
cat("DETAILED RESULTS (by period and parameter):\n")
cat("--------------------------------------------\n\n")

detailed_table <- normality_results %>%
  mutate(
    result = case_when(
      is.na(shapiro_p) ~ "Insufficient data",
      normal ~ "NORMAL",
      TRUE ~ "NOT NORMAL"
    ),
    shapiro_p_fmt = ifelse(is.na(shapiro_p), "NA", sprintf("%.4f", shapiro_p))
  ) %>%
  select(period, parameter, n, result, shapiro_p_fmt, skewness, kurtosis)

print(detailed_table, n = Inf)
cat("\n")

# Overall recommendation
cat("========================================\n")
cat("RECOMMENDATION FOR TABLE 2\n")
cat("========================================\n\n")

# Count how many parameters are consistently normal
consistently_normal <- param_summary %>%
  filter(pct_normal >= 80) %>%  # Normal in ≥80% of periods
  nrow()

consistently_nonnormal <- param_summary %>%
  filter(pct_normal < 50) %>%  # Non-normal in ≥50% of periods
  nrow()

if (consistently_nonnormal > consistently_normal) {
  cat("✗ MOST PARAMETERS ARE NOT NORMALLY DISTRIBUTED\n\n")
  cat("Recommendation: Use MEDIAN (IQR) for Table 2\n")
  cat("  - More appropriate for non-normal data\n")
  cat("  - More robust to outliers\n")
  cat("  - Still use mixed effects models for p-values (robust to non-normality with large N)\n\n")
  recommendation <- "median_iqr"
} else if (consistently_normal > 4) {
  cat("✓ MOST PARAMETERS ARE NORMALLY DISTRIBUTED\n\n")
  cat("Recommendation: Use MEAN ± SD for Table 2\n")
  cat("  - Standard for normally distributed data\n")
  cat("  - Continue using mixed effects models for p-values\n\n")
  recommendation <- "mean_sd"
} else {
  cat("⚠ MIXED RESULTS: Some normal, some not\n\n")
  cat("Recommendation: Use MEDIAN (IQR) for Table 2\n")
  cat("  - Conservative choice\n")
  cat("  - Applicable to both normal and non-normal distributions\n")
  cat("  - Still use mixed effects models for p-values\n\n")
  recommendation <- "median_iqr"
}

# List specific parameters that are consistently non-normal
non_normal_params <- param_summary %>%
  filter(pct_normal < 50) %>%
  pull(parameter)

if (length(non_normal_params) > 0) {
  cat("Parameters consistently non-normal:\n")
  for (param in non_normal_params) {
    cat(sprintf("  - %s\n", param))
  }
  cat("\n")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

# Save detailed results
write.csv(normality_results, "normality_test_results.csv", row.names = FALSE)
write.csv(param_summary, "normality_summary_by_parameter.csv", row.names = FALSE)

# Save recommendation
recommendation_file <- data.frame(
  recommendation = recommendation,
  date_tested = Sys.Date(),
  n_patients = nrow(baseline_cohort_fixed),
  consistently_normal = consistently_normal,
  consistently_nonnormal = consistently_nonnormal
)
write.csv(recommendation_file, "normality_recommendation.csv", row.names = FALSE)

cat("Results saved:\n")
cat("  - normality_test_results.csv (detailed results)\n")
cat("  - normality_summary_by_parameter.csv (summary)\n")
cat("  - normality_recommendation.csv (recommendation)\n\n")

cat("##################################################\n")
cat("NORMALITY TESTING COMPLETE\n")
cat("##################################################\n")
