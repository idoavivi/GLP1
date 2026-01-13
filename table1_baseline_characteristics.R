# =============================================================================
# TABLE 1: BASELINE CHARACTERISTICS
# =============================================================================
# Baseline characteristics of the GLP-1 obesity cohort
# Fixed baseline cohort: Patients with ≥3 valid baseline AND ≥3 valid 1-30d
# =============================================================================

library(tidyverse)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("TABLE 1: BASELINE CHARACTERISTICS\n")
cat("GLP-1 Obesity Cohort\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading cleaned data...\n")
load("glp1_cleaned_data.RData")

cat(sprintf("Loaded: %d patients in obesity cohort\n", nrow(obesity_cohort)))
cat(sprintf("        %d activity records\n", nrow(activity_cleaned)))
cat(sprintf("        %d weight records\n", nrow(weight_cleaned)))
cat(sprintf("        %d BMI records\n\n", nrow(bmi_data)))

# =============================================================================
# DEFINE BASELINE COHORT (EXACT TABLE 2 LOGIC)
# =============================================================================

cat("========================================\n")
cat("STEP 1: Defining Baseline Cohort\n")
cat("========================================\n\n")

cat("Criteria:\n")
cat("  1. BMI ≥30 OR obesity diagnosis\n")
cat("  2. On GLP-1 therapy\n")
cat("  3. ≥3 valid baseline activity days (-180 to 0)\n")
cat("  4. ≥3 valid 1-30d follow-up activity days\n\n")

# Patients with baseline activity (at least 3 valid days in -180 to 0 period)
baseline_activity_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_baseline_days = n(), .groups = "drop")

cat(sprintf("Patients with ≥3 valid baseline activity days: %d\n",
            nrow(baseline_activity_patients)))

# Patients with 1-30d follow-up activity
followup_1_30d_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 30,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_followup_days = n(), .groups = "drop")

cat(sprintf("Patients with ≥3 valid 1-30d follow-up days: %d\n",
            nrow(followup_1_30d_patients)))

# BASELINE COHORT = intersection
baseline_cohort <- baseline_activity_patients %>%
  inner_join(followup_1_30d_patients, by = "person_id")

cat(sprintf("\n*** BASELINE COHORT: N = %d ***\n\n", nrow(baseline_cohort)))

# =============================================================================
# BASELINE ANTHROPOMETRIC DATA
# =============================================================================

cat("========================================\n")
cat("STEP 2: Baseline Anthropometrics\n")
cat("========================================\n\n")

# Baseline weight (most recent in -180 to 0 window)
baseline_weight <- weight_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  filter(days_from_initiation == max(days_from_initiation)) %>%
  slice(1) %>%
  summarize(baseline_weight_kg = first(weight_kg), .groups = "drop")

cat(sprintf("Patients with baseline weight: %d / %d (%.1f%%)\n",
            nrow(baseline_weight),
            nrow(baseline_cohort),
            100 * nrow(baseline_weight) / nrow(baseline_cohort)))

# Baseline BMI (most recent in -180 to 0 window)
baseline_bmi <- bmi_data %>%
  filter(person_id %in% baseline_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  filter(days_from_initiation == max(days_from_initiation)) %>%
  slice(1) %>%
  summarize(baseline_bmi = first(bmi), .groups = "drop")

cat(sprintf("Patients with baseline BMI: %d / %d (%.1f%%)\n\n",
            nrow(baseline_bmi),
            nrow(baseline_cohort),
            100 * nrow(baseline_bmi) / nrow(baseline_cohort)))

# =============================================================================
# BASELINE ACTIVITY DATA
# =============================================================================

cat("========================================\n")
cat("STEP 3: Baseline Activity\n")
cat("========================================\n\n")

baseline_activity <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  summarize(
    n_valid_days = n(),
    baseline_steps = mean(steps, na.rm = TRUE),
    baseline_activity_calories = mean(activity_calories, na.rm = TRUE),
    baseline_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    baseline_mvpa_min = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
    baseline_wear_time = mean(total_wear_minutes, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("Baseline activity calculated for: %d / %d patients\n\n",
            nrow(baseline_activity),
            nrow(baseline_cohort)))

# =============================================================================
# COMBINE ALL BASELINE DATA
# =============================================================================

cat("========================================\n")
cat("STEP 4: Combining Baseline Data\n")
cat("========================================\n\n")

baseline_data_combined <- baseline_cohort %>%
  select(person_id) %>%
  left_join(baseline_weight, by = "person_id") %>%
  left_join(baseline_bmi, by = "person_id") %>%
  left_join(baseline_activity, by = "person_id") %>%
  left_join(
    obesity_cohort %>% select(person_id, inclusion_reason),
    by = "person_id"
  )

cat(sprintf("Combined baseline data: N=%d\n\n", nrow(baseline_data_combined)))

# =============================================================================
# CALCULATE SUMMARY STATISTICS
# =============================================================================

cat("========================================\n")
cat("STEP 5: Summary Statistics\n")
cat("========================================\n\n")

# Overall N
total_n <- nrow(baseline_cohort)

# Anthropometrics
weight_summary <- baseline_data_combined %>%
  summarize(
    n = sum(!is.na(baseline_weight_kg)),
    mean = mean(baseline_weight_kg, na.rm = TRUE),
    sd = sd(baseline_weight_kg, na.rm = TRUE),
    median = median(baseline_weight_kg, na.rm = TRUE),
    q25 = quantile(baseline_weight_kg, 0.25, na.rm = TRUE),
    q75 = quantile(baseline_weight_kg, 0.75, na.rm = TRUE)
  )

bmi_summary <- baseline_data_combined %>%
  summarize(
    n = sum(!is.na(baseline_bmi)),
    mean = mean(baseline_bmi, na.rm = TRUE),
    sd = sd(baseline_bmi, na.rm = TRUE),
    median = median(baseline_bmi, na.rm = TRUE),
    q25 = quantile(baseline_bmi, 0.25, na.rm = TRUE),
    q75 = quantile(baseline_bmi, 0.75, na.rm = TRUE)
  )

# BMI categories
bmi_categories <- baseline_data_combined %>%
  filter(!is.na(baseline_bmi)) %>%
  mutate(
    bmi_category = case_when(
      baseline_bmi >= 40 ~ "Class III (≥40)",
      baseline_bmi >= 35 ~ "Class II (35-39.9)",
      baseline_bmi >= 30 ~ "Class I (30-34.9)",
      TRUE ~ "Other"
    )
  ) %>%
  count(bmi_category) %>%
  mutate(pct = 100 * n / sum(n))

# Inclusion reason
inclusion_summary <- baseline_data_combined %>%
  count(inclusion_reason) %>%
  mutate(pct = 100 * n / sum(n))

# Activity metrics
steps_summary <- baseline_data_combined %>%
  summarize(
    n = sum(!is.na(baseline_steps)),
    mean = mean(baseline_steps, na.rm = TRUE),
    sd = sd(baseline_steps, na.rm = TRUE),
    median = median(baseline_steps, na.rm = TRUE),
    q25 = quantile(baseline_steps, 0.25, na.rm = TRUE),
    q75 = quantile(baseline_steps, 0.75, na.rm = TRUE)
  )

mvpa_summary <- baseline_data_combined %>%
  summarize(
    n = sum(!is.na(baseline_mvpa_min)),
    mean = mean(baseline_mvpa_min, na.rm = TRUE),
    sd = sd(baseline_mvpa_min, na.rm = TRUE),
    median = median(baseline_mvpa_min, na.rm = TRUE),
    q25 = quantile(baseline_mvpa_min, 0.25, na.rm = TRUE),
    q75 = quantile(baseline_mvpa_min, 0.75, na.rm = TRUE)
  )

sedentary_summary <- baseline_data_combined %>%
  summarize(
    n = sum(!is.na(baseline_sedentary_min)),
    mean = mean(baseline_sedentary_min, na.rm = TRUE),
    sd = sd(baseline_sedentary_min, na.rm = TRUE),
    median = median(baseline_sedentary_min, na.rm = TRUE),
    q25 = quantile(baseline_sedentary_min, 0.25, na.rm = TRUE),
    q75 = quantile(baseline_sedentary_min, 0.75, na.rm = TRUE)
  )

calories_summary <- baseline_data_combined %>%
  summarize(
    n = sum(!is.na(baseline_activity_calories)),
    mean = mean(baseline_activity_calories, na.rm = TRUE),
    sd = sd(baseline_activity_calories, na.rm = TRUE),
    median = median(baseline_activity_calories, na.rm = TRUE),
    q25 = quantile(baseline_activity_calories, 0.25, na.rm = TRUE),
    q75 = quantile(baseline_activity_calories, 0.75, na.rm = TRUE)
  )

valid_days_summary <- baseline_data_combined %>%
  summarize(
    n = sum(!is.na(n_valid_days)),
    mean = mean(n_valid_days, na.rm = TRUE),
    sd = sd(n_valid_days, na.rm = TRUE),
    median = median(n_valid_days, na.rm = TRUE),
    q25 = quantile(n_valid_days, 0.25, na.rm = TRUE),
    q75 = quantile(n_valid_days, 0.75, na.rm = TRUE)
  )

# =============================================================================
# CREATE TABLE 1
# =============================================================================

cat("========================================\n")
cat("STEP 6: Creating Table 1\n")
cat("========================================\n\n")

# Build table
table1 <- tibble(
  Characteristic = c(
    "Total N",
    "",
    "Obesity Inclusion Criteria",
    paste0("  ", inclusion_summary$inclusion_reason),
    "",
    "Anthropometrics",
    "  Weight (kg)",
    "  BMI (kg/m²)",
    "",
    "BMI Categories",
    paste0("  ", bmi_categories$bmi_category),
    "",
    "Baseline Activity Metrics",
    "  Valid days with activity data",
    "  Steps per day",
    "  MVPA (min/day)",
    "  Sedentary time (min/day)",
    "  Activity calories (kcal/day)"
  ),
  `Value (Mean ± SD or N (%))` = c(
    as.character(total_n),
    "",
    "",
    sprintf("%d (%.1f%%)", inclusion_summary$n, inclusion_summary$pct),
    "",
    "",
    sprintf("%.1f ± %.1f", weight_summary$mean, weight_summary$sd),
    sprintf("%.1f ± %.1f", bmi_summary$mean, bmi_summary$sd),
    "",
    "",
    sprintf("%d (%.1f%%)", bmi_categories$n, bmi_categories$pct),
    "",
    "",
    sprintf("%.1f ± %.1f", valid_days_summary$mean, valid_days_summary$sd),
    sprintf("%.0f ± %.0f", steps_summary$mean, steps_summary$sd),
    sprintf("%.1f ± %.1f", mvpa_summary$mean, mvpa_summary$sd),
    sprintf("%.1f ± %.1f", sedentary_summary$mean, sedentary_summary$sd),
    sprintf("%.0f ± %.0f", calories_summary$mean, calories_summary$sd)
  )
)

print(table1)
cat("\n")

# =============================================================================
# SAVE TABLE 1
# =============================================================================

cat("========================================\n")
cat("STEP 7: Saving Table 1\n")
cat("========================================\n\n")

# Save CSV
write_csv(table1, "table1_baseline_characteristics.csv")
cat("Saved: table1_baseline_characteristics.csv\n")

# Save HTML version
table1_html <- table1 %>%
  kable(format = "html", escape = FALSE, align = c("l", "c")) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  column_spec(1, bold = TRUE, width = "5cm") %>%
  column_spec(2, width = "4cm") %>%
  footnote(
    general = c(
      paste0("Total N = ", total_n, " patients in baseline cohort."),
      "Baseline period: 180 days before GLP-1 initiation.",
      "Inclusion criteria: BMI ≥30 OR obesity diagnosis, ≥3 valid baseline activity days, ≥3 valid 1-30d follow-up days.",
      "Valid day: Fitbit day with ≥10 hours wear time.",
      "MVPA: Moderate-to-vigorous physical activity (fairly active + very active minutes).",
      "Values shown as mean ± SD for continuous variables, N (%) for categorical variables."
    ),
    general_title = "Notes:",
    footnote_as_chunk = TRUE
  )

writeLines(as.character(table1_html), "table1_baseline_characteristics.html")
cat("Saved: table1_baseline_characteristics.html\n")

# =============================================================================
# DETAILED BASELINE DATA (FOR ADDITIONAL ANALYSIS)
# =============================================================================

cat("\n========================================\n")
cat("STEP 8: Saving Detailed Baseline Data\n")
cat("========================================\n\n")

# Save patient-level baseline data
write_csv(baseline_data_combined, "baseline_data_patient_level.csv")
cat("Saved: baseline_data_patient_level.csv\n")

cat("\nThis file contains:\n")
cat("  - person_id\n")
cat("  - baseline_weight_kg\n")
cat("  - baseline_bmi\n")
cat("  - baseline_steps\n")
cat("  - baseline_activity_calories\n")
cat("  - baseline_sedentary_min\n")
cat("  - baseline_mvpa_min\n")
cat("  - baseline_wear_time\n")
cat("  - n_valid_days\n")
cat("  - inclusion_reason\n\n")

# =============================================================================
# MISSING DATA SUMMARY
# =============================================================================

cat("========================================\n")
cat("Missing Data Summary\n")
cat("========================================\n\n")

missing_summary <- tibble(
  Variable = c("Weight", "BMI", "Steps", "MVPA", "Sedentary", "Activity Calories"),
  `N Available` = c(
    weight_summary$n,
    bmi_summary$n,
    steps_summary$n,
    mvpa_summary$n,
    sedentary_summary$n,
    calories_summary$n
  ),
  `N Missing` = total_n - c(
    weight_summary$n,
    bmi_summary$n,
    steps_summary$n,
    mvpa_summary$n,
    sedentary_summary$n,
    calories_summary$n
  ),
  `% Missing` = 100 * (total_n - c(
    weight_summary$n,
    bmi_summary$n,
    steps_summary$n,
    mvpa_summary$n,
    sedentary_summary$n,
    calories_summary$n
  )) / total_n
)

print(missing_summary)
cat("\n")

write_csv(missing_summary, "table1_missing_data_summary.csv")
cat("Saved: table1_missing_data_summary.csv\n")

# =============================================================================
# NOTES FOR DEMOGRAPHICS
# =============================================================================

cat("\n========================================\n")
cat("NOTES: Demographics Not Yet Included\n")
cat("========================================\n\n")

cat("The following demographics are NOT yet in Table 1:\n")
cat("  - Age\n")
cat("  - Sex/Gender\n")
cat("  - Race/Ethnicity\n")
cat("  - Comorbidities (diabetes, hypertension, etc.)\n\n")

cat("To add demographics:\n")
cat("  1. Run pull_demographics_diagnoses.R in All of Us Workbench\n")
cat("  2. This will query the person and condition_occurrence tables\n")
cat("  3. Save the demographics data to an RData file\n")
cat("  4. Update this script to merge demographics with baseline_cohort\n\n")

cat("##################################################\n")
cat("TABLE 1 COMPLETE\n")
cat("##################################################\n\n")

cat(sprintf("Baseline cohort: N = %d\n", total_n))
cat(sprintf("Data completeness:\n"))
cat(sprintf("  Weight: %.1f%%\n", 100 * weight_summary$n / total_n))
cat(sprintf("  BMI: %.1f%%\n", 100 * bmi_summary$n / total_n))
cat(sprintf("  Activity: 100%%\n"))
