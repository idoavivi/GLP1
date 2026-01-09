# =============================================================================
# PRIMARY ANALYSIS: BASELINE CHARACTERISTICS AND LONGITUDINAL OUTCOMES
# =============================================================================
# Table 1: Baseline characteristics of cohort with 1-30d follow-up
# Table 2: Longitudinal outcomes (baseline through nadir) with p-values
# =============================================================================

library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

cat("\n##################################################\n")
cat("PRIMARY ANALYSIS\n")
cat("GLP-1 Activity & Weight Outcomes\n")
cat("##################################################\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
load("glp1_cleaned_data.RData")

# Prepare data - datasets already have days_from_initiation
cat("Preparing datasets...\n")

activity_all <- activity_cleaned
weight_all <- weight_cleaned
bmi_measured <- bmi_data

# =============================================================================
# EXCLUDE PATIENTS WITH BASELINE BMI < 30
# =============================================================================

cat("\n========================================\n")
cat("Filtering: Exclude BMI < 30 patients\n")
cat("========================================\n\n")

baseline_bmi_check <- bmi_measured %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop")

patients_to_exclude <- baseline_bmi_check %>%
  filter(baseline_bmi < 30) %>%
  pull(person_id)

cat(sprintf("Patients with baseline BMI < 30: %d\n", length(patients_to_exclude)))

if(length(patients_to_exclude) > 0) {
  cat("Excluding these patients from all analyses...\n")

  activity_all <- activity_all %>% filter(!person_id %in% patients_to_exclude)
  weight_all <- weight_all %>% filter(!person_id %in% patients_to_exclude)
  bmi_measured <- bmi_measured %>% filter(!person_id %in% patients_to_exclude)
  drug_glp1_clean <- drug_glp1_clean %>% filter(!person_id %in% patients_to_exclude)

  cat(sprintf("Remaining patients: %d\n", length(unique(activity_all$person_id))))
}

# =============================================================================
# DEFINE BASELINE COHORT: Patients with baseline AND 1-30d follow-up
# =============================================================================

cat("\n========================================\n")
cat("Defining Baseline Cohort\n")
cat("========================================\n\n")

# Patients with baseline activity (at least 3 valid days in -180 to 0 period)
baseline_activity_patients <- activity_all %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_baseline_days = n(), .groups = "drop")

cat(sprintf("Patients with ≥3 valid baseline activity days: %d\n",
            nrow(baseline_activity_patients)))

# Patients with 1-30d follow-up activity
followup_activity_patients <- activity_all %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 30,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_followup_days = n(), .groups = "drop")

cat(sprintf("Patients with ≥3 valid 1-30d follow-up days: %d\n",
            nrow(followup_activity_patients)))

# Baseline cohort = patients with BOTH
baseline_cohort <- baseline_activity_patients %>%
  inner_join(followup_activity_patients, by = "person_id")

cat(sprintf("\nBaseline cohort (with baseline AND 1-30d follow-up): %d patients\n\n",
            nrow(baseline_cohort)))

# Filter all datasets to baseline cohort
activity_all <- activity_all %>%
  filter(person_id %in% baseline_cohort$person_id)

weight_all <- weight_all %>%
  filter(person_id %in% baseline_cohort$person_id)

bmi_measured <- bmi_measured %>%
  filter(person_id %in% baseline_cohort$person_id)

drug_glp1_clean <- drug_glp1_clean %>%
  filter(person_id %in% baseline_cohort$person_id)

# =============================================================================
# TABLE 1: BASELINE CHARACTERISTICS
# =============================================================================

cat("\n========================================\n")
cat("TABLE 1: Baseline Characteristics\n")
cat("========================================\n\n")

# Calculate baseline weight
baseline_weight <- weight_all %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop")

# Calculate baseline BMI
baseline_bmi <- bmi_measured %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(
    baseline_bmi = mean(bmi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    bmi_category = case_when(
      baseline_bmi >= 40 ~ "Class III (≥40)",
      baseline_bmi >= 35 ~ "Class II (35-39.9)",
      baseline_bmi >= 30 ~ "Class I (30-34.9)",
      TRUE ~ "Other"
    )
  )

# Calculate baseline activity metrics
baseline_activity <- activity_all %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  summarize(
    baseline_steps = mean(steps, na.rm = TRUE),
    baseline_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    baseline_mvpa = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
    baseline_activity_cal = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  )

# Count prescription fills per patient
prescription_fills <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(n_prescriptions = n(), .groups = "drop")

# Calculate demographics from person table if available
# Check if person exists and is a data frame (not a function)
if(exists("person") && is.data.frame(person)) {
  demographics <- person %>%
    inner_join(glp1_initiation, by = "person_id") %>%
    mutate(
      age_at_initiation = year(glp1_initiation_date) - year_of_birth,
      sex = case_when(
        sex_at_birth_concept_id == 45878463 ~ "Female",
        sex_at_birth_concept_id == 45880669 ~ "Male",
        TRUE ~ "Other/Unknown"
      ),
      race = case_when(
        race_concept_id == 8527 ~ "White",
        race_concept_id == 8516 ~ "Black or African American",
        race_concept_id == 8515 ~ "Asian",
        race_concept_id == 8657 ~ "American Indian or Alaska Native",
        race_concept_id == 8557 ~ "Native Hawaiian or Other Pacific Islander",
        TRUE ~ "Other/Unknown"
      ),
      ethnicity = case_when(
        ethnicity_concept_id == 38003563 ~ "Hispanic or Latino",
        ethnicity_concept_id == 38003564 ~ "Not Hispanic or Latino",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(person_id, age_at_initiation, sex, race, ethnicity)
} else {
  cat("Note: Demographics (age, sex, race, ethnicity) not available in RData file.\n")
  cat("To include demographics, load person table from BigQuery.\n\n")
  demographics <- tibble(person_id = baseline_cohort$person_id)
}

# Get diagnoses from obesity_cohort if available
if(exists("obesity_cohort") && is.data.frame(obesity_cohort)) {
  # Extract diagnoses from obesity_cohort
  diagnoses <- obesity_cohort %>%
    filter(person_id %in% baseline_cohort$person_id) %>%
    select(person_id, contains("has_")) %>%
    distinct()

  # If specific diagnosis columns don't exist, create them as NA
  if(!"has_hypertension" %in% names(diagnoses)) diagnoses$has_hypertension <- NA
  if(!"has_diabetes" %in% names(diagnoses)) diagnoses$has_diabetes <- NA
  if(!"has_dyslipidemia" %in% names(diagnoses)) diagnoses$has_dyslipidemia <- NA
  if(!"has_ihd" %in% names(diagnoses)) diagnoses$has_ihd <- NA
  if(!"has_stroke" %in% names(diagnoses)) diagnoses$has_stroke <- NA
  if(!"has_osteoarthritis" %in% names(diagnoses)) diagnoses$has_osteoarthritis <- NA
} else {
  cat("Note: Diagnoses not available in RData file.\n")
  cat("Creating placeholder columns for HTN, DM, dyslipidemia, IHD, CVA, OA.\n\n")
  diagnoses <- baseline_cohort %>%
    select(person_id) %>%
    mutate(
      has_hypertension = NA,
      has_diabetes = NA,
      has_dyslipidemia = NA,
      has_ihd = NA,
      has_stroke = NA,
      has_osteoarthritis = NA
    )
}

# Calculate height if available
if(exists("height_clean") && is.data.frame(height_clean)) {
  height_data <- height_clean
} else {
  cat("Note: Height data not in RData. Calculating from weight and BMI...\n")
  height_data <- baseline_weight %>%
    inner_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
    mutate(
      height_cm = 100 * sqrt(baseline_weight_kg / baseline_bmi)
    ) %>%
    filter(height_cm >= 150, height_cm <= 220) %>%
    select(person_id, height_cm)
}

# Combine all baseline characteristics
table1_data <- baseline_cohort %>%
  left_join(demographics, by = "person_id") %>%
  left_join(diagnoses, by = "person_id") %>%
  left_join(baseline_weight, by = "person_id") %>%
  left_join(height_data, by = "person_id") %>%
  left_join(baseline_bmi, by = "person_id") %>%
  left_join(baseline_activity, by = "person_id") %>%
  left_join(prescription_fills, by = "person_id")

# Create Table 1 summary
table1_characteristics <- c(
  "N",
  if("age_at_initiation" %in% names(table1_data)) "Age, years" else NULL,
  if("sex" %in% names(table1_data)) "Sex, Female (%)" else NULL,
  if("race" %in% names(table1_data)) c("Race", "  White (%)", "  Black or African American (%)",
                                        "  Asian (%)", "  Other (%)") else NULL,
  if("ethnicity" %in% names(table1_data)) c("Ethnicity", "  Hispanic or Latino (%)") else NULL,
  "Weight, kg",
  "Height, cm",
  "BMI, kg/m²",
  "BMI Category",
  "  Class I (30-34.9), n (%)",
  "  Class II (35-39.9), n (%)",
  "  Class III (≥40), n (%)",
  "GLP-1 prescriptions, n",
  if("has_hypertension" %in% names(table1_data)) "Comorbidities" else NULL,
  if("has_hypertension" %in% names(table1_data)) "  Hypertension, n (%)" else NULL,
  if("has_diabetes" %in% names(table1_data)) "  Diabetes, n (%)" else NULL,
  if("has_dyslipidemia" %in% names(table1_data)) "  Dyslipidemia, n (%)" else NULL,
  if("has_ihd" %in% names(table1_data)) "  Ischemic Heart Disease, n (%)" else NULL,
  if("has_stroke" %in% names(table1_data)) "  Cerebrovascular Accident, n (%)" else NULL,
  if("has_osteoarthritis" %in% names(table1_data)) "  Osteoarthritis, n (%)" else NULL,
  "Baseline Activity",
  "  Steps per day",
  "  Sedentary minutes per day",
  "  MVPA minutes per day",
  "  Activity calories per day"
)

table1_values <- c(
  as.character(nrow(table1_data)),
  if("age_at_initiation" %in% names(table1_data))
    sprintf("%.1f ± %.1f", mean(table1_data$age_at_initiation, na.rm = TRUE),
            sd(table1_data$age_at_initiation, na.rm = TRUE)) else NULL,
  if("sex" %in% names(table1_data))
    sprintf("%d (%.1f%%)", sum(table1_data$sex == "Female", na.rm = TRUE),
            100 * mean(table1_data$sex == "Female", na.rm = TRUE)) else NULL,
  if("race" %in% names(table1_data)) c(
    "",
    sprintf("%d (%.1f%%)", sum(table1_data$race == "White", na.rm = TRUE),
            100 * mean(table1_data$race == "White", na.rm = TRUE)),
    sprintf("%d (%.1f%%)", sum(table1_data$race == "Black or African American", na.rm = TRUE),
            100 * mean(table1_data$race == "Black or African American", na.rm = TRUE)),
    sprintf("%d (%.1f%%)", sum(table1_data$race == "Asian", na.rm = TRUE),
            100 * mean(table1_data$race == "Asian", na.rm = TRUE)),
    sprintf("%d (%.1f%%)", sum(!table1_data$race %in% c("White", "Black or African American", "Asian"), na.rm = TRUE),
            100 * mean(!table1_data$race %in% c("White", "Black or African American", "Asian"), na.rm = TRUE))
  ) else NULL,
  if("ethnicity" %in% names(table1_data)) c(
    "",
    sprintf("%d (%.1f%%)", sum(table1_data$ethnicity == "Hispanic or Latino", na.rm = TRUE),
            100 * mean(table1_data$ethnicity == "Hispanic or Latino", na.rm = TRUE))
  ) else NULL,
  sprintf("%.1f ± %.1f", mean(table1_data$baseline_weight_kg, na.rm = TRUE),
          sd(table1_data$baseline_weight_kg, na.rm = TRUE)),
  sprintf("%.1f ± %.1f", mean(table1_data$height_cm, na.rm = TRUE),
          sd(table1_data$height_cm, na.rm = TRUE)),
  sprintf("%.1f ± %.1f", mean(table1_data$baseline_bmi, na.rm = TRUE),
          sd(table1_data$baseline_bmi, na.rm = TRUE)),
  "",
  sprintf("%d (%.1f%%)",
          sum(table1_data$bmi_category == "Class I (30-34.9)", na.rm = TRUE),
          100 * mean(table1_data$bmi_category == "Class I (30-34.9)", na.rm = TRUE)),
  sprintf("%d (%.1f%%)",
          sum(table1_data$bmi_category == "Class II (35-39.9)", na.rm = TRUE),
          100 * mean(table1_data$bmi_category == "Class II (35-39.9)", na.rm = TRUE)),
  sprintf("%d (%.1f%%)",
          sum(table1_data$bmi_category == "Class III (≥40)", na.rm = TRUE),
          100 * mean(table1_data$bmi_category == "Class III (≥40)", na.rm = TRUE)),
  sprintf("%.1f ± %.1f", mean(table1_data$n_prescriptions, na.rm = TRUE),
          sd(table1_data$n_prescriptions, na.rm = TRUE)),
  if("has_hypertension" %in% names(table1_data)) "" else NULL,
  if("has_hypertension" %in% names(table1_data))
    sprintf("%d (%.1f%%)", sum(table1_data$has_hypertension == TRUE, na.rm = TRUE),
            100 * mean(table1_data$has_hypertension == TRUE, na.rm = TRUE)) else NULL,
  if("has_diabetes" %in% names(table1_data))
    sprintf("%d (%.1f%%)", sum(table1_data$has_diabetes == TRUE, na.rm = TRUE),
            100 * mean(table1_data$has_diabetes == TRUE, na.rm = TRUE)) else NULL,
  if("has_dyslipidemia" %in% names(table1_data))
    sprintf("%d (%.1f%%)", sum(table1_data$has_dyslipidemia == TRUE, na.rm = TRUE),
            100 * mean(table1_data$has_dyslipidemia == TRUE, na.rm = TRUE)) else NULL,
  if("has_ihd" %in% names(table1_data))
    sprintf("%d (%.1f%%)", sum(table1_data$has_ihd == TRUE, na.rm = TRUE),
            100 * mean(table1_data$has_ihd == TRUE, na.rm = TRUE)) else NULL,
  if("has_stroke" %in% names(table1_data))
    sprintf("%d (%.1f%%)", sum(table1_data$has_stroke == TRUE, na.rm = TRUE),
            100 * mean(table1_data$has_stroke == TRUE, na.rm = TRUE)) else NULL,
  if("has_osteoarthritis" %in% names(table1_data))
    sprintf("%d (%.1f%%)", sum(table1_data$has_osteoarthritis == TRUE, na.rm = TRUE),
            100 * mean(table1_data$has_osteoarthritis == TRUE, na.rm = TRUE)) else NULL,
  "",
  sprintf("%.0f ± %.0f", mean(table1_data$baseline_steps, na.rm = TRUE),
          sd(table1_data$baseline_steps, na.rm = TRUE)),
  sprintf("%.0f ± %.0f", mean(table1_data$baseline_sedentary_min, na.rm = TRUE),
          sd(table1_data$baseline_sedentary_min, na.rm = TRUE)),
  sprintf("%.0f ± %.0f", mean(table1_data$baseline_mvpa, na.rm = TRUE),
          sd(table1_data$baseline_mvpa, na.rm = TRUE)),
  sprintf("%.0f ± %.0f", mean(table1_data$baseline_activity_cal, na.rm = TRUE),
          sd(table1_data$baseline_activity_cal, na.rm = TRUE))
)

table1_summary <- tibble(
  Characteristic = table1_characteristics,
  `Mean ± SD or n (%)` = table1_values
) %>%
  filter(!is.na(Characteristic))

print(table1_summary)

write_csv(table1_summary, "table1_baseline_characteristics.csv")
cat("\nSaved: table1_baseline_characteristics.csv\n")

# =============================================================================
# TABLE 2: LONGITUDINAL OUTCOMES
# =============================================================================

cat("\n========================================\n")
cat("TABLE 2: Longitudinal Outcomes\n")
cat("========================================\n\n")

# Define time periods
time_periods <- tribble(
  ~period, ~days_min, ~days_max,
  "Baseline", -180, 0,
  "1-30 days", 1, 30,
  "31-90 days", 31, 90,
  "91-180 days", 91, 180,
  "181-365 days", 181, 365
)

# Calculate weight at each period
weight_by_period <- time_periods %>%
  rowwise() %>%
  mutate(
    weight_data = list({
      weight_all %>%
        filter(days_from_initiation >= days_min,
               days_from_initiation <= days_max) %>%
        group_by(person_id) %>%
        summarize(weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop")
    })
  ) %>%
  ungroup() %>%
  mutate(
    n = map_int(weight_data, nrow),
    mean_weight = map_dbl(weight_data, ~mean(.x$weight_kg, na.rm = TRUE)),
    sd_weight = map_dbl(weight_data, ~sd(.x$weight_kg, na.rm = TRUE))
  )

# Calculate activity at each period
activity_by_period <- time_periods %>%
  rowwise() %>%
  mutate(
    activity_data = list({
      activity_all %>%
        filter(days_from_initiation >= days_min,
               days_from_initiation <= days_max,
               is_valid_day == TRUE) %>%
        group_by(person_id) %>%
        filter(n() >= 3) %>%
        summarize(
          steps = mean(steps, na.rm = TRUE),
          sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
          mvpa = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
          activity_cal = mean(activity_calories, na.rm = TRUE),
          .groups = "drop"
        )
    })
  ) %>%
  ungroup() %>%
  mutate(
    n = map_int(activity_data, nrow),
    mean_steps = map_dbl(activity_data, ~mean(.x$steps, na.rm = TRUE)),
    sd_steps = map_dbl(activity_data, ~sd(.x$steps, na.rm = TRUE)),
    mean_sedentary = map_dbl(activity_data, ~mean(.x$sedentary_min, na.rm = TRUE)),
    sd_sedentary = map_dbl(activity_data, ~sd(.x$sedentary_min, na.rm = TRUE)),
    mean_mvpa = map_dbl(activity_data, ~mean(.x$mvpa, na.rm = TRUE)),
    sd_mvpa = map_dbl(activity_data, ~sd(.x$mvpa, na.rm = TRUE)),
    mean_calories = map_dbl(activity_data, ~mean(.x$activity_cal, na.rm = TRUE)),
    sd_calories = map_dbl(activity_data, ~sd(.x$activity_cal, na.rm = TRUE))
  )

# Calculate nadir weight (minimum after 12 weeks)
nadir_weight_data <- weight_all %>%
  filter(days_from_initiation > 84) %>%
  group_by(person_id) %>%
  summarize(
    nadir_weight = min(weight_kg, na.rm = TRUE),
    .groups = "drop"
  )

nadir_weight_summary <- tibble(
  period = "Nadir (>12 weeks)",
  n = nrow(nadir_weight_data),
  mean_weight = mean(nadir_weight_data$nadir_weight, na.rm = TRUE),
  sd_weight = sd(nadir_weight_data$nadir_weight, na.rm = TRUE)
)

# =============================================================================
# MIXED EFFECTS MODELS FOR LONGITUDINAL OUTCOMES (used for p-values in Table 2)
# =============================================================================

cat("\n========================================\n")
cat("Running Mixed Effects Models\n")
cat("========================================\n\n")

# Prepare longitudinal data with period_numeric for mixed effects
period_numeric_map <- tribble(
  ~period, ~period_numeric,
  "Baseline", 0,
  "1-30 days", 15,
  "31-90 days", 60,
  "91-180 days", 135,
  "181-365 days", 273
)

# Weight longitudinal data for mixed effects
weight_long <- weight_by_period %>%
  select(period, weight_data) %>%
  unnest(weight_data) %>%
  inner_join(period_numeric_map, by = "period")

cat(sprintf("Weight: %d observations from %d patients\n",
            nrow(weight_long), n_distinct(weight_long$person_id)))

# Fit weight mixed effects model
if(n_distinct(weight_long$person_id) >= 3 && nrow(weight_long) >= 10) {
  model_weight <- lmer(weight_kg ~ period_numeric + (1 | person_id), data = weight_long)
  p_value_weight <- summary(model_weight)$coefficients["period_numeric", "Pr(>|t|)"]
  cat(sprintf("  Weight p-value: %.4f\n", p_value_weight))
} else {
  p_value_weight <- NA_real_
  cat("  Insufficient data for weight model\n")
}

# Activity longitudinal data for mixed effects
activity_long <- activity_by_period %>%
  select(period, activity_data) %>%
  unnest(activity_data) %>%
  inner_join(period_numeric_map, by = "period")

cat(sprintf("Activity: %d observations from %d patients\n",
            nrow(activity_long), n_distinct(activity_long$person_id)))

# Fit activity mixed effects models
if(n_distinct(activity_long$person_id) >= 3 && nrow(activity_long) >= 10) {
  model_steps <- lmer(steps ~ period_numeric + (1 | person_id), data = activity_long)
  p_value_steps <- summary(model_steps)$coefficients["period_numeric", "Pr(>|t|)"]

  model_mvpa <- lmer(mvpa ~ period_numeric + (1 | person_id), data = activity_long)
  p_value_mvpa <- summary(model_mvpa)$coefficients["period_numeric", "Pr(>|t|)"]

  model_calories <- lmer(activity_cal ~ period_numeric + (1 | person_id), data = activity_long)
  p_value_calories <- summary(model_calories)$coefficients["period_numeric", "Pr(>|t|)"]

  cat(sprintf("  Steps p-value: %.4f\n", p_value_steps))
  cat(sprintf("  MVPA p-value: %.4f\n", p_value_mvpa))
  cat(sprintf("  Calories p-value: %.4f\n", p_value_calories))
} else {
  p_value_steps <- p_value_mvpa <- p_value_calories <- NA_real_
  cat("  Insufficient data for activity models\n")
}

# Combine weight summary with mixed effects p-value
weight_summary <- weight_by_period %>%
  select(period, n, mean_weight, sd_weight) %>%
  bind_rows(nadir_weight_summary)

# Combine activity summary
activity_summary <- activity_by_period %>%
  select(period, n, mean_steps, sd_steps, mean_sedentary, sd_sedentary,
         mean_mvpa, sd_mvpa, mean_calories, sd_calories)

# Format Table 2
cat("\n========================================\n")
cat("TABLE 2: Longitudinal Outcomes\n")
cat("========================================\n\n")

cat("Weight Outcomes:\n")
weight_table <- weight_summary %>%
  mutate(
    `Mean ± SD` = sprintf("%.1f ± %.1f", mean_weight, sd_weight),
    `N` = as.character(n)
  ) %>%
  select(Period = period, N, `Mean ± SD`)

# Add note about mixed effects p-value
weight_table_with_note <- weight_table %>%
  add_row(
    Period = sprintf("P-value for linear trend (mixed effects): %.4f",
                    if_else(is.na(p_value_weight), 0, p_value_weight)),
    N = "",
    `Mean ± SD` = ""
  )

print(weight_table_with_note)

cat("\nActivity Outcomes:\n")

# Steps
steps_table <- activity_summary %>%
  mutate(
    Metric = "Steps per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_steps, sd_steps),
    `N` = as.character(n)
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`)

# MVPA
mvpa_table <- activity_summary %>%
  mutate(
    Metric = "MVPA minutes per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_mvpa, sd_mvpa),
    `N` = as.character(n)
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`)

# Sedentary
sedentary_table <- activity_summary %>%
  mutate(
    Metric = "Sedentary minutes per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_sedentary, sd_sedentary),
    `N` = as.character(n)
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`)

# Calories
calories_table <- activity_summary %>%
  mutate(
    Metric = "Activity calories per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_calories, sd_calories),
    `N` = as.character(n)
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`)

activity_table <- bind_rows(steps_table, mvpa_table, sedentary_table, calories_table)

# Add p-value notes
activity_table_with_notes <- activity_table %>%
  add_row(
    Period = "",
    Metric = sprintf("P-values for linear trends (mixed effects):"),
    N = "",
    `Mean ± SD` = ""
  ) %>%
  add_row(
    Period = "",
    Metric = sprintf("  Steps: p=%.4f", if_else(is.na(p_value_steps), 0, p_value_steps)),
    N = "",
    `Mean ± SD` = ""
  ) %>%
  add_row(
    Period = "",
    Metric = sprintf("  MVPA: p=%.4f", if_else(is.na(p_value_mvpa), 0, p_value_mvpa)),
    N = "",
    `Mean ± SD` = ""
  ) %>%
  add_row(
    Period = "",
    Metric = sprintf("  Activity Calories: p=%.4f", if_else(is.na(p_value_calories), 0, p_value_calories)),
    N = "",
    `Mean ± SD` = ""
  )

print(activity_table_with_notes)

# Save Table 2
write_csv(weight_table, "table2_weight_outcomes.csv")
write_csv(activity_table, "table2_activity_outcomes.csv")

cat("\nSaved: table2_weight_outcomes.csv\n")
cat("Saved: table2_activity_outcomes.csv\n")

# =============================================================================
# TABLE 3: DETAILED MIXED EFFECTS RESULTS
# =============================================================================

cat("\n========================================\n")
cat("TABLE 3: Mixed Effects Model Details\n")
cat("========================================\n\n")

# Extract coefficients and calculate changes
if(exists("model_weight")) {
  coef_weight <- fixef(model_weight)["period_numeric"]
  baseline_weight_me <- fixef(model_weight)["(Intercept)"]
  change_1year_weight <- coef_weight * 273
  pct_change_weight <- (change_1year_weight / baseline_weight_me) * 100
} else {
  coef_weight <- baseline_weight_me <- change_1year_weight <- pct_change_weight <- NA_real_
}

if(exists("model_steps")) {
  coef_steps <- fixef(model_steps)["period_numeric"]
  baseline_steps_me <- fixef(model_steps)["(Intercept)"]
  change_1year_steps <- coef_steps * 273
  pct_change_steps <- (change_1year_steps / baseline_steps_me) * 100
} else {
  coef_steps <- baseline_steps_me <- change_1year_steps <- pct_change_steps <- NA_real_
}

if(exists("model_mvpa")) {
  coef_mvpa <- fixef(model_mvpa)["period_numeric"]
  baseline_mvpa_me <- fixef(model_mvpa)["(Intercept)"]
  change_1year_mvpa <- coef_mvpa * 273
  pct_change_mvpa <- (change_1year_mvpa / baseline_mvpa_me) * 100
} else {
  coef_mvpa <- baseline_mvpa_me <- change_1year_mvpa <- pct_change_mvpa <- NA_real_
}

if(exists("model_calories")) {
  coef_cal <- fixef(model_calories)["period_numeric"]
  baseline_cal_me <- fixef(model_calories)["(Intercept)"]
  change_1year_cal <- coef_cal * 273
  pct_change_cal <- (change_1year_cal / baseline_cal_me) * 100
} else {
  coef_cal <- baseline_cal_me <- change_1year_cal <- pct_change_cal <- NA_real_
}

# Create Table 3
mixed_effects_summary <- tibble(
  Outcome = c("Weight (kg)", "Steps (per day)", "MVPA (min/day)", "Activity Calories (kcal/day)"),
  `Baseline (intercept)` = c(
    sprintf("%.1f", baseline_weight_me),
    sprintf("%.0f", baseline_steps_me),
    sprintf("%.1f", baseline_mvpa_me),
    sprintf("%.0f", baseline_cal_me)
  ),
  `Change per day (slope)` = c(
    sprintf("%.3f", coef_weight),
    sprintf("%.2f", coef_steps),
    sprintf("%.3f", coef_mvpa),
    sprintf("%.2f", coef_cal)
  ),
  `Change at 1 year` = c(
    sprintf("%.2f", change_1year_weight),
    sprintf("%.0f", change_1year_steps),
    sprintf("%.1f", change_1year_mvpa),
    sprintf("%.0f", change_1year_cal)
  ),
  `Percent change at 1 year` = c(
    sprintf("%.1f%%", pct_change_weight),
    sprintf("%.1f%%", pct_change_steps),
    sprintf("%.1f%%", pct_change_mvpa),
    sprintf("%.1f%%", pct_change_cal)
  ),
  `P-value` = c(
    sprintf("%.4f", p_value_weight),
    sprintf("%.4f", p_value_steps),
    sprintf("%.4f", p_value_mvpa),
    sprintf("%.4f", p_value_calories)
  )
)

print(mixed_effects_summary)

write_csv(mixed_effects_summary, "table3_mixed_effects_models.csv")
cat("Saved: table3_mixed_effects_models.csv\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("PRIMARY ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat("Cohort: Patients with baseline (-180 to 0d) AND 1-30d follow-up\n")
cat(sprintf("  N = %d patients\n\n", nrow(baseline_cohort)))

cat("Generated files:\n")
cat("  - table1_baseline_characteristics.csv\n")
cat("  - table2_weight_outcomes.csv\n")
cat("  - table2_activity_outcomes.csv\n")
cat("  - table3_mixed_effects_models.csv\n\n")
