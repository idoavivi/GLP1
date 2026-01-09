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

# Calculate age from glp1_initiation and birth year if available
# Note: If person table not available, this section will be skipped
if(exists("person")) {
  demographics <- person %>%
    inner_join(glp1_initiation, by = "person_id") %>%
    mutate(
      age_at_initiation = year(glp1_initiation_date) - year_of_birth,
      sex = case_when(
        sex_at_birth_concept_id == 45878463 ~ "Female",
        sex_at_birth_concept_id == 45880669 ~ "Male",
        TRUE ~ "Other/Unknown"
      )
    ) %>%
    select(person_id, age_at_initiation, sex)
} else {
  cat("Note: Demographics (age, sex) not available in RData file.\n")
  cat("To include demographics, load person table from BigQuery.\n\n")
  demographics <- tibble(person_id = baseline_cohort$person_id)
}

# Calculate height if available
if(exists("height_clean")) {
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
  left_join(baseline_weight, by = "person_id") %>%
  left_join(height_data, by = "person_id") %>%
  left_join(baseline_bmi, by = "person_id") %>%
  left_join(baseline_activity, by = "person_id") %>%
  left_join(prescription_fills, by = "person_id")

# Create Table 1 summary
table1_summary <- tibble(
  Characteristic = c(
    "N",
    if("age_at_initiation" %in% names(table1_data)) "Age, years" else NULL,
    if("sex" %in% names(table1_data)) "Sex, Female (%)" else NULL,
    "Weight, kg",
    "Height, cm",
    "BMI, kg/m²",
    "BMI Category",
    "  Class I (30-34.9), n (%)",
    "  Class II (35-39.9), n (%)",
    "  Class III (≥40), n (%)",
    "GLP-1 prescriptions, n",
    "Baseline Activity",
    "  Steps per day",
    "  Sedentary minutes per day",
    "  MVPA minutes per day",
    "  Activity calories per day"
  ),
  `Mean ± SD or n (%)` = c(
    as.character(nrow(table1_data)),
    if("age_at_initiation" %in% names(table1_data))
      sprintf("%.1f ± %.1f", mean(table1_data$age_at_initiation, na.rm = TRUE),
              sd(table1_data$age_at_initiation, na.rm = TRUE)) else NULL,
    if("sex" %in% names(table1_data))
      sprintf("%d (%.1f%%)", sum(table1_data$sex == "Female", na.rm = TRUE),
              100 * mean(table1_data$sex == "Female", na.rm = TRUE)) else NULL,
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

# Combine weight data with nadir
weight_summary <- weight_by_period %>%
  select(period, n, mean_weight, sd_weight) %>%
  bind_rows(nadir_weight_summary)

# Calculate p-values (paired t-test vs baseline)
baseline_weight_vals <- weight_by_period$weight_data[[1]]

weight_summary <- weight_summary %>%
  rowwise() %>%
  mutate(
    p_value = if_else(
      period == "Baseline",
      NA_real_,
      {
        if(period == "Nadir (>12 weeks)") {
          # Nadir comparison
          common_ids <- intersect(baseline_weight_vals$person_id,
                                 nadir_weight_data$person_id)
          if(length(common_ids) >= 3) {
            baseline_vals <- baseline_weight_vals %>%
              filter(person_id %in% common_ids) %>%
              arrange(person_id) %>%
              pull(weight_kg)
            nadir_vals <- nadir_weight_data %>%
              filter(person_id %in% common_ids) %>%
              arrange(person_id) %>%
              pull(nadir_weight)
            t.test(baseline_vals, nadir_vals, paired = TRUE)$p.value
          } else {
            NA_real_
          }
        } else {
          # Regular period comparison
          period_idx <- which(weight_by_period$period == period)
          period_data <- weight_by_period$weight_data[[period_idx]]
          common_ids <- intersect(baseline_weight_vals$person_id, period_data$person_id)
          if(length(common_ids) >= 3) {
            baseline_vals <- baseline_weight_vals %>%
              filter(person_id %in% common_ids) %>%
              arrange(person_id) %>%
              pull(weight_kg)
            period_vals <- period_data %>%
              filter(person_id %in% common_ids) %>%
              arrange(person_id) %>%
              pull(weight_kg)
            t.test(baseline_vals, period_vals, paired = TRUE)$p.value
          } else {
            NA_real_
          }
        }
      }
    )
  ) %>%
  ungroup()

# Calculate p-values for activity metrics
baseline_activity_vals <- activity_by_period$activity_data[[1]]

activity_summary <- activity_by_period %>%
  select(period, n, mean_steps, sd_steps, mean_sedentary, sd_sedentary,
         mean_mvpa, sd_mvpa, mean_calories, sd_calories) %>%
  rowwise() %>%
  mutate(
    p_value_steps = if_else(
      period == "Baseline",
      NA_real_,
      {
        period_idx <- which(activity_by_period$period == period)
        period_data <- activity_by_period$activity_data[[period_idx]]
        common_ids <- intersect(baseline_activity_vals$person_id, period_data$person_id)
        if(length(common_ids) >= 3) {
          baseline_vals <- baseline_activity_vals %>%
            filter(person_id %in% common_ids) %>%
            arrange(person_id) %>%
            pull(steps)
          period_vals <- period_data %>%
            filter(person_id %in% common_ids) %>%
            arrange(person_id) %>%
            pull(steps)
          t.test(baseline_vals, period_vals, paired = TRUE)$p.value
        } else {
          NA_real_
        }
      }
    ),
    p_value_mvpa = if_else(
      period == "Baseline",
      NA_real_,
      {
        period_idx <- which(activity_by_period$period == period)
        period_data <- activity_by_period$activity_data[[period_idx]]
        common_ids <- intersect(baseline_activity_vals$person_id, period_data$person_id)
        if(length(common_ids) >= 3) {
          baseline_vals <- baseline_activity_vals %>%
            filter(person_id %in% common_ids) %>%
            arrange(person_id) %>%
            pull(mvpa)
          period_vals <- period_data %>%
            filter(person_id %in% common_ids) %>%
            arrange(person_id) %>%
            pull(mvpa)
          t.test(baseline_vals, period_vals, paired = TRUE)$p.value
        } else {
          NA_real_
        }
      }
    ),
    p_value_calories = if_else(
      period == "Baseline",
      NA_real_,
      {
        period_idx <- which(activity_by_period$period == period)
        period_data <- activity_by_period$activity_data[[period_idx]]
        common_ids <- intersect(baseline_activity_vals$person_id, period_data$person_id)
        if(length(common_ids) >= 3) {
          baseline_vals <- baseline_activity_vals %>%
            filter(person_id %in% common_ids) %>%
            arrange(person_id) %>%
            pull(activity_cal)
          period_vals <- period_data %>%
            filter(person_id %in% common_ids) %>%
            arrange(person_id) %>%
            pull(activity_cal)
          t.test(baseline_vals, period_vals, paired = TRUE)$p.value
        } else {
          NA_real_
        }
      }
    )
  ) %>%
  ungroup()

# Format Table 2
cat("\nWeight Outcomes:\n")
weight_table <- weight_summary %>%
  mutate(
    `Mean ± SD` = sprintf("%.1f ± %.1f", mean_weight, sd_weight),
    `N` = as.character(n),
    `P-value` = if_else(is.na(p_value), "—", sprintf("%.4f", p_value))
  ) %>%
  select(Period = period, N, `Mean ± SD`, `P-value`)

print(weight_table)

cat("\nActivity Outcomes:\n")

# Steps
steps_table <- activity_summary %>%
  mutate(
    Metric = "Steps per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_steps, sd_steps),
    `N` = as.character(n),
    `P-value` = if_else(is.na(p_value_steps), "—", sprintf("%.4f", p_value_steps))
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`, `P-value`)

# MVPA
mvpa_table <- activity_summary %>%
  mutate(
    Metric = "MVPA minutes per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_mvpa, sd_mvpa),
    `N` = as.character(n),
    `P-value` = if_else(is.na(p_value_mvpa), "—", sprintf("%.4f", p_value_mvpa))
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`, `P-value`)

# Sedentary
sedentary_table <- activity_summary %>%
  mutate(
    Metric = "Sedentary minutes per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_sedentary, sd_sedentary),
    `N` = as.character(n),
    `P-value` = "—"  # No p-value for sedentary in primary analysis
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`, `P-value`)

# Calories
calories_table <- activity_summary %>%
  mutate(
    Metric = "Activity calories per day",
    `Mean ± SD` = sprintf("%.0f ± %.0f", mean_calories, sd_calories),
    `N` = as.character(n),
    `P-value` = if_else(is.na(p_value_calories), "—", sprintf("%.4f", p_value_calories))
  ) %>%
  select(Period = period, Metric, N, `Mean ± SD`, `P-value`)

activity_table <- bind_rows(steps_table, mvpa_table, sedentary_table, calories_table)
print(activity_table)

# Save Table 2
write_csv(weight_table, "table2_weight_outcomes.csv")
write_csv(activity_table, "table2_activity_outcomes.csv")

cat("\nSaved: table2_weight_outcomes.csv\n")
cat("Saved: table2_activity_outcomes.csv\n")

# =============================================================================
# MIXED EFFECTS MODELS FOR LONGITUDINAL TRENDS
# =============================================================================

cat("\n========================================\n")
cat("Mixed Effects Models\n")
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

# Weight longitudinal data
weight_long <- weight_by_period %>%
  select(period, weight_data) %>%
  unnest(weight_data) %>%
  inner_join(period_numeric_map, by = "period")

cat(sprintf("Weight observations: %d from %d unique patients\n",
            nrow(weight_long), n_distinct(weight_long$person_id)))

# Fit weight mixed effects model
if(n_distinct(weight_long$person_id) >= 3 && nrow(weight_long) >= 10) {
  model_weight <- lmer(weight_kg ~ period_numeric + (1 | person_id),
                      data = weight_long)

  cat("\nWeight Mixed Effects Model:\n")
  print(summary(model_weight))

  coef_weight <- fixef(model_weight)["period_numeric"]
  baseline_weight_me <- fixef(model_weight)["(Intercept)"]

  # Calculate change at 1 year (273 days)
  change_1year <- coef_weight * 273
  pct_change_1year <- (change_1year / baseline_weight_me) * 100

  cat(sprintf("\nWeight change per day: %.3f kg (p=%.4f)\n",
              coef_weight,
              summary(model_weight)$coefficients["period_numeric", "Pr(>|t|)"]))
  cat(sprintf("Estimated change at 1 year: %.2f kg (%.1f%%)\n",
              change_1year, pct_change_1year))
}

# Activity longitudinal data
activity_long <- activity_by_period %>%
  select(period, activity_data) %>%
  unnest(activity_data) %>%
  inner_join(period_numeric_map, by = "period")

cat(sprintf("\nActivity observations: %d from %d unique patients\n",
            nrow(activity_long), n_distinct(activity_long$person_id)))

# Steps mixed effects model
if(n_distinct(activity_long$person_id) >= 3 && nrow(activity_long) >= 10) {
  model_steps <- lmer(steps ~ period_numeric + (1 | person_id),
                     data = activity_long)

  cat("\nSteps Mixed Effects Model:\n")
  print(summary(model_steps))

  coef_steps <- fixef(model_steps)["period_numeric"]
  baseline_steps_me <- fixef(model_steps)["(Intercept)"]
  change_1year_steps <- coef_steps * 273
  pct_change_steps <- (change_1year_steps / baseline_steps_me) * 100

  cat(sprintf("\nSteps change per day: %.2f steps (p=%.4f)\n",
              coef_steps,
              summary(model_steps)$coefficients["period_numeric", "Pr(>|t|)"]))
  cat(sprintf("Estimated change at 1 year: %.0f steps (%.1f%%)\n",
              change_1year_steps, pct_change_steps))
}

# MVPA mixed effects model
if(n_distinct(activity_long$person_id) >= 3 && nrow(activity_long) >= 10) {
  model_mvpa <- lmer(mvpa ~ period_numeric + (1 | person_id),
                    data = activity_long)

  cat("\nMVPA Mixed Effects Model:\n")
  print(summary(model_mvpa))

  coef_mvpa <- fixef(model_mvpa)["period_numeric"]
  baseline_mvpa_me <- fixef(model_mvpa)["(Intercept)"]
  change_1year_mvpa <- coef_mvpa * 273
  pct_change_mvpa <- (change_1year_mvpa / baseline_mvpa_me) * 100

  cat(sprintf("\nMVPA change per day: %.3f min (p=%.4f)\n",
              coef_mvpa,
              summary(model_mvpa)$coefficients["period_numeric", "Pr(>|t|)"]))
  cat(sprintf("Estimated change at 1 year: %.1f min (%.1f%%)\n",
              change_1year_mvpa, pct_change_mvpa))
}

# Activity calories mixed effects model
if(n_distinct(activity_long$person_id) >= 3 && nrow(activity_long) >= 10) {
  model_calories <- lmer(activity_cal ~ period_numeric + (1 | person_id),
                        data = activity_long)

  cat("\nActivity Calories Mixed Effects Model:\n")
  print(summary(model_calories))

  coef_cal <- fixef(model_calories)["period_numeric"]
  baseline_cal_me <- fixef(model_calories)["(Intercept)"]
  change_1year_cal <- coef_cal * 273
  pct_change_cal <- (change_1year_cal / baseline_cal_me) * 100

  cat(sprintf("\nActivity calories change per day: %.2f kcal (p=%.4f)\n",
              coef_cal,
              summary(model_calories)$coefficients["period_numeric", "Pr(>|t|)"]))
  cat(sprintf("Estimated change at 1 year: %.0f kcal (%.1f%%)\n",
              change_1year_cal, pct_change_cal))
}

# Save mixed effects results
mixed_effects_summary <- tibble(
  Outcome = c("Weight (kg)", "Steps (per day)", "MVPA (min/day)", "Activity Calories (kcal/day)"),
  `Change per day` = c(
    if(exists("coef_weight")) sprintf("%.3f", coef_weight) else NA_character_,
    if(exists("coef_steps")) sprintf("%.2f", coef_steps) else NA_character_,
    if(exists("coef_mvpa")) sprintf("%.3f", coef_mvpa) else NA_character_,
    if(exists("coef_cal")) sprintf("%.2f", coef_cal) else NA_character_
  ),
  `Change at 1 year` = c(
    if(exists("change_1year")) sprintf("%.2f", change_1year) else NA_character_,
    if(exists("change_1year_steps")) sprintf("%.0f", change_1year_steps) else NA_character_,
    if(exists("change_1year_mvpa")) sprintf("%.1f", change_1year_mvpa) else NA_character_,
    if(exists("change_1year_cal")) sprintf("%.0f", change_1year_cal) else NA_character_
  ),
  `Percent change` = c(
    if(exists("pct_change_1year")) sprintf("%.1f%%", pct_change_1year) else NA_character_,
    if(exists("pct_change_steps")) sprintf("%.1f%%", pct_change_steps) else NA_character_,
    if(exists("pct_change_mvpa")) sprintf("%.1f%%", pct_change_mvpa) else NA_character_,
    if(exists("pct_change_cal")) sprintf("%.1f%%", pct_change_cal) else NA_character_
  ),
  `P-value` = c(
    if(exists("model_weight")) sprintf("%.4f", summary(model_weight)$coefficients["period_numeric", "Pr(>|t|)"]) else NA_character_,
    if(exists("model_steps")) sprintf("%.4f", summary(model_steps)$coefficients["period_numeric", "Pr(>|t|)"]) else NA_character_,
    if(exists("model_mvpa")) sprintf("%.4f", summary(model_mvpa)$coefficients["period_numeric", "Pr(>|t|)"]) else NA_character_,
    if(exists("model_calories")) sprintf("%.4f", summary(model_calories)$coefficients["period_numeric", "Pr(>|t|)"]) else NA_character_
  )
)

print(mixed_effects_summary)

write_csv(mixed_effects_summary, "table3_mixed_effects_models.csv")
cat("\nSaved: table3_mixed_effects_models.csv\n")

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
