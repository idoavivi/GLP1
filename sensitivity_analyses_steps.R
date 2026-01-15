# =============================================================================
# SENSITIVITY ANALYSES: STEPS CHANGE BY SUBGROUPS
# =============================================================================
# 1. Steps change by BMI class (I, II, III)
# 2. Steps change by baseline steps (tertiles)
# 3. Steps change by sex
# 4. Nadir weight cohort: steps change by weight response (<5%, 5-10%, >10%)
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("SENSITIVITY ANALYSES: STEPS CHANGE BY SUBGROUPS\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD DATA (memory-first pattern)
# =============================================================================

cat("Checking for data...\n")

has_data_in_memory <- (exists("obesity_cohort") && is.data.frame(obesity_cohort) &&
                       (exists("activity_cleaned") || exists("activity_final")) &&
                       (exists("weight_cleaned") || exists("weight_final")) &&
                       (exists("bmi_data") || exists("bmi_final")))

if (has_data_in_memory) {
  cat("✓ Using data from memory (from comprehensive_data_cleaning.R)\n")

  # Handle object name mapping
  if (exists("activity_final")) activity_cleaned <- activity_final
  if (exists("weight_final")) weight_cleaned <- weight_final
  if (exists("bmi_final")) bmi_data <- bmi_final
  if (exists("drug_final")) drug_glp1_clean <- drug_final
  if (exists("glp1_initiation_final")) glp1_initiation <- glp1_initiation_final
} else {
  cat("Loading from file: glp1_cleaned_data.RData\n")
  load("glp1_cleaned_data.RData")
}

cat(sprintf("  Obesity cohort: %d patients\n", nrow(obesity_cohort)))

# Check for demographics
if (exists("person") && is.data.frame(person)) {
  cat("✓ Demographics available: N=%d\n", nrow(person))
} else {
  cat("⚠ Demographics not available. Sex-stratified analysis will be skipped.\n")
}

cat("\n")

# =============================================================================
# DEFINE FIXED BASELINE COHORT (same as Table 2)
# =============================================================================

cat("========================================\n")
cat("DEFINING BASELINE COHORT\n")
cat("========================================\n\n")

# Patients with baseline activity
baseline_activity_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_baseline_days = n(), .groups = "drop")

# Patients with 1-30d follow-up
followup_1_30d_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 30,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_followup_days = n(), .groups = "drop")

# Fixed baseline cohort
baseline_cohort <- baseline_activity_patients %>%
  inner_join(followup_1_30d_patients, by = "person_id")

cat(sprintf("Fixed baseline cohort: N=%d\n\n", nrow(baseline_cohort)))

# Filter all data to baseline cohort
activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

weight_cleaned <- weight_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

bmi_data <- bmi_data %>%
  filter(person_id %in% baseline_cohort$person_id)

# =============================================================================
# CALCULATE BASELINE CHARACTERISTICS
# =============================================================================

cat("========================================\n")
cat("CALCULATING BASELINE CHARACTERISTICS\n")
cat("========================================\n\n")

# Baseline steps (mean over baseline period)
baseline_steps <- activity_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  summarize(baseline_steps = mean(steps, na.rm = TRUE), .groups = "drop")

# Baseline BMI (most recent in -365 to 0 period)
baseline_bmi <- bmi_data %>%
  filter(days_from_initiation >= -365, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, baseline_bmi = bmi)

# Baseline weight (for nadir analysis)
baseline_weight <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg)

# Combine baseline characteristics
baseline_chars <- baseline_cohort %>%
  select(person_id) %>%
  left_join(baseline_steps, by = "person_id") %>%
  left_join(baseline_bmi, by = "person_id") %>%
  left_join(baseline_weight, by = "person_id")

# Add demographics if available
if (exists("person") && is.data.frame(person)) {
  baseline_chars <- baseline_chars %>%
    left_join(
      person %>% select(person_id, sex = sex_at_birth),
      by = "person_id"
    )
}

cat(sprintf("Baseline characteristics calculated for N=%d patients\n", nrow(baseline_chars)))
cat(sprintf("  Missing baseline steps: %d\n", sum(is.na(baseline_chars$baseline_steps))))
cat(sprintf("  Missing baseline BMI: %d\n", sum(is.na(baseline_chars$baseline_bmi))))
cat(sprintf("  Missing baseline weight: %d\n", sum(is.na(baseline_chars$baseline_weight))))
if ("sex" %in% colnames(baseline_chars)) {
  cat(sprintf("  Missing sex: %d\n", sum(is.na(baseline_chars$sex))))
}
cat("\n")

# =============================================================================
# CALCULATE ACTIVITY BY PERIOD (for all analyses)
# =============================================================================

cat("========================================\n")
cat("CALCULATING ACTIVITY BY PERIOD\n")
cat("========================================\n\n")

# Calculate person-period means for steps
time_periods <- tribble(
  ~period, ~days_min, ~days_max,
  "Baseline", -180, 0,
  "1-30d", 1, 30,
  "31-90d", 31, 90,
  "91-180d", 91, 180,
  "181-365d", 181, 365
)

activity_by_period_list <- list()

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  period_activity <- activity_cleaned %>%
    filter(days_from_initiation >= days_min,
           days_from_initiation <= days_max,
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    summarize(steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
    mutate(period = period_name)

  activity_by_period_list[[period_name]] <- period_activity
  cat(sprintf("  %s: N=%d\n", period_name, nrow(period_activity)))
}

# Combine all periods
activity_long <- bind_rows(activity_by_period_list) %>%
  mutate(period = factor(period, levels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")))

cat(sprintf("\nActivity long format: %d observations from %d patients\n\n",
            nrow(activity_long), n_distinct(activity_long$person_id)))

# =============================================================================
# ANALYSIS 1: STEPS CHANGE BY BMI CLASS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 1: STEPS BY BMI CLASS\n")
cat("========================================\n\n")

# Define BMI classes
baseline_chars <- baseline_chars %>%
  mutate(
    bmi_class = case_when(
      baseline_bmi >= 30 & baseline_bmi < 35 ~ "Class I (30-34.9)",
      baseline_bmi >= 35 & baseline_bmi < 40 ~ "Class II (35-39.9)",
      baseline_bmi >= 40 ~ "Class III (≥40)",
      TRUE ~ NA_character_
    )
  )

cat("BMI class distribution:\n")
print(table(baseline_chars$bmi_class, useNA = "ifany"))
cat("\n")

# Add BMI class to activity data
activity_bmi <- activity_long %>%
  left_join(baseline_chars %>% select(person_id, bmi_class), by = "person_id") %>%
  filter(!is.na(bmi_class))

# Function to run model and extract results
run_stratified_model <- function(data, strata_var, strata_value) {

  data_subset <- data %>%
    filter(.data[[strata_var]] == strata_value) %>%
    filter(!is.na(steps))

  n_patients <- n_distinct(data_subset$person_id)

  if (n_patients < 10) {
    return(tibble(
      strata = strata_value,
      n_patients = n_patients,
      period = c("1-30d", "31-90d", "91-180d", "181-365d"),
      estimate = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      note = "Insufficient sample size"
    ))
  }

  model <- lmer(steps ~ period + (1 | person_id), data = data_subset)
  coef_summary <- summary(model)$coefficients

  # Extract coefficients for each period (vs baseline)
  results <- tibble(
    strata = strata_value,
    n_patients = n_patients,
    period = c("1-30d", "31-90d", "91-180d", "181-365d"),
    estimate = coef_summary[2:5, "Estimate"],
    se = coef_summary[2:5, "Std. Error"],
    p_value = coef_summary[2:5, "Pr(>|t|)"]
  )

  return(results)
}

# Run models for each BMI class
bmi_classes <- c("Class I (30-34.9)", "Class II (35-39.9)", "Class III (≥40)")
bmi_results <- map_dfr(bmi_classes, ~run_stratified_model(activity_bmi, "bmi_class", .x))

cat("Results by BMI class:\n")
print(bmi_results)
cat("\n")

# Create summary table
bmi_summary <- activity_bmi %>%
  group_by(bmi_class, period) %>%
  summarize(
    n = n(),
    median_steps = median(steps, na.rm = TRUE),
    q25 = quantile(steps, 0.25, na.rm = TRUE),
    q75 = quantile(steps, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    value = sprintf("%.0f (%.0f-%.0f)", median_steps, q25, q75)
  )

cat("Summary statistics by BMI class:\n")
print(bmi_summary %>% select(bmi_class, period, n, value))
cat("\n")

# =============================================================================
# ANALYSIS 2: STEPS CHANGE BY BASELINE STEPS TERTILES
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 2: STEPS BY BASELINE TERTILES\n")
cat("========================================\n\n")

# Calculate tertiles
baseline_steps_tertiles <- quantile(baseline_chars$baseline_steps,
                                     probs = c(0, 1/3, 2/3, 1),
                                     na.rm = TRUE)

cat("Baseline steps tertile cutoffs:\n")
print(baseline_steps_tertiles)
cat("\n")

baseline_chars <- baseline_chars %>%
  mutate(
    steps_tertile = case_when(
      baseline_steps < baseline_steps_tertiles[2] ~ "Low (T1)",
      baseline_steps >= baseline_steps_tertiles[2] &
        baseline_steps < baseline_steps_tertiles[3] ~ "Medium (T2)",
      baseline_steps >= baseline_steps_tertiles[3] ~ "High (T3)",
      TRUE ~ NA_character_
    )
  )

cat("Baseline steps tertile distribution:\n")
print(table(baseline_chars$steps_tertile, useNA = "ifany"))
cat("\n")

# Add tertile to activity data
activity_tertile <- activity_long %>%
  left_join(baseline_chars %>% select(person_id, steps_tertile), by = "person_id") %>%
  filter(!is.na(steps_tertile))

# Run models for each tertile
tertiles <- c("Low (T1)", "Medium (T2)", "High (T3)")
tertile_results <- map_dfr(tertiles, ~run_stratified_model(activity_tertile, "steps_tertile", .x))

cat("Results by baseline steps tertile:\n")
print(tertile_results)
cat("\n")

# Summary statistics
tertile_summary <- activity_tertile %>%
  group_by(steps_tertile, period) %>%
  summarize(
    n = n(),
    median_steps = median(steps, na.rm = TRUE),
    q25 = quantile(steps, 0.25, na.rm = TRUE),
    q75 = quantile(steps, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    value = sprintf("%.0f (%.0f-%.0f)", median_steps, q25, q75)
  )

cat("Summary statistics by baseline steps tertile:\n")
print(tertile_summary %>% select(steps_tertile, period, n, value))
cat("\n")

# =============================================================================
# ANALYSIS 3: STEPS CHANGE BY SEX
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 3: STEPS BY SEX\n")
cat("========================================\n\n")

if ("sex" %in% colnames(baseline_chars)) {

  cat("Sex distribution:\n")
  print(table(baseline_chars$sex, useNA = "ifany"))
  cat("\n")

  # Add sex to activity data
  activity_sex <- activity_long %>%
    left_join(baseline_chars %>% select(person_id, sex), by = "person_id") %>%
    filter(!is.na(sex), sex %in% c("Male", "Female"))

  # Run models for each sex
  sex_results <- map_dfr(c("Male", "Female"), ~run_stratified_model(activity_sex, "sex", .x))

  cat("Results by sex:\n")
  print(sex_results)
  cat("\n")

  # Summary statistics
  sex_summary <- activity_sex %>%
    group_by(sex, period) %>%
    summarize(
      n = n(),
      median_steps = median(steps, na.rm = TRUE),
      q25 = quantile(steps, 0.25, na.rm = TRUE),
      q75 = quantile(steps, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      value = sprintf("%.0f (%.0f-%.0f)", median_steps, q25, q75)
    )

  cat("Summary statistics by sex:\n")
  print(sex_summary %>% select(sex, period, n, value))
  cat("\n")

} else {
  cat("⚠ Sex data not available. Skipping sex-stratified analysis.\n\n")
  sex_results <- NULL
  sex_summary <- NULL
}

# =============================================================================
# ANALYSIS 4: NADIR WEIGHT COHORT - STEPS BY WEIGHT RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 4: NADIR WEIGHT & WEIGHT RESPONSE\n")
cat("========================================\n\n")

cat("Step 1: Identifying nadir weight (≥84 days from initiation)...\n")

# Find nadir weight (minimum weight ≥84 days from initiation)
nadir_weight <- weight_cleaned %>%
  filter(days_from_initiation >= 84) %>%
  group_by(person_id) %>%
  arrange(weight_kg) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, nadir_weight = weight_kg, nadir_days = days_from_initiation)

cat(sprintf("  Patients with ≥1 weight measurement ≥84 days: N=%d\n", nrow(nadir_weight)))

# Calculate percent weight change
weight_change <- baseline_chars %>%
  select(person_id, baseline_weight) %>%
  inner_join(nadir_weight, by = "person_id") %>%
  filter(!is.na(baseline_weight), !is.na(nadir_weight)) %>%
  mutate(
    weight_change_kg = nadir_weight - baseline_weight,
    weight_change_pct = 100 * (nadir_weight - baseline_weight) / baseline_weight
  )

cat(sprintf("  Patients with both baseline and nadir weight: N=%d\n", nrow(weight_change)))
cat(sprintf("  Median weight change: %.1f kg (%.1f%%)\n",
            median(weight_change$weight_change_kg, na.rm = TRUE),
            median(weight_change$weight_change_pct, na.rm = TRUE)))
cat("\n")

# Define weight response categories
weight_change <- weight_change %>%
  mutate(
    weight_response = case_when(
      weight_change_pct > -5 ~ "<5% loss",
      weight_change_pct <= -5 & weight_change_pct > -10 ~ "5-10% loss",
      weight_change_pct <= -10 ~ ">10% loss",
      TRUE ~ NA_character_
    )
  )

cat("Weight response distribution:\n")
print(table(weight_change$weight_response, useNA = "ifany"))
cat("\n")

cat("Step 2: Analyzing steps by weight response category...\n\n")

# Filter activity to nadir cohort only
activity_nadir <- activity_long %>%
  inner_join(weight_change %>% select(person_id, weight_response), by = "person_id") %>%
  filter(!is.na(weight_response))

cat(sprintf("Activity data for nadir cohort: N=%d patients, %d observations\n",
            n_distinct(activity_nadir$person_id), nrow(activity_nadir)))
cat("\n")

# Run models for each weight response category
weight_categories <- c("<5% loss", "5-10% loss", ">10% loss")
weight_response_results <- map_dfr(
  weight_categories,
  ~run_stratified_model(activity_nadir, "weight_response", .x)
)

cat("Results by weight response:\n")
print(weight_response_results)
cat("\n")

# Summary statistics
weight_response_summary <- activity_nadir %>%
  group_by(weight_response, period) %>%
  summarize(
    n = n(),
    median_steps = median(steps, na.rm = TRUE),
    q25 = quantile(steps, 0.25, na.rm = TRUE),
    q75 = quantile(steps, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    value = sprintf("%.0f (%.0f-%.0f)", median_steps, q25, q75)
  )

cat("Summary statistics by weight response:\n")
print(weight_response_summary %>% select(weight_response, period, n, value))
cat("\n")

# =============================================================================
# SAVE ALL RESULTS
# =============================================================================

cat("\n========================================\n")
cat("SAVING RESULTS\n")
cat("========================================\n\n")

# Save model results
write_csv(bmi_results, "sensitivity_bmi_class_results.csv")
cat("Saved: sensitivity_bmi_class_results.csv\n")

write_csv(tertile_results, "sensitivity_baseline_tertile_results.csv")
cat("Saved: sensitivity_baseline_tertile_results.csv\n")

if (!is.null(sex_results)) {
  write_csv(sex_results, "sensitivity_sex_results.csv")
  cat("Saved: sensitivity_sex_results.csv\n")
}

write_csv(weight_response_results, "sensitivity_weight_response_results.csv")
cat("Saved: sensitivity_weight_response_results.csv\n")

# Save summary statistics
write_csv(bmi_summary, "sensitivity_bmi_class_summary.csv")
write_csv(tertile_summary, "sensitivity_baseline_tertile_summary.csv")
if (!is.null(sex_summary)) {
  write_csv(sex_summary, "sensitivity_sex_summary.csv")
}
write_csv(weight_response_summary, "sensitivity_weight_response_summary.csv")

# Save weight change data
write_csv(weight_change, "nadir_weight_change.csv")
cat("Saved: nadir_weight_change.csv\n")

cat("\n##################################################\n")
cat("SENSITIVITY ANALYSES COMPLETE\n")
cat("##################################################\n\n")

cat("Summary:\n")
cat(sprintf("  1. BMI class: %d classes analyzed\n", length(bmi_classes)))
cat(sprintf("  2. Baseline steps tertiles: %d tertiles analyzed\n", length(tertiles)))
if (!is.null(sex_results)) {
  cat(sprintf("  3. Sex: 2 groups analyzed\n"))
} else {
  cat(sprintf("  3. Sex: SKIPPED (no data)\n"))
}
cat(sprintf("  4. Weight response: %d categories, N=%d nadir cohort\n",
            length(weight_categories), n_distinct(activity_nadir$person_id)))
cat("\n")
