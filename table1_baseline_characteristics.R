# =============================================================================
# TABLE 1: BASELINE CHARACTERISTICS
# =============================================================================
# Baseline characteristics of the GLP-1 obesity cohort
# Fixed baseline cohort: Patients with ≥3 valid baseline AND ≥3 valid 1-30d
# Includes demographics and comorbidities from pull_demographics_diagnoses.R
# =============================================================================

library(tidyverse)
library(knitr)
library(kableExtra)
library(htmltools)

cat("\n##################################################\n")
cat("TABLE 1: BASELINE CHARACTERISTICS\n")
cat("GLP-1 Obesity Cohort\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD DATA (or use objects in memory if available)
# =============================================================================

cat("Checking for data...\n")

# Check if objects exist in memory (from comprehensive_data_cleaning.R)
# This allows running in same session after comprehensive_data_cleaning.R
# comprehensive_data_cleaning.R uses _final suffix for object names
has_data_in_memory <- (exists("obesity_cohort") && is.data.frame(obesity_cohort) &&
                       (exists("activity_cleaned") || exists("activity_final")) &&
                       (exists("weight_cleaned") || exists("weight_final")) &&
                       (exists("bmi_data") || exists("bmi_final")))

if (has_data_in_memory) {
  cat("✓ Using data from memory (from comprehensive_data_cleaning.R)\n")
  cat(sprintf("  Obesity cohort: %d patients\n", nrow(obesity_cohort)))

  # Handle object name mapping (comprehensive_data_cleaning.R uses _final suffix)
  if (exists("activity_final")) {
    activity_cleaned <- activity_final
  }
  if (exists("weight_final")) {
    weight_cleaned <- weight_final
  }
  if (exists("bmi_final")) {
    bmi_data <- bmi_final
  }
  if (exists("drug_final")) {
    drug_glp1_clean <- drug_final
  }
  if (exists("glp1_initiation_final")) {
    glp1_initiation <- glp1_initiation_final
  }

} else {
  cat("Loading from file: glp1_cleaned_data.RData\n")
  load("glp1_cleaned_data.RData")
}

cat(sprintf("\nData loaded:\n"))
cat(sprintf("  Obesity cohort: %d patients\n", nrow(obesity_cohort)))
cat(sprintf("  Activity records: %d\n", nrow(activity_cleaned)))
cat(sprintf("  Weight records: %d\n", nrow(weight_cleaned)))
cat(sprintf("  BMI records: %d\n\n", nrow(bmi_data)))

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
# LOAD DEMOGRAPHICS (if available)
# =============================================================================

cat("========================================\n")
cat("STEP 4: Loading Demographics\n")
cat("========================================\n\n")

# Check if person demographics are available
if (exists("person") && is.data.frame(person)) {
  cat(sprintf("✓ Demographics available for %d patients\n", nrow(person)))
  has_demographics <- TRUE

  # Check completeness
  cat(sprintf("  Age: %d (%.1f%% complete)\n",
              sum(!is.na(person$age)),
              100 * sum(!is.na(person$age)) / nrow(person)))
  cat(sprintf("  Sex: %d (%.1f%% complete)\n",
              sum(!is.na(person$sex) & person$sex != "Other/Unknown"),
              100 * sum(!is.na(person$sex) & person$sex != "Other/Unknown") / nrow(person)))
  cat(sprintf("  Race: %d (%.1f%% complete)\n",
              sum(!is.na(person$race) & person$race != "Other/Unknown"),
              100 * sum(!is.na(person$race) & person$race != "Other/Unknown") / nrow(person)))
} else {
  cat("⚠ Demographics not found (run pull_demographics_diagnoses.R in All of Us)\n")
  has_demographics <- FALSE
}
cat("\n")

# Check if diagnoses are available
if (exists("obesity_cohort") &&
    "has_hypertension" %in% colnames(obesity_cohort)) {
  cat("✓ Diagnoses available in obesity_cohort\n")
  has_diagnoses <- TRUE
} else {
  cat("⚠ Diagnoses not found (run pull_demographics_diagnoses.R in All of Us)\n")
  has_diagnoses <- FALSE
}
cat("\n")

# =============================================================================
# COMBINE ALL BASELINE DATA
# =============================================================================

cat("========================================\n")
cat("STEP 5: Combining Baseline Data\n")
cat("========================================\n\n")

baseline_data_combined <- baseline_cohort %>%
  select(person_id) %>%
  left_join(baseline_weight, by = "person_id") %>%
  left_join(baseline_bmi, by = "person_id") %>%
  left_join(baseline_activity, by = "person_id") %>%
  left_join(
    obesity_cohort %>% select(person_id, inclusion_reason,
                              matches("has_")),
    by = "person_id"
  )

# Add demographics if available
if (has_demographics) {
  baseline_data_combined <- baseline_data_combined %>%
    left_join(person %>% select(person_id, age, sex, race, ethnicity),
              by = "person_id")
}

cat(sprintf("Combined baseline data: N=%d\n\n", nrow(baseline_data_combined)))

# =============================================================================
# CALCULATE SUMMARY STATISTICS
# =============================================================================

cat("========================================\n")
cat("STEP 6: Summary Statistics\n")
cat("========================================\n\n")

# Overall N
total_n <- nrow(baseline_cohort)

# Demographics (if available)
if (has_demographics) {
  age_summary <- baseline_data_combined %>%
    summarize(
      n = sum(!is.na(age)),
      mean = mean(age, na.rm = TRUE),
      sd = sd(age, na.rm = TRUE),
      median = median(age, na.rm = TRUE),
      q25 = quantile(age, 0.25, na.rm = TRUE),
      q75 = quantile(age, 0.75, na.rm = TRUE)
    )

  sex_summary <- baseline_data_combined %>%
    filter(!is.na(sex), sex != "Other/Unknown") %>%
    count(sex) %>%
    mutate(pct = 100 * n / sum(n))

  race_summary <- baseline_data_combined %>%
    filter(!is.na(race), race != "Other/Unknown") %>%
    count(race) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    arrange(desc(n))

  ethnicity_summary <- baseline_data_combined %>%
    filter(!is.na(ethnicity), ethnicity != "Unknown") %>%
    count(ethnicity) %>%
    mutate(pct = 100 * n / sum(n))
}

# Comorbidities (if available)
if (has_diagnoses) {
  comorbidity_summary <- baseline_data_combined %>%
    summarize(
      hypertension = sum(has_hypertension, na.rm = TRUE),
      diabetes = sum(has_diabetes, na.rm = TRUE),
      dyslipidemia = sum(has_dyslipidemia, na.rm = TRUE),
      ihd = sum(has_ihd, na.rm = TRUE),
      stroke = sum(has_stroke, na.rm = TRUE),
      osteoarthritis = sum(has_osteoarthritis, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "condition", values_to = "n") %>%
    mutate(pct = 100 * n / total_n)
}

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
cat("STEP 7: Creating Table 1\n")
cat("========================================\n\n")

# Build table - start with basic structure
table1_rows <- list(
  list("Total N", as.character(total_n)),
  list("", "")
)

# Add demographics if available
if (has_demographics) {
  demo_rows <- list(
    list("Demographics", ""),
    list("  Age (years)", sprintf("%.1f ± %.1f", age_summary$mean, age_summary$sd))
  )

  # Add sex breakdown
  if (nrow(sex_summary) > 0) {
    for (i in 1:nrow(sex_summary)) {
      demo_rows <- c(demo_rows, list(
        list(paste0("  ", sex_summary$sex[i]), sprintf("%d (%.1f%%)", sex_summary$n[i], sex_summary$pct[i]))
      ))
    }
  }

  # Add race breakdown (top categories)
  if (nrow(race_summary) > 0) {
    demo_rows <- c(demo_rows, list(list("  Race:", "")))
    for (i in 1:min(5, nrow(race_summary))) {  # Top 5 races
      demo_rows <- c(demo_rows, list(
        list(paste0("    ", race_summary$race[i]), sprintf("%d (%.1f%%)", race_summary$n[i], race_summary$pct[i]))
      ))
    }
  }

  # Add ethnicity
  if (nrow(ethnicity_summary) > 0) {
    demo_rows <- c(demo_rows, list(list("  Ethnicity:", "")))
    for (i in 1:nrow(ethnicity_summary)) {
      demo_rows <- c(demo_rows, list(
        list(paste0("    ", ethnicity_summary$ethnicity[i]), sprintf("%d (%.1f%%)", ethnicity_summary$n[i], ethnicity_summary$pct[i]))
      ))
    }
  }

  table1_rows <- c(table1_rows, demo_rows, list(list("", "")))
}

# Add obesity inclusion criteria
obesity_rows <- list(
  list("Obesity Inclusion Criteria", "")
)
for (i in 1:nrow(inclusion_summary)) {
  obesity_rows <- c(obesity_rows, list(
    list(paste0("  ", inclusion_summary$inclusion_reason[i]), sprintf("%d (%.1f%%)", inclusion_summary$n[i], inclusion_summary$pct[i]))
  ))
}
table1_rows <- c(table1_rows, obesity_rows, list(list("", "")))

# Add anthropometrics
anthro_rows <- list(
  list("Anthropometrics", ""),
  list("  Weight (kg)", sprintf("%.1f ± %.1f", weight_summary$mean, weight_summary$sd)),
  list("  BMI (kg/m²)", sprintf("%.1f ± %.1f", bmi_summary$mean, bmi_summary$sd)),
  list("", ""),
  list("BMI Categories", "")
)
for (i in 1:nrow(bmi_categories)) {
  anthro_rows <- c(anthro_rows, list(
    list(paste0("  ", bmi_categories$bmi_category[i]), sprintf("%d (%.1f%%)", bmi_categories$n[i], bmi_categories$pct[i]))
  ))
}
table1_rows <- c(table1_rows, anthro_rows, list(list("", "")))

# Add comorbidities if available
if (has_diagnoses) {
  comorbidity_rows <- list(list("Comorbidities", ""))
  # Create readable names
  condition_names <- c(
    hypertension = "Hypertension",
    diabetes = "Diabetes mellitus",
    dyslipidemia = "Dyslipidemia",
    ihd = "Ischemic heart disease",
    stroke = "Stroke/CVA",
    osteoarthritis = "Osteoarthritis"
  )
  for (i in 1:nrow(comorbidity_summary)) {
    condition <- comorbidity_summary$condition[i]
    readable_name <- condition_names[condition]
    comorbidity_rows <- c(comorbidity_rows, list(
      list(paste0("  ", readable_name), sprintf("%d (%.1f%%)", comorbidity_summary$n[i], comorbidity_summary$pct[i]))
    ))
  }
  table1_rows <- c(table1_rows, comorbidity_rows, list(list("", "")))
}

# Add activity metrics
activity_rows <- list(
  list("Baseline Activity Metrics", ""),
  list("  Valid days with activity data", sprintf("%.1f ± %.1f", valid_days_summary$mean, valid_days_summary$sd)),
  list("  Steps per day", sprintf("%.0f ± %.0f", steps_summary$mean, steps_summary$sd)),
  list("  MVPA (min/day)", sprintf("%.1f ± %.1f", mvpa_summary$mean, mvpa_summary$sd)),
  list("  Sedentary time (min/day)", sprintf("%.1f ± %.1f", sedentary_summary$mean, sedentary_summary$sd)),
  list("  Activity calories (kcal/day)", sprintf("%.0f ± %.0f", calories_summary$mean, calories_summary$sd))
)
table1_rows <- c(table1_rows, activity_rows)

# Convert to tibble
table1 <- tibble(
  Characteristic = sapply(table1_rows, function(x) x[[1]]),
  `Value (Mean ± SD or N (%))` = sapply(table1_rows, function(x) x[[2]])
)

print(table1)
cat("\n")

# =============================================================================
# SAVE TABLE 1
# =============================================================================

cat("========================================\n")
cat("STEP 8: Saving Table 1\n")
cat("========================================\n\n")

# Save CSV
write_csv(table1, "table1_baseline_characteristics.csv")
cat("Saved: table1_baseline_characteristics.csv\n")

# Save HTML version with error handling
tryCatch({
  # Build footnotes based on what data is available
  footnotes <- c(
    paste0("Total N = ", total_n, " patients in baseline cohort."),
    "Baseline period: 180 days before GLP-1 initiation.",
    "Inclusion criteria: BMI ≥30 OR obesity diagnosis, ≥3 valid baseline activity days, ≥3 valid 1-30d follow-up days.",
    "Valid day: Fitbit day with ≥10 hours wear time.",
    "MVPA: Moderate-to-vigorous physical activity (fairly active + very active minutes).",
    "Values shown as mean ± SD for continuous variables, N (%) for categorical variables."
  )

  if (has_demographics) {
    footnotes <- c(footnotes, "Demographics: Age, sex, race, ethnicity from All of Us person table.")
  }

  if (has_diagnoses) {
    footnotes <- c(footnotes, "Comorbidities: Diagnoses from All of Us condition_occurrence table.")
  }

  # Generate HTML table
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
      general = footnotes,
      general_title = "Notes:",
      footnote_as_chunk = TRUE
    )

  # Save HTML
  html_output <- as.character(table1_html)
  writeLines(html_output, "table1_baseline_characteristics.html")
  cat("Saved: table1_baseline_characteristics.html\n")

}, error = function(e) {
  cat("⚠ Error generating HTML (kableExtra may not be installed):\n")
  cat(paste0("  ", e$message, "\n"))
  cat("  CSV file still saved successfully.\n")
  cat("  To fix: install.packages('kableExtra')\n")
})

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

cat("\n##################################################\n")
cat("TABLE 1 COMPLETE\n")
cat("##################################################\n\n")

cat(sprintf("Baseline cohort: N = %d\n\n", total_n))

cat("Data included:\n")
cat(sprintf("  ✓ Obesity inclusion criteria\n"))
cat(sprintf("  ✓ Anthropometrics (Weight: %.1f%%, BMI: %.1f%%)\n",
            100 * weight_summary$n / total_n,
            100 * bmi_summary$n / total_n))
cat(sprintf("  ✓ Activity metrics: 100%%\n"))

if (has_demographics) {
  cat(sprintf("  ✓ Demographics (age, sex, race, ethnicity)\n"))
} else {
  cat(sprintf("  ⚠ Demographics not available (run pull_demographics_diagnoses.R)\n"))
}

if (has_diagnoses) {
  cat(sprintf("  ✓ Comorbidities (HTN, DM, dyslipidemia, IHD, CVA, OA)\n"))
} else {
  cat(sprintf("  ⚠ Comorbidities not available (run pull_demographics_diagnoses.R)\n"))
}

cat("\n")
