# Investigate Weight Gain During GLP-1 Treatment
# Specifically focuses on the 91-180d period weight gain anomaly
# Checks: data quality, treatment adherence, cohort definitions

library(bigrquery)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

cat("\n##################################################\n")
cat("INVESTIGATING WEIGHT GAIN DURING TREATMENT\n")
cat("Focus: 91-180d period showing unexpected increases\n")
cat("##################################################\n\n")

# Connect to BigQuery
project_id <- "all-of-us-data-tools"
dataset_id <- "AoU_CDR_2024q2r2"
billing_project <- "idoaviv-tauber-org"

# Load existing analysis results
if (file.exists("period_analysis_short_results.RData")) {
  load("period_analysis_short_results.RData")
  cat("Loaded period_analysis_short_results.RData\n\n")
} else {
  cat("ERROR: period_analysis_short_results.RData not found\n")
  cat("Please run period_analysis_optimized.R first\n")
  stop()
}

# ========================================
# CRITICAL QUESTION 1: Weight measurement selection
# ========================================

cat("========================================\n")
cat("QUESTION 1: How are we selecting weights?\n")
cat("========================================\n\n")

# Query weight data
weight_query <- sprintf("
  SELECT
    person_id,
    measurement_date,
    value_as_number * 0.453592 AS weight_kg
  FROM `%s.%s.measurement`
  WHERE measurement_concept_id = 3025315
    AND value_as_number IS NOT NULL
    AND value_as_number > 0
", project_id, dataset_id)

weight_raw <- bq_project_query(billing_project, weight_query) %>%
  bq_table_download()

# Merge with initiation
weight_with_glp1 <- weight_raw %>%
  mutate(measurement_date = as.Date(measurement_date)) %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days"))) %>%
  filter(!is.na(weight_kg), weight_kg > 30, weight_kg < 300)  # Basic cleaning

# Focus on patients in the short periods analysis
cohort_patients <- baseline_data_short$person_id

weight_cohort <- weight_with_glp1 %>%
  filter(person_id %in% cohort_patients)

# Calculate what weights we're actually using
cat("Calculating weights by period for cohort patients...\n\n")

period_weight_summary <- weight_cohort %>%
  mutate(
    period = case_when(
      days_from_initiation >= -90 & days_from_initiation <= -31 ~ "Baseline",
      days_from_initiation >= 1 & days_from_initiation <= 30 ~ "1-30d",
      days_from_initiation >= 31 & days_from_initiation <= 90 ~ "31-90d",
      days_from_initiation >= 91 & days_from_initiation <= 180 ~ "91-180d",
      days_from_initiation >= 181 & days_from_initiation <= 365 ~ "181-365d",
      days_from_initiation > 0 ~ "Other post",
      TRUE ~ "Other pre"
    )
  ) %>%
  filter(period %in% c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) %>%
  group_by(period) %>%
  summarize(
    n_measurements = n(),
    n_patients = n_distinct(person_id),
    mean_weight = mean(weight_kg),
    median_weight = median(weight_kg),
    sd_weight = sd(weight_kg),
    min_weight = min(weight_kg),
    max_weight = max(weight_kg),
    .groups = "drop"
  )

cat("Weight measurements by period:\n")
print(period_weight_summary)

# ========================================
# CRITICAL QUESTION 2: What's in the 91-180d period?
# ========================================

cat("\n\n========================================\n")
cat("QUESTION 2: Detailed look at 91-180d period\n")
cat("========================================\n\n")

# Get weights in 91-180d period by patient
weights_91_180 <- weight_cohort %>%
  filter(days_from_initiation >= 91, days_from_initiation <= 180) %>%
  group_by(person_id) %>%
  summarize(
    n_measurements = n(),
    min_weight = min(weight_kg),
    mean_weight = mean(weight_kg),
    max_weight = max(weight_kg),
    range_weight = max_weight - min_weight,
    .groups = "drop"
  )

cat(sprintf("Patients with weight data in 91-180d: %d\n", nrow(weights_91_180)))
cat("\nMeasurement frequency:\n")
print(table(weights_91_180$n_measurements))

cat("\nWithin-period weight variability:\n")
cat(sprintf("  Mean range: %.1f kg\n", mean(weights_91_180$range_weight)))
cat(sprintf("  Median range: %.1f kg\n", median(weights_91_180$range_weight)))
cat(sprintf("  Patients with >10kg range: %d\n", sum(weights_91_180$range_weight > 10)))

# Get baseline weights for comparison
weights_baseline <- weight_cohort %>%
  filter(days_from_initiation >= -90, days_from_initiation <= -31) %>%
  group_by(person_id) %>%
  summarize(
    baseline_weight = max(weight_kg),  # Using MAX as in analysis
    .groups = "drop"
  )

# Compare baseline to 91-180d
comparison_91_180 <- weights_baseline %>%
  inner_join(weights_91_180, by = "person_id") %>%
  mutate(
    change_min = min_weight - baseline_weight,
    change_mean = mean_weight - baseline_weight,
    change_max = max_weight - baseline_weight
  )

cat("\n91-180d period weight changes vs baseline:\n")
cat(sprintf("  Mean change (using MIN from period): %.1f kg\n", mean(comparison_91_180$change_min)))
cat(sprintf("  Mean change (using MEAN from period): %.1f kg\n", mean(comparison_91_180$change_mean)))
cat(sprintf("  Mean change (using MAX from period): %.1f kg\n", mean(comparison_91_180$change_max)))

# Patients showing gain
gainers_91_180 <- comparison_91_180 %>%
  filter(change_min > 0) %>%  # Even the MIN is higher than baseline
  arrange(desc(change_min))

cat(sprintf("\n\nPatients with weight GAIN in 91-180d (MIN > baseline): %d (%.1f%%)\n",
            nrow(gainers_91_180),
            100 * nrow(gainers_91_180) / nrow(comparison_91_180)))

if (nrow(gainers_91_180) > 0) {
  cat("\nTop 10 weight gainers (91-180d MIN vs baseline MAX):\n")
  print(head(gainers_91_180, 10))
}

# ========================================
# CRITICAL QUESTION 3: Are these patients on treatment?
# ========================================

cat("\n\n========================================\n")
cat("QUESTION 3: Treatment adherence in 91-180d period\n")
cat("========================================\n\n")

# Query drug exposures for weight gainers
if (nrow(gainers_91_180) > 0) {

  drug_query_gainers <- sprintf("
    SELECT
      de.person_id,
      de.drug_exposure_start_date,
      de.drug_exposure_end_date,
      c.concept_name
    FROM `%s.%s.drug_exposure` de
    JOIN `%s.%s.concept` c ON de.drug_concept_id = c.concept_id
    WHERE (LOWER(c.concept_name) LIKE '%%semaglutide%%'
       OR LOWER(c.concept_name) LIKE '%%tirzepatide%%')
      AND de.person_id IN (%s)
  ", project_id, dataset_id, project_id, dataset_id,
  paste(sprintf("'%s'", head(gainers_91_180$person_id, 100)), collapse = ", "))

  drug_gainers <- bq_project_query(billing_project, drug_query_gainers) %>%
    bq_table_download() %>%
    filter(!grepl("topical|recombinant|insulin|metformin|pioglitazone", concept_name, ignore.case = TRUE)) %>%
    mutate(
      drug_start_date = as.Date(drug_exposure_start_date),
      drug_end_date = as.Date(drug_exposure_end_date)
    ) %>%
    left_join(glp1_initiation, by = "person_id") %>%
    mutate(
      days_start_from_init = as.numeric(difftime(drug_start_date, glp1_initiation_date, units = "days")),
      days_end_from_init = as.numeric(difftime(drug_end_date, glp1_initiation_date, units = "days"))
    )

  # Check how many gainers have Rx during 91-180d period
  rx_during_91_180 <- drug_gainers %>%
    filter(
      (days_start_from_init <= 180 & (is.na(days_end_from_init) | days_end_from_init >= 91))
    ) %>%
    group_by(person_id) %>%
    summarize(
      n_rx_91_180 = n(),
      .groups = "drop"
    )

  gainers_with_rx_check <- gainers_91_180 %>%
    left_join(rx_during_91_180, by = "person_id") %>%
    mutate(n_rx_91_180 = replace_na(n_rx_91_180, 0))

  cat(sprintf("Weight gainers with active Rx during 91-180d: %d / %d (%.1f%%)\n",
              sum(gainers_with_rx_check$n_rx_91_180 > 0),
              nrow(gainers_with_rx_check),
              100 * sum(gainers_with_rx_check$n_rx_91_180 > 0) / nrow(gainers_with_rx_check)))

  cat("\nDistribution of Rx during 91-180d for weight gainers:\n")
  print(table(gainers_with_rx_check$n_rx_91_180))
}

# ========================================
# CRITICAL QUESTION 4: Individual trajectories
# ========================================

cat("\n\n========================================\n")
cat("QUESTION 4: Individual weight trajectories\n")
cat("========================================\n\n")

# Sample 5 weight gainers and plot their complete trajectories
if (nrow(gainers_91_180) > 0) {

  sample_ids <- head(gainers_91_180$person_id, 5)

  trajectories_detailed <- weight_cohort %>%
    filter(person_id %in% sample_ids) %>%
    arrange(person_id, measurement_date)

  cat("Detailed trajectories for top 5 weight gainers:\n\n")

  for (pid in sample_ids) {
    cat(sprintf("\n--- Patient %s ---\n", pid))

    patient_weights <- trajectories_detailed %>%
      filter(person_id == pid) %>%
      select(measurement_date, days_from_initiation, weight_kg)

    print(patient_weights)

    baseline_w <- patient_weights %>%
      filter(days_from_initiation >= -90, days_from_initiation <= -31) %>%
      pull(weight_kg)

    period_91_180_w <- patient_weights %>%
      filter(days_from_initiation >= 91, days_from_initiation <= 180) %>%
      pull(weight_kg)

    if (length(baseline_w) > 0 & length(period_91_180_w) > 0) {
      cat(sprintf("  Baseline: max=%.1f kg (from %d measurements)\n",
                  max(baseline_w), length(baseline_w)))
      cat(sprintf("  91-180d: min=%.1f, mean=%.1f, max=%.1f kg (from %d measurements)\n",
                  min(period_91_180_w), mean(period_91_180_w), max(period_91_180_w),
                  length(period_91_180_w)))
      cat(sprintf("  Difference: %.1f kg GAIN\n", min(period_91_180_w) - max(baseline_w)))
    }
  }

  # Plot trajectories
  p <- ggplot(trajectories_detailed, aes(x = days_from_initiation, y = weight_kg,
                                          color = factor(person_id), group = person_id)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = c(-90, -31), linetype = "dotted", color = "blue") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = c(91, 180), linetype = "dotted", color = "orange") +
    labs(title = "Weight Trajectories: Top 5 Weight Gainers",
         subtitle = "Blue = baseline window, Red = initiation, Orange = 91-180d period",
         x = "Days from GLP-1 Initiation",
         y = "Weight (kg)",
         color = "Patient ID") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave("diagnostic_top_gainers_trajectories.png", p, width = 12, height = 8, dpi = 300)
  cat("\n\nPlot saved: diagnostic_top_gainers_trajectories.png\n")
}

# ========================================
# QUESTION 5: What if we use MEAN instead of MIN?
# ========================================

cat("\n\n========================================\n")
cat("QUESTION 5: Sensitivity to weight aggregation method\n")
cat("========================================\n\n")

# Calculate period weights using different methods
all_weights_by_method <- weight_cohort %>%
  mutate(
    period = case_when(
      days_from_initiation >= -90 & days_from_initiation <= -31 ~ "Baseline",
      days_from_initiation >= 91 & days_from_initiation <= 180 ~ "91-180d",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(person_id, period) %>%
  summarize(
    weight_min = min(weight_kg),
    weight_mean = mean(weight_kg),
    weight_median = median(weight_kg),
    weight_max = max(weight_kg),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(weight_min, weight_mean, weight_median, weight_max, n)
  ) %>%
  filter(!is.na(weight_min_Baseline) & !is.na(`weight_min_91-180d`))

# For baseline we use MAX, for 91-180d we test MIN vs MEAN
comparison_methods <- all_weights_by_method %>%
  mutate(
    change_using_min = `weight_min_91-180d` - weight_max_Baseline,
    change_using_mean = `weight_mean_91-180d` - weight_max_Baseline,
    change_using_median = `weight_median_91-180d` - weight_max_Baseline
  )

cat("Mean weight change by aggregation method:\n")
cat(sprintf("  Using MIN from 91-180d: %.1f kg\n", mean(comparison_methods$change_using_min)))
cat(sprintf("  Using MEAN from 91-180d: %.1f kg\n", mean(comparison_methods$change_using_mean)))
cat(sprintf("  Using MEDIAN from 91-180d: %.1f kg\n", mean(comparison_methods$change_using_median)))

cat("\nPatients showing gain by method:\n")
cat(sprintf("  Using MIN: %d (%.1f%%)\n",
            sum(comparison_methods$change_using_min > 0),
            100 * sum(comparison_methods$change_using_min > 0) / nrow(comparison_methods)))
cat(sprintf("  Using MEAN: %d (%.1f%%)\n",
            sum(comparison_methods$change_using_mean > 0),
            100 * sum(comparison_methods$change_using_mean > 0) / nrow(comparison_methods)))
cat(sprintf("  Using MEDIAN: %d (%.1f%%)\n",
            sum(comparison_methods$change_using_median > 0),
            100 * sum(comparison_methods$change_using_median > 0) / nrow(comparison_methods)))

# ========================================
# SUMMARY AND RECOMMENDATIONS
# ========================================

cat("\n\n##################################################\n")
cat("SUMMARY AND RECOMMENDATIONS\n")
cat("##################################################\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("1. Patients with weight data in 91-180d: %d\n", nrow(weights_91_180)))
cat(sprintf("2. Patients showing weight GAIN (91-180d MIN > baseline MAX): %d\n", nrow(gainers_91_180)))
cat(sprintf("3. Mean weight change in 91-180d vs baseline: %.1f kg\n",
            mean(comparison_91_180$change_min)))

cat("\nPOSSIBLE EXPLANATIONS:\n")
cat("A. Data quality issues (measurement errors, unit conversions)\n")
cat("B. Treatment discontinuation/non-adherence\n")
cat("C. Natural weight fluctuation/rebound\n")
cat("D. Selection bias from using MIN (cherry-picking lowest weight)\n")

cat("\nRECOMMENDED ACTIONS:\n")
cat("1. Add data quality filters (remove extreme values, rapid changes)\n")
cat("2. Verify active treatment requirement at each period\n")
cat("3. Consider using MEAN instead of MIN for period weights\n")
cat("4. Add sensitivity analysis comparing aggregation methods\n")
cat("5. Consider excluding patients with weight GAIN during treatment\n")

cat("\nNEXT STEP:\n")
cat("Run data_quality_exploration.R to identify specific data issues\n")
