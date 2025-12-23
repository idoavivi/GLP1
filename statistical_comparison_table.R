# =============================================================================
# Statistical Comparison Table: Baseline vs Follow-up Timepoints
# Shows significant changes in weight and activity with p-values
# =============================================================================

library(tidyverse)
library(broom)

# Load windowed analysis results
load("windowed_analysis_results.RData")
load("glp1_processed_data.RData")

cat("=============================================================================\n")
cat("STATISTICAL COMPARISON: BASELINE VS FOLLOW-UP TIMEPOINTS\n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 1: SELECT OPTIMAL BASELINE
# =============================================================================

# Use the recommended baseline from windowed analysis
recommended_baseline <- windowed_analysis_results$recommended_baseline
baseline_window_start <- recommended_baseline$window_start
baseline_window_end <- recommended_baseline$window_end
baseline_min_days <- 7

cat(sprintf("Selected Baseline Window: %d to %d days (minimum 7 days Fitbit)\n",
            baseline_window_start, baseline_window_end))
cat(sprintf("N = %d patients in baseline\n\n", recommended_baseline$n_patients))

# =============================================================================
# STEP 2: GET BASELINE METRICS FOR EACH PATIENT
# =============================================================================

# Get eligible patients
eligible_person_ids <- windowed_analysis_results$eligible_patients$person_id

# Calculate baseline metrics per patient
baseline_patient_metrics <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_window_start,
         days_from_initiation <= baseline_window_end) %>%
  group_by(person_id) %>%
  filter(n() >= baseline_min_days) %>%
  summarize(
    baseline_steps = mean(steps, na.rm = TRUE),
    baseline_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    baseline_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    baseline_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
    baseline_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    baseline_activity_cal = mean(activity_calories, na.rm = TRUE),
    n_baseline_days = n(),
    .groups = "drop"
  )

# Get baseline weight
baseline_patient_weight <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_window_start,
         days_from_initiation <= baseline_window_end) %>%
  group_by(person_id) %>%
  slice_max(measurement_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg, baseline_bmi = bmi)

cat(sprintf("Baseline metrics calculated for %d patients\n\n",
            nrow(baseline_patient_metrics)))

# =============================================================================
# STEP 3: GET FOLLOW-UP METRICS FROM WINDOWED ANALYSIS
# =============================================================================

followup_data <- windowed_analysis_results$followup_individual
nadir_data <- windowed_analysis_results$nadir_individual

# Function to calculate paired statistics
calculate_paired_comparison <- function(baseline_metrics, baseline_weight,
                                       followup_metrics, followup_weight,
                                       timepoint_label) {

  # Merge baseline and follow-up
  merged <- baseline_metrics %>%
    inner_join(followup_metrics, by = "person_id") %>%
    inner_join(baseline_weight, by = "person_id") %>%
    inner_join(followup_weight, by = "person_id")

  n_patients <- nrow(merged)

  if (n_patients < 10) {
    return(NULL)
  }

  # Calculate statistics for each metric
  results <- tibble(
    timepoint = timepoint_label,
    n_patients = n_patients,

    # Weight
    baseline_weight_mean = mean(merged$baseline_weight, na.rm = TRUE),
    baseline_weight_sd = sd(merged$baseline_weight, na.rm = TRUE),
    followup_weight_mean = mean(merged$followup_weight, na.rm = TRUE),
    followup_weight_sd = sd(merged$followup_weight, na.rm = TRUE),
    weight_change = mean(merged$followup_weight - merged$baseline_weight, na.rm = TRUE),
    weight_change_pct = 100 * mean((merged$followup_weight - merged$baseline_weight) / merged$baseline_weight, na.rm = TRUE),
    weight_pvalue = tryCatch(t.test(merged$followup_weight, merged$baseline_weight, paired = TRUE)$p.value,
                              error = function(e) NA),

    # Steps
    baseline_steps_mean = mean(merged$baseline_steps, na.rm = TRUE),
    baseline_steps_sd = sd(merged$baseline_steps, na.rm = TRUE),
    followup_steps_mean = mean(merged$followup_steps, na.rm = TRUE),
    followup_steps_sd = sd(merged$followup_steps, na.rm = TRUE),
    steps_change = mean(merged$followup_steps - merged$baseline_steps, na.rm = TRUE),
    steps_change_pct = 100 * mean((merged$followup_steps - merged$baseline_steps) / merged$baseline_steps, na.rm = TRUE),
    steps_pvalue = tryCatch(t.test(merged$followup_steps, merged$baseline_steps, paired = TRUE)$p.value,
                             error = function(e) NA),

    # Sedentary minutes
    baseline_sedentary_mean = mean(merged$baseline_sedentary_min, na.rm = TRUE),
    baseline_sedentary_sd = sd(merged$baseline_sedentary_min, na.rm = TRUE),
    followup_sedentary_mean = mean(merged$followup_sedentary_min, na.rm = TRUE),
    followup_sedentary_sd = sd(merged$followup_sedentary_min, na.rm = TRUE),
    sedentary_change = mean(merged$followup_sedentary_min - merged$baseline_sedentary_min, na.rm = TRUE),
    sedentary_pvalue = tryCatch(t.test(merged$followup_sedentary_min, merged$baseline_sedentary_min, paired = TRUE)$p.value,
                                 error = function(e) NA),

    # Lightly active minutes
    baseline_lightly_active_mean = mean(merged$baseline_lightly_active_min, na.rm = TRUE),
    baseline_lightly_active_sd = sd(merged$baseline_lightly_active_min, na.rm = TRUE),
    followup_lightly_active_mean = mean(merged$followup_lightly_active_min, na.rm = TRUE),
    followup_lightly_active_sd = sd(merged$followup_lightly_active_min, na.rm = TRUE),
    lightly_active_change = mean(merged$followup_lightly_active_min - merged$baseline_lightly_active_min, na.rm = TRUE),
    lightly_active_pvalue = tryCatch(t.test(merged$followup_lightly_active_min, merged$baseline_lightly_active_min, paired = TRUE)$p.value,
                                      error = function(e) NA),

    # Fairly active minutes
    baseline_fairly_active_mean = mean(merged$baseline_fairly_active_min, na.rm = TRUE),
    baseline_fairly_active_sd = sd(merged$baseline_fairly_active_min, na.rm = TRUE),
    followup_fairly_active_mean = mean(merged$followup_fairly_active_min, na.rm = TRUE),
    followup_fairly_active_sd = sd(merged$followup_fairly_active_min, na.rm = TRUE),
    fairly_active_change = mean(merged$followup_fairly_active_min - merged$baseline_fairly_active_min, na.rm = TRUE),
    fairly_active_pvalue = tryCatch(t.test(merged$followup_fairly_active_min, merged$baseline_fairly_active_min, paired = TRUE)$p.value,
                                     error = function(e) NA),

    # Very active minutes
    baseline_very_active_mean = mean(merged$baseline_very_active_min, na.rm = TRUE),
    baseline_very_active_sd = sd(merged$baseline_very_active_min, na.rm = TRUE),
    followup_very_active_mean = mean(merged$followup_very_active_min, na.rm = TRUE),
    followup_very_active_sd = sd(merged$followup_very_active_min, na.rm = TRUE),
    very_active_change = mean(merged$followup_very_active_min - merged$baseline_very_active_min, na.rm = TRUE),
    very_active_pvalue = tryCatch(t.test(merged$followup_very_active_min, merged$baseline_very_active_min, paired = TRUE)$p.value,
                                   error = function(e) NA),

    # Activity calories
    baseline_activity_cal_mean = mean(merged$baseline_activity_cal, na.rm = TRUE),
    baseline_activity_cal_sd = sd(merged$baseline_activity_cal, na.rm = TRUE),
    followup_activity_cal_mean = mean(merged$followup_activity_cal, na.rm = TRUE),
    followup_activity_cal_sd = sd(merged$followup_activity_cal, na.rm = TRUE),
    activity_cal_change = mean(merged$followup_activity_cal - merged$baseline_activity_cal, na.rm = TRUE),
    activity_cal_pvalue = tryCatch(t.test(merged$followup_activity_cal, merged$baseline_activity_cal, paired = TRUE)$p.value,
                                    error = function(e) NA)
  )

  return(results)
}

# =============================================================================
# STEP 4: CALCULATE COMPARISONS FOR ALL TIMEPOINTS
# =============================================================================

cat("Calculating paired comparisons for all timepoints...\n\n")

comparison_results <- list()

# Follow-up timepoints
if (!is.null(followup_data)) {
  for (tp in unique(followup_data$timepoint_days)) {
    tp_data <- followup_data %>%
      filter(timepoint_days == tp) %>%
      select(person_id,
             followup_steps = mean_steps,
             followup_sedentary_min = mean_sedentary_min,
             followup_lightly_active_min = mean_lightly_active_min,
             followup_fairly_active_min = mean_fairly_active_min,
             followup_very_active_min = mean_very_active_min,
             followup_activity_cal = mean_activity_calories)

    tp_weight <- followup_data %>%
      filter(timepoint_days == tp) %>%
      select(person_id, followup_weight = min_weight)

    result <- calculate_paired_comparison(
      baseline_patient_metrics,
      baseline_patient_weight,
      tp_data,
      tp_weight,
      paste0("Day ", tp)
    )

    if (!is.null(result)) {
      comparison_results[[paste0("day_", tp)]] <- result
    }
  }
}

# Nadir timepoint
if (!is.null(nadir_data)) {
  nadir_metrics <- nadir_data %>%
    select(person_id,
           followup_steps = mean_steps,
           followup_sedentary_min = mean_sedentary_min,
           followup_lightly_active_min = mean_lightly_active_min,
           followup_fairly_active_min = mean_fairly_active_min,
           followup_very_active_min = mean_very_active_min,
           followup_activity_cal = mean_activity_calories)

  nadir_weight <- nadir_data %>%
    select(person_id, followup_weight = nadir_weight)

  result <- calculate_paired_comparison(
    baseline_patient_metrics,
    baseline_patient_weight,
    nadir_metrics,
    nadir_weight,
    "Nadir"
  )

  if (!is.null(result)) {
    comparison_results[["nadir"]] <- result
  }
}

# Combine all results
all_comparisons <- bind_rows(comparison_results)

cat(sprintf("Completed comparisons for %d timepoints\n\n", nrow(all_comparisons)))

# =============================================================================
# STEP 5: CREATE PUBLICATION TABLE
# =============================================================================

# Format p-values with significance stars
format_pvalue <- function(p) {
  if (is.na(p)) return("NA")
  stars <- case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
  sprintf("%.4f%s", p, stars)
}

# Create formatted table
publication_table <- all_comparisons %>%
  mutate(
    # Weight
    `Weight (kg)` = sprintf("%.1f (%.1f)", followup_weight_mean, followup_weight_sd),
    `Weight Change` = sprintf("%.1f kg (%.1f%%)", weight_change, weight_change_pct),
    `Weight p-value` = sapply(weight_pvalue, format_pvalue),

    # Steps
    `Steps (n/day)` = sprintf("%.0f (%.0f)", followup_steps_mean, followup_steps_sd),
    `Steps Change` = sprintf("%.0f (%.1f%%)", steps_change, steps_change_pct),
    `Steps p-value` = sapply(steps_pvalue, format_pvalue),

    # Sedentary
    `Sedentary (min)` = sprintf("%.0f (%.0f)", followup_sedentary_mean, followup_sedentary_sd),
    `Sedentary Change` = sprintf("%.0f", sedentary_change),
    `Sedentary p-value` = sapply(sedentary_pvalue, format_pvalue),

    # Lightly Active
    `Light Active (min)` = sprintf("%.0f (%.0f)", followup_lightly_active_mean, followup_lightly_active_sd),
    `Light Change` = sprintf("%.0f", lightly_active_change),
    `Light p-value` = sapply(lightly_active_pvalue, format_pvalue),

    # Fairly Active
    `Fairly Active (min)` = sprintf("%.0f (%.0f)", followup_fairly_active_mean, followup_fairly_active_sd),
    `Fairly Change` = sprintf("%.0f", fairly_active_change),
    `Fairly p-value` = sapply(fairly_active_pvalue, format_pvalue),

    # Very Active
    `Very Active (min)` = sprintf("%.0f (%.0f)", followup_very_active_mean, followup_very_active_sd),
    `Very Change` = sprintf("%.0f", very_active_change),
    `Very p-value` = sapply(very_active_pvalue, format_pvalue),

    # Activity Calories
    `Activity Cal (kcal)` = sprintf("%.0f (%.0f)", followup_activity_cal_mean, followup_activity_cal_sd),
    `Cal Change` = sprintf("%.0f", activity_cal_change),
    `Cal p-value` = sapply(activity_cal_pvalue, format_pvalue)
  ) %>%
  select(
    Timepoint = timepoint,
    N = n_patients,
    `Weight (kg)`, `Weight Change`, `Weight p-value`,
    `Steps (n/day)`, `Steps Change`, `Steps p-value`,
    `Sedentary (min)`, `Sedentary Change`, `Sedentary p-value`,
    `Light Active (min)`, `Light Change`, `Light p-value`,
    `Fairly Active (min)`, `Fairly Change`, `Fairly p-value`,
    `Very Active (min)`, `Very Change`, `Very p-value`,
    `Activity Cal (kcal)`, `Cal Change`, `Cal p-value`
  )

# Print table
cat("\n=============================================================================\n")
cat("PUBLICATION TABLE: BASELINE VS FOLLOW-UP COMPARISONS\n")
cat("=============================================================================\n\n")
cat(sprintf("Baseline: %d to %d days (N = %d)\n",
            baseline_window_start, baseline_window_end,
            recommended_baseline$n_patients))
cat(sprintf("Baseline Weight: %.1f (%.1f) kg\n",
            recommended_baseline$mean_weight, recommended_baseline$sd_weight))
cat(sprintf("Baseline Steps: %.0f (%.0f) steps/day\n",
            recommended_baseline$mean_steps, recommended_baseline$sd_steps))
cat(sprintf("Baseline Activity Cal: %.0f (%.0f) kcal/day\n\n",
            recommended_baseline$mean_activity_calories,
            recommended_baseline$sd_activity_calories))

cat("Significance: *** p<0.001, ** p<0.01, * p<0.05\n")
cat("Values shown as Mean (SD)\n\n")

print(publication_table, n = Inf, width = Inf)

# =============================================================================
# STEP 6: SAVE RESULTS
# =============================================================================

# Save detailed results
write_csv(all_comparisons, "statistical_comparisons_detailed.csv")
write_csv(publication_table, "statistical_comparisons_table.csv")

# Create a simple summary for key findings
key_findings <- all_comparisons %>%
  select(timepoint, n_patients,
         weight_change, weight_change_pct, weight_pvalue,
         steps_change, steps_change_pct, steps_pvalue,
         very_active_change, very_active_pvalue,
         activity_cal_change, activity_cal_pvalue) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

write_csv(key_findings, "key_findings_summary.csv")

cat("\n=== Results Saved ===\n")
cat("  - statistical_comparisons_detailed.csv (full results)\n")
cat("  - statistical_comparisons_table.csv (publication table)\n")
cat("  - key_findings_summary.csv (brief summary)\n\n")

# =============================================================================
# STEP 7: SUMMARY OF SIGNIFICANT FINDINGS
# =============================================================================

cat("\n=============================================================================\n")
cat("SUMMARY OF SIGNIFICANT FINDINGS (p < 0.05)\n")
cat("=============================================================================\n\n")

significant_findings <- all_comparisons %>%
  mutate(
    weight_sig = ifelse(weight_pvalue < 0.05, "SIG", "NS"),
    steps_sig = ifelse(steps_pvalue < 0.05, "SIG", "NS"),
    very_active_sig = ifelse(very_active_pvalue < 0.05, "SIG", "NS"),
    activity_cal_sig = ifelse(activity_cal_pvalue < 0.05, "SIG", "NS")
  )

for (i in 1:nrow(significant_findings)) {
  cat(sprintf("--- %s (N=%d) ---\n",
              significant_findings$timepoint[i],
              significant_findings$n_patients[i]))

  # Weight
  if (significant_findings$weight_sig[i] == "SIG") {
    cat(sprintf("  Weight: %.1f kg (%.1f%%) loss, p=%.4f ***\n",
                abs(significant_findings$weight_change[i]),
                abs(significant_findings$weight_change_pct[i]),
                significant_findings$weight_pvalue[i]))
  } else {
    cat(sprintf("  Weight: %.1f kg change, p=%.4f (NS)\n",
                significant_findings$weight_change[i],
                significant_findings$weight_pvalue[i]))
  }

  # Steps
  if (significant_findings$steps_sig[i] == "SIG") {
    direction <- ifelse(significant_findings$steps_change[i] < 0, "decrease", "increase")
    cat(sprintf("  Steps: %.0f steps %s (%.1f%%), p=%.4f ***\n",
                abs(significant_findings$steps_change[i]),
                direction,
                abs(significant_findings$steps_change_pct[i]),
                significant_findings$steps_pvalue[i]))
  } else {
    cat(sprintf("  Steps: %.0f change, p=%.4f (NS)\n",
                significant_findings$steps_change[i],
                significant_findings$steps_pvalue[i]))
  }

  # Very active minutes
  if (significant_findings$very_active_sig[i] == "SIG") {
    direction <- ifelse(significant_findings$very_active_change[i] < 0, "decrease", "increase")
    cat(sprintf("  Very Active: %.1f min %s, p=%.4f ***\n",
                abs(significant_findings$very_active_change[i]),
                direction,
                significant_findings$very_active_pvalue[i]))
  }

  # Activity calories
  if (significant_findings$activity_cal_sig[i] == "SIG") {
    direction <- ifelse(significant_findings$activity_cal_change[i] < 0, "decrease", "increase")
    cat(sprintf("  Activity Calories: %.0f kcal %s, p=%.4f ***\n",
                abs(significant_findings$activity_cal_change[i]),
                direction,
                significant_findings$activity_cal_pvalue[i]))
  }

  cat("\n")
}

cat("=============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
