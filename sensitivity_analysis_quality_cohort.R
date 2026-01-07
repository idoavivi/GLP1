# =============================================================================
# SENSITIVITY ANALYSIS: HIGH-QUALITY COHORT
# =============================================================================
# Restrict to patients with:
# 1. Good treatment persistence (≥4 prescription fills)
# 2. Responder status (≥5% weight loss at 6 months)
# 3. Frequent weight measurements (≥4 measurements/year)
# 4. Baseline weight ≥80 kg (sanity check for BMI ≥30)
# =============================================================================

library(tidyverse)

cat("\n##################################################\n")
cat("SENSITIVITY ANALYSIS: HIGH-QUALITY COHORT\n")
cat("##################################################\n\n")

# Load cleaned data
load("glp1_cleaned_data.RData")

# Check what variables are in the RData file and adapt
cat("Available objects in RData file:\n")
cat(paste(ls(), collapse=", "), "\n\n")

if (!exists("weight_cleaned")) {
  if (exists("weight_with_glp1")) {
    weight_cleaned <- weight_with_glp1
    cat("Using 'weight_with_glp1' variable from RData file\n")
  } else if (exists("weight_final")) {
    weight_cleaned <- weight_final
    cat("Using 'weight_final' variable from RData file\n")
  } else {
    stop("ERROR: No weight data found in RData file!")
  }
}

if (!exists("activity_cleaned")) {
  if (exists("activity_with_glp1")) {
    activity_cleaned <- activity_with_glp1
    cat("Using 'activity_with_glp1' variable from RData file\n")
  } else if (exists("activity_final")) {
    activity_cleaned <- activity_final
    cat("Using 'activity_final' variable from RData file\n")
  } else {
    stop("ERROR: No activity data found in RData file!")
  }
}

if (!exists("drug_glp1_clean") && exists("drug_final")) {
  drug_glp1_clean <- drug_final
  cat("Using 'drug_final' variable from RData file\n")
}

cat("\n")

# Filter to obesity cohort
final_person_ids <- obesity_cohort$person_id

activity_with_glp1 <- activity_cleaned %>%
  filter(person_id %in% final_person_ids)

weight_with_glp1 <- weight_cleaned %>%
  filter(person_id %in% final_person_ids)

cat(sprintf("Starting cohort: %d patients\n\n", length(final_person_ids)))

# =============================================================================
# CRITERION 1: BASELINE WEIGHT ≥80 KG (SANITY CHECK)
# =============================================================================

cat("========================================\n")
cat("CRITERION 1: Baseline Weight ≥80 kg\n")
cat("========================================\n\n")

baseline_weights <- weight_with_glp1 %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  filter(n() >= 2) %>%
  summarize(baseline_weight = max(weight_kg), .groups = "drop")

valid_baseline_patients <- baseline_weights %>%
  filter(baseline_weight >= 80) %>%
  pull(person_id)

cat(sprintf("Patients with baseline weight ≥80 kg: %d\n", length(valid_baseline_patients)))
cat(sprintf("Excluded (baseline <80 kg): %d\n\n",
            nrow(baseline_weights) - length(valid_baseline_patients)))

# =============================================================================
# CRITERION 2: TREATMENT PERSISTENCE (≥4 FILLS)
# =============================================================================

cat("========================================\n")
cat("CRITERION 2: Treatment Persistence ≥4 Fills\n")
cat("========================================\n\n")

persistent_patients <- drug_glp1_clean %>%
  filter(person_id %in% valid_baseline_patients) %>%
  group_by(person_id) %>%
  summarize(
    n_fills = n_distinct(drug_start_date),
    days_on_treatment = as.numeric(difftime(max(drug_start_date), min(drug_start_date), units = "days")),
    .groups = "drop"
  ) %>%
  filter(n_fills >= 4) %>%
  pull(person_id)

cat(sprintf("Patients with ≥4 fills: %d\n", length(persistent_patients)))
cat(sprintf("Excluded (1-3 fills): %d\n\n",
            length(valid_baseline_patients) - length(persistent_patients)))

# =============================================================================
# CRITERION 3: RESPONDER STATUS (≥5% LOSS AT 6 MONTHS)
# =============================================================================

cat("========================================\n")
cat("CRITERION 3: Responder Status (≥5% at 6mo)\n")
cat("========================================\n\n")

# Get 6-month weight
weight_6mo <- weight_with_glp1 %>%
  filter(person_id %in% persistent_patients,
         days_from_initiation >= 150,
         days_from_initiation <= 210) %>%
  group_by(person_id) %>%
  filter(n() >= 1) %>%
  summarize(weight_6mo = min(weight_kg), .groups = "drop")

# Calculate response
responders <- baseline_weights %>%
  filter(person_id %in% persistent_patients) %>%
  inner_join(weight_6mo, by = "person_id") %>%
  mutate(
    weight_loss_pct = 100 * (baseline_weight - weight_6mo) / baseline_weight
  ) %>%
  filter(weight_loss_pct >= 5) %>%
  pull(person_id)

cat(sprintf("Patients with ≥5%% loss at 6 months: %d\n", length(responders)))
cat(sprintf("Excluded (non-responders): %d\n\n",
            length(persistent_patients) - length(responders)))

# =============================================================================
# CRITERION 4: FREQUENT MEASUREMENTS (≥4/YEAR)
# =============================================================================

cat("========================================\n")
cat("CRITERION 4: Frequent Measurements ≥4/year\n")
cat("========================================\n\n")

frequent_weighers <- weight_with_glp1 %>%
  filter(person_id %in% responders,
         days_from_initiation >= 0,
         days_from_initiation <= 365) %>%
  group_by(person_id) %>%
  summarize(n_measurements = n(), .groups = "drop") %>%
  filter(n_measurements >= 4) %>%
  pull(person_id)

cat(sprintf("Patients with ≥4 measurements/year: %d\n", length(frequent_weighers)))
cat(sprintf("Excluded (infrequent weighing): %d\n\n",
            length(responders) - length(frequent_weighers)))

# =============================================================================
# FINAL HIGH-QUALITY COHORT
# =============================================================================

high_quality_cohort <- frequent_weighers

cat("\n========================================\n")
cat("FINAL HIGH-QUALITY COHORT\n")
cat("========================================\n\n")

cat(sprintf("Original cohort: %d patients\n", length(final_person_ids)))
cat(sprintf("High-quality cohort: %d patients\n", length(high_quality_cohort)))
cat(sprintf("Retention rate: %.1f%%\n\n", 100 * length(high_quality_cohort) / length(final_person_ids)))

# =============================================================================
# WEIGHT TRAJECTORY ANALYSIS
# =============================================================================

cat("========================================\n")
cat("WEIGHT TRAJECTORY ANALYSIS\n")
cat("========================================\n\n")

# Define periods
periods <- list(
  "Baseline" = c(-180, 0),
  "1-90d" = c(1, 90),
  "91-180d" = c(91, 180),
  "181-365d" = c(181, 365)
)

# Get baseline for high-quality cohort
hq_baseline <- baseline_weights %>%
  filter(person_id %in% high_quality_cohort)

cat(sprintf("Baseline weight (high-quality cohort):\n"))
cat(sprintf("  N: %d\n", nrow(hq_baseline)))
cat(sprintf("  Mean: %.1f ± %.1f kg\n",
            mean(hq_baseline$baseline_weight),
            sd(hq_baseline$baseline_weight)))
cat(sprintf("  Median: %.1f kg\n\n", median(hq_baseline$baseline_weight)))

# Analyze each period
results_list <- list()

for (period_name in names(periods)[-1]) {  # Skip baseline
  period <- periods[[period_name]]

  cat(sprintf("Period: %s (days %d-%d)\n", period_name, period[1], period[2]))
  cat("----------------------------------------\n")

  # Get minimum weight in period
  period_weights <- weight_with_glp1 %>%
    filter(person_id %in% high_quality_cohort,
           days_from_initiation >= period[1],
           days_from_initiation <= period[2]) %>%
    group_by(person_id) %>%
    filter(n() >= 2) %>%
    summarize(period_weight = min(weight_kg), .groups = "drop")

  # Calculate change from baseline
  changes <- hq_baseline %>%
    inner_join(period_weights, by = "person_id") %>%
    mutate(
      weight_change_kg = period_weight - baseline_weight,
      weight_change_pct = 100 * (period_weight - baseline_weight) / baseline_weight
    )

  cat(sprintf("  N patients: %d\n", nrow(changes)))
  cat(sprintf("  Mean change: %.1f kg (%.1f%%)\n",
              mean(changes$weight_change_kg),
              mean(changes$weight_change_pct)))
  cat(sprintf("  Median change: %.1f kg (%.1f%%)\n",
              median(changes$weight_change_kg),
              median(changes$weight_change_pct)))
  cat(sprintf("  SD: %.1f kg (%.1f%%)\n",
              sd(changes$weight_change_kg),
              sd(changes$weight_change_pct)))

  # Weight test
  if (nrow(changes) >= 10) {
    weight_test <- t.test(changes$period_weight, changes$baseline_weight, paired = TRUE)
    cat(sprintf("  p-value: %.4f\n", weight_test$p.value))
  }

  # Distribution
  distribution <- changes %>%
    mutate(
      category = case_when(
        weight_change_pct <= -10 ~ "Excellent (≥10%)",
        weight_change_pct <= -5 ~ "Good (5-10%)",
        weight_change_pct <= -2 ~ "Moderate (2-5%)",
        TRUE ~ "Poor (<2%)"
      )
    ) %>%
    count(category) %>%
    mutate(percent = 100 * n / sum(n))

  cat("\n  Distribution:\n")
  print(distribution)
  cat("\n")

  # Store results
  results_list[[period_name]] <- changes
}

# =============================================================================
# ACTIVITY ANALYSIS
# =============================================================================

cat("========================================\n")
cat("ACTIVITY ANALYSIS\n")
cat("========================================\n\n")

# Get baseline activity
hq_baseline_activity <- activity_with_glp1 %>%
  filter(person_id %in% high_quality_cohort,
         days_from_initiation >= -180,
         days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(
    baseline_steps = mean(steps),
    baseline_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0)),
    .groups = "drop"
  )

cat(sprintf("Baseline activity (high-quality cohort):\n"))
cat(sprintf("  N: %d\n", nrow(hq_baseline_activity)))
cat(sprintf("  Mean steps: %.0f ± %.0f\n",
            mean(hq_baseline_activity$baseline_steps),
            sd(hq_baseline_activity$baseline_steps)))
cat(sprintf("  Mean MVPA: %.1f ± %.1f min\n\n",
            mean(hq_baseline_activity$baseline_MVPA),
            sd(hq_baseline_activity$baseline_MVPA)))

# Analyze periods
for (period_name in names(periods)[-1]) {
  period <- periods[[period_name]]

  cat(sprintf("Period: %s (days %d-%d)\n", period_name, period[1], period[2]))

  period_activity <- activity_with_glp1 %>%
    filter(person_id %in% high_quality_cohort,
           days_from_initiation >= period[1],
           days_from_initiation <= period[2],
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    summarize(
      period_steps = mean(steps),
      period_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0)),
      .groups = "drop"
    )

  activity_changes <- hq_baseline_activity %>%
    inner_join(period_activity, by = "person_id") %>%
    mutate(
      steps_change = period_steps - baseline_steps,
      steps_change_pct = 100 * steps_change / baseline_steps,
      MVPA_change = period_MVPA - baseline_MVPA
    )

  cat(sprintf("  N patients: %d\n", nrow(activity_changes)))
  cat(sprintf("  Steps change: %.0f (%.1f%%), p=%.4f\n",
              mean(activity_changes$steps_change),
              mean(activity_changes$steps_change_pct),
              if(nrow(activity_changes) >= 10) t.test(activity_changes$period_steps,
                                                       activity_changes$baseline_steps,
                                                       paired = TRUE)$p.value else NA))
  cat(sprintf("  MVPA change: %.1f min, p=%.4f\n\n",
              mean(activity_changes$MVPA_change),
              if(nrow(activity_changes) >= 10) t.test(activity_changes$period_MVPA,
                                                       activity_changes$baseline_MVPA,
                                                       paired = TRUE)$p.value else NA))
}

# =============================================================================
# COMPARISON TO FULL COHORT
# =============================================================================

cat("========================================\n")
cat("COMPARISON: HIGH-QUALITY vs FULL COHORT\n")
cat("========================================\n\n")

# Load diagnostic results for comparison
if (file.exists("weight_diagnostics.RData")) {
  load("weight_diagnostics.RData")

  # Compare 1-year weight loss
  full_cohort_1yr <- baseline_vs_loss %>%
    summarize(
      n = n(),
      mean_loss_pct = mean(weight_loss_pct),
      median_loss_pct = median(weight_loss_pct),
      sd_loss_pct = sd(weight_loss_pct)
    )

  hq_cohort_1yr <- hq_baseline %>%
    inner_join(weight_with_glp1 %>%
                 filter(person_id %in% high_quality_cohort,
                        days_from_initiation >= 1,
                        days_from_initiation <= 365) %>%
                 group_by(person_id) %>%
                 summarize(min_weight = min(weight_kg), .groups = "drop"),
               by = "person_id") %>%
    mutate(weight_loss_pct = 100 * (baseline_weight - min_weight) / baseline_weight) %>%
    summarize(
      n = n(),
      mean_loss_pct = mean(weight_loss_pct),
      median_loss_pct = median(weight_loss_pct),
      sd_loss_pct = sd(weight_loss_pct)
    )

  cat("1-Year Weight Loss:\n\n")
  cat("FULL COHORT:\n")
  cat(sprintf("  N: %d\n", full_cohort_1yr$n))
  cat(sprintf("  Mean: %.1f%% (SD: %.1f%%)\n", full_cohort_1yr$mean_loss_pct, full_cohort_1yr$sd_loss_pct))
  cat(sprintf("  Median: %.1f%%\n\n", full_cohort_1yr$median_loss_pct))

  cat("HIGH-QUALITY COHORT:\n")
  cat(sprintf("  N: %d\n", hq_cohort_1yr$n))
  cat(sprintf("  Mean: %.1f%% (SD: %.1f%%)\n", hq_cohort_1yr$mean_loss_pct, hq_cohort_1yr$sd_loss_pct))
  cat(sprintf("  Median: %.1f%%\n\n", hq_cohort_1yr$median_loss_pct))

  cat(sprintf("IMPROVEMENT:\n"))
  cat(sprintf("  Mean loss: +%.1f%% (%.1f%% → %.1f%%)\n",
              hq_cohort_1yr$mean_loss_pct - full_cohort_1yr$mean_loss_pct,
              full_cohort_1yr$mean_loss_pct,
              hq_cohort_1yr$mean_loss_pct))
  cat(sprintf("  SD reduction: -%.1f%% (%.1f%% → %.1f%%)\n",
              full_cohort_1yr$sd_loss_pct - hq_cohort_1yr$sd_loss_pct,
              full_cohort_1yr$sd_loss_pct,
              hq_cohort_1yr$sd_loss_pct))
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

save(
  high_quality_cohort,
  hq_baseline,
  hq_baseline_activity,
  results_list,
  file = "sensitivity_analysis_results.RData"
)

cat("\n\nSaved: sensitivity_analysis_results.RData\n")

cat("\n##################################################\n")
cat("SENSITIVITY ANALYSIS COMPLETE\n")
cat("##################################################\n\n")
