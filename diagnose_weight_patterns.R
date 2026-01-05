# =============================================================================
# DIAGNOSTIC ANALYSIS: Understanding Weight Loss Patterns and Variability
# =============================================================================
# Investigates why:
# 1. Weight loss is minimal during follow-up periods
# 2. Standard deviation is so large (25-28 kg)
# 3. Nadir shows much larger loss than period averages
# =============================================================================

library(tidyverse)

cat("\n##################################################\n")
cat("DIAGNOSTIC ANALYSIS: WEIGHT PATTERNS\n")
cat("##################################################\n\n")

# Load cleaned data
if (!file.exists("glp1_cleaned_data.RData")) {
  stop("ERROR: glp1_cleaned_data.RData not found!\n",
       "Please run comprehensive_data_cleaning.R first.")
}

load("glp1_cleaned_data.RData")

# Check what's in the RData file
cat("Objects in glp1_cleaned_data.RData:\n")
print(ls())

# Use the FINAL obesity cohort data, not the intermediate cleaned data
if (!exists("obesity_cohort")) {
  stop("ERROR: obesity_cohort not found in RData file!")
}

# Filter to obesity cohort ONLY
final_person_ids <- obesity_cohort$person_id

# CRITICAL: Use only obesity cohort patients
activity_with_glp1 <- activity_cleaned %>%
  filter(person_id %in% final_person_ids)

weight_with_glp1 <- weight_cleaned %>%
  filter(person_id %in% final_person_ids)

cat(sprintf("\nData loaded successfully.\n"))
cat(sprintf("Obesity cohort: %d patients\n", length(final_person_ids)))
cat(sprintf("Weight data: %d measurements\n", nrow(weight_with_glp1)))
cat(sprintf("Activity data: %d records\n\n", nrow(activity_with_glp1)))

# =============================================================================
# ANALYSIS 1: WEIGHT CHANGE DISTRIBUTION BY PERIOD
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 1: Weight Change Distribution\n")
cat("========================================\n\n")

# Define periods
periods <- list(
  "1-90d" = c(1, 90),
  "91-180d" = c(91, 180),
  "181-365d" = c(181, 365)
)

# Get baseline weight (max in -180 to 0)
baseline_weights <- weight_with_glp1 %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  filter(n() >= 2) %>%
  summarize(baseline_weight = max(weight_kg), .groups = "drop")

cat(sprintf("Patients with valid baseline weight: %d\n", nrow(baseline_weights)))

# SANITY CHECK: Verify baseline weights make sense for obesity cohort
cat("\nBaseline weight distribution (should be ≥80-90 kg for BMI ≥30):\n")
cat(sprintf("  Mean: %.1f kg\n", mean(baseline_weights$baseline_weight)))
cat(sprintf("  Median: %.1f kg\n", median(baseline_weights$baseline_weight)))
cat(sprintf("  Min: %.1f kg\n", min(baseline_weights$baseline_weight)))
cat(sprintf("  Max: %.1f kg\n", max(baseline_weights$baseline_weight)))

weight_sanity <- baseline_weights %>%
  mutate(
    weight_category = case_when(
      baseline_weight < 70 ~ "<70 kg (IMPOSSIBLE for BMI≥30)",
      baseline_weight < 80 ~ "70-80 kg (Very short only)",
      baseline_weight < 100 ~ "80-100 kg",
      baseline_weight < 120 ~ "100-120 kg",
      TRUE ~ "≥120 kg"
    )
  ) %>%
  count(weight_category) %>%
  mutate(percent = 100 * n / sum(n))

print(weight_sanity)
cat("\n")

# Analyze each period
for (period_name in names(periods)) {
  period <- periods[[period_name]]

  cat(sprintf("Period: %s (days %d-%d)\n", period_name, period[1], period[2]))
  cat("----------------------------------------\n")

  # Get minimum weight in period
  period_weights <- weight_with_glp1 %>%
    filter(days_from_initiation >= period[1],
           days_from_initiation <= period[2]) %>%
    group_by(person_id) %>%
    filter(n() >= 2) %>%
    summarize(period_weight = min(weight_kg), .groups = "drop")

  # Calculate change from baseline
  changes <- baseline_weights %>%
    inner_join(period_weights, by = "person_id") %>%
    mutate(
      weight_change_kg = period_weight - baseline_weight,
      weight_change_pct = 100 * (period_weight - baseline_weight) / baseline_weight,
      category = case_when(
        weight_change_pct <= -5 ~ "Large loss (>5%)",
        weight_change_pct <= -2 ~ "Moderate loss (2-5%)",
        weight_change_pct < 2 ~ "Stable (±2%)",
        weight_change_pct < 5 ~ "Moderate gain (2-5%)",
        TRUE ~ "Large gain (>5%)"
      )
    )

  # Summary statistics
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

  # Distribution by category
  cat("\n  Distribution by category:\n")
  category_summary <- changes %>%
    count(category) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    arrange(desc(percent))
  print(category_summary)

  # Quantiles
  cat("\n  Weight change percentiles:\n")
  quantiles <- quantile(changes$weight_change_pct, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  print(quantiles)

  cat("\n\n")
}

# =============================================================================
# ANALYSIS 2: BASELINE WEIGHT VS WEIGHT LOSS CORRELATION
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 2: Baseline Weight vs Loss\n")
cat("========================================\n\n")

# Get 1-year weight change
weight_1yr <- weight_with_glp1 %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 365) %>%
  group_by(person_id) %>%
  filter(n() >= 2) %>%
  summarize(min_weight_1yr = min(weight_kg), .groups = "drop")

baseline_vs_loss <- baseline_weights %>%
  inner_join(weight_1yr, by = "person_id") %>%
  mutate(
    weight_loss_kg = baseline_weight - min_weight_1yr,
    weight_loss_pct = 100 * weight_loss_kg / baseline_weight,
    baseline_category = case_when(
      baseline_weight < 80 ~ "<80 kg",
      baseline_weight < 100 ~ "80-100 kg",
      baseline_weight < 120 ~ "100-120 kg",
      TRUE ~ "≥120 kg"
    )
  )

cat("Weight loss by baseline weight category:\n")
baseline_summary <- baseline_vs_loss %>%
  group_by(baseline_category) %>%
  summarize(
    n = n(),
    mean_baseline = mean(baseline_weight),
    mean_loss_kg = mean(weight_loss_kg),
    mean_loss_pct = mean(weight_loss_pct),
    sd_loss_pct = sd(weight_loss_pct),
    .groups = "drop"
  )
print(baseline_summary)

cat("\nCorrelation between baseline weight and percent weight loss:\n")
cor_test <- cor.test(baseline_vs_loss$baseline_weight, baseline_vs_loss$weight_loss_pct)
cat(sprintf("  r = %.3f, p = %.4f\n\n", cor_test$estimate, cor_test$p.value))

# =============================================================================
# ANALYSIS 3: TREATMENT PERSISTENCE
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 3: Treatment Persistence\n")
cat("========================================\n\n")

# Count prescription fills per patient
fills_per_patient <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(
    n_fills = n_distinct(drug_start_date),
    first_fill = min(drug_start_date),
    last_fill = max(drug_start_date),
    days_on_treatment = as.numeric(difftime(last_fill, first_fill, units = "days")),
    .groups = "drop"
  )

cat("Prescription fills distribution:\n")
fills_summary <- fills_per_patient %>%
  count(n_fills) %>%
  mutate(percent = 100 * n / sum(n))
print(fills_summary)

cat("\nDays on treatment:\n")
cat(sprintf("  Mean: %.0f days\n", mean(fills_per_patient$days_on_treatment)))
cat(sprintf("  Median: %.0f days\n", median(fills_per_patient$days_on_treatment)))
cat(sprintf("  <90 days: %d (%.1f%%)\n",
            sum(fills_per_patient$days_on_treatment < 90),
            100 * mean(fills_per_patient$days_on_treatment < 90)))
cat(sprintf("  <180 days: %d (%.1f%%)\n",
            sum(fills_per_patient$days_on_treatment < 180),
            100 * mean(fills_per_patient$days_on_treatment < 180)))

# Weight loss by persistence
persistence_vs_loss <- fills_per_patient %>%
  inner_join(baseline_vs_loss, by = "person_id") %>%
  mutate(
    persistence_category = case_when(
      n_fills <= 2 ~ "1-2 fills",
      n_fills <= 4 ~ "3-4 fills",
      n_fills <= 6 ~ "5-6 fills",
      TRUE ~ "≥7 fills"
    )
  )

cat("\n\nWeight loss by prescription persistence:\n")
persistence_summary <- persistence_vs_loss %>%
  group_by(persistence_category) %>%
  summarize(
    n = n(),
    mean_fills = mean(n_fills),
    mean_days = mean(days_on_treatment),
    mean_loss_pct = mean(weight_loss_pct),
    sd_loss_pct = sd(weight_loss_pct),
    .groups = "drop"
  )
print(persistence_summary)

# =============================================================================
# ANALYSIS 4: RESPONDERS VS NON-RESPONDERS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 4: Responders vs Non-Responders\n")
cat("========================================\n\n")

# Define responder as ≥5% weight loss at 6 months
weight_6mo <- weight_with_glp1 %>%
  filter(days_from_initiation >= 150, days_from_initiation <= 210) %>%  # Around 6 months
  group_by(person_id) %>%
  filter(n() >= 1) %>%
  summarize(weight_6mo = min(weight_kg), .groups = "drop")

responder_analysis <- baseline_weights %>%
  inner_join(weight_6mo, by = "person_id") %>%
  mutate(
    weight_loss_pct = 100 * (baseline_weight - weight_6mo) / baseline_weight,
    responder_status = case_when(
      weight_loss_pct >= 10 ~ "Excellent (≥10%)",
      weight_loss_pct >= 5 ~ "Good (5-10%)",
      weight_loss_pct >= 0 ~ "Poor (0-5%)",
      TRUE ~ "Weight gain"
    )
  )

cat("6-month response distribution:\n")
responder_summary <- responder_analysis %>%
  count(responder_status) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(percent))
print(responder_summary)

cat("\n\nOverall 6-month outcomes:\n")
cat(sprintf("  N patients with 6-month data: %d\n", nrow(responder_analysis)))
cat(sprintf("  Mean weight loss: %.1f%%\n", mean(responder_analysis$weight_loss_pct)))
cat(sprintf("  Median weight loss: %.1f%%\n", median(responder_analysis$weight_loss_pct)))
cat(sprintf("  SD: %.1f%%\n", sd(responder_analysis$weight_loss_pct)))
cat(sprintf("  Responders (≥5%%): %d (%.1f%%)\n",
            sum(responder_analysis$weight_loss_pct >= 5),
            100 * mean(responder_analysis$weight_loss_pct >= 5)))

# =============================================================================
# ANALYSIS 5: WEIGHT MEASUREMENT FREQUENCY
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 5: Weight Measurement Frequency\n")
cat("========================================\n\n")

measurement_freq <- weight_with_glp1 %>%
  filter(days_from_initiation >= 0, days_from_initiation <= 365) %>%
  group_by(person_id) %>%
  summarize(
    n_measurements = n(),
    days_span = max(days_from_initiation) - min(days_from_initiation),
    measurements_per_month = n_measurements / (days_span / 30),
    .groups = "drop"
  )

cat("Weight measurement frequency (first year):\n")
cat(sprintf("  Mean measurements: %.1f\n", mean(measurement_freq$n_measurements)))
cat(sprintf("  Median measurements: %.0f\n", median(measurement_freq$n_measurements)))
cat(sprintf("  Mean per month: %.1f\n", mean(measurement_freq$measurements_per_month)))

freq_categories <- measurement_freq %>%
  mutate(
    freq_category = case_when(
      n_measurements <= 3 ~ "≤3 measurements",
      n_measurements <= 6 ~ "4-6 measurements",
      n_measurements <= 12 ~ "7-12 measurements",
      TRUE ~ ">12 measurements"
    )
  ) %>%
  count(freq_category) %>%
  mutate(percent = 100 * n / sum(n))

print(freq_categories)

# =============================================================================
# SAVE DIAGNOSTIC RESULTS
# =============================================================================

cat("\n========================================\n")
cat("Saving diagnostic results...\n")
cat("========================================\n\n")

save(
  changes,
  baseline_vs_loss,
  persistence_vs_loss,
  responder_analysis,
  measurement_freq,
  file = "weight_diagnostics.RData"
)

cat("Saved: weight_diagnostics.RData\n\n")

cat("##################################################\n")
cat("DIAGNOSTIC ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat("KEY FINDINGS TO REVIEW:\n")
cat("1. Weight change distribution by period\n")
cat("2. Baseline weight correlation with loss\n")
cat("3. Treatment persistence (# fills, duration)\n")
cat("4. Responder vs non-responder rates\n")
cat("5. Weight measurement frequency\n\n")

cat("These analyses will help identify:\n")
cat("  - Why average weight loss is low (non-responders?)\n")
cat("  - Why SD is high (heterogeneous response?)\n")
cat("  - Whether persistence/adherence is an issue\n")
cat("  - Whether measurement frequency affects results\n")
