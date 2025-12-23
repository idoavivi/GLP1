# =============================================================================
# Optimized Window Analysis for GLP-1 and Physical Activity
# Baseline selection and follow-up analysis with specified criteria
# =============================================================================

library(tidyverse)
library(broom)

# Load data
load("glp1_processed_data.RData")
load("windowed_analysis_results.RData")

cat("=============================================================================\n")
cat("GLP-1 ACTIVITY ANALYSIS: BASELINE AND FOLLOW-UP\n")
cat("=============================================================================\n\n")

# Get eligible patients
eligible_person_ids <- windowed_analysis_results$eligible_patients$person_id

cat(sprintf("Total eligible patients: %d\n\n", length(eligible_person_ids)))

# =============================================================================
# PART 1: BASELINE WINDOW SELECTION
# =============================================================================

cat("### PART 1: BASELINE WINDOW SELECTION ###\n\n")

# Define baseline windows (days prior to GLP-1 initiation, including day 0)
baseline_windows <- list(
  "30d" = list(days = c(-30, 0), min_activity_days = 3),
  "60d" = list(days = c(-60, 0), min_activity_days = 5),
  "90d" = list(days = c(-90, 0), min_activity_days = 7),
  "120d" = list(days = c(-120, 0), min_activity_days = 10),
  "180d" = list(days = c(-180, 0), min_activity_days = 10)
)

# Calculate metrics for each baseline window
baseline_results_list <- list()

for (window_name in names(baseline_windows)) {
  window_def <- baseline_windows[[window_name]]
  start_day <- window_def$days[1]
  end_day <- window_def$days[2]
  min_days <- window_def$min_activity_days

  cat(sprintf("Testing baseline: %s (%d to %d days, min %d activity days)\n",
              window_name, start_day, end_day, min_days))

  # Get activity data
  activity_baseline <- activity_with_glp1 %>%
    filter(person_id %in% eligible_person_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day) %>%
    group_by(person_id) %>%
    filter(n() >= min_days) %>%  # Must have minimum days
    summarize(
      baseline_steps = mean(steps, na.rm = TRUE),
      baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      baseline_light = mean(lightly_active_minutes, na.rm = TRUE),
      baseline_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      baseline_very = mean(very_active_minutes, na.rm = TRUE),
      baseline_calories = mean(activity_calories, na.rm = TRUE),
      n_activity_days = n(),
      .groups = "drop"
    )

  # Get weight data - latest weight in window for each patient
  weight_baseline <- weight_with_glp1 %>%
    filter(person_id %in% eligible_person_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    slice_max(measurement_date, n = 1, with_ties = FALSE) %>%  # Latest weight
    ungroup() %>%
    select(person_id, baseline_weight = weight_kg, baseline_bmi = bmi)

  # Combine
  baseline_combined <- activity_baseline %>%
    full_join(weight_baseline, by = "person_id") %>%
    filter(!is.na(baseline_weight) | !is.na(baseline_steps))  # At least one type of data

  # Calculate summary metrics
  baseline_results_list[[window_name]] <- tibble(
    window = window_name,
    n_patients = nrow(baseline_combined),
    n_with_weight = sum(!is.na(baseline_combined$baseline_weight)),
    n_with_activity = sum(!is.na(baseline_combined$baseline_steps)),
    mean_weight = mean(baseline_combined$baseline_weight, na.rm = TRUE),
    mean_steps = mean(baseline_combined$baseline_steps, na.rm = TRUE),
    mean_very_active = mean(baseline_combined$baseline_very, na.rm = TRUE),
    mean_calories = mean(baseline_combined$baseline_calories, na.rm = TRUE),
    total_active_min = mean(baseline_combined$baseline_light +
                           baseline_combined$baseline_fairly +
                           baseline_combined$baseline_very, na.rm = TRUE)
  )

  cat(sprintf("  N=%d patients (weight: %d, activity: %d)\n",
              nrow(baseline_combined),
              sum(!is.na(baseline_combined$baseline_weight)),
              sum(!is.na(baseline_combined$baseline_steps))))
}

baseline_comparison <- bind_rows(baseline_results_list)

cat("\n--- Baseline Window Comparison ---\n")
print(baseline_comparison, n = Inf)

# Select best baseline: highest n_patients, then highest weight, then most activity
best_baseline <- baseline_comparison %>%
  arrange(desc(n_patients), desc(mean_weight), desc(total_active_min)) %>%
  slice(1)

cat(sprintf("\n*** SELECTED BASELINE: %s ***\n", best_baseline$window))
cat(sprintf("N = %d patients\n", best_baseline$n_patients))
cat(sprintf("Mean weight: %.1f kg\n", best_baseline$mean_weight))
cat(sprintf("Mean steps: %.0f steps/day\n", best_baseline$mean_steps))
cat(sprintf("Mean very active: %.1f min/day\n\n", best_baseline$mean_very_active))

# Extract selected baseline data
selected_window <- baseline_windows[[best_baseline$window]]
baseline_start <- selected_window$days[1]
baseline_end <- selected_window$days[2]
baseline_min_days <- selected_window$min_activity_days

# Get final baseline data for selected window
baseline_activity <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_start,
         days_from_initiation <= baseline_end) %>%
  group_by(person_id) %>%
  filter(n() >= baseline_min_days) %>%
  summarize(
    baseline_steps = mean(steps, na.rm = TRUE),
    baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    baseline_light = mean(lightly_active_minutes, na.rm = TRUE),
    baseline_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    baseline_very = mean(very_active_minutes, na.rm = TRUE),
    baseline_calories = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  )

baseline_weight <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_start,
         days_from_initiation <= baseline_end,
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  slice_max(measurement_date, n = 1) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg, baseline_bmi = bmi)

# =============================================================================
# PART 2: FOLLOW-UP TIMEPOINTS
# =============================================================================

cat("\n### PART 2: FOLLOW-UP TIMEPOINTS (VECTORIZED) ###\n\n")

# Define follow-up timepoints
followup_timepoints <- c(30, 60, 90, 120, 180, 365)

# Follow-up parameters
followup_weight_window <- 15  # ±15 days for weight
followup_activity_window <- 15  # ±15 days for activity
followup_min_activity_days <- 3  # minimum 3 days
active_rx_window <- 90  # prescription within 90 days

cat("Using vectorized approach for follow-up analysis...\n")
cat(sprintf("Timepoints: %s days\n", paste(followup_timepoints, collapse = ", ")))
cat(sprintf("Weight window: ±%d days | Activity window: ±%d days\n",
            followup_weight_window, followup_activity_window))
cat(sprintf("Active Rx window: %d days\n\n", active_rx_window))

# =============================================================================
# Step 1: Create patient-timepoint grid
# =============================================================================

cat("Step 1: Creating patient-timepoint combinations...\n")

patient_timepoint_grid <- glp1_initiation %>%
  filter(person_id %in% eligible_person_ids) %>%
  select(person_id, glp1_initiation_date) %>%
  crossing(timepoint_days = followup_timepoints) %>%
  mutate(
    target_date = glp1_initiation_date + timepoint_days,
    weight_window_start = target_date - followup_weight_window,
    weight_window_end = target_date + followup_weight_window,
    activity_window_start = target_date - followup_activity_window,
    activity_window_end = target_date + followup_activity_window,
    active_rx_cutoff = target_date - active_rx_window
  )

cat(sprintf("  %d combinations created\n\n", nrow(patient_timepoint_grid)))

# =============================================================================
# Step 2: Vectorized active treatment check
# =============================================================================

cat("Step 2: Checking active treatment (vectorized)...\n")

drug_glp1_clean <- drug_glp1 %>%
  filter(!is.na(drug_start_date)) %>%
  mutate(drug_start_date = as.Date(drug_start_date))

active_treatment <- patient_timepoint_grid %>%
  left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
  mutate(
    is_active = drug_start_date <= target_date &
                drug_start_date >= active_rx_cutoff
  ) %>%
  group_by(person_id, timepoint_days, target_date, weight_window_start, weight_window_end,
           activity_window_start, activity_window_end, glp1_initiation_date) %>%
  summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
  filter(has_active_rx)

cat(sprintf("  %d combinations with active treatment\n\n", nrow(active_treatment)))

# =============================================================================
# Step 3: Calculate weight metrics (vectorized)
# =============================================================================

cat("Step 3: Calculating weight metrics (vectorized)...\n")

weight_followup <- active_treatment %>%
  inner_join(
    anthro_completed %>% select(person_id, measurement_date, weight_kg),
    by = "person_id",
    relationship = "many-to-many"
  ) %>%
  filter(measurement_date >= weight_window_start,
         measurement_date <= weight_window_end,
         !is.na(weight_kg)) %>%
  group_by(person_id, timepoint_days) %>%
  summarize(followup_weight = min(weight_kg, na.rm = TRUE), .groups = "drop")

cat(sprintf("  %d patient-timepoint records with weight\n\n", nrow(weight_followup)))

# =============================================================================
# Step 4: Calculate activity metrics (vectorized)
# =============================================================================

cat("Step 4: Calculating activity metrics (vectorized)...\n")

activity_followup <- active_treatment %>%
  inner_join(
    fitbit_activity_filtered %>% select(person_id, date, steps, sedentary_minutes,
                                        lightly_active_minutes, fairly_active_minutes,
                                        very_active_minutes, activity_calories),
    by = "person_id",
    relationship = "many-to-many"
  ) %>%
  filter(date >= activity_window_start,
         date <= activity_window_end) %>%
  group_by(person_id, timepoint_days) %>%
  summarize(
    n_activity_days = n(),
    followup_steps = mean(steps, na.rm = TRUE),
    followup_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    followup_light = mean(lightly_active_minutes, na.rm = TRUE),
    followup_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    followup_very = mean(very_active_minutes, na.rm = TRUE),
    followup_calories = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_activity_days >= followup_min_activity_days)

cat(sprintf("  %d patient-timepoint records with activity\n\n", nrow(activity_followup)))

# =============================================================================
# Step 5: Combine and organize by timepoint
# =============================================================================

cat("Step 5: Combining results...\n")

followup_combined <- activity_followup %>%
  full_join(weight_followup, by = c("person_id", "timepoint_days"))

followup_data_list <- list()

for (tp in followup_timepoints) {
  tp_data <- followup_combined %>%
    filter(timepoint_days == tp) %>%
    mutate(timepoint = paste0("Day ", tp)) %>%
    select(-timepoint_days)

  if (nrow(tp_data) > 0) {
    followup_data_list[[paste0("day_", tp)]] <- tp_data
    cat(sprintf("  Day %d: %d patients\n", tp, nrow(tp_data)))
  }
}

cat("\n")

# =============================================================================
# PART 3: NADIR ANALYSIS
# =============================================================================

cat("\n### PART 3: NADIR ANALYSIS (VECTORIZED) ###\n\n")

nadir_min_activity_days <- 3
nadir_activity_window <- 15

cat("Step 1: Finding nadir weights (vectorized)...\n")

# Find nadir (lowest weight) for each patient
nadir_weights <- anthro_completed %>%
  inner_join(
    glp1_initiation %>% filter(person_id %in% eligible_person_ids),
    by = "person_id"
  ) %>%
  filter(measurement_date >= glp1_initiation_date, !is.na(weight_kg)) %>%
  group_by(person_id, glp1_initiation_date) %>%
  slice_min(weight_kg, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(person_id, glp1_initiation_date, nadir_date = measurement_date,
         nadir_weight = weight_kg) %>%
  mutate(
    days_to_nadir = as.numeric(nadir_date - glp1_initiation_date),
    nadir_window_start = nadir_date - nadir_activity_window,
    nadir_window_end = nadir_date + nadir_activity_window,
    active_rx_cutoff = nadir_date - active_rx_window
  )

cat(sprintf("  %d patients with nadir weights\n\n", nrow(nadir_weights)))

cat("Step 2: Checking active treatment at nadir (vectorized)...\n")

# Check active treatment at nadir
nadir_active <- nadir_weights %>%
  left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
  mutate(
    is_active = drug_start_date <= nadir_date &
                drug_start_date >= active_rx_cutoff
  ) %>%
  group_by(person_id, nadir_date, nadir_weight, days_to_nadir,
           nadir_window_start, nadir_window_end) %>%
  summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
  filter(has_active_rx)

cat(sprintf("  %d patients with active treatment at nadir\n\n", nrow(nadir_active)))

cat("Step 3: Calculating activity at nadir (vectorized)...\n")

# Get activity around nadir
nadir_activity <- nadir_active %>%
  inner_join(
    fitbit_activity_filtered %>% select(person_id, date, steps, sedentary_minutes,
                                        lightly_active_minutes, fairly_active_minutes,
                                        very_active_minutes, activity_calories),
    by = "person_id",
    relationship = "many-to-many"
  ) %>%
  filter(date >= nadir_window_start, date <= nadir_window_end) %>%
  group_by(person_id, nadir_weight, days_to_nadir) %>%
  summarize(
    n_activity_days = n(),
    followup_steps = mean(steps, na.rm = TRUE),
    followup_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    followup_light = mean(lightly_active_minutes, na.rm = TRUE),
    followup_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    followup_very = mean(very_active_minutes, na.rm = TRUE),
    followup_calories = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_activity_days >= nadir_min_activity_days) %>%
  mutate(
    followup_weight = nadir_weight,
    timepoint = "Nadir"
  ) %>%
  select(-nadir_weight)

if (nrow(nadir_activity) > 0) {
  followup_data_list[["nadir"]] <- nadir_activity
  cat(sprintf("  Nadir: %d patients (mean %.1f days to nadir)\n\n",
              nrow(nadir_activity),
              mean(nadir_activity$days_to_nadir)))
} else {
  cat("  No patients with sufficient nadir data\n\n")
}

# =============================================================================
# PART 4: CREATE STATISTICAL COMPARISON TABLE
# =============================================================================

cat("### PART 4: STATISTICAL COMPARISON TABLE ###\n\n")

# Format p-value helper
format_pvalue <- function(p) {
  if (is.na(p)) return("NA")
  stars <- case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
  if (p < 0.001) {
    return("<0.001***")
  } else {
    return(sprintf("%.4f%s", p, stars))
  }
}

# Create comparison table
comparison_table_list <- list()

for (tp_name in names(followup_data_list)) {
  tp_data <- followup_data_list[[tp_name]]

  # Merge with baseline
  merged <- baseline_activity %>%
    inner_join(tp_data, by = "person_id") %>%
    inner_join(baseline_weight, by = "person_id") %>%
    filter(!is.na(baseline_weight), !is.na(followup_weight))

  if (nrow(merged) < 10) next

  # Statistical tests
  tests <- list(
    weight = t.test(merged$followup_weight, merged$baseline_weight, paired = TRUE),
    steps = t.test(merged$followup_steps, merged$baseline_steps, paired = TRUE),
    sedentary = t.test(merged$followup_sedentary, merged$baseline_sedentary, paired = TRUE),
    light = t.test(merged$followup_light, merged$baseline_light, paired = TRUE),
    fairly = t.test(merged$followup_fairly, merged$baseline_fairly, paired = TRUE),
    very = t.test(merged$followup_very, merged$baseline_very, paired = TRUE),
    calories = t.test(merged$followup_calories, merged$baseline_calories, paired = TRUE)
  )

  comparison_table_list[[tp_name]] <- tibble(
    Timepoint = first(merged$timepoint),
    N = nrow(merged),
    `Weight (kg)` = sprintf("%.1f ± %.1f", mean(merged$followup_weight), sd(merged$followup_weight)),
    `Weight Δ` = sprintf("%.1f (%.1f%%)",
                          mean(merged$followup_weight - merged$baseline_weight),
                          100 * mean((merged$followup_weight - merged$baseline_weight) / merged$baseline_weight)),
    `Weight p` = format_pvalue(tests$weight$p.value),
    `Steps` = sprintf("%.0f ± %.0f", mean(merged$followup_steps), sd(merged$followup_steps)),
    `Steps Δ` = sprintf("%.0f (%.1f%%)",
                         mean(merged$followup_steps - merged$baseline_steps),
                         100 * mean((merged$followup_steps - merged$baseline_steps) / merged$baseline_steps)),
    `Steps p` = format_pvalue(tests$steps$p.value),
    `Sedentary (min)` = sprintf("%.0f ± %.0f", mean(merged$followup_sedentary), sd(merged$followup_sedentary)),
    `Sedentary p` = format_pvalue(tests$sedentary$p.value),
    `Light (min)` = sprintf("%.0f ± %.0f", mean(merged$followup_light), sd(merged$followup_light)),
    `Light p` = format_pvalue(tests$light$p.value),
    `Fairly (min)` = sprintf("%.0f ± %.0f", mean(merged$followup_fairly), sd(merged$followup_fairly)),
    `Fairly p` = format_pvalue(tests$fairly$p.value),
    `Very (min)` = sprintf("%.0f ± %.0f", mean(merged$followup_very), sd(merged$followup_very)),
    `Very p` = format_pvalue(tests$very$p.value),
    `Activity Cal` = sprintf("%.0f ± %.0f", mean(merged$followup_calories), sd(merged$followup_calories)),
    `Cal p` = format_pvalue(tests$calories$p.value)
  )
}

final_table <- bind_rows(comparison_table_list)

# =============================================================================
# DISPLAY AND SAVE RESULTS
# =============================================================================

cat("=============================================================================\n")
cat("PUBLICATION TABLE: BASELINE VS FOLLOW-UP\n")
cat("=============================================================================\n\n")
cat(sprintf("Baseline: %s (%d to %d days, min %d activity days)\n",
            best_baseline$window, baseline_start, baseline_end, baseline_min_days))
cat(sprintf("Baseline N: %d patients\n", best_baseline$n_patients))
cat(sprintf("Baseline Weight: %.1f kg\n", best_baseline$mean_weight))
cat(sprintf("Baseline Steps: %.0f steps/day\n\n", best_baseline$mean_steps))
cat("Follow-up: Weight (±15d, lowest), Activity (±15d, ≥3 days)\n")
cat("Active treatment: GLP-1 prescription within 90 days\n")
cat("Nadir: Minimum 3 activity days (±15d)\n\n")
cat("*** p<0.001, ** p<0.01, * p<0.05\n\n")

print(final_table, n = Inf, width = Inf)

# Save results
write_csv(final_table, "publication_comparison_table.csv")
write_csv(baseline_comparison, "baseline_window_comparison.csv")

# Save detailed data
all_followup_data <- bind_rows(followup_data_list)
write_csv(all_followup_data, "followup_patient_data.csv")

cat("\n=== Files Saved ===\n")
cat("- publication_comparison_table.csv (main results)\n")
cat("- baseline_window_comparison.csv (baseline selection)\n")
cat("- followup_patient_data.csv (patient-level data)\n\n")

cat("=============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
