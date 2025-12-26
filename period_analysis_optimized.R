# =============================================================================
# Period-Based Analysis with Baseline Selection and Nadir Analysis
# =============================================================================
# Uses optimized baseline selection and focuses on key aggregated periods
# Plus nadir weight with surrounding activity data
# =============================================================================

library(tidyverse)

# Load data
load("glp1_processed_data.RData")
load("windowed_analysis_results.RData")

cat("=============================================================================\n")
cat("PERIOD-BASED ANALYSIS WITH OPTIMIZED BASELINE AND NADIR\n")
cat("=============================================================================\n\n")

# Check for optional packages
use_mixed_models <- FALSE
if (require(lme4, quietly = TRUE) && require(lmerTest, quietly = TRUE)) {
  use_mixed_models <- TRUE
  cat("Mixed effects models: ENABLED\n\n")
} else {
  cat("WARNING: lme4 and/or lmerTest not available.\n")
  cat("Proceeding with paired t-tests only.\n\n")
}

# Get eligible patients
eligible_person_ids <- windowed_analysis_results$eligible_patients$person_id

# =============================================================================
# WEAR TIME PROCESSING (CRITICAL FOR DATA QUALITY)
# =============================================================================

cat("=============================================================================\n")
cat("WEAR TIME PROCESSING\n")
cat("=============================================================================\n\n")

cat("Implementing rigorous wear time criteria for publishable results:\n")
cat("  - Valid day: ≥10 hours (600 minutes) of wear time\n")
cat("  - Period requirement: ≥4 valid days (increased from ≥3)\n")
cat("  - Creating wear-adjusted proportional metrics\n\n")

# Add wear time and proportional metrics to activity data
activity_with_glp1 <- activity_with_glp1 %>%
  mutate(
    # Calculate total wear time (sum of all activity categories)
    total_wear_minutes = coalesce(sedentary_minutes, 0) +
                         coalesce(lightly_active_minutes, 0) +
                         coalesce(fairly_active_minutes, 0) +
                         coalesce(very_active_minutes, 0),

    # Valid day criterion: ≥600 minutes (10 hours) of wear
    is_valid_day = total_wear_minutes >= 600,

    # Wear-adjusted proportional metrics (% of wear time)
    pct_sedentary = if_else(total_wear_minutes > 0,
                            100 * sedentary_minutes / total_wear_minutes,
                            NA_real_),
    pct_light = if_else(total_wear_minutes > 0,
                        100 * lightly_active_minutes / total_wear_minutes,
                        NA_real_),
    pct_fairly = if_else(total_wear_minutes > 0,
                         100 * fairly_active_minutes / total_wear_minutes,
                         NA_real_),
    pct_very = if_else(total_wear_minutes > 0,
                       100 * very_active_minutes / total_wear_minutes,
                       NA_real_),
    pct_MVPA = if_else(total_wear_minutes > 0,
                       100 * (coalesce(fairly_active_minutes, 0) +
                              coalesce(very_active_minutes, 0)) / total_wear_minutes,
                       NA_real_)
  )

# Report wear time statistics
total_activity_days <- nrow(activity_with_glp1)
valid_days <- sum(activity_with_glp1$is_valid_day, na.rm = TRUE)
wear_time_stats <- activity_with_glp1 %>%
  summarize(
    mean_wear = mean(total_wear_minutes, na.rm = TRUE),
    median_wear = median(total_wear_minutes, na.rm = TRUE),
    pct_valid = 100 * mean(is_valid_day, na.rm = TRUE)
  )

cat(sprintf("Total activity days: %d\n", total_activity_days))
cat(sprintf("Valid days (≥10h wear): %d (%.1f%%)\n",
            valid_days,
            100 * valid_days / total_activity_days))
cat(sprintf("Mean wear time: %.1f minutes (%.1f hours)\n",
            wear_time_stats$mean_wear,
            wear_time_stats$mean_wear / 60))
cat(sprintf("Median wear time: %.1f minutes (%.1f hours)\n\n",
            wear_time_stats$median_wear,
            wear_time_stats$median_wear / 60))

# =============================================================================
# STEP 0: BASELINE SELECTION AND COHORT MATCHING
# =============================================================================

cat("=============================================================================\n")
cat("STEP 0: BASELINE SELECTION AND COHORT MATCHING\n")
cat("=============================================================================\n\n")

cat("Strategy: Find patients with BOTH baseline AND 1-90d data, then select best baseline\n\n")

# Prepare drug data for active treatment check
drug_glp1_clean <- drug_glp1 %>%
  filter(!is.na(drug_start_date)) %>%
  mutate(
    drug_start_date = as.Date(drug_start_date),
    drug_end_date = if_else(!is.na(drug_end_date), as.Date(drug_end_date), as.Date(NA))
  )

# Define candidate baseline windows
baseline_windows <- list(
  "30d" = c(-30, 0),
  "90d" = c(-90, 0),
  "180d" = c(-180, 0)
)

# CRITICAL REQUIREMENT CLARIFICATION:
# - Baseline: MUST have weight + activity
# - Follow-up: MUST have activity + active treatment (weight is OPTIONAL)

# First, get 1-90d period data (activity + active treatment, NO weight requirement)
cat("Step 1: Identifying patients with 1-90d activity data + active treatment...\n")

period_1_90d_activity <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= 1,
         days_from_initiation <= 90,
         is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
  group_by(person_id) %>%
  filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
  summarize(
    period_steps = mean(steps, na.rm = TRUE),
    period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    period_light = mean(lightly_active_minutes, na.rm = TRUE),
    period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    period_very = mean(very_active_minutes, na.rm = TRUE),
    period_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
    period_calories = mean(activity_calories, na.rm = TRUE),
    # Wear-adjusted proportional metrics
    period_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
    period_pct_light = mean(pct_light, na.rm = TRUE),
    period_pct_fairly = mean(pct_fairly, na.rm = TRUE),
    period_pct_very = mean(pct_very, na.rm = TRUE),
    period_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
    period_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
    n_period_days = n(),
    .groups = "drop"
  )

# Get 1-90d weight separately (OPTIONAL - for those who have it)
period_1_90d_weight <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= 1,
         days_from_initiation <= 90,
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  summarize(period_weight = min(weight_kg, na.rm = TRUE), .groups = "drop")

# Check active treatment at midpoint (day 45)
patients_with_multiple_fills <- drug_glp1_clean %>%
  filter(person_id %in% period_1_90d_activity$person_id) %>%
  group_by(person_id) %>%
  summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
  filter(n_fills >= 2) %>%
  select(person_id)

active_treatment_1_90d <- period_1_90d_activity %>%
  select(person_id) %>%
  inner_join(patients_with_multiple_fills, by = "person_id") %>%
  left_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
  mutate(
    midpoint_date = glp1_initiation_date + 45,
    active_rx_cutoff = midpoint_date - 90
  ) %>%
  left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
  mutate(
    is_active = (drug_start_date <= midpoint_date & drug_start_date >= active_rx_cutoff) |
                (drug_start_date <= midpoint_date & (is.na(drug_end_date) | drug_end_date >= midpoint_date))
  ) %>%
  group_by(person_id) %>%
  summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
  filter(has_active_rx) %>%
  select(person_id)

# Final 1-90d data with active treatment (activity required, weight optional)
period_1_90d_final <- period_1_90d_activity %>%
  inner_join(active_treatment_1_90d, by = "person_id") %>%
  left_join(period_1_90d_weight, by = "person_id")  # LEFT JOIN - weight optional

patients_with_1_90d <- period_1_90d_final$person_id
cat(sprintf("  Patients with 1-90d activity + active treatment: %d\n\n", length(patients_with_1_90d)))

# Now evaluate baseline windows ONLY for patients who have 1-90d data
# BASELINE REQUIRES: weight + activity (both mandatory)
cat("Step 2: Evaluating baseline windows for these patients (weight + activity required)...\n\n")

baseline_evaluations <- list()

for (window_name in names(baseline_windows)) {
  window <- baseline_windows[[window_name]]

  cat(sprintf("Evaluating baseline window: %d to %d days\n", window[1], window[2]))

  # Get baseline activity for patients with 1-90d data
  baseline_activity <- activity_with_glp1 %>%
    filter(person_id %in% patients_with_1_90d,
           days_from_initiation >= window[1],
           days_from_initiation <= window[2],
           is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
    group_by(person_id) %>%
    filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
    summarize(
      baseline_steps = mean(steps, na.rm = TRUE),
      baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      baseline_light = mean(lightly_active_minutes, na.rm = TRUE),
      baseline_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      baseline_very = mean(very_active_minutes, na.rm = TRUE),
      baseline_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
      baseline_calories = mean(activity_calories, na.rm = TRUE),
      # Wear-adjusted proportional metrics
      baseline_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
      baseline_pct_light = mean(pct_light, na.rm = TRUE),
      baseline_pct_fairly = mean(pct_fairly, na.rm = TRUE),
      baseline_pct_very = mean(pct_very, na.rm = TRUE),
      baseline_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
      baseline_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
      n_baseline_days = n(),
      .groups = "drop"
    )

  # Get baseline weight (HIGHEST) for patients with 1-90d data
  baseline_weight <- weight_with_glp1 %>%
    filter(person_id %in% patients_with_1_90d,
           days_from_initiation >= window[1],
           days_from_initiation <= window[2],
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(baseline_weight = max(weight_kg, na.rm = TRUE), .groups = "drop")

  # Combine baseline activity + weight (require BOTH)
  baseline_combined <- baseline_activity %>%
    inner_join(baseline_weight, by = "person_id")

  # Calculate metrics for selection
  n_patients <- nrow(baseline_combined)
  mean_weight <- mean(baseline_combined$baseline_weight)
  mean_steps <- mean(baseline_combined$baseline_steps)
  mean_activity <- mean(baseline_combined$baseline_fairly + baseline_combined$baseline_very, na.rm = TRUE)

  baseline_evaluations[[window_name]] <- list(
    window = window,
    n_patients = n_patients,
    mean_weight = mean_weight,
    mean_steps = mean_steps,
    mean_activity = mean_activity,
    data = baseline_combined
  )

  cat(sprintf("  N patients with baseline data: %d\n", n_patients))
  cat(sprintf("  Mean weight: %.1f kg\n", mean_weight))
  cat(sprintf("  Mean steps: %.0f\n", mean_steps))
  cat(sprintf("  Mean activity: %.1f min/day\n\n", mean_activity))
}

# Select best baseline: highest weight AND most steps/activity
# Weight as primary criterion, then activity
selected_baseline <- NULL
max_weight <- 0
max_activity <- 0

for (window_name in names(baseline_evaluations)) {
  eval <- baseline_evaluations[[window_name]]

  if (eval$mean_weight > max_weight ||
      (eval$mean_weight == max_weight && eval$mean_activity > max_activity)) {
    max_weight <- eval$mean_weight
    max_activity <- eval$mean_activity
    selected_baseline <- window_name
  }
}

cat("=============================================================================\n")
cat(sprintf("SELECTED BASELINE: %s (days %d to %d)\n",
            selected_baseline,
            baseline_windows[[selected_baseline]][1],
            baseline_windows[[selected_baseline]][2]))
cat(sprintf("  N = %d patients\n", baseline_evaluations[[selected_baseline]]$n_patients))
cat(sprintf("  Mean weight: %.1f kg\n", baseline_evaluations[[selected_baseline]]$mean_weight))
cat(sprintf("  Mean steps: %.0f\n", baseline_evaluations[[selected_baseline]]$mean_steps))
cat("=============================================================================\n\n")

# Use selected baseline
baseline_data <- baseline_evaluations[[selected_baseline]]$data
baseline_start <- baseline_windows[[selected_baseline]][1]
baseline_end <- baseline_windows[[selected_baseline]][2]

# Final matched cohort: patients with BOTH baseline AND 1-90d data
final_cohort_ids <- baseline_data$person_id

cat("=============================================================================\n")
cat(sprintf("FINAL MATCHED COHORT: %d patients\n", length(final_cohort_ids)))
cat("  - These patients have BOTH baseline AND 1-90d data\n")
cat("  - All comparisons will use this exact same cohort\n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 1: CALCULATE FOLLOW-UP PERIODS (1-90d, 91-180d, 181-365d)
# =============================================================================

cat("=============================================================================\n")
cat("STEP 1: CALCULATING FOLLOW-UP PERIODS\n")
cat("=============================================================================\n\n")

# Define follow-up periods
time_periods <- list(
  "1-90d" = c(1, 90),
  "91-180d" = c(91, 180),
  "181-365d" = c(181, 365)
)

period_data_list <- list()

for (period_name in names(time_periods)) {
  period_range <- time_periods[[period_name]]
  start_day <- period_range[1]
  end_day <- period_range[2]
  midpoint_day <- round((start_day + end_day) / 2)

  cat(sprintf("Processing %s (days %d-%d)...\n", period_name, start_day, end_day))

  # For 1-90d, use the already-calculated data filtered to final cohort
  if (period_name == "1-90d") {
    period_combined <- period_1_90d_final %>%
      filter(person_id %in% final_cohort_ids) %>%
      mutate(period = period_name)
    period_data_list[[period_name]] <- period_combined
    cat(sprintf("  N = %d patients (matched cohort)\n", nrow(period_combined)))
    next
  }

  # Activity data - ONLY matched cohort patients
  activity_period <- activity_with_glp1 %>%
    filter(person_id %in% final_cohort_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
    group_by(person_id) %>%
    filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
    summarize(
      period_steps = mean(steps, na.rm = TRUE),
      period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      period_light = mean(lightly_active_minutes, na.rm = TRUE),
      period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      period_very = mean(very_active_minutes, na.rm = TRUE),
      period_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
      period_calories = mean(activity_calories, na.rm = TRUE),
      # Wear-adjusted proportional metrics
      period_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
      period_pct_light = mean(pct_light, na.rm = TRUE),
      period_pct_fairly = mean(pct_fairly, na.rm = TRUE),
      period_pct_very = mean(pct_very, na.rm = TRUE),
      period_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
      period_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
      n_period_days = n(),
      .groups = "drop"
    )

  # Weight data - LOWEST weight (OPTIONAL)
  weight_period <- weight_with_glp1 %>%
    filter(person_id %in% final_cohort_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(period_weight = min(weight_kg, na.rm = TRUE), .groups = "drop")

  # Check active treatment at midpoint (for patients with activity data)
  # Require ≥2 prescription fills
  patients_with_multiple_fills <- drug_glp1_clean %>%
    filter(person_id %in% activity_period$person_id) %>%
    group_by(person_id) %>%
    summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
    filter(n_fills >= 2) %>%
    select(person_id)

  # Check prescription within 90 days of midpoint
  active_treatment_status <- activity_period %>%
    select(person_id) %>%
    inner_join(patients_with_multiple_fills, by = "person_id") %>%
    left_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
    mutate(
      midpoint_date = glp1_initiation_date + midpoint_day,
      active_rx_cutoff = midpoint_date - 90
    ) %>%
    left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
    mutate(
      is_active = (drug_start_date <= midpoint_date & drug_start_date >= active_rx_cutoff) |
                  (drug_start_date <= midpoint_date & (is.na(drug_end_date) | drug_end_date >= midpoint_date))
    ) %>%
    group_by(person_id) %>%
    summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
    filter(has_active_rx) %>%
    select(person_id)

  # Combine: activity (required) + weight (optional) + active treatment (required)
  period_combined <- activity_period %>%
    inner_join(active_treatment_status, by = "person_id") %>%
    left_join(weight_period, by = "person_id") %>%
    mutate(period = period_name)

  period_data_list[[period_name]] <- period_combined

  cat(sprintf("  N = %d patients with active treatment\n", nrow(period_combined)))
}

cat("\n")

# =============================================================================
# STEP 2: CALCULATE NADIR WEIGHT AND ACTIVITY
# =============================================================================

cat("=============================================================================\n")
cat("STEP 2: CALCULATING NADIR WEIGHT AND ACTIVITY\n")
cat("=============================================================================\n\n")

# Find nadir (lowest on-treatment weight) for each patient in matched cohort
nadir_weights <- weight_with_glp1 %>%
  filter(person_id %in% final_cohort_ids,
         days_from_initiation > 0,  # MUST be on-treatment (not baseline)
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  slice_min(weight_kg, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(person_id, nadir_weight = weight_kg, nadir_date = measurement_date,
         days_to_nadir = days_from_initiation)

# Check active treatment at nadir
nadir_with_active_check <- nadir_weights %>%
  left_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
  mutate(active_rx_cutoff = nadir_date - 90) %>%
  left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
  mutate(
    is_active = (drug_start_date <= nadir_date & drug_start_date >= active_rx_cutoff) |
                (drug_start_date <= nadir_date & (is.na(drug_end_date) | drug_end_date >= nadir_date))
  ) %>%
  group_by(person_id, nadir_weight, nadir_date, days_to_nadir) %>%
  summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
  filter(has_active_rx)

cat(sprintf("Patients with nadir weight on active treatment: %d\n\n", nrow(nadir_with_active_check)))

# Get activity in ±30 day window around nadir
nadir_activity <- nadir_with_active_check %>%
  select(person_id, days_to_nadir) %>%
  left_join(activity_with_glp1, by = "person_id", relationship = "many-to-many") %>%
  mutate(
    days_from_nadir = days_from_initiation - days_to_nadir
  ) %>%
  filter(abs(days_from_nadir) <= 30,  # ±30 days from nadir
         is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
  group_by(person_id) %>%
  filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
  summarize(
    nadir_steps = mean(steps, na.rm = TRUE),
    nadir_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    nadir_light = mean(lightly_active_minutes, na.rm = TRUE),
    nadir_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    nadir_very = mean(very_active_minutes, na.rm = TRUE),
    nadir_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
    nadir_calories = mean(activity_calories, na.rm = TRUE),
    # Wear-adjusted proportional metrics
    nadir_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
    nadir_pct_light = mean(pct_light, na.rm = TRUE),
    nadir_pct_fairly = mean(pct_fairly, na.rm = TRUE),
    nadir_pct_very = mean(pct_very, na.rm = TRUE),
    nadir_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
    nadir_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
    n_nadir_days = n(),
    .groups = "drop"
  )

# Combine nadir weight and activity
nadir_data <- nadir_with_active_check %>%
  select(person_id, nadir_weight, days_to_nadir) %>%
  inner_join(nadir_activity, by = "person_id")

cat(sprintf("Patients with nadir weight and activity (≥3 days): %d\n", nrow(nadir_data)))
cat(sprintf("Mean time to nadir: %.1f ± %.1f days\n\n",
            mean(nadir_data$days_to_nadir),
            sd(nadir_data$days_to_nadir)))

# =============================================================================
# STEP 3: CREATE COMPREHENSIVE TABLE WITH PAIRED T-TESTS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 3: CREATING COMPREHENSIVE TABLE WITH STATISTICAL TESTS\n")
cat("=============================================================================\n\n")

# Function to format p-value
format_pvalue <- function(p) {
  if (is.na(p)) return("—")
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

# Initialize results
results_list <- list()

# Baseline
results_list[["Baseline"]] <- tibble(
  Timepoint = "Baseline",
  Days = sprintf("%d to %d", baseline_start, baseline_end),
  N = nrow(baseline_data),
  Weight_mean = mean(baseline_data$baseline_weight, na.rm = TRUE),
  Weight_sd = sd(baseline_data$baseline_weight, na.rm = TRUE),
  Steps_mean = mean(baseline_data$baseline_steps, na.rm = TRUE),
  Steps_sd = sd(baseline_data$baseline_steps, na.rm = TRUE),
  Sedentary_mean = mean(baseline_data$baseline_sedentary, na.rm = TRUE),
  Sedentary_sd = sd(baseline_data$baseline_sedentary, na.rm = TRUE),
  Light_mean = mean(baseline_data$baseline_light, na.rm = TRUE),
  Light_sd = sd(baseline_data$baseline_light, na.rm = TRUE),
  Fairly_mean = mean(baseline_data$baseline_fairly, na.rm = TRUE),
  Fairly_sd = sd(baseline_data$baseline_fairly, na.rm = TRUE),
  Very_mean = mean(baseline_data$baseline_very, na.rm = TRUE),
  Very_sd = sd(baseline_data$baseline_very, na.rm = TRUE),
  Calories_mean = mean(baseline_data$baseline_calories, na.rm = TRUE),
  Calories_sd = sd(baseline_data$baseline_calories, na.rm = TRUE)
)

# Process each follow-up period
for (pname in names(period_data_list)) {
  period_df <- period_data_list[[pname]]
  period_range <- time_periods[[pname]]

  # Merge with baseline for paired analysis (all patients in matched cohort)
  merged_activity <- baseline_data %>%
    inner_join(period_df, by = "person_id")

  # For weight: only those with weight data at both timepoints
  merged_weight <- merged_activity %>%
    filter(!is.na(baseline_weight), !is.na(period_weight))

  n_paired_weight <- nrow(merged_weight)
  n_paired_activity <- nrow(merged_activity)

  # Weight test (if enough paired data)
  weight_test_p <- NA_real_
  if (n_paired_weight >= 10) {
    weight_test <- t.test(merged_weight$period_weight, merged_weight$baseline_weight, paired = TRUE)
    weight_test_p <- weight_test$p.value
  }

  # Activity tests (all matched cohort)
  steps_test_p <- NA_real_
  sedentary_test_p <- NA_real_
  light_test_p <- NA_real_
  fairly_test_p <- NA_real_
  very_test_p <- NA_real_
  calories_test_p <- NA_real_

  if (n_paired_activity >= 10) {
    steps_test <- t.test(merged_activity$period_steps, merged_activity$baseline_steps, paired = TRUE)
    steps_test_p <- steps_test$p.value

    sedentary_test <- t.test(merged_activity$period_sedentary, merged_activity$baseline_sedentary, paired = TRUE)
    sedentary_test_p <- sedentary_test$p.value

    light_test <- t.test(merged_activity$period_light, merged_activity$baseline_light, paired = TRUE)
    light_test_p <- light_test$p.value

    fairly_test <- t.test(merged_activity$period_fairly, merged_activity$baseline_fairly, paired = TRUE)
    fairly_test_p <- fairly_test$p.value

    very_test <- t.test(merged_activity$period_very, merged_activity$baseline_very, paired = TRUE)
    very_test_p <- very_test$p.value

    calories_test <- t.test(merged_activity$period_calories, merged_activity$baseline_calories, paired = TRUE)
    calories_test_p <- calories_test$p.value
  }

  results_list[[pname]] <- tibble(
    Timepoint = pname,
    Days = sprintf("%d to %d", period_range[1], period_range[2]),
    N = nrow(period_df),
    Weight_mean = mean(merged_activity$period_weight, na.rm = TRUE),
    Weight_sd = sd(merged_activity$period_weight, na.rm = TRUE),
    Steps_mean = mean(merged_activity$period_steps, na.rm = TRUE),
    Steps_sd = sd(merged_activity$period_steps, na.rm = TRUE),
    Sedentary_mean = mean(merged_activity$period_sedentary, na.rm = TRUE),
    Sedentary_sd = sd(merged_activity$period_sedentary, na.rm = TRUE),
    Light_mean = mean(merged_activity$period_light, na.rm = TRUE),
    Light_sd = sd(merged_activity$period_light, na.rm = TRUE),
    Fairly_mean = mean(merged_activity$period_fairly, na.rm = TRUE),
    Fairly_sd = sd(merged_activity$period_fairly, na.rm = TRUE),
    Very_mean = mean(merged_activity$period_very, na.rm = TRUE),
    Very_sd = sd(merged_activity$period_very, na.rm = TRUE),
    Calories_mean = mean(merged_activity$period_calories, na.rm = TRUE),
    Calories_sd = sd(merged_activity$period_calories, na.rm = TRUE),
    N_paired = n_paired_activity,
    N_paired_weight = n_paired_weight,
    Weight_p = weight_test_p,
    Steps_p = steps_test_p,
    Sedentary_p = sedentary_test_p,
    Light_p = light_test_p,
    Fairly_p = fairly_test_p,
    Very_p = very_test_p,
    Calories_p = calories_test_p
  )
}

# Process nadir
merged_nadir_activity <- baseline_data %>%
  inner_join(nadir_data, by = "person_id")

merged_nadir_weight <- merged_nadir_activity %>%
  filter(!is.na(baseline_weight), !is.na(nadir_weight))

n_paired_nadir_weight <- nrow(merged_nadir_weight)
n_paired_nadir_activity <- nrow(merged_nadir_activity)

# Weight test (if enough paired data)
nadir_weight_test_p <- NA_real_
if (n_paired_nadir_weight >= 10) {
  weight_test <- t.test(merged_nadir_weight$nadir_weight, merged_nadir_weight$baseline_weight, paired = TRUE)
  nadir_weight_test_p <- weight_test$p.value
}

# Activity tests (all matched cohort with nadir data)
nadir_steps_test_p <- NA_real_
nadir_sedentary_test_p <- NA_real_
nadir_light_test_p <- NA_real_
nadir_fairly_test_p <- NA_real_
nadir_very_test_p <- NA_real_
nadir_calories_test_p <- NA_real_

if (n_paired_nadir_activity >= 10) {
  steps_test <- t.test(merged_nadir_activity$nadir_steps, merged_nadir_activity$baseline_steps, paired = TRUE)
  nadir_steps_test_p <- steps_test$p.value

  sedentary_test <- t.test(merged_nadir_activity$nadir_sedentary, merged_nadir_activity$baseline_sedentary, paired = TRUE)
  nadir_sedentary_test_p <- sedentary_test$p.value

  light_test <- t.test(merged_nadir_activity$nadir_light, merged_nadir_activity$baseline_light, paired = TRUE)
  nadir_light_test_p <- light_test$p.value

  fairly_test <- t.test(merged_nadir_activity$nadir_fairly, merged_nadir_activity$baseline_fairly, paired = TRUE)
  nadir_fairly_test_p <- fairly_test$p.value

  very_test <- t.test(merged_nadir_activity$nadir_very, merged_nadir_activity$baseline_very, paired = TRUE)
  nadir_very_test_p <- very_test$p.value

  calories_test <- t.test(merged_nadir_activity$nadir_calories, merged_nadir_activity$baseline_calories, paired = TRUE)
  nadir_calories_test_p <- calories_test$p.value
}

mean_days_to_nadir <- mean(merged_nadir_activity$days_to_nadir)
sd_days_to_nadir <- sd(merged_nadir_activity$days_to_nadir)

results_list[["Nadir"]] <- tibble(
  Timepoint = "Nadir",
  Days = sprintf("%.0f ± %.0f", mean_days_to_nadir, sd_days_to_nadir),
  N = nrow(nadir_data),
  Weight_mean = mean(merged_nadir_activity$nadir_weight, na.rm = TRUE),
  Weight_sd = sd(merged_nadir_activity$nadir_weight, na.rm = TRUE),
  Steps_mean = mean(merged_nadir_activity$nadir_steps, na.rm = TRUE),
  Steps_sd = sd(merged_nadir_activity$nadir_steps, na.rm = TRUE),
  Sedentary_mean = mean(merged_nadir_activity$nadir_sedentary, na.rm = TRUE),
  Sedentary_sd = sd(merged_nadir_activity$nadir_sedentary, na.rm = TRUE),
  Light_mean = mean(merged_nadir_activity$nadir_light, na.rm = TRUE),
  Light_sd = sd(merged_nadir_activity$nadir_light, na.rm = TRUE),
  Fairly_mean = mean(merged_nadir_activity$nadir_fairly, na.rm = TRUE),
  Fairly_sd = sd(merged_nadir_activity$nadir_fairly, na.rm = TRUE),
  Very_mean = mean(merged_nadir_activity$nadir_very, na.rm = TRUE),
  Very_sd = sd(merged_nadir_activity$nadir_very, na.rm = TRUE),
  Calories_mean = mean(merged_nadir_activity$nadir_calories, na.rm = TRUE),
  Calories_sd = sd(merged_nadir_activity$nadir_calories, na.rm = TRUE),
  N_paired = n_paired_nadir_activity,
  N_paired_weight = n_paired_nadir_weight,
  Weight_p = nadir_weight_test_p,
  Steps_p = nadir_steps_test_p,
  Sedentary_p = nadir_sedentary_test_p,
  Light_p = nadir_light_test_p,
  Fairly_p = nadir_fairly_test_p,
  Very_p = nadir_very_test_p,
  Calories_p = nadir_calories_test_p
)

# Combine all results
all_results <- bind_rows(results_list)

# Calculate changes from baseline
baseline_means <- all_results %>% filter(Timepoint == "Baseline")

comprehensive_table <- all_results %>%
  mutate(
    Weight_change = Weight_mean - baseline_means$Weight_mean,
    Weight_pct = 100 * Weight_change / baseline_means$Weight_mean,
    Steps_change = Steps_mean - baseline_means$Steps_mean,
    Steps_pct = 100 * Steps_change / baseline_means$Steps_mean,
    Sedentary_change = Sedentary_mean - baseline_means$Sedentary_mean,
    Light_change = Light_mean - baseline_means$Light_mean,
    Fairly_change = Fairly_mean - baseline_means$Fairly_mean,
    Very_change = Very_mean - baseline_means$Very_mean,
    Calories_change = Calories_mean - baseline_means$Calories_mean
  ) %>%
  mutate(
    `Weight (kg)` = ifelse(is.na(Weight_mean) | is.nan(Weight_mean), "NA",
                           sprintf("%.1f ± %.1f", Weight_mean, Weight_sd)),
    `Weight Δ` = ifelse(Timepoint == "Baseline", "—",
                        sprintf("%.1f (%.1f%%)", Weight_change, Weight_pct)),
    `Weight p` = ifelse(Timepoint == "Baseline" | is.na(Weight_p), "—",
                       sapply(Weight_p, format_pvalue)),
    `Steps (n/day)` = ifelse(is.na(Steps_mean) | is.nan(Steps_mean), "NA",
                             sprintf("%.0f ± %.0f", Steps_mean, Steps_sd)),
    `Steps Δ` = ifelse(Timepoint == "Baseline", "—",
                      sprintf("%.0f (%.1f%%)", Steps_change, Steps_pct)),
    `Steps p` = ifelse(Timepoint == "Baseline" | is.na(Steps_p), "—",
                      sapply(Steps_p, format_pvalue)),
    `Sedentary (min)` = ifelse(is.na(Sedentary_mean) | is.nan(Sedentary_mean), "NA",
                               sprintf("%.0f ± %.0f", Sedentary_mean, Sedentary_sd)),
    `Sedentary Δ` = ifelse(Timepoint == "Baseline", "—",
                          sprintf("%.0f", Sedentary_change)),
    `Sedentary p` = ifelse(Timepoint == "Baseline" | is.na(Sedentary_p), "—",
                          sapply(Sedentary_p, format_pvalue)),
    `Light (min)` = ifelse(is.na(Light_mean) | is.nan(Light_mean), "NA",
                          sprintf("%.0f ± %.0f", Light_mean, Light_sd)),
    `Light Δ` = ifelse(Timepoint == "Baseline", "—",
                      sprintf("%.0f", Light_change)),
    `Light p` = ifelse(Timepoint == "Baseline" | is.na(Light_p), "—",
                      sapply(Light_p, format_pvalue)),
    `Fairly (min)` = ifelse(is.na(Fairly_mean) | is.nan(Fairly_mean), "NA",
                           sprintf("%.0f ± %.0f", Fairly_mean, Fairly_sd)),
    `Fairly Δ` = ifelse(Timepoint == "Baseline", "—",
                       sprintf("%.0f", Fairly_change)),
    `Fairly p` = ifelse(Timepoint == "Baseline" | is.na(Fairly_p), "—",
                       sapply(Fairly_p, format_pvalue)),
    `Very (min)` = ifelse(is.na(Very_mean) | is.nan(Very_mean), "NA",
                         sprintf("%.0f ± %.0f", Very_mean, Very_sd)),
    `Very Δ` = ifelse(Timepoint == "Baseline", "—",
                     sprintf("%.0f", Very_change)),
    `Very p` = ifelse(Timepoint == "Baseline" | is.na(Very_p), "—",
                     sapply(Very_p, format_pvalue)),
    `Calories (kcal)` = ifelse(is.na(Calories_mean) | is.nan(Calories_mean), "NA",
                              sprintf("%.0f ± %.0f", Calories_mean, Calories_sd)),
    `Calories Δ` = ifelse(Timepoint == "Baseline", "—",
                         sprintf("%.0f", Calories_change)),
    `Calories p` = ifelse(Timepoint == "Baseline" | is.na(Calories_p), "—",
                         sapply(Calories_p, format_pvalue))
  ) %>%
  select(Timepoint, Days, N, N_paired,
         `Weight (kg)`, `Weight Δ`, `Weight p`,
         `Steps (n/day)`, `Steps Δ`, `Steps p`,
         `Sedentary (min)`, `Sedentary Δ`, `Sedentary p`,
         `Light (min)`, `Light Δ`, `Light p`,
         `Fairly (min)`, `Fairly Δ`, `Fairly p`,
         `Very (min)`, `Very Δ`, `Very p`,
         `Calories (kcal)`, `Calories Δ`, `Calories p`)

# =============================================================================
# DISPLAY RESULTS
# =============================================================================

cat("=============================================================================\n")
cat("COMPREHENSIVE PERIOD ANALYSIS WITH OPTIMIZED BASELINE AND NADIR\n")
cat("=============================================================================\n\n")
cat(sprintf("MATCHED COHORT: %d patients\n", nrow(baseline_data)))
cat(sprintf("  - These patients have BOTH baseline AND 1-90d data\n"))
cat(sprintf("  - Baseline N = 1-90d N = N_paired (exact match)\n\n"))
cat(sprintf("Baseline: Days %d to %d (selected for highest weight + most activity)\n",
            baseline_start, baseline_end))
cat("Follow-up Periods: 1-90d, 91-180d, 181-365d\n")
cat("Nadir: Lowest on-treatment weight (>0 days, activity = mean of ±30 days)\n\n")
cat("Measurement Strategy:\n")
cat("  - Weight: HIGHEST at baseline, LOWEST during follow-up\n")
cat("  - Activity: AVERAGE during period (≥3 days required)\n")
cat("  - Eligibility: Active treatment (≥2 fills, Rx within 90 days at period midpoint)\n\n")
cat("Statistical Tests: Paired t-tests (each timepoint vs baseline)\n")
cat("*** p<0.001, ** p<0.01, * p<0.05\n")
cat("\nNOTE: 'NA' values indicate insufficient data for that metric\n\n")

print(comprehensive_table, n = Inf, width = Inf)

cat("\n=============================================================================\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n\n")

write_csv(comprehensive_table, "period_analysis_optimized_table.csv")
cat("  ✓ period_analysis_optimized_table.csv\n")

# Save as HTML for better readability
if (require(knitr, quietly = TRUE) && require(kableExtra, quietly = TRUE)) {
  html_table <- comprehensive_table %>%
    kable(format = "html", escape = FALSE, align = "c") %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "left",
      font_size = 12
    ) %>%
    column_spec(1, bold = TRUE, width = "8em") %>%
    column_spec(2, width = "6em") %>%
    add_header_above(c(" " = 4, "Weight" = 3, "Steps" = 3, "Sedentary" = 3,
                       "Light" = 3, "Fairly" = 3, "Very" = 3, "Calories" = 3)) %>%
    footnote(
      general = c(
        sprintf("MATCHED COHORT: N=%d patients with BOTH baseline AND 1-90d data", nrow(baseline_data)),
        "Baseline N = 1-90d N = N_paired (exact match for valid paired comparisons)",
        sprintf("Baseline: Days %d to %d (selected for highest weight + most activity)", baseline_start, baseline_end),
        "Follow-up: 1-90d, 91-180d, 181-365d, plus Nadir",
        "Nadir: Lowest on-treatment weight (>0 days, activity = mean of ±30 days)",
        "Active treatment: ≥2 prescription fills, Rx within 90 days at period midpoint",
        "Statistical Tests: Paired t-tests (each timepoint vs baseline)",
        "*** p<0.001, ** p<0.01, * p<0.05",
        "NA values indicate insufficient data for that metric"
      ),
      general_title = "Notes:"
    )

  save_kable(html_table, "period_analysis_optimized_table.html")
  cat("  ✓ period_analysis_optimized_table.html (formatted HTML table)\n")
} else {
  # Fallback: simple HTML table without kableExtra
  html_output <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<style>\n",
    "body { font-family: Arial, sans-serif; margin: 20px; }\n",
    "h1 { color: #333; }\n",
    "table { border-collapse: collapse; width: 100%; margin-top: 20px; }\n",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }\n",
    "th { background-color: #4CAF50; color: white; }\n",
    "tr:nth-child(even) { background-color: #f2f2f2; }\n",
    "tr:hover { background-color: #ddd; }\n",
    ".notes { margin-top: 20px; font-size: 0.9em; color: #666; }\n",
    ".important { color: #d9534f; font-weight: bold; }\n",
    "</style>\n",
    "</head>\n<body>\n",
    "<h1>GLP-1 Period Analysis - Optimized with Baseline Selection and Nadir</h1>\n",
    sprintf("<p><span class='important'>MATCHED COHORT:</span> N=%d patients with BOTH baseline AND 1-90d data<br>\n", nrow(baseline_data)),
    "<strong>Baseline N = 1-90d N = N_paired (exact match)</strong><br><br>\n",
    sprintf("<strong>Baseline:</strong> Days %d to %d (highest weight + most activity)<br>\n", baseline_start, baseline_end),
    "<strong>Follow-up:</strong> 1-90d, 91-180d, 181-365d, plus Nadir<br>\n",
    "<strong>Nadir:</strong> Lowest on-treatment weight (>0 days, activity = mean of ±30 days)<br>\n",
    "<strong>Active treatment:</strong> ≥2 prescription fills, Rx within 90 days at period midpoint<br>\n",
    "<strong>Statistical Tests:</strong> Paired t-tests (each timepoint vs baseline)</p>\n"
  )

  # Convert table to HTML
  html_output <- paste0(html_output, "<table>\n<thead>\n<tr>\n")
  for (col in names(comprehensive_table)) {
    html_output <- paste0(html_output, "<th>", col, "</th>")
  }
  html_output <- paste0(html_output, "\n</tr>\n</thead>\n<tbody>\n")

  for (i in 1:nrow(comprehensive_table)) {
    html_output <- paste0(html_output, "<tr>\n")
    for (col in names(comprehensive_table)) {
      html_output <- paste0(html_output, "<td>", comprehensive_table[[col]][i], "</td>")
    }
    html_output <- paste0(html_output, "\n</tr>\n")
  }

  html_output <- paste0(
    html_output,
    "</tbody>\n</table>\n",
    "<div class='notes'>\n",
    "<p><strong>Notes:</strong><br>\n",
    "*** p<0.001, ** p<0.01, * p<0.05<br>\n",
    "Δ = Change from baseline<br>\n",
    "p = P-value from paired t-test<br>\n",
    "NA = Insufficient data for that metric</p>\n",
    "</div>\n",
    "</body>\n</html>"
  )

  writeLines(html_output, "period_analysis_optimized_table.html")
  cat("  ✓ period_analysis_optimized_table.html (simple HTML table)\n")
}

write_csv(all_results, "period_analysis_optimized_detailed.csv")
cat("  ✓ period_analysis_optimized_detailed.csv\n")

# Create long format data for sensitivity analysis compatibility
cat("Creating long format data for sensitivity analysis...\n")

baseline_long <- baseline_data %>%
  select(person_id,
         weight = baseline_weight,
         steps = baseline_steps,
         sedentary = baseline_sedentary,
         light = baseline_light,
         fairly = baseline_fairly,
         very = baseline_very,
         calories = baseline_calories) %>%
  mutate(period = "Baseline")

period_long_list <- list()
for (pname in names(period_data_list)) {
  period_long_list[[pname]] <- period_data_list[[pname]] %>%
    select(person_id,
           weight = period_weight,
           steps = period_steps,
           sedentary = period_sedentary,
           light = period_light,
           fairly = period_fairly,
           very = period_very,
           calories = period_calories) %>%
    mutate(period = pname)
}

all_data_long <- bind_rows(baseline_long, bind_rows(period_long_list))

cat(sprintf("  ✓ Long format: %d observations from %d patients, %d periods\n",
            nrow(all_data_long),
            n_distinct(all_data_long$person_id),
            n_distinct(all_data_long$period)))

# Save all data for further analysis
save(
  baseline_data,
  period_data_list,
  nadir_data,
  comprehensive_table,
  all_results,
  all_data_long,
  baseline_start,
  baseline_end,
  selected_baseline,
  file = "period_analysis_optimized_results.RData"
)
cat("  ✓ period_analysis_optimized_results.RData\n")

cat("\n=============================================================================\n")
cat("PERIOD ANALYSIS (1-90d, 91-180d, 181-365d) COMPLETE\n")
cat("=============================================================================\n")

# =============================================================================
# =============================================================================
# SECOND ANALYSIS: SHORTER PERIODS (1-30d, 31-90d, 91-180d, 181-365d, Nadir)
# =============================================================================
# =============================================================================

cat("\n\n")
cat("=============================================================================\n")
cat("=============================================================================\n")
cat("STARTING SECOND ANALYSIS WITH SHORTER PERIODS\n")
cat("=============================================================================\n")
cat("=============================================================================\n\n")

cat("Periods: 1-30d, 31-90d, 91-180d, 181-365d, plus Nadir\n")
cat("Baseline reference: 1-30d period\n\n")

# Get patients with 1-30d period data
period_1_30d_activity <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= 1,
         days_from_initiation <= 30,
         is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
  group_by(person_id) %>%
  filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
  summarize(
    period_steps = mean(steps, na.rm = TRUE),
    period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    period_light = mean(lightly_active_minutes, na.rm = TRUE),
    period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    period_very = mean(very_active_minutes, na.rm = TRUE),
    period_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
    period_calories = mean(activity_calories, na.rm = TRUE),
    # Wear-adjusted proportional metrics
    period_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
    period_pct_light = mean(pct_light, na.rm = TRUE),
    period_pct_fairly = mean(pct_fairly, na.rm = TRUE),
    period_pct_very = mean(pct_very, na.rm = TRUE),
    period_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
    period_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
    n_period_days = n(),
    .groups = "drop"
  )

period_1_30d_weight <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= 1,
         days_from_initiation <= 30,
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  summarize(period_weight = min(weight_kg, na.rm = TRUE), .groups = "drop")

# Check active treatment at midpoint (day 15)
patients_1_30d_fills <- drug_glp1_clean %>%
  filter(person_id %in% period_1_30d_activity$person_id) %>%
  group_by(person_id) %>%
  summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
  filter(n_fills >= 2) %>%
  select(person_id)

active_treatment_1_30d <- period_1_30d_activity %>%
  select(person_id) %>%
  inner_join(patients_1_30d_fills, by = "person_id") %>%
  left_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
  mutate(
    midpoint_date = glp1_initiation_date + 15,
    active_rx_cutoff = midpoint_date - 90
  ) %>%
  left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
  mutate(
    is_active = (drug_start_date <= midpoint_date & drug_start_date >= active_rx_cutoff) |
                (drug_start_date <= midpoint_date & (is.na(drug_end_date) | drug_end_date >= midpoint_date))
  ) %>%
  group_by(person_id) %>%
  summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
  filter(has_active_rx) %>%
  select(person_id)

period_1_30d_final <- period_1_30d_activity %>%
  inner_join(active_treatment_1_30d, by = "person_id") %>%
  left_join(period_1_30d_weight, by = "person_id")

patients_with_1_30d <- period_1_30d_final$person_id
cat(sprintf("Patients with 1-30d activity + active treatment: %d\n\n", length(patients_with_1_30d)))

# Evaluate baseline windows for these patients
cat("Evaluating baseline windows for 1-30d cohort...\n\n")

baseline_evaluations_short <- list()

for (window_name in names(baseline_windows)) {
  window <- baseline_windows[[window_name]]

  baseline_activity <- activity_with_glp1 %>%
    filter(person_id %in% patients_with_1_30d,
           days_from_initiation >= window[1],
           days_from_initiation <= window[2],
           is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
    group_by(person_id) %>%
    filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
    summarize(
      baseline_steps = mean(steps, na.rm = TRUE),
      baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      baseline_light = mean(lightly_active_minutes, na.rm = TRUE),
      baseline_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      baseline_very = mean(very_active_minutes, na.rm = TRUE),
      baseline_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
      baseline_calories = mean(activity_calories, na.rm = TRUE),
      # Wear-adjusted proportional metrics
      baseline_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
      baseline_pct_light = mean(pct_light, na.rm = TRUE),
      baseline_pct_fairly = mean(pct_fairly, na.rm = TRUE),
      baseline_pct_very = mean(pct_very, na.rm = TRUE),
      baseline_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
      baseline_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
      n_baseline_days = n(),
      .groups = "drop"
    )

  baseline_weight <- weight_with_glp1 %>%
    filter(person_id %in% patients_with_1_30d,
           days_from_initiation >= window[1],
           days_from_initiation <= window[2],
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(baseline_weight = max(weight_kg, na.rm = TRUE), .groups = "drop")

  baseline_combined <- baseline_activity %>%
    inner_join(baseline_weight, by = "person_id")

  baseline_evaluations_short[[window_name]] <- list(
    window = window,
    n_patients = nrow(baseline_combined),
    mean_weight = mean(baseline_combined$baseline_weight),
    mean_steps = mean(baseline_combined$baseline_steps),
    mean_activity = mean(baseline_combined$baseline_fairly + baseline_combined$baseline_very, na.rm = TRUE),
    data = baseline_combined
  )

  cat(sprintf("%s: N=%d, Weight=%.1f kg\n", window_name, nrow(baseline_combined), mean(baseline_combined$baseline_weight)))
}

# Select best baseline
selected_baseline_short <- NULL
max_weight <- 0
max_activity <- 0

for (window_name in names(baseline_evaluations_short)) {
  eval <- baseline_evaluations_short[[window_name]]
  if (eval$mean_weight > max_weight ||
      (eval$mean_weight == max_weight && eval$mean_activity > max_activity)) {
    max_weight <- eval$mean_weight
    max_activity <- eval$mean_activity
    selected_baseline_short <- window_name
  }
}

baseline_data_short <- baseline_evaluations_short[[selected_baseline_short]]$data
baseline_start_short <- baseline_windows[[selected_baseline_short]][1]
baseline_end_short <- baseline_windows[[selected_baseline_short]][2]
final_cohort_ids_short <- baseline_data_short$person_id

cat(sprintf("\nSelected baseline: %s (N=%d patients)\n\n", selected_baseline_short, length(final_cohort_ids_short)))

# Calculate follow-up periods
time_periods_short <- list(
  "1-30d" = c(1, 30),
  "31-90d" = c(31, 90),
  "91-180d" = c(91, 180),
  "181-365d" = c(181, 365)
)

period_data_list_short <- list()

for (period_name in names(time_periods_short)) {
  period_range <- time_periods_short[[period_name]]
  start_day <- period_range[1]
  end_day <- period_range[2]
  midpoint_day <- round((start_day + end_day) / 2)

  cat(sprintf("Processing %s...\n", period_name))

  if (period_name == "1-30d") {
    period_combined <- period_1_30d_final %>%
      filter(person_id %in% final_cohort_ids_short) %>%
      mutate(period = period_name)
    period_data_list_short[[period_name]] <- period_combined
    cat(sprintf("  N = %d\n", nrow(period_combined)))
    next
  }

  activity_period <- activity_with_glp1 %>%
    filter(person_id %in% final_cohort_ids_short,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
    group_by(person_id) %>%
    filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
    summarize(
      period_steps = mean(steps, na.rm = TRUE),
      period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      period_light = mean(lightly_active_minutes, na.rm = TRUE),
      period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      period_very = mean(very_active_minutes, na.rm = TRUE),
      period_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
      period_calories = mean(activity_calories, na.rm = TRUE),
      # Wear-adjusted proportional metrics
      period_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
      period_pct_light = mean(pct_light, na.rm = TRUE),
      period_pct_fairly = mean(pct_fairly, na.rm = TRUE),
      period_pct_very = mean(pct_very, na.rm = TRUE),
      period_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
      period_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
      n_period_days = n(),
      .groups = "drop"
    )

  weight_period <- weight_with_glp1 %>%
    filter(person_id %in% final_cohort_ids_short,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(period_weight = min(weight_kg, na.rm = TRUE), .groups = "drop")

  patients_period_fills <- drug_glp1_clean %>%
    filter(person_id %in% activity_period$person_id) %>%
    group_by(person_id) %>%
    summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
    filter(n_fills >= 2) %>%
    select(person_id)

  active_treatment_status <- activity_period %>%
    select(person_id) %>%
    inner_join(patients_period_fills, by = "person_id") %>%
    left_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
    mutate(
      midpoint_date = glp1_initiation_date + midpoint_day,
      active_rx_cutoff = midpoint_date - 90
    ) %>%
    left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
    mutate(
      is_active = (drug_start_date <= midpoint_date & drug_start_date >= active_rx_cutoff) |
                  (drug_start_date <= midpoint_date & (is.na(drug_end_date) | drug_end_date >= midpoint_date))
    ) %>%
    group_by(person_id) %>%
    summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
    filter(has_active_rx) %>%
    select(person_id)

  period_combined <- activity_period %>%
    inner_join(active_treatment_status, by = "person_id") %>%
    left_join(weight_period, by = "person_id") %>%
    mutate(period = period_name)

  period_data_list_short[[period_name]] <- period_combined
  cat(sprintf("  N = %d\n", nrow(period_combined)))
}

# Calculate nadir for short analysis
nadir_weights_short <- weight_with_glp1 %>%
  filter(person_id %in% final_cohort_ids_short,
         days_from_initiation > 0,
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  slice_min(weight_kg, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(person_id, nadir_weight = weight_kg, nadir_date = measurement_date,
         days_to_nadir = days_from_initiation)

nadir_with_active_short <- nadir_weights_short %>%
  left_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
  mutate(active_rx_cutoff = nadir_date - 90) %>%
  left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
  mutate(
    is_active = (drug_start_date <= nadir_date & drug_start_date >= active_rx_cutoff) |
                (drug_start_date <= nadir_date & (is.na(drug_end_date) | drug_end_date >= nadir_date))
  ) %>%
  group_by(person_id, nadir_weight, nadir_date, days_to_nadir) %>%
  summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
  filter(has_active_rx)

nadir_activity_short <- nadir_with_active_short %>%
  select(person_id, days_to_nadir) %>%
  left_join(activity_with_glp1, by = "person_id", relationship = "many-to-many") %>%
  mutate(days_from_nadir = days_from_initiation - days_to_nadir) %>%
  filter(abs(days_from_nadir) <= 30,
         is_valid_day == TRUE) %>%  # WEAR TIME: only valid days (≥10h)
  group_by(person_id) %>%
  filter(n() >= 4) %>%  # INCREASED FROM 3: require ≥4 valid days
  summarize(
    nadir_steps = mean(steps, na.rm = TRUE),
    nadir_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    nadir_light = mean(lightly_active_minutes, na.rm = TRUE),
    nadir_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    nadir_very = mean(very_active_minutes, na.rm = TRUE),
    nadir_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
    nadir_calories = mean(activity_calories, na.rm = TRUE),
    # Wear-adjusted proportional metrics
    nadir_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
    nadir_pct_light = mean(pct_light, na.rm = TRUE),
    nadir_pct_fairly = mean(pct_fairly, na.rm = TRUE),
    nadir_pct_very = mean(pct_very, na.rm = TRUE),
    nadir_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
    nadir_mean_wear = mean(total_wear_minutes, na.rm = TRUE),
    n_nadir_days = n(),
    .groups = "drop"
  )

nadir_data_short <- nadir_with_active_short %>%
  select(person_id, nadir_weight, days_to_nadir) %>%
  inner_join(nadir_activity_short, by = "person_id")

cat(sprintf("\nNadir: N=%d patients\n\n", nrow(nadir_data_short)))

# Create comprehensive table for short periods
results_list_short <- list()

# Baseline
results_list_short[["Baseline"]] <- tibble(
  Timepoint = "Baseline",
  Days = sprintf("%d to %d", baseline_start_short, baseline_end_short),
  N = nrow(baseline_data_short),
  Weight_mean = mean(baseline_data_short$baseline_weight, na.rm = TRUE),
  Weight_sd = sd(baseline_data_short$baseline_weight, na.rm = TRUE),
  Steps_mean = mean(baseline_data_short$baseline_steps, na.rm = TRUE),
  Steps_sd = sd(baseline_data_short$baseline_steps, na.rm = TRUE),
  Sedentary_mean = mean(baseline_data_short$baseline_sedentary, na.rm = TRUE),
  Sedentary_sd = sd(baseline_data_short$baseline_sedentary, na.rm = TRUE),
  Light_mean = mean(baseline_data_short$baseline_light, na.rm = TRUE),
  Light_sd = sd(baseline_data_short$baseline_light, na.rm = TRUE),
  Fairly_mean = mean(baseline_data_short$baseline_fairly, na.rm = TRUE),
  Fairly_sd = sd(baseline_data_short$baseline_fairly, na.rm = TRUE),
  Very_mean = mean(baseline_data_short$baseline_very, na.rm = TRUE),
  Very_sd = sd(baseline_data_short$baseline_very, na.rm = TRUE),
  Calories_mean = mean(baseline_data_short$baseline_calories, na.rm = TRUE),
  Calories_sd = sd(baseline_data_short$baseline_calories, na.rm = TRUE)
)

# Process each period (use same logic as main analysis)
for (pname in names(period_data_list_short)) {
  period_df <- period_data_list_short[[pname]]
  period_range <- time_periods_short[[pname]]

  merged_activity <- baseline_data_short %>%
    inner_join(period_df, by = "person_id")

  merged_weight <- merged_activity %>%
    filter(!is.na(baseline_weight), !is.na(period_weight))

  n_paired_weight <- nrow(merged_weight)
  n_paired_activity <- nrow(merged_activity)

  weight_test_p <- NA_real_
  if (n_paired_weight >= 10) {
    weight_test <- t.test(merged_weight$period_weight, merged_weight$baseline_weight, paired = TRUE)
    weight_test_p <- weight_test$p.value
  }

  steps_test_p <- NA_real_
  sedentary_test_p <- NA_real_
  light_test_p <- NA_real_
  fairly_test_p <- NA_real_
  very_test_p <- NA_real_
  calories_test_p <- NA_real_

  if (n_paired_activity >= 10) {
    steps_test <- t.test(merged_activity$period_steps, merged_activity$baseline_steps, paired = TRUE)
    steps_test_p <- steps_test$p.value

    sedentary_test <- t.test(merged_activity$period_sedentary, merged_activity$baseline_sedentary, paired = TRUE)
    sedentary_test_p <- sedentary_test$p.value

    light_test <- t.test(merged_activity$period_light, merged_activity$baseline_light, paired = TRUE)
    light_test_p <- light_test$p.value

    fairly_test <- t.test(merged_activity$period_fairly, merged_activity$baseline_fairly, paired = TRUE)
    fairly_test_p <- fairly_test$p.value

    very_test <- t.test(merged_activity$period_very, merged_activity$baseline_very, paired = TRUE)
    very_test_p <- very_test$p.value

    calories_test <- t.test(merged_activity$period_calories, merged_activity$baseline_calories, paired = TRUE)
    calories_test_p <- calories_test$p.value
  }

  results_list_short[[pname]] <- tibble(
    Timepoint = pname,
    Days = sprintf("%d to %d", period_range[1], period_range[2]),
    N = nrow(period_df),
    Weight_mean = mean(merged_activity$period_weight, na.rm = TRUE),
    Weight_sd = sd(merged_activity$period_weight, na.rm = TRUE),
    Steps_mean = mean(merged_activity$period_steps, na.rm = TRUE),
    Steps_sd = sd(merged_activity$period_steps, na.rm = TRUE),
    Sedentary_mean = mean(merged_activity$period_sedentary, na.rm = TRUE),
    Sedentary_sd = sd(merged_activity$period_sedentary, na.rm = TRUE),
    Light_mean = mean(merged_activity$period_light, na.rm = TRUE),
    Light_sd = sd(merged_activity$period_light, na.rm = TRUE),
    Fairly_mean = mean(merged_activity$period_fairly, na.rm = TRUE),
    Fairly_sd = sd(merged_activity$period_fairly, na.rm = TRUE),
    Very_mean = mean(merged_activity$period_very, na.rm = TRUE),
    Very_sd = sd(merged_activity$period_very, na.rm = TRUE),
    Calories_mean = mean(merged_activity$period_calories, na.rm = TRUE),
    Calories_sd = sd(merged_activity$period_calories, na.rm = TRUE),
    N_paired = n_paired_activity,
    N_paired_weight = n_paired_weight,
    Weight_p = weight_test_p,
    Steps_p = steps_test_p,
    Sedentary_p = sedentary_test_p,
    Light_p = light_test_p,
    Fairly_p = fairly_test_p,
    Very_p = very_test_p,
    Calories_p = calories_test_p
  )
}

# Process nadir
merged_nadir_activity_short <- baseline_data_short %>%
  inner_join(nadir_data_short, by = "person_id")

merged_nadir_weight_short <- merged_nadir_activity_short %>%
  filter(!is.na(baseline_weight), !is.na(nadir_weight))

n_paired_nadir_weight_short <- nrow(merged_nadir_weight_short)
n_paired_nadir_activity_short <- nrow(merged_nadir_activity_short)

nadir_weight_test_p_short <- NA_real_
if (n_paired_nadir_weight_short >= 10) {
  weight_test <- t.test(merged_nadir_weight_short$nadir_weight, merged_nadir_weight_short$baseline_weight, paired = TRUE)
  nadir_weight_test_p_short <- weight_test$p.value
}

nadir_steps_test_p_short <- NA_real_
nadir_sedentary_test_p_short <- NA_real_
nadir_light_test_p_short <- NA_real_
nadir_fairly_test_p_short <- NA_real_
nadir_very_test_p_short <- NA_real_
nadir_calories_test_p_short <- NA_real_

if (n_paired_nadir_activity_short >= 10) {
  steps_test <- t.test(merged_nadir_activity_short$nadir_steps, merged_nadir_activity_short$baseline_steps, paired = TRUE)
  nadir_steps_test_p_short <- steps_test$p.value

  sedentary_test <- t.test(merged_nadir_activity_short$nadir_sedentary, merged_nadir_activity_short$baseline_sedentary, paired = TRUE)
  nadir_sedentary_test_p_short <- sedentary_test$p.value

  light_test <- t.test(merged_nadir_activity_short$nadir_light, merged_nadir_activity_short$baseline_light, paired = TRUE)
  nadir_light_test_p_short <- light_test$p.value

  fairly_test <- t.test(merged_nadir_activity_short$nadir_fairly, merged_nadir_activity_short$baseline_fairly, paired = TRUE)
  nadir_fairly_test_p_short <- fairly_test$p.value

  very_test <- t.test(merged_nadir_activity_short$nadir_very, merged_nadir_activity_short$baseline_very, paired = TRUE)
  nadir_very_test_p_short <- very_test$p.value

  calories_test <- t.test(merged_nadir_activity_short$nadir_calories, merged_nadir_activity_short$baseline_calories, paired = TRUE)
  nadir_calories_test_p_short <- calories_test$p.value
}

mean_days_to_nadir_short <- mean(merged_nadir_activity_short$days_to_nadir)
sd_days_to_nadir_short <- sd(merged_nadir_activity_short$days_to_nadir)

results_list_short[["Nadir"]] <- tibble(
  Timepoint = "Nadir",
  Days = sprintf("%.0f ± %.0f", mean_days_to_nadir_short, sd_days_to_nadir_short),
  N = nrow(nadir_data_short),
  Weight_mean = mean(merged_nadir_activity_short$nadir_weight, na.rm = TRUE),
  Weight_sd = sd(merged_nadir_activity_short$nadir_weight, na.rm = TRUE),
  Steps_mean = mean(merged_nadir_activity_short$nadir_steps, na.rm = TRUE),
  Steps_sd = sd(merged_nadir_activity_short$nadir_steps, na.rm = TRUE),
  Sedentary_mean = mean(merged_nadir_activity_short$nadir_sedentary, na.rm = TRUE),
  Sedentary_sd = sd(merged_nadir_activity_short$nadir_sedentary, na.rm = TRUE),
  Light_mean = mean(merged_nadir_activity_short$nadir_light, na.rm = TRUE),
  Light_sd = sd(merged_nadir_activity_short$nadir_light, na.rm = TRUE),
  Fairly_mean = mean(merged_nadir_activity_short$nadir_fairly, na.rm = TRUE),
  Fairly_sd = sd(merged_nadir_activity_short$nadir_fairly, na.rm = TRUE),
  Very_mean = mean(merged_nadir_activity_short$nadir_very, na.rm = TRUE),
  Very_sd = sd(merged_nadir_activity_short$nadir_very, na.rm = TRUE),
  Calories_mean = mean(merged_nadir_activity_short$nadir_calories, na.rm = TRUE),
  Calories_sd = sd(merged_nadir_activity_short$nadir_calories, na.rm = TRUE),
  N_paired = n_paired_nadir_activity_short,
  N_paired_weight = n_paired_nadir_weight_short,
  Weight_p = nadir_weight_test_p_short,
  Steps_p = nadir_steps_test_p_short,
  Sedentary_p = nadir_sedentary_test_p_short,
  Light_p = nadir_light_test_p_short,
  Fairly_p = nadir_fairly_test_p_short,
  Very_p = nadir_very_test_p_short,
  Calories_p = nadir_calories_test_p_short
)

# Combine results and create comprehensive table
all_results_short <- bind_rows(results_list_short)

baseline_means_short <- all_results_short %>% filter(Timepoint == "Baseline")

comprehensive_table_short <- all_results_short %>%
  mutate(
    Weight_change = Weight_mean - baseline_means_short$Weight_mean,
    Weight_pct = 100 * Weight_change / baseline_means_short$Weight_mean,
    Steps_change = Steps_mean - baseline_means_short$Steps_mean,
    Steps_pct = 100 * Steps_change / baseline_means_short$Steps_mean,
    Sedentary_change = Sedentary_mean - baseline_means_short$Sedentary_mean,
    Light_change = Light_mean - baseline_means_short$Light_mean,
    Fairly_change = Fairly_mean - baseline_means_short$Fairly_mean,
    Very_change = Very_mean - baseline_means_short$Very_mean,
    Calories_change = Calories_mean - baseline_means_short$Calories_mean
  ) %>%
  mutate(
    `Weight (kg)` = ifelse(is.na(Weight_mean) | is.nan(Weight_mean), "NA",
                           sprintf("%.1f ± %.1f", Weight_mean, Weight_sd)),
    `Weight Δ` = ifelse(Timepoint == "Baseline", "—",
                        sprintf("%.1f (%.1f%%)", Weight_change, Weight_pct)),
    `Weight p` = ifelse(Timepoint == "Baseline" | is.na(Weight_p), "—",
                       sapply(Weight_p, format_pvalue)),
    `Steps (n/day)` = ifelse(is.na(Steps_mean) | is.nan(Steps_mean), "NA",
                             sprintf("%.0f ± %.0f", Steps_mean, Steps_sd)),
    `Steps Δ` = ifelse(Timepoint == "Baseline", "—",
                      sprintf("%.0f (%.1f%%)", Steps_change, Steps_pct)),
    `Steps p` = ifelse(Timepoint == "Baseline" | is.na(Steps_p), "—",
                      sapply(Steps_p, format_pvalue)),
    `Sedentary (min)` = ifelse(is.na(Sedentary_mean) | is.nan(Sedentary_mean), "NA",
                               sprintf("%.0f ± %.0f", Sedentary_mean, Sedentary_sd)),
    `Sedentary Δ` = ifelse(Timepoint == "Baseline", "—",
                          sprintf("%.0f", Sedentary_change)),
    `Sedentary p` = ifelse(Timepoint == "Baseline" | is.na(Sedentary_p), "—",
                          sapply(Sedentary_p, format_pvalue)),
    `Light (min)` = ifelse(is.na(Light_mean) | is.nan(Light_mean), "NA",
                          sprintf("%.0f ± %.0f", Light_mean, Light_sd)),
    `Light Δ` = ifelse(Timepoint == "Baseline", "—",
                      sprintf("%.0f", Light_change)),
    `Light p` = ifelse(Timepoint == "Baseline" | is.na(Light_p), "—",
                      sapply(Light_p, format_pvalue)),
    `Fairly (min)` = ifelse(is.na(Fairly_mean) | is.nan(Fairly_mean), "NA",
                           sprintf("%.0f ± %.0f", Fairly_mean, Fairly_sd)),
    `Fairly Δ` = ifelse(Timepoint == "Baseline", "—",
                       sprintf("%.0f", Fairly_change)),
    `Fairly p` = ifelse(Timepoint == "Baseline" | is.na(Fairly_p), "—",
                       sapply(Fairly_p, format_pvalue)),
    `Very (min)` = ifelse(is.na(Very_mean) | is.nan(Very_mean), "NA",
                         sprintf("%.0f ± %.0f", Very_mean, Very_sd)),
    `Very Δ` = ifelse(Timepoint == "Baseline", "—",
                     sprintf("%.0f", Very_change)),
    `Very p` = ifelse(Timepoint == "Baseline" | is.na(Very_p), "—",
                     sapply(Very_p, format_pvalue)),
    `Calories (kcal)` = ifelse(is.na(Calories_mean) | is.nan(Calories_mean), "NA",
                              sprintf("%.0f ± %.0f", Calories_mean, Calories_sd)),
    `Calories Δ` = ifelse(Timepoint == "Baseline", "—",
                         sprintf("%.0f", Calories_change)),
    `Calories p` = ifelse(Timepoint == "Baseline" | is.na(Calories_p), "—",
                         sapply(Calories_p, format_pvalue))
  ) %>%
  select(Timepoint, Days, N, N_paired,
         `Weight (kg)`, `Weight Δ`, `Weight p`,
         `Steps (n/day)`, `Steps Δ`, `Steps p`,
         `Sedentary (min)`, `Sedentary Δ`, `Sedentary p`,
         `Light (min)`, `Light Δ`, `Light p`,
         `Fairly (min)`, `Fairly Δ`, `Fairly p`,
         `Very (min)`, `Very Δ`, `Very p`,
         `Calories (kcal)`, `Calories Δ`, `Calories p`)

# Display and save results
cat("\n=============================================================================\n")
cat("SHORT PERIODS ANALYSIS (1-30d, 31-90d, 91-180d, 181-365d, Nadir)\n")
cat("=============================================================================\n\n")

print(comprehensive_table_short, n = Inf, width = Inf)

cat("\n=== SAVING SHORT PERIODS RESULTS ===\n\n")

write_csv(comprehensive_table_short, "period_analysis_short_table.csv")
cat("  ✓ period_analysis_short_table.csv\n")

# Save as HTML
if (require(knitr, quietly = TRUE) && require(kableExtra, quietly = TRUE)) {
  html_table_short <- comprehensive_table_short %>%
    kable(format = "html", escape = FALSE, align = "c") %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "left",
      font_size = 12
    ) %>%
    column_spec(1, bold = TRUE, width = "8em") %>%
    column_spec(2, width = "6em") %>%
    add_header_above(c(" " = 4, "Weight" = 3, "Steps" = 3, "Sedentary" = 3,
                       "Light" = 3, "Fairly" = 3, "Very" = 3, "Calories" = 3)) %>%
    footnote(
      general = c(
        sprintf("MATCHED COHORT: N=%d patients with BOTH baseline AND 1-30d data", nrow(baseline_data_short)),
        "Baseline N = 1-30d N = N_paired (exact match for valid paired comparisons)",
        sprintf("Baseline: Days %d to %d (selected for highest weight + most activity)", baseline_start_short, baseline_end_short),
        "Follow-up: 1-30d, 31-90d, 91-180d, 181-365d, plus Nadir",
        "Nadir: Lowest on-treatment weight (>0 days, activity = mean of ±30 days)",
        "Active treatment: ≥2 prescription fills, Rx within 90 days at period midpoint",
        "Statistical Tests: Paired t-tests (each timepoint vs baseline)",
        "*** p<0.001, ** p<0.01, * p<0.05",
        "NA values indicate insufficient data for that metric"
      ),
      general_title = "Notes:"
    )

  save_kable(html_table_short, "period_analysis_short_table.html")
  cat("  ✓ period_analysis_short_table.html (formatted HTML table)\n")
} else {
  # Fallback HTML
  html_output_short <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<style>\n",
    "body { font-family: Arial, sans-serif; margin: 20px; }\n",
    "h1 { color: #333; }\n",
    "table { border-collapse: collapse; width: 100%; margin-top: 20px; }\n",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }\n",
    "th { background-color: #4CAF50; color: white; }\n",
    "tr:nth-child(even) { background-color: #f2f2f2; }\n",
    "tr:hover { background-color: #ddd; }\n",
    ".notes { margin-top: 20px; font-size: 0.9em; color: #666; }\n",
    ".important { color: #d9534f; font-weight: bold; }\n",
    "</style>\n",
    "</head>\n<body>\n",
    "<h1>GLP-1 Period Analysis - Short Periods (1-30d, 31-90d, 91-180d, 181-365d, Nadir)</h1>\n",
    sprintf("<p><span class='important'>MATCHED COHORT:</span> N=%d patients with BOTH baseline AND 1-30d data<br>\n", nrow(baseline_data_short)),
    "<strong>Baseline N = 1-30d N = N_paired (exact match)</strong><br><br>\n",
    sprintf("<strong>Baseline:</strong> Days %d to %d (highest weight + most activity)<br>\n", baseline_start_short, baseline_end_short),
    "<strong>Follow-up:</strong> 1-30d, 31-90d, 91-180d, 181-365d, plus Nadir<br>\n",
    "<strong>Nadir:</strong> Lowest on-treatment weight (>0 days, activity = mean of ±30 days)<br>\n",
    "<strong>Active treatment:</strong> ≥2 prescription fills, Rx within 90 days at period midpoint<br>\n",
    "<strong>Statistical Tests:</strong> Paired t-tests (each timepoint vs baseline)</p>\n"
  )

  html_output_short <- paste0(html_output_short, "<table>\n<thead>\n<tr>\n")
  for (col in names(comprehensive_table_short)) {
    html_output_short <- paste0(html_output_short, "<th>", col, "</th>")
  }
  html_output_short <- paste0(html_output_short, "\n</tr>\n</thead>\n<tbody>\n")

  for (i in 1:nrow(comprehensive_table_short)) {
    html_output_short <- paste0(html_output_short, "<tr>\n")
    for (col in names(comprehensive_table_short)) {
      html_output_short <- paste0(html_output_short, "<td>", comprehensive_table_short[[col]][i], "</td>")
    }
    html_output_short <- paste0(html_output_short, "\n</tr>\n")
  }

  html_output_short <- paste0(
    html_output_short,
    "</tbody>\n</table>\n",
    "<div class='notes'>\n",
    "<p><strong>Notes:</strong><br>\n",
    "*** p<0.001, ** p<0.01, * p<0.05<br>\n",
    "Δ = Change from baseline<br>\n",
    "p = P-value from paired t-test<br>\n",
    "NA = Insufficient data for that metric</p>\n",
    "</div>\n",
    "</body>\n</html>"
  )

  writeLines(html_output_short, "period_analysis_short_table.html")
  cat("  ✓ period_analysis_short_table.html (simple HTML table)\n")
}

write_csv(all_results_short, "period_analysis_short_detailed.csv")
cat("  ✓ period_analysis_short_detailed.csv\n")

# Create long format data for sensitivity analysis compatibility
cat("Creating long format data for sensitivity analysis...\n")

baseline_long_short <- baseline_data_short %>%
  select(person_id,
         weight = baseline_weight,
         steps = baseline_steps,
         sedentary = baseline_sedentary,
         light = baseline_light,
         fairly = baseline_fairly,
         very = baseline_very,
         calories = baseline_calories) %>%
  mutate(period = "Baseline")

period_long_list_short <- list()
for (pname in names(period_data_list_short)) {
  period_long_list_short[[pname]] <- period_data_list_short[[pname]] %>%
    select(person_id,
           weight = period_weight,
           steps = period_steps,
           sedentary = period_sedentary,
           light = period_light,
           fairly = period_fairly,
           very = period_very,
           calories = period_calories) %>%
    mutate(period = pname)
}

all_data_long_short <- bind_rows(baseline_long_short, bind_rows(period_long_list_short))

cat(sprintf("  ✓ Long format: %d observations from %d patients, %d periods\n",
            nrow(all_data_long_short),
            n_distinct(all_data_long_short$person_id),
            n_distinct(all_data_long_short$period)))

save(
  baseline_data_short,
  period_data_list_short,
  nadir_data_short,
  comprehensive_table_short,
  all_results_short,
  all_data_long_short,
  baseline_start_short,
  baseline_end_short,
  selected_baseline_short,
  file = "period_analysis_short_results.RData"
)
cat("  ✓ period_analysis_short_results.RData\n")

cat("\n=============================================================================\n")
cat("SHORT PERIODS ANALYSIS COMPLETE\n")
cat("=============================================================================\n")

# =============================================================================
# =============================================================================
# MIXED EFFECTS MODELS (if lme4/lmerTest available)
# =============================================================================
# =============================================================================

if (use_mixed_models) {

  cat("\n\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n")
  cat("MIXED EFFECTS MODELS ANALYSIS\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n\n")

  cat("Fitting linear mixed effects models with random intercepts per patient\n")
  cat("This accounts for correlation between repeated measures\n\n")

  # =============================================================================
  # MAIN ANALYSIS MIXED EFFECTS (1-90d, 91-180d, 181-365d)
  # =============================================================================

  cat("=============================================================================\n")
  cat("MAIN ANALYSIS MIXED EFFECTS: 1-90d, 91-180d, 181-365d\n")
  cat("=============================================================================\n\n")

  # Reshape to long format for mixed effects
  # Include all patients with baseline data
  long_data_main <- baseline_data %>%
    select(person_id,
           baseline_weight, baseline_steps, baseline_sedentary,
           baseline_light, baseline_fairly, baseline_very, baseline_calories) %>%
    mutate(period = "Baseline") %>%
    rename(weight = baseline_weight,
           steps = baseline_steps,
           sedentary = baseline_sedentary,
           light = baseline_light,
           fairly = baseline_fairly,
           very = baseline_very,
           calories = baseline_calories)

  # Add each follow-up period
  for (pname in names(period_data_list)) {
    period_long <- period_data_list[[pname]] %>%
      select(person_id,
             period_weight, period_steps, period_sedentary,
             period_light, period_fairly, period_very, period_calories) %>%
      mutate(period = pname) %>%
      rename(weight = period_weight,
             steps = period_steps,
             sedentary = period_sedentary,
             light = period_light,
             fairly = period_fairly,
             very = period_very,
             calories = period_calories)

    long_data_main <- bind_rows(long_data_main, period_long)
  }

  # Convert period to factor with baseline as reference
  long_data_main <- long_data_main %>%
    mutate(period = factor(period, levels = c("Baseline", names(period_data_list))))

  cat(sprintf("Data reshaped: %d observations from %d patients\n\n",
              nrow(long_data_main), n_distinct(long_data_main$person_id)))

  # Fit mixed effects models for each outcome
  mixed_results_main <- list()

  # Weight model
  cat("Fitting weight model...\n")
  weight_data <- long_data_main %>% filter(!is.na(weight))
  if (n_distinct(weight_data$person_id) >= 10) {
    tryCatch({
      weight_lmer <- lmer(weight ~ period + (1|person_id), data = weight_data)
      weight_summary <- summary(weight_lmer)
      mixed_results_main$weight <- list(
        model = weight_lmer,
        summary = weight_summary,
        n_obs = nrow(weight_data),
        n_patients = n_distinct(weight_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(weight_data), n_distinct(weight_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_main$weight <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_main$weight <- NULL
  }

  # Steps model
  cat("Fitting steps model...\n")
  steps_data <- long_data_main %>% filter(!is.na(steps))
  if (n_distinct(steps_data$person_id) >= 10) {
    tryCatch({
      steps_lmer <- lmer(steps ~ period + (1|person_id), data = steps_data)
      steps_summary <- summary(steps_lmer)
      mixed_results_main$steps <- list(
        model = steps_lmer,
        summary = steps_summary,
        n_obs = nrow(steps_data),
        n_patients = n_distinct(steps_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(steps_data), n_distinct(steps_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_main$steps <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_main$steps <- NULL
  }

  # Sedentary model
  cat("Fitting sedentary minutes model...\n")
  sedentary_data <- long_data_main %>% filter(!is.na(sedentary))
  if (n_distinct(sedentary_data$person_id) >= 10) {
    tryCatch({
      sedentary_lmer <- lmer(sedentary ~ period + (1|person_id), data = sedentary_data)
      sedentary_summary <- summary(sedentary_lmer)
      mixed_results_main$sedentary <- list(
        model = sedentary_lmer,
        summary = sedentary_summary,
        n_obs = nrow(sedentary_data),
        n_patients = n_distinct(sedentary_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(sedentary_data), n_distinct(sedentary_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_main$sedentary <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_main$sedentary <- NULL
  }

  # Light activity model
  cat("Fitting light activity model...\n")
  light_data <- long_data_main %>% filter(!is.na(light))
  if (n_distinct(light_data$person_id) >= 10) {
    tryCatch({
      light_lmer <- lmer(light ~ period + (1|person_id), data = light_data)
      light_summary <- summary(light_lmer)
      mixed_results_main$light <- list(
        model = light_lmer,
        summary = light_summary,
        n_obs = nrow(light_data),
        n_patients = n_distinct(light_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(light_data), n_distinct(light_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_main$light <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_main$light <- NULL
  }

  # Fairly active model
  cat("Fitting fairly active model...\n")
  fairly_data <- long_data_main %>% filter(!is.na(fairly))
  if (n_distinct(fairly_data$person_id) >= 10) {
    tryCatch({
      fairly_lmer <- lmer(fairly ~ period + (1|person_id), data = fairly_data)
      fairly_summary <- summary(fairly_lmer)
      mixed_results_main$fairly <- list(
        model = fairly_lmer,
        summary = fairly_summary,
        n_obs = nrow(fairly_data),
        n_patients = n_distinct(fairly_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(fairly_data), n_distinct(fairly_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_main$fairly <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_main$fairly <- NULL
  }

  # Very active model
  cat("Fitting very active model...\n")
  very_data <- long_data_main %>% filter(!is.na(very))
  if (n_distinct(very_data$person_id) >= 10) {
    tryCatch({
      very_lmer <- lmer(very ~ period + (1|person_id), data = very_data)
      very_summary <- summary(very_lmer)
      mixed_results_main$very <- list(
        model = very_lmer,
        summary = very_summary,
        n_obs = nrow(very_data),
        n_patients = n_distinct(very_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(very_data), n_distinct(very_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_main$very <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_main$very <- NULL
  }

  # Calories model
  cat("Fitting calories model...\n")
  calories_data <- long_data_main %>% filter(!is.na(calories))
  if (n_distinct(calories_data$person_id) >= 10) {
    tryCatch({
      calories_lmer <- lmer(calories ~ period + (1|person_id), data = calories_data)
      calories_summary <- summary(calories_lmer)
      mixed_results_main$calories <- list(
        model = calories_lmer,
        summary = calories_summary,
        n_obs = nrow(calories_data),
        n_patients = n_distinct(calories_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(calories_data), n_distinct(calories_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_main$calories <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_main$calories <- NULL
  }

  cat("\n")

  # Create summary table of mixed effects results
  mixed_effects_table_main <- tibble()

  for (outcome_name in names(mixed_results_main)) {
    if (!is.null(mixed_results_main[[outcome_name]])) {
      coef_table <- coef(summary(mixed_results_main[[outcome_name]]$model))

      # Extract coefficients for each period (skip intercept)
      for (i in 2:nrow(coef_table)) {
        period_name <- rownames(coef_table)[i]
        period_name <- gsub("period", "", period_name)

        mixed_effects_table_main <- bind_rows(
          mixed_effects_table_main,
          tibble(
            Outcome = outcome_name,
            Period = period_name,
            Estimate = coef_table[i, "Estimate"],
            SE = coef_table[i, "Std. Error"],
            t_value = coef_table[i, "t value"],
            p_value = coef_table[i, "Pr(>|t|)"],
            N_obs = mixed_results_main[[outcome_name]]$n_obs,
            N_patients = mixed_results_main[[outcome_name]]$n_patients
          )
        )
      }
    }
  }

  # Save mixed effects results
  write_csv(mixed_effects_table_main, "period_analysis_mixed_effects_main.csv")
  cat("  ✓ period_analysis_mixed_effects_main.csv\n\n")

  # =============================================================================
  # SHORT PERIODS MIXED EFFECTS (1-30d, 31-90d, 91-180d, 181-365d)
  # =============================================================================

  cat("=============================================================================\n")
  cat("SHORT PERIODS MIXED EFFECTS: 1-30d, 31-90d, 91-180d, 181-365d\n")
  cat("=============================================================================\n\n")

  # Reshape to long format for mixed effects
  long_data_short <- baseline_data_short %>%
    select(person_id,
           baseline_weight, baseline_steps, baseline_sedentary,
           baseline_light, baseline_fairly, baseline_very, baseline_calories) %>%
    mutate(period = "Baseline") %>%
    rename(weight = baseline_weight,
           steps = baseline_steps,
           sedentary = baseline_sedentary,
           light = baseline_light,
           fairly = baseline_fairly,
           very = baseline_very,
           calories = baseline_calories)

  # Add each follow-up period
  for (pname in names(period_data_list_short)) {
    period_long <- period_data_list_short[[pname]] %>%
      select(person_id,
             period_weight, period_steps, period_sedentary,
             period_light, period_fairly, period_very, period_calories) %>%
      mutate(period = pname) %>%
      rename(weight = period_weight,
             steps = period_steps,
             sedentary = period_sedentary,
             light = period_light,
             fairly = period_fairly,
             very = period_very,
             calories = period_calories)

    long_data_short <- bind_rows(long_data_short, period_long)
  }

  # Convert period to factor with baseline as reference
  long_data_short <- long_data_short %>%
    mutate(period = factor(period, levels = c("Baseline", names(period_data_list_short))))

  cat(sprintf("Data reshaped: %d observations from %d patients\n\n",
              nrow(long_data_short), n_distinct(long_data_short$person_id)))

  # Fit mixed effects models for each outcome
  mixed_results_short <- list()

  # Weight model
  cat("Fitting weight model...\n")
  weight_data <- long_data_short %>% filter(!is.na(weight))
  if (n_distinct(weight_data$person_id) >= 10) {
    tryCatch({
      weight_lmer <- lmer(weight ~ period + (1|person_id), data = weight_data)
      weight_summary <- summary(weight_lmer)
      mixed_results_short$weight <- list(
        model = weight_lmer,
        summary = weight_summary,
        n_obs = nrow(weight_data),
        n_patients = n_distinct(weight_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(weight_data), n_distinct(weight_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_short$weight <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_short$weight <- NULL
  }

  # Steps model
  cat("Fitting steps model...\n")
  steps_data <- long_data_short %>% filter(!is.na(steps))
  if (n_distinct(steps_data$person_id) >= 10) {
    tryCatch({
      steps_lmer <- lmer(steps ~ period + (1|person_id), data = steps_data)
      steps_summary <- summary(steps_lmer)
      mixed_results_short$steps <- list(
        model = steps_lmer,
        summary = steps_summary,
        n_obs = nrow(steps_data),
        n_patients = n_distinct(steps_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(steps_data), n_distinct(steps_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_short$steps <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_short$steps <- NULL
  }

  # Sedentary model
  cat("Fitting sedentary minutes model...\n")
  sedentary_data <- long_data_short %>% filter(!is.na(sedentary))
  if (n_distinct(sedentary_data$person_id) >= 10) {
    tryCatch({
      sedentary_lmer <- lmer(sedentary ~ period + (1|person_id), data = sedentary_data)
      sedentary_summary <- summary(sedentary_lmer)
      mixed_results_short$sedentary <- list(
        model = sedentary_lmer,
        summary = sedentary_summary,
        n_obs = nrow(sedentary_data),
        n_patients = n_distinct(sedentary_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(sedentary_data), n_distinct(sedentary_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_short$sedentary <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_short$sedentary <- NULL
  }

  # Light activity model
  cat("Fitting light activity model...\n")
  light_data <- long_data_short %>% filter(!is.na(light))
  if (n_distinct(light_data$person_id) >= 10) {
    tryCatch({
      light_lmer <- lmer(light ~ period + (1|person_id), data = light_data)
      light_summary <- summary(light_lmer)
      mixed_results_short$light <- list(
        model = light_lmer,
        summary = light_summary,
        n_obs = nrow(light_data),
        n_patients = n_distinct(light_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(light_data), n_distinct(light_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_short$light <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_short$light <- NULL
  }

  # Fairly active model
  cat("Fitting fairly active model...\n")
  fairly_data <- long_data_short %>% filter(!is.na(fairly))
  if (n_distinct(fairly_data$person_id) >= 10) {
    tryCatch({
      fairly_lmer <- lmer(fairly ~ period + (1|person_id), data = fairly_data)
      fairly_summary <- summary(fairly_lmer)
      mixed_results_short$fairly <- list(
        model = fairly_lmer,
        summary = fairly_summary,
        n_obs = nrow(fairly_data),
        n_patients = n_distinct(fairly_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(fairly_data), n_distinct(fairly_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_short$fairly <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_short$fairly <- NULL
  }

  # Very active model
  cat("Fitting very active model...\n")
  very_data <- long_data_short %>% filter(!is.na(very))
  if (n_distinct(very_data$person_id) >= 10) {
    tryCatch({
      very_lmer <- lmer(very ~ period + (1|person_id), data = very_data)
      very_summary <- summary(very_lmer)
      mixed_results_short$very <- list(
        model = very_lmer,
        summary = very_summary,
        n_obs = nrow(very_data),
        n_patients = n_distinct(very_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(very_data), n_distinct(very_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_short$very <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_short$very <- NULL
  }

  # Calories model
  cat("Fitting calories model...\n")
  calories_data <- long_data_short %>% filter(!is.na(calories))
  if (n_distinct(calories_data$person_id) >= 10) {
    tryCatch({
      calories_lmer <- lmer(calories ~ period + (1|person_id), data = calories_data)
      calories_summary <- summary(calories_lmer)
      mixed_results_short$calories <- list(
        model = calories_lmer,
        summary = calories_summary,
        n_obs = nrow(calories_data),
        n_patients = n_distinct(calories_data$person_id)
      )
      cat(sprintf("  ✓ N=%d observations, %d patients\n",
                  nrow(calories_data), n_distinct(calories_data$person_id)))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed: %s\n", e$message))
      mixed_results_short$calories <- NULL
    })
  } else {
    cat("  ✗ Insufficient data\n")
    mixed_results_short$calories <- NULL
  }

  cat("\n")

  # Create summary table of mixed effects results
  mixed_effects_table_short <- tibble()

  for (outcome_name in names(mixed_results_short)) {
    if (!is.null(mixed_results_short[[outcome_name]])) {
      coef_table <- coef(summary(mixed_results_short[[outcome_name]]$model))

      # Extract coefficients for each period (skip intercept)
      for (i in 2:nrow(coef_table)) {
        period_name <- rownames(coef_table)[i]
        period_name <- gsub("period", "", period_name)

        mixed_effects_table_short <- bind_rows(
          mixed_effects_table_short,
          tibble(
            Outcome = outcome_name,
            Period = period_name,
            Estimate = coef_table[i, "Estimate"],
            SE = coef_table[i, "Std. Error"],
            t_value = coef_table[i, "t value"],
            p_value = coef_table[i, "Pr(>|t|)"],
            N_obs = mixed_results_short[[outcome_name]]$n_obs,
            N_patients = mixed_results_short[[outcome_name]]$n_patients
          )
        )
      }
    }
  }

  # Save mixed effects results
  write_csv(mixed_effects_table_short, "period_analysis_mixed_effects_short.csv")
  cat("  ✓ period_analysis_mixed_effects_short.csv\n\n")

  # Save all mixed effects objects
  save(
    mixed_results_main,
    mixed_effects_table_main,
    long_data_main,
    mixed_results_short,
    mixed_effects_table_short,
    long_data_short,
    file = "period_analysis_mixed_effects_results.RData"
  )
  cat("  ✓ period_analysis_mixed_effects_results.RData\n\n")

  cat("=============================================================================\n")
  cat("MIXED EFFECTS MODELS COMPLETE\n")
  cat("=============================================================================\n\n")

  cat("Interpretation:\n")
  cat("- Estimates show change from baseline for each period\n")
  cat("- Random intercepts account for correlation between repeated measures\n")
  cat("- P-values from lmerTest using Satterthwaite approximation\n")
  cat("- Compare to paired t-test results for consistency\n\n")

} else {
  cat("\n\n")
  cat("=============================================================================\n")
  cat("MIXED EFFECTS MODELS SKIPPED\n")
  cat("=============================================================================\n\n")
  cat("lme4/lmerTest packages not available. Install with:\n")
  cat("  install.packages(c('lme4', 'lmerTest'))\n\n")
}

cat("\n\n")
cat("=============================================================================\n")
cat("=============================================================================\n")
cat("ALL ANALYSES COMPLETE\n")
cat("=============================================================================\n")
cat("=============================================================================\n")
