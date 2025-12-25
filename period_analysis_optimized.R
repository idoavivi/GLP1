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
# STEP 0: DETERMINE FINAL 1-90d COHORT (WITH ACTIVE TREATMENT)
# =============================================================================

cat("=============================================================================\n")
cat("STEP 0: DETERMINING FINAL 1-90d COHORT\n")
cat("=============================================================================\n\n")

# Prepare drug data for active treatment check
drug_glp1_clean <- drug_glp1 %>%
  filter(!is.na(drug_start_date)) %>%
  mutate(
    drug_start_date = as.Date(drug_start_date),
    drug_end_date = if_else(!is.na(drug_end_date), as.Date(drug_end_date), as.Date(NA))
  )

# Activity data for 1-90d period
activity_1_90d_temp <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= 1,
         days_from_initiation <= 90) %>%
  mutate(
    wear_time = sedentary_minutes + lightly_active_minutes +
                coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0),
    MVPA = coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0)
  ) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(
    period_steps = mean(steps, na.rm = TRUE),
    period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    period_light = mean(lightly_active_minutes, na.rm = TRUE),
    period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    period_very = mean(very_active_minutes, na.rm = TRUE),
    period_MVPA = mean(MVPA, na.rm = TRUE),
    period_calories = mean(activity_calories, na.rm = TRUE),
    n_period_days = n(),
    .groups = "drop"
  )

# Weight data for 1-90d period
weight_1_90d_temp <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= 1,
         days_from_initiation <= 90,
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  summarize(period_weight = min(weight_kg, na.rm = TRUE), .groups = "drop")

# Check active treatment at midpoint (day 45)
all_patients_temp <- unique(c(activity_1_90d_temp$person_id, weight_1_90d_temp$person_id))

# Require ≥2 prescription fills
patients_with_multiple_fills <- drug_glp1_clean %>%
  filter(person_id %in% all_patients_temp) %>%
  group_by(person_id) %>%
  summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
  filter(n_fills >= 2) %>%
  select(person_id)

# Check prescription within 90 days of midpoint (day 45)
active_treatment_1_90d <- tibble(person_id = all_patients_temp) %>%
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

# Final 1-90d cohort (with active treatment)
final_1_90d_cohort <- activity_1_90d_temp %>%
  full_join(weight_1_90d_temp, by = "person_id") %>%
  inner_join(active_treatment_1_90d, by = "person_id")

final_cohort_ids <- final_1_90d_cohort$person_id

cat(sprintf("Final 1-90d cohort (with active treatment): %d patients\n\n", length(final_cohort_ids)))

# =============================================================================
# STEP 1: BASELINE SELECTION ALGORITHM
# =============================================================================

cat("=============================================================================\n")
cat("STEP 1: BASELINE SELECTION (FOR FINAL 1-90d COHORT ONLY)\n")
cat("=============================================================================\n\n")

# Define candidate baseline windows
baseline_windows <- list(
  "30d" = c(-30, 0),
  "90d" = c(-90, 0),
  "180d" = c(-180, 0)
)

# Evaluate each baseline window
baseline_evaluations <- list()

for (window_name in names(baseline_windows)) {
  window <- baseline_windows[[window_name]]

  cat(sprintf("Evaluating baseline window: %d to %d days\n", window[1], window[2]))

  # Get baseline activity for FINAL 1-90d cohort only
  baseline_activity <- activity_with_glp1 %>%
    filter(person_id %in% final_cohort_ids,
           days_from_initiation >= window[1],
           days_from_initiation <= window[2]) %>%
    mutate(
      wear_time = sedentary_minutes + lightly_active_minutes +
                  coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0),
      MVPA = coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0)
    ) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    summarize(
      baseline_steps = mean(steps, na.rm = TRUE),
      baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      baseline_light = mean(lightly_active_minutes, na.rm = TRUE),
      baseline_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      baseline_very = mean(very_active_minutes, na.rm = TRUE),
      baseline_MVPA = mean(MVPA, na.rm = TRUE),
      baseline_calories = mean(activity_calories, na.rm = TRUE),
      n_baseline_days = n(),
      .groups = "drop"
    )

  # Get baseline weight (HIGHEST)
  baseline_weight <- weight_with_glp1 %>%
    filter(person_id %in% final_cohort_ids,
           days_from_initiation >= window[1],
           days_from_initiation <= window[2],
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(baseline_weight = max(weight_kg, na.rm = TRUE), .groups = "drop")

  # Combine
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

  cat(sprintf("  N patients: %d\n", n_patients))
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

# =============================================================================
# STEP 2: CALCULATE FOLLOW-UP PERIODS (1-90d, 91-180d, 181-365d)
# =============================================================================

cat("=============================================================================\n")
cat("STEP 2: CALCULATING FOLLOW-UP PERIODS\n")
cat("=============================================================================\n\n")

# Define follow-up periods
time_periods <- list(
  "1-90d" = c(1, 90),
  "91-180d" = c(91, 180),
  "181-365d" = c(181, 365)
)

# Get baseline patient IDs (should match final_cohort_ids)
baseline_patient_ids <- baseline_data$person_id

cat(sprintf("Baseline cohort: %d patients\n", length(baseline_patient_ids)))
cat(sprintf("Match with 1-90d cohort: %s\n\n",
            ifelse(setequal(baseline_patient_ids, final_cohort_ids), "YES", "NO")))

period_data_list <- list()

for (period_name in names(time_periods)) {
  period_range <- time_periods[[period_name]]
  start_day <- period_range[1]
  end_day <- period_range[2]
  midpoint_day <- round((start_day + end_day) / 2)

  cat(sprintf("Processing %s (days %d-%d)...\n", period_name, start_day, end_day))

  # For 1-90d, use the pre-calculated cohort for consistency
  if (period_name == "1-90d") {
    period_combined <- final_1_90d_cohort %>%
      mutate(period = period_name)
    period_data_list[[period_name]] <- period_combined
    cat(sprintf("  N = %d patients (using pre-calculated cohort)\n", nrow(period_combined)))
    next
  }

  # Activity data - ONLY baseline cohort patients
  activity_period <- activity_with_glp1 %>%
    filter(person_id %in% baseline_patient_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day) %>%
    mutate(
      wear_time = sedentary_minutes + lightly_active_minutes +
                  coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0),
      MVPA = coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0)
    ) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    summarize(
      period_steps = mean(steps, na.rm = TRUE),
      period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      period_light = mean(lightly_active_minutes, na.rm = TRUE),
      period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      period_very = mean(very_active_minutes, na.rm = TRUE),
      period_MVPA = mean(MVPA, na.rm = TRUE),
      period_calories = mean(activity_calories, na.rm = TRUE),
      n_period_days = n(),
      .groups = "drop"
    )

  # Weight data - LOWEST weight
  weight_period <- weight_with_glp1 %>%
    filter(person_id %in% baseline_patient_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(period_weight = min(weight_kg, na.rm = TRUE), .groups = "drop")

  # Check active treatment at midpoint
  all_patients <- unique(c(activity_period$person_id, weight_period$person_id))

  # Require ≥2 prescription fills
  patients_with_multiple_fills <- drug_glp1_clean %>%
    filter(person_id %in% all_patients) %>%
    group_by(person_id) %>%
    summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
    filter(n_fills >= 2) %>%
    select(person_id)

  # Check prescription within 90 days of midpoint
  active_treatment_status <- tibble(person_id = all_patients) %>%
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

  # Combine and filter to active treatment only
  period_combined <- activity_period %>%
    full_join(weight_period, by = "person_id") %>%
    inner_join(active_treatment_status, by = "person_id") %>%
    mutate(period = period_name)

  period_data_list[[period_name]] <- period_combined

  cat(sprintf("  N = %d patients with active treatment\n", nrow(period_combined)))
}

cat("\n")

# =============================================================================
# STEP 3: CALCULATE NADIR WEIGHT AND ACTIVITY
# =============================================================================

cat("=============================================================================\n")
cat("STEP 3: CALCULATING NADIR WEIGHT AND ACTIVITY\n")
cat("=============================================================================\n\n")

# Find nadir (lowest on-treatment weight) for each patient
nadir_weights <- weight_with_glp1 %>%
  filter(person_id %in% baseline_patient_ids,
         days_from_initiation > 0,  # On treatment only
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
  filter(abs(days_from_nadir) <= 30) %>%  # ±30 days from nadir
  mutate(
    wear_time = sedentary_minutes + lightly_active_minutes +
                coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0),
    MVPA = coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0)
  ) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%  # Minimum 3 days
  summarize(
    nadir_steps = mean(steps, na.rm = TRUE),
    nadir_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    nadir_light = mean(lightly_active_minutes, na.rm = TRUE),
    nadir_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    nadir_very = mean(very_active_minutes, na.rm = TRUE),
    nadir_MVPA = mean(MVPA, na.rm = TRUE),
    nadir_calories = mean(activity_calories, na.rm = TRUE),
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
# STEP 4: CREATE COMPREHENSIVE TABLE WITH PAIRED T-TESTS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 4: CREATING COMPREHENSIVE TABLE WITH STATISTICAL TESTS\n")
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
  Weight_mean = mean(baseline_data$baseline_weight),
  Weight_sd = sd(baseline_data$baseline_weight),
  Steps_mean = mean(baseline_data$baseline_steps),
  Steps_sd = sd(baseline_data$baseline_steps),
  Sedentary_mean = mean(baseline_data$baseline_sedentary),
  Sedentary_sd = sd(baseline_data$baseline_sedentary),
  Light_mean = mean(baseline_data$baseline_light),
  Light_sd = sd(baseline_data$baseline_light),
  Fairly_mean = mean(baseline_data$baseline_fairly),
  Fairly_sd = sd(baseline_data$baseline_fairly),
  Very_mean = mean(baseline_data$baseline_very),
  Very_sd = sd(baseline_data$baseline_very),
  Calories_mean = mean(baseline_data$baseline_calories),
  Calories_sd = sd(baseline_data$baseline_calories)
)

# Process each follow-up period
for (pname in names(period_data_list)) {
  period_df <- period_data_list[[pname]]
  period_range <- time_periods[[pname]]

  # Merge with baseline for paired analysis
  merged <- baseline_data %>%
    inner_join(period_df, by = "person_id") %>%
    filter(!is.na(baseline_weight), !is.na(period_weight))

  n_paired <- nrow(merged)

  if (n_paired >= 10) {
    # Paired t-tests
    weight_test <- t.test(merged$period_weight, merged$baseline_weight, paired = TRUE)
    steps_test <- t.test(merged$period_steps, merged$baseline_steps, paired = TRUE)
    sedentary_test <- t.test(merged$period_sedentary, merged$baseline_sedentary, paired = TRUE)
    light_test <- t.test(merged$period_light, merged$baseline_light, paired = TRUE)
    fairly_test <- t.test(merged$period_fairly, merged$baseline_fairly, paired = TRUE)
    very_test <- t.test(merged$period_very, merged$baseline_very, paired = TRUE)
    calories_test <- t.test(merged$period_calories, merged$baseline_calories, paired = TRUE)

    results_list[[pname]] <- tibble(
      Timepoint = pname,
      Days = sprintf("%d to %d", period_range[1], period_range[2]),
      N = nrow(period_df),
      Weight_mean = mean(merged$period_weight),
      Weight_sd = sd(merged$period_weight),
      Steps_mean = mean(merged$period_steps),
      Steps_sd = sd(merged$period_steps),
      Sedentary_mean = mean(merged$period_sedentary),
      Sedentary_sd = sd(merged$period_sedentary),
      Light_mean = mean(merged$period_light),
      Light_sd = sd(merged$period_light),
      Fairly_mean = mean(merged$period_fairly),
      Fairly_sd = sd(merged$period_fairly),
      Very_mean = mean(merged$period_very),
      Very_sd = sd(merged$period_very),
      Calories_mean = mean(merged$period_calories),
      Calories_sd = sd(merged$period_calories),
      N_paired = n_paired,
      Weight_p = weight_test$p.value,
      Steps_p = steps_test$p.value,
      Sedentary_p = sedentary_test$p.value,
      Light_p = light_test$p.value,
      Fairly_p = fairly_test$p.value,
      Very_p = very_test$p.value,
      Calories_p = calories_test$p.value
    )
  }
}

# Process nadir
merged_nadir <- baseline_data %>%
  inner_join(nadir_data, by = "person_id") %>%
  filter(!is.na(baseline_weight), !is.na(nadir_weight))

n_paired_nadir <- nrow(merged_nadir)

if (n_paired_nadir >= 10) {
  weight_test <- t.test(merged_nadir$nadir_weight, merged_nadir$baseline_weight, paired = TRUE)
  steps_test <- t.test(merged_nadir$nadir_steps, merged_nadir$baseline_steps, paired = TRUE)
  sedentary_test <- t.test(merged_nadir$nadir_sedentary, merged_nadir$baseline_sedentary, paired = TRUE)
  light_test <- t.test(merged_nadir$nadir_light, merged_nadir$baseline_light, paired = TRUE)
  fairly_test <- t.test(merged_nadir$nadir_fairly, merged_nadir$baseline_fairly, paired = TRUE)
  very_test <- t.test(merged_nadir$nadir_very, merged_nadir$baseline_very, paired = TRUE)
  calories_test <- t.test(merged_nadir$nadir_calories, merged_nadir$baseline_calories, paired = TRUE)

  mean_days_to_nadir <- mean(merged_nadir$days_to_nadir)
  sd_days_to_nadir <- sd(merged_nadir$days_to_nadir)

  results_list[["Nadir"]] <- tibble(
    Timepoint = "Nadir",
    Days = sprintf("%.0f ± %.0f", mean_days_to_nadir, sd_days_to_nadir),
    N = nrow(nadir_data),
    Weight_mean = mean(merged_nadir$nadir_weight),
    Weight_sd = sd(merged_nadir$nadir_weight),
    Steps_mean = mean(merged_nadir$nadir_steps),
    Steps_sd = sd(merged_nadir$nadir_steps),
    Sedentary_mean = mean(merged_nadir$nadir_sedentary),
    Sedentary_sd = sd(merged_nadir$nadir_sedentary),
    Light_mean = mean(merged_nadir$nadir_light),
    Light_sd = sd(merged_nadir$nadir_light),
    Fairly_mean = mean(merged_nadir$nadir_fairly),
    Fairly_sd = sd(merged_nadir$nadir_fairly),
    Very_mean = mean(merged_nadir$nadir_very),
    Very_sd = sd(merged_nadir$nadir_very),
    Calories_mean = mean(merged_nadir$nadir_calories),
    Calories_sd = sd(merged_nadir$nadir_calories),
    N_paired = n_paired_nadir,
    Weight_p = weight_test$p.value,
    Steps_p = steps_test$p.value,
    Sedentary_p = sedentary_test$p.value,
    Light_p = light_test$p.value,
    Fairly_p = fairly_test$p.value,
    Very_p = very_test$p.value,
    Calories_p = calories_test$p.value
  )
}

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
cat(sprintf("Baseline: Days %d to %d (selected for highest weight + most activity)\n",
            baseline_start, baseline_end))
cat(sprintf("Baseline N: %d patients\n", nrow(baseline_data)))
cat(sprintf("  - IMPORTANT: Baseline includes ONLY patients in final 1-90d cohort\n"))
cat(sprintf("  - This ensures same patients at baseline and follow-up\n\n"))
cat("Follow-up Periods: 1-90d, 91-180d, 181-365d\n")
cat("Nadir: Lowest on-treatment weight (activity = mean of ±30 days)\n\n")
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
        sprintf("Baseline: Days %d to %d (selected for highest weight + most activity)", baseline_start, baseline_end),
        sprintf("IMPORTANT: Baseline includes ONLY patients in final 1-90d cohort (N=%d)", nrow(baseline_data)),
        "This ensures same patients at baseline and follow-up for valid paired comparisons",
        "Follow-up: 1-90d, 91-180d, 181-365d, plus Nadir",
        "Nadir: Lowest on-treatment weight (activity = mean of ±30 days)",
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
    sprintf("<p><strong>Baseline:</strong> Days %d to %d (highest weight + most activity)<br>\n", baseline_start, baseline_end),
    sprintf("<span class='important'>IMPORTANT:</span> Baseline includes ONLY patients in final 1-90d cohort (N=%d)<br>\n", nrow(baseline_data)),
    "This ensures same patients at baseline and follow-up for valid paired comparisons<br><br>\n",
    "<strong>Follow-up:</strong> 1-90d, 91-180d, 181-365d, plus Nadir<br>\n",
    "<strong>Nadir:</strong> Lowest on-treatment weight (activity = mean of ±30 days)<br>\n",
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

# Save all data for further analysis
save(
  baseline_data,
  period_data_list,
  nadir_data,
  comprehensive_table,
  all_results,
  baseline_start,
  baseline_end,
  selected_baseline,
  file = "period_analysis_optimized_results.RData"
)
cat("  ✓ period_analysis_optimized_results.RData\n")

cat("\n=============================================================================\n")
cat("PERIOD ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
