# =============================================================================
# GLP-1 and Physical Activity: Windowed Analysis
# Examines specific time windows before and after GLP-1 initiation
# =============================================================================

library(tidyverse)
library(lubridate)

# Optional packages for table formatting (not required)
suppressWarnings({
  suppressMessages({
    require(knitr, quietly = TRUE)
    require(kableExtra, quietly = TRUE)
  })
})

# Load processed data
load("glp1_processed_data.RData")

cat("=============================================================================\n")
cat("GLP-1 WINDOWED ANALYSIS\n")
cat("=============================================================================\n\n")

# =============================================================================
# ELIGIBILITY CRITERIA: BMI >= 30 OR BMI >= 27 WITH OBESITY DIAGNOSIS
# =============================================================================

cat("\n### ELIGIBILITY FILTERING ###\n\n")

# Step 1: Identify patients with obesity diagnosis
if (exists("dataset_41386742_condition_df")) {
  condition_df <- dataset_41386742_condition_df
} else {
  # If condition data not loaded, set empty
  cat("Warning: Condition data not found. Proceeding with BMI criteria only.\n")
  condition_df <- tibble(person_id = integer(), standard_concept_name = character())
}

# Identify obesity diagnoses (ICD-10 E66.x, SNOMED obesity concepts)
obesity_keywords <- c("obesity", "obese", "overweight")
patients_with_obesity_dx <- condition_df %>%
  filter(str_detect(tolower(standard_concept_name), paste(obesity_keywords, collapse = "|"))) %>%
  distinct(person_id) %>%
  mutate(has_obesity_dx = TRUE)

cat(sprintf("Patients with obesity diagnosis: %d\n", nrow(patients_with_obesity_dx)))

# Step 2: Calculate baseline BMI for each patient
# Use most recent BMI before or at GLP-1 initiation
baseline_bmi <- weight_with_glp1 %>%
  filter(days_from_initiation <= 0) %>%
  filter(!is.na(bmi)) %>%
  group_by(person_id) %>%
  slice_max(measurement_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(person_id, baseline_bmi = bmi, baseline_weight = weight_kg)

cat(sprintf("Patients with baseline BMI: %d\n", nrow(baseline_bmi)))

# Step 3: Apply eligibility criteria
eligible_patients <- baseline_bmi %>%
  left_join(patients_with_obesity_dx, by = "person_id") %>%
  mutate(has_obesity_dx = replace_na(has_obesity_dx, FALSE)) %>%
  mutate(
    eligible = case_when(
      baseline_bmi >= 30 ~ TRUE,
      baseline_bmi >= 27 & has_obesity_dx ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(eligible)

cat(sprintf("\nEligibility Summary:\n"))
cat(sprintf("  BMI >= 30: %d patients\n",
            sum(eligible_patients$baseline_bmi >= 30)))
cat(sprintf("  BMI 27-29.9 with obesity dx: %d patients\n",
            sum(eligible_patients$baseline_bmi >= 27 &
                eligible_patients$baseline_bmi < 30 &
                eligible_patients$has_obesity_dx)))
cat(sprintf("  TOTAL ELIGIBLE: %d patients\n", nrow(eligible_patients)))

# Step 4: Filter all datasets to eligible patients only
eligible_person_ids <- eligible_patients$person_id

activity_with_glp1 <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids)

weight_with_glp1 <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids)

glp1_initiation <- glp1_initiation %>%
  filter(person_id %in% eligible_person_ids)

fitbit_activity_filtered <- fitbit_activity_filtered %>%
  filter(person_id %in% eligible_person_ids)

anthro_completed <- anthro_completed %>%
  filter(person_id %in% eligible_person_ids)

drug_glp1 <- drug_glp1 %>%
  filter(person_id %in% eligible_person_ids)

cat(sprintf("\nDatasets filtered to %d eligible patients\n", length(eligible_person_ids)))

# =============================================================================
# PART 1: PRE-GLP1 BASELINE WINDOW SELECTION
# =============================================================================

cat("\n### PART 1: PRE-GLP1 BASELINE WINDOW SELECTION ###\n\n")

# Define baseline windows (including day 0 = initiation day)
baseline_windows <- list(
  "30d" = c(-30, 0),
  "60d" = c(-60, 0),
  "90d" = c(-90, 0),
  "180d" = c(-180, 0)
)

# Define minimum days thresholds
min_days_thresholds <- c(3, 5, 7, 10)

# Function to calculate baseline metrics for a window
calculate_baseline_metrics <- function(activity_data, weight_data,
                                      window_start, window_end,
                                      min_days) {

  # Filter activity data for window
  activity_window <- activity_data %>%
    filter(days_from_initiation >= window_start,
           days_from_initiation <= window_end)

  # Count days per person
  days_per_person <- activity_window %>%
    group_by(person_id) %>%
    summarize(n_days = n(), .groups = "drop")

  # Filter for minimum days
  eligible_persons <- days_per_person %>%
    filter(n_days >= min_days) %>%
    pull(person_id)

  if (length(eligible_persons) == 0) {
    return(tibble(
      n_patients = 0,
      mean_weight = NA, sd_weight = NA,
      mean_steps = NA, sd_steps = NA,
      mean_sedentary_min = NA, sd_sedentary_min = NA,
      mean_lightly_active_min = NA, sd_lightly_active_min = NA,
      mean_fairly_active_min = NA, sd_fairly_active_min = NA,
      mean_very_active_min = NA, sd_very_active_min = NA,
      mean_activity_calories = NA, sd_activity_calories = NA,
      mean_days_per_person = NA
    ))
  }

  # Calculate activity metrics
  activity_metrics <- activity_window %>%
    filter(person_id %in% eligible_persons) %>%
    group_by(person_id) %>%
    summarize(
      mean_steps = mean(steps, na.rm = TRUE),
      mean_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
      mean_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
      mean_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
      mean_very_active_min = mean(very_active_minutes, na.rm = TRUE),
      mean_activity_calories = mean(activity_calories, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    )

  # Calculate weight metrics
  weight_window <- weight_data %>%
    filter(days_from_initiation >= window_start,
           days_from_initiation <= window_end,
           person_id %in% eligible_persons) %>%
    group_by(person_id) %>%
    summarize(mean_weight = mean(weight_kg, na.rm = TRUE), .groups = "drop")

  # Combine and summarize
  results <- tibble(
    n_patients = length(eligible_persons),
    mean_weight = mean(weight_window$mean_weight, na.rm = TRUE),
    sd_weight = sd(weight_window$mean_weight, na.rm = TRUE),
    mean_steps = mean(activity_metrics$mean_steps, na.rm = TRUE),
    sd_steps = sd(activity_metrics$mean_steps, na.rm = TRUE),
    mean_sedentary_min = mean(activity_metrics$mean_sedentary_min, na.rm = TRUE),
    sd_sedentary_min = sd(activity_metrics$mean_sedentary_min, na.rm = TRUE),
    mean_lightly_active_min = mean(activity_metrics$mean_lightly_active_min, na.rm = TRUE),
    sd_lightly_active_min = sd(activity_metrics$mean_lightly_active_min, na.rm = TRUE),
    mean_fairly_active_min = mean(activity_metrics$mean_fairly_active_min, na.rm = TRUE),
    sd_fairly_active_min = sd(activity_metrics$mean_fairly_active_min, na.rm = TRUE),
    mean_very_active_min = mean(activity_metrics$mean_very_active_min, na.rm = TRUE),
    sd_very_active_min = sd(activity_metrics$mean_very_active_min, na.rm = TRUE),
    mean_activity_calories = mean(activity_metrics$mean_activity_calories, na.rm = TRUE),
    sd_activity_calories = sd(activity_metrics$mean_activity_calories, na.rm = TRUE),
    mean_days_per_person = mean(activity_metrics$n_days, na.rm = TRUE)
  )

  return(results)
}

# Calculate metrics for all combinations
baseline_results <- expand_grid(
  window_name = names(baseline_windows),
  min_days = min_days_thresholds
) %>%
  mutate(
    window_start = map_dbl(window_name, ~baseline_windows[[.x]][1]),
    window_end = map_dbl(window_name, ~baseline_windows[[.x]][2])
  ) %>%
  rowwise() %>%
  mutate(
    metrics = list(calculate_baseline_metrics(
      activity_with_glp1,
      weight_with_glp1,
      window_start,
      window_end,
      min_days
    ))
  ) %>%
  ungroup() %>%
  unnest(metrics)

cat("Baseline Window Analysis Results:\n")
cat("==================================\n\n")

# Print results for each minimum days threshold
for (min_d in min_days_thresholds) {
  cat(sprintf("\n--- Minimum %d days of Fitbit data ---\n\n", min_d))

  results_subset <- baseline_results %>%
    filter(min_days == min_d) %>%
    select(window_name, n_patients, mean_weight, mean_steps,
           mean_sedentary_min, mean_lightly_active_min,
           mean_fairly_active_min, mean_very_active_min,
           mean_activity_calories, mean_days_per_person)

  print(results_subset, n = Inf)
  cat("\n")
}

# Calculate activity score to choose winner
# Higher very active + fairly active + lightly active minutes = better
# Lower sedentary minutes = better
baseline_results <- baseline_results %>%
  mutate(
    total_active_min = mean_lightly_active_min + mean_fairly_active_min + mean_very_active_min,
    activity_score = total_active_min - (mean_sedentary_min / 10)  # Penalize sedentary
  )

# Select winner for each min_days threshold
cat("\n--- RECOMMENDED BASELINE WINDOWS ---\n\n")

baseline_winners <- baseline_results %>%
  group_by(min_days) %>%
  slice_max(activity_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(min_days, window_name, n_patients, mean_steps, total_active_min,
         mean_activity_calories, activity_score)

print(baseline_winners, n = Inf)

# Select overall recommended baseline (using 7 days minimum as good balance)
recommended_baseline <- baseline_results %>%
  filter(min_days == 7) %>%
  slice_max(activity_score, n = 1, with_ties = FALSE)

cat(sprintf("\n*** RECOMMENDED BASELINE WINDOW: %s (minimum 7 days) ***\n",
            recommended_baseline$window_name))
cat(sprintf("N = %d patients\n", recommended_baseline$n_patients))
cat(sprintf("Mean steps: %.1f (±%.1f)\n",
            recommended_baseline$mean_steps, recommended_baseline$sd_steps))
cat(sprintf("Mean weight: %.1f kg (±%.1f)\n",
            recommended_baseline$mean_weight, recommended_baseline$sd_weight))

# Save baseline window definition
baseline_window_start <- recommended_baseline$window_start
baseline_window_end <- recommended_baseline$window_end
baseline_min_days <- 7

# =============================================================================
# PART 2: POST-GLP1 FOLLOW-UP WINDOWS
# =============================================================================

cat("\n\n### PART 2: POST-GLP1 FOLLOW-UP WINDOWS ###\n\n")

# Define follow-up timepoints (days after GLP-1 initiation)
followup_timepoints <- c(30, 60, 90, 120, 180, 360)

# Window around timepoint for measurements (±15 days)
measurement_window <- 15

# Minimum Fitbit days in window
min_fitbit_days_followup <- 3

# Active treatment window (prescription within 90 days)
active_rx_window <- 90

# Function to check if patient has active GLP-1 at timepoint
check_active_glp1 <- function(person_id, target_date, drug_data, window_days = 90) {
  person_drugs <- drug_data %>%
    filter(person_id == !!person_id)

  if (nrow(person_drugs) == 0) return(FALSE)

  # Ensure dates are Date objects
  person_drugs <- person_drugs %>%
    mutate(
      drug_start_date = as.Date(drug_start_date),
      drug_end_date = as.Date(drug_end_date)
    )

  target_date <- as.Date(target_date)
  cutoff_date <- target_date - window_days

  # Check if any prescription within window days before target
  has_recent_rx <- person_drugs %>%
    filter(!is.na(drug_start_date),
           drug_start_date <= target_date,
           drug_start_date >= cutoff_date) %>%
    nrow() > 0

  # Check if any prescription ongoing at target date
  has_ongoing_rx <- person_drugs %>%
    filter(!is.na(drug_start_date),
           drug_start_date <= target_date,
           (is.na(drug_end_date) | drug_end_date >= target_date)) %>%
    nrow() > 0

  return(has_recent_rx | has_ongoing_rx)
}

# Function to calculate metrics for a follow-up timepoint
calculate_followup_metrics <- function(activity_data, weight_data, drug_data,
                                      glp1_init_data, timepoint_days,
                                      window_days = 15, min_days = 3,
                                      active_rx_days = 90) {

  # Get patients and their target dates
  patient_dates <- glp1_init_data %>%
    mutate(target_date = glp1_initiation_date + days(timepoint_days)) %>%
    select(person_id, glp1_initiation_date, target_date)

  results_list <- list()

  for (i in 1:nrow(patient_dates)) {
    pid <- patient_dates$person_id[i]
    tdate <- patient_dates$target_date[i]
    init_date <- patient_dates$glp1_initiation_date[i]

    # Check active GLP-1
    is_active <- check_active_glp1(pid, tdate, drug_data, active_rx_days)
    if (!is_active) next

    # Get activity data in window
    activity_window <- activity_data %>%
      filter(person_id == pid,
             date >= (tdate - days(window_days)),
             date <= (tdate + days(window_days)))

    # Check minimum days
    if (nrow(activity_window) < min_days) next

    # Calculate activity metrics
    activity_metrics <- activity_window %>%
      summarize(
        mean_steps = mean(steps, na.rm = TRUE),
        mean_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
        mean_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
        mean_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
        mean_very_active_min = mean(very_active_minutes, na.rm = TRUE),
        mean_activity_calories = mean(activity_calories, na.rm = TRUE),
        n_days = n()
      )

    # Get weight in window (lowest weight)
    weight_window <- weight_data %>%
      filter(person_id == pid,
             measurement_date >= (tdate - days(window_days)),
             measurement_date <= (tdate + days(window_days)))

    if (nrow(weight_window) > 0) {
      min_weight <- min(weight_window$weight_kg, na.rm = TRUE)
      min_bmi <- weight_window %>%
        filter(weight_kg == min_weight) %>%
        pull(bmi) %>%
        first()
    } else {
      min_weight <- NA
      min_bmi <- NA
    }

    # Compile results
    results_list[[length(results_list) + 1]] <- tibble(
      person_id = pid,
      timepoint_days = timepoint_days,
      days_from_initiation = as.numeric(tdate - init_date),
      min_weight = min_weight,
      min_bmi = min_bmi,
      mean_steps = activity_metrics$mean_steps,
      mean_sedentary_min = activity_metrics$mean_sedentary_min,
      mean_lightly_active_min = activity_metrics$mean_lightly_active_min,
      mean_fairly_active_min = activity_metrics$mean_fairly_active_min,
      mean_very_active_min = activity_metrics$mean_very_active_min,
      mean_activity_calories = activity_metrics$mean_activity_calories,
      n_fitbit_days = activity_metrics$n_days
    )
  }

  if (length(results_list) == 0) {
    return(NULL)
  }

  bind_rows(results_list)
}

# Calculate metrics for all timepoints
cat("Calculating follow-up metrics for each timepoint...\n\n")

followup_results_list <- list()

for (tp in followup_timepoints) {
  cat(sprintf("Processing day %d...\n", tp))

  results <- calculate_followup_metrics(
    fitbit_activity_filtered,
    anthro_completed,
    drug_glp1,
    glp1_initiation,
    timepoint_days = tp,
    window_days = measurement_window,
    min_days = min_fitbit_days_followup,
    active_rx_days = active_rx_window
  )

  if (!is.null(results)) {
    followup_results_list[[as.character(tp)]] <- results
  }
}

# Combine all timepoint results
followup_results <- bind_rows(followup_results_list)

# Summarize by timepoint
followup_summary <- followup_results %>%
  group_by(timepoint_days) %>%
  summarize(
    n_patients = n(),
    mean_days_from_init = mean(days_from_initiation, na.rm = TRUE),
    sd_days_from_init = sd(days_from_initiation, na.rm = TRUE),
    mean_weight = mean(min_weight, na.rm = TRUE),
    sd_weight = sd(min_weight, na.rm = TRUE),
    mean_bmi = mean(min_bmi, na.rm = TRUE),
    sd_bmi = sd(min_bmi, na.rm = TRUE),
    mean_steps = mean(mean_steps, na.rm = TRUE),
    sd_steps = sd(mean_steps, na.rm = TRUE),
    mean_sedentary_min = mean(mean_sedentary_min, na.rm = TRUE),
    sd_sedentary_min = sd(mean_sedentary_min, na.rm = TRUE),
    mean_lightly_active_min = mean(mean_lightly_active_min, na.rm = TRUE),
    sd_lightly_active_min = sd(mean_lightly_active_min, na.rm = TRUE),
    mean_fairly_active_min = mean(mean_fairly_active_min, na.rm = TRUE),
    sd_fairly_active_min = sd(mean_fairly_active_min, na.rm = TRUE),
    mean_very_active_min = mean(mean_very_active_min, na.rm = TRUE),
    sd_very_active_min = sd(mean_very_active_min, na.rm = TRUE),
    mean_activity_calories = mean(mean_activity_calories, na.rm = TRUE),
    sd_activity_calories = sd(mean_activity_calories, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== FOLLOW-UP TIMEPOINT SUMMARY ===\n\n")
print(followup_summary, n = Inf)

# =============================================================================
# PART 3: NADIR WEIGHT ANALYSIS
# =============================================================================

cat("\n\n### PART 3: NADIR WEIGHT ANALYSIS ###\n\n")

# For each patient, find nadir (lowest weight on treatment)
nadir_results_list <- list()

for (pid in glp1_initiation$person_id) {
  # Get initiation date
  init_date <- glp1_initiation %>%
    filter(person_id == pid) %>%
    pull(glp1_initiation_date)

  # Get all weights after initiation
  weight_post <- anthro_completed %>%
    filter(person_id == pid,
           measurement_date >= init_date)

  if (nrow(weight_post) == 0) next

  # Find nadir weight
  nadir_idx <- which.min(weight_post$weight_kg)
  nadir_date <- weight_post$measurement_date[nadir_idx]
  nadir_weight <- weight_post$weight_kg[nadir_idx]
  nadir_bmi <- weight_post$bmi[nadir_idx]
  days_to_nadir <- as.numeric(nadir_date - init_date)

  # Check if patient has active GLP-1 at nadir
  is_active <- check_active_glp1(pid, nadir_date, drug_glp1, active_rx_window)
  if (!is_active) next

  # Get activity data around nadir (±15 days)
  activity_nadir <- fitbit_activity_filtered %>%
    filter(person_id == pid,
           date >= (nadir_date - days(measurement_window)),
           date <= (nadir_date + days(measurement_window)))

  # Check minimum days
  if (nrow(activity_nadir) < min_fitbit_days_followup) next

  # Calculate activity metrics
  activity_metrics <- activity_nadir %>%
    summarize(
      mean_steps = mean(steps, na.rm = TRUE),
      mean_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
      mean_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
      mean_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
      mean_very_active_min = mean(very_active_minutes, na.rm = TRUE),
      mean_activity_calories = mean(activity_calories, na.rm = TRUE),
      n_days = n()
    )

  # Compile results
  nadir_results_list[[length(nadir_results_list) + 1]] <- tibble(
    person_id = pid,
    nadir_date = nadir_date,
    days_from_initiation = days_to_nadir,
    nadir_weight = nadir_weight,
    nadir_bmi = nadir_bmi,
    mean_steps = activity_metrics$mean_steps,
    mean_sedentary_min = activity_metrics$mean_sedentary_min,
    mean_lightly_active_min = activity_metrics$mean_lightly_active_min,
    mean_fairly_active_min = activity_metrics$mean_fairly_active_min,
    mean_very_active_min = activity_metrics$mean_very_active_min,
    mean_activity_calories = activity_metrics$mean_activity_calories,
    n_fitbit_days = activity_metrics$n_days
  )
}

# Combine nadir results
if (length(nadir_results_list) > 0) {
  nadir_results <- bind_rows(nadir_results_list)

  # Summarize nadir
  nadir_summary <- nadir_results %>%
    summarize(
      n_patients = n(),
      mean_days_to_nadir = mean(days_from_initiation, na.rm = TRUE),
      sd_days_to_nadir = sd(days_from_initiation, na.rm = TRUE),
      median_days_to_nadir = median(days_from_initiation, na.rm = TRUE),
      mean_nadir_weight = mean(nadir_weight, na.rm = TRUE),
      sd_nadir_weight = sd(nadir_weight, na.rm = TRUE),
      mean_nadir_bmi = mean(nadir_bmi, na.rm = TRUE),
      sd_nadir_bmi = sd(nadir_bmi, na.rm = TRUE),
      mean_steps = mean(mean_steps, na.rm = TRUE),
      sd_steps = sd(mean_steps, na.rm = TRUE),
      mean_sedentary_min = mean(mean_sedentary_min, na.rm = TRUE),
      sd_sedentary_min = sd(mean_sedentary_min, na.rm = TRUE),
      mean_lightly_active_min = mean(mean_lightly_active_min, na.rm = TRUE),
      sd_lightly_active_min = sd(mean_lightly_active_min, na.rm = TRUE),
      mean_fairly_active_min = mean(mean_fairly_active_min, na.rm = TRUE),
      sd_fairly_active_min = sd(mean_fairly_active_min, na.rm = TRUE),
      mean_very_active_min = mean(mean_very_active_min, na.rm = TRUE),
      sd_very_active_min = sd(mean_very_active_min, na.rm = TRUE),
      mean_activity_calories = mean(mean_activity_calories, na.rm = TRUE),
      sd_activity_calories = sd(mean_activity_calories, na.rm = TRUE)
    )

  cat("=== NADIR WEIGHT SUMMARY ===\n\n")
  print(nadir_summary)

  cat("\nNadir Weight Distribution:\n")
  cat(sprintf("  Mean days to nadir: %.1f (±%.1f)\n",
              nadir_summary$mean_days_to_nadir, nadir_summary$sd_days_to_nadir))
  cat(sprintf("  Median days to nadir: %.1f\n",
              nadir_summary$median_days_to_nadir))
  cat(sprintf("  Mean nadir weight: %.1f kg (±%.1f)\n",
              nadir_summary$mean_nadir_weight, nadir_summary$sd_nadir_weight))
  cat(sprintf("  Mean steps at nadir: %.1f (±%.1f)\n",
              nadir_summary$mean_steps, nadir_summary$sd_steps))

} else {
  nadir_summary <- NULL
  cat("No patients met nadir criteria\n")
}

# =============================================================================
# PART 4: COMBINED SUMMARY TABLE
# =============================================================================

cat("\n\n### PART 4: COMPREHENSIVE SUMMARY TABLE ###\n\n")

# Get baseline metrics for recommended window
baseline_metrics <- baseline_results %>%
  filter(window_name == recommended_baseline$window_name,
         min_days == baseline_min_days) %>%
  mutate(timepoint = "Baseline") %>%
  select(timepoint, n_patients, mean_weight, sd_weight,
         mean_steps, sd_steps, mean_sedentary_min, sd_sedentary_min,
         mean_lightly_active_min, sd_lightly_active_min,
         mean_fairly_active_min, sd_fairly_active_min,
         mean_very_active_min, sd_very_active_min,
         mean_activity_calories, sd_activity_calories)

# Format follow-up metrics
followup_metrics <- followup_summary %>%
  mutate(timepoint = paste0("Day ", timepoint_days)) %>%
  select(timepoint, n_patients, mean_weight, sd_weight,
         mean_steps, sd_steps, mean_sedentary_min, sd_sedentary_min,
         mean_lightly_active_min, sd_lightly_active_min,
         mean_fairly_active_min, sd_fairly_active_min,
         mean_very_active_min, sd_very_active_min,
         mean_activity_calories, sd_activity_calories)

# Add nadir if available
if (!is.null(nadir_summary)) {
  nadir_metrics <- nadir_summary %>%
    mutate(timepoint = "Nadir",
           mean_weight = mean_nadir_weight,
           sd_weight = sd_nadir_weight) %>%
    select(timepoint, n_patients, mean_weight, sd_weight,
           mean_steps, sd_steps, mean_sedentary_min, sd_sedentary_min,
           mean_lightly_active_min, sd_lightly_active_min,
           mean_fairly_active_min, sd_fairly_active_min,
           mean_very_active_min, sd_very_active_min,
           mean_activity_calories, sd_activity_calories)

  combined_summary <- bind_rows(baseline_metrics, followup_metrics, nadir_metrics)
} else {
  combined_summary <- bind_rows(baseline_metrics, followup_metrics)
}

# Format for nice display
combined_summary_formatted <- combined_summary %>%
  mutate(
    Weight = sprintf("%.1f (%.1f)", mean_weight, sd_weight),
    Steps = sprintf("%.0f (%.0f)", mean_steps, sd_steps),
    Sedentary = sprintf("%.0f (%.0f)", mean_sedentary_min, sd_sedentary_min),
    Lightly_Active = sprintf("%.0f (%.0f)", mean_lightly_active_min, sd_lightly_active_min),
    Fairly_Active = sprintf("%.0f (%.0f)", mean_fairly_active_min, sd_fairly_active_min),
    Very_Active = sprintf("%.0f (%.0f)", mean_very_active_min, sd_very_active_min),
    Activity_Cal = sprintf("%.0f (%.0f)", mean_activity_calories, sd_activity_calories)
  ) %>%
  select(Timepoint = timepoint, N = n_patients, Weight, Steps,
         Sedentary, Lightly_Active, Fairly_Active, Very_Active, Activity_Cal)

cat("COMPREHENSIVE SUMMARY TABLE\n")
cat("(Values shown as Mean (SD))\n\n")
print(combined_summary_formatted, n = Inf)

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n\n=== SAVING RESULTS ===\n\n")

# Save all results
windowed_analysis_results <- list(
  eligible_patients = eligible_patients,
  baseline_windows_all = baseline_results,
  baseline_winners = baseline_winners,
  recommended_baseline = recommended_baseline,
  followup_summary = followup_summary,
  followup_individual = followup_results,
  nadir_summary = nadir_summary,
  nadir_individual = if(exists("nadir_results")) nadir_results else NULL,
  combined_summary = combined_summary
)

save(windowed_analysis_results, file = "windowed_analysis_results.RData")

# Save summary table as CSV
write_csv(combined_summary_formatted, "summary_table.csv")
write_csv(combined_summary, "summary_table_raw.csv")

# Save individual timepoint data
write_csv(followup_results, "followup_individual_data.csv")
if (exists("nadir_results")) {
  write_csv(nadir_results, "nadir_individual_data.csv")
}

cat("Results saved:\n")
cat("  - windowed_analysis_results.RData (complete results)\n")
cat("  - summary_table.csv (formatted summary)\n")
cat("  - summary_table_raw.csv (raw summary with mean/SD)\n")
cat("  - followup_individual_data.csv (patient-level follow-up data)\n")
if (exists("nadir_results")) {
  cat("  - nadir_individual_data.csv (patient-level nadir data)\n")
}

cat("\n=============================================================================\n")
cat("WINDOWED ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
