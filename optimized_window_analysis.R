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

cat("\n### PART 2: FOLLOW-UP TIMEPOINTS ###\n\n")

# Define follow-up timepoints
followup_timepoints <- c(30, 60, 90, 120, 180, 365)

# Follow-up parameters
followup_weight_window <- 15  # ±15 days for weight
followup_activity_window <- 15  # ±15 days for activity
followup_min_activity_days <- 3  # minimum 3 days
active_rx_window <- 90  # prescription within 90 days

# Function to calculate follow-up data for a timepoint
calculate_followup_data <- function(person_ids, init_data, activity_data,
                                   weight_data, drug_data, timepoint_days,
                                   weight_window, activity_window,
                                   min_activity_days, rx_window) {

  results_list <- list()

  for (pid in person_ids) {
    # Get initiation date
    init_date <- init_data %>%
      filter(person_id == pid) %>%
      pull(glp1_initiation_date)

    if (length(init_date) == 0) next

    target_date <- init_date + timepoint_days

    # Check active GLP-1: prescription within 90 days before target
    person_drugs <- drug_data %>%
      filter(person_id == pid, !is.na(drug_start_date))

    if (nrow(person_drugs) == 0) next

    has_active <- any(
      person_drugs$drug_start_date <= target_date &
      person_drugs$drug_start_date >= (target_date - rx_window),
      na.rm = TRUE
    )

    if (!has_active) next

    # Get weight: lowest within ±window days
    weight_window_data <- weight_data %>%
      filter(person_id == pid,
             measurement_date >= (target_date - weight_window),
             measurement_date <= (target_date + weight_window),
             !is.na(weight_kg))

    followup_weight <- if (nrow(weight_window_data) > 0) {
      min(weight_window_data$weight_kg)
    } else {
      NA_real_
    }

    # Get activity: within ±window days, minimum days required
    activity_window_data <- activity_data %>%
      filter(person_id == pid,
             date >= (target_date - activity_window),
             date <= (target_date + activity_window))

    if (nrow(activity_window_data) < min_activity_days) next

    # Calculate activity metrics
    results_list[[length(results_list) + 1]] <- tibble(
      person_id = pid,
      followup_weight = followup_weight,
      followup_steps = mean(activity_window_data$steps, na.rm = TRUE),
      followup_sedentary = mean(activity_window_data$sedentary_minutes, na.rm = TRUE),
      followup_light = mean(activity_window_data$lightly_active_minutes, na.rm = TRUE),
      followup_fairly = mean(activity_window_data$fairly_active_minutes, na.rm = TRUE),
      followup_very = mean(activity_window_data$very_active_minutes, na.rm = TRUE),
      followup_calories = mean(activity_window_data$activity_calories, na.rm = TRUE),
      n_activity_days = nrow(activity_window_data)
    )
  }

  if (length(results_list) == 0) return(NULL)
  bind_rows(results_list)
}

# Calculate for all timepoints
followup_data_list <- list()

for (tp in followup_timepoints) {
  cat(sprintf("Calculating follow-up: Day %d...\n", tp))

  tp_data <- calculate_followup_data(
    eligible_person_ids,
    glp1_initiation,
    fitbit_activity_filtered,
    anthro_completed,
    drug_glp1,
    tp,
    followup_weight_window,
    followup_activity_window,
    followup_min_activity_days,
    active_rx_window
  )

  if (!is.null(tp_data)) {
    followup_data_list[[paste0("day_", tp)]] <- tp_data %>%
      mutate(timepoint = paste0("Day ", tp))
  }
}

# =============================================================================
# PART 3: NADIR ANALYSIS
# =============================================================================

cat("\nCalculating nadir...\n")

nadir_min_activity_days <- 3
nadir_activity_window <- 15

nadir_results_list <- list()

for (pid in eligible_person_ids) {
  # Get initiation date
  init_date <- glp1_initiation %>%
    filter(person_id == pid) %>%
    pull(glp1_initiation_date)

  if (length(init_date) == 0) next

  # Find nadir weight (lowest weight after initiation)
  weight_post <- anthro_completed %>%
    filter(person_id == pid,
           measurement_date >= init_date,
           !is.na(weight_kg))

  if (nrow(weight_post) == 0) next

  nadir_weight <- min(weight_post$weight_kg)
  nadir_date <- weight_post %>%
    filter(weight_kg == nadir_weight) %>%
    pull(measurement_date) %>%
    first()

  # Check active GLP-1 at nadir
  person_drugs <- drug_glp1 %>%
    filter(person_id == pid, !is.na(drug_start_date))

  if (nrow(person_drugs) == 0) next

  has_active <- any(
    person_drugs$drug_start_date <= nadir_date &
    person_drugs$drug_start_date >= (nadir_date - active_rx_window),
    na.rm = TRUE
  )

  if (!has_active) next

  # Get activity around nadir
  activity_nadir <- fitbit_activity_filtered %>%
    filter(person_id == pid,
           date >= (nadir_date - nadir_activity_window),
           date <= (nadir_date + nadir_activity_window))

  if (nrow(activity_nadir) < nadir_min_activity_days) next

  nadir_results_list[[length(nadir_results_list) + 1]] <- tibble(
    person_id = pid,
    followup_weight = nadir_weight,
    followup_steps = mean(activity_nadir$steps, na.rm = TRUE),
    followup_sedentary = mean(activity_nadir$sedentary_minutes, na.rm = TRUE),
    followup_light = mean(activity_nadir$lightly_active_minutes, na.rm = TRUE),
    followup_fairly = mean(activity_nadir$fairly_active_minutes, na.rm = TRUE),
    followup_very = mean(activity_nadir$very_active_minutes, na.rm = TRUE),
    followup_calories = mean(activity_nadir$activity_calories, na.rm = TRUE),
    timepoint = "Nadir",
    days_to_nadir = as.numeric(nadir_date - init_date)
  )
}

if (length(nadir_results_list) > 0) {
  followup_data_list[["nadir"]] <- bind_rows(nadir_results_list)
  cat(sprintf("Nadir: %d patients (mean %.1f days to nadir)\n",
              length(nadir_results_list),
              mean(bind_rows(nadir_results_list)$days_to_nadir)))
}

cat("\n")

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
