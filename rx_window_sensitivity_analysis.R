# Rx Window Sensitivity Analysis
# Test how different active treatment windows affect results
# Testing: 60, 90, 120 days for "Rx within X days of period midpoint"
# Efficient approach: query data once, apply different Rx windows

library(bigrquery)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

cat("\n##################################################\n")
cat("RX WINDOW SENSITIVITY ANALYSIS\n")
cat("Testing active Rx windows: 60, 90, 120 days\n")
cat("##################################################\n\n")

# Connect to BigQuery
project_id <- "all-of-us-data-tools"
dataset_id <- "AoU_CDR_2024q2r2"
billing_project <- "idoaviv-tauber-org"

cat("Querying GLP-1 drug exposure data...\n")

# Query GLP-1 drug exposure
drug_query <- sprintf("
  SELECT
    de.person_id,
    de.drug_concept_id,
    de.drug_exposure_start_date,
    de.drug_exposure_end_date,
    c.concept_name
  FROM `%s.%s.drug_exposure` de
  JOIN `%s.%s.concept` c ON de.drug_concept_id = c.concept_id
  WHERE LOWER(c.concept_name) LIKE '%%semaglutide%%'
     OR LOWER(c.concept_name) LIKE '%%tirzepatide%%'
", project_id, dataset_id, project_id, dataset_id)

drug_glp1_raw <- bq_project_query(billing_project, drug_query) %>%
  bq_table_download()

# Clean drug data
drug_glp1_clean <- drug_glp1_raw %>%
  filter(!grepl("topical|recombinant|insulin|metformin|pioglitazone", concept_name, ignore.case = TRUE)) %>%
  mutate(
    drug_start_date = as.Date(drug_exposure_start_date),
    drug_end_date = as.Date(drug_exposure_end_date)
  ) %>%
  select(person_id, drug_concept_id, drug_start_date, drug_end_date, concept_name)

# Define initiation (first prescription)
glp1_initiation <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(glp1_initiation_date = min(drug_start_date, na.rm = TRUE), .groups = "drop")

cat("Querying Fitbit activity data...\n")

# Query Fitbit activity data
activity_query <- sprintf("
  SELECT
    person_id,
    date AS activity_date,
    steps,
    activity_calories,
    sedentary_minutes,
    lightly_active_minutes,
    fairly_active_minutes,
    very_active_minutes
  FROM `%s.%s.activity_summary`
", project_id, dataset_id)

activity_raw <- bq_project_query(billing_project, activity_query) %>%
  bq_table_download()

# Clean activity data
activity_clean <- activity_raw %>%
  mutate(activity_date = as.Date(activity_date)) %>%
  filter(
    steps >= 100,
    steps <= 25000,
    !is.na(steps),
    sedentary_minutes + lightly_active_minutes + fairly_active_minutes + very_active_minutes <= 1440
  )

# Add wear time and proportional metrics
activity_clean <- activity_clean %>%
  mutate(
    total_wear_minutes = coalesce(sedentary_minutes, 0) +
                         coalesce(lightly_active_minutes, 0) +
                         coalesce(fairly_active_minutes, 0) +
                         coalesce(very_active_minutes, 0),
    is_valid_day = total_wear_minutes >= 600,
    pct_sedentary = if_else(total_wear_minutes > 0,
                            100 * sedentary_minutes / total_wear_minutes,
                            NA_real_),
    pct_MVPA = if_else(total_wear_minutes > 0,
                       100 * (coalesce(fairly_active_minutes, 0) +
                              coalesce(very_active_minutes, 0)) / total_wear_minutes,
                       NA_real_)
  )

# Merge with initiation dates
activity_with_glp1 <- activity_clean %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(activity_date, glp1_initiation_date, units = "days")))

cat("Querying weight data...\n")

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

weight_with_glp1 <- weight_raw %>%
  mutate(measurement_date = as.Date(measurement_date)) %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days")))

cat("Data loaded. Running sensitivity analysis...\n\n")

# Function to run analysis with specific Rx window
run_analysis_with_rx_window <- function(activity_data, weight_data, drug_data, initiation_data, rx_window_days) {

  cat(sprintf("\n========================================\n"))
  cat(sprintf("Running analysis with Rx window: %d days\n", rx_window_days))
  cat(sprintf("========================================\n\n"))

  # Calculate 1-90d period activity (with wear time filtering)
  period_1_90d_activity <- activity_data %>%
    filter(days_from_initiation >= 1,
           days_from_initiation <= 90,
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 4) %>%
    summarize(
      period_steps = mean(steps, na.rm = TRUE),
      period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      period_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
      period_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
      period_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
      n_period_days = n(),
      .groups = "drop"
    )

  eligible_person_ids <- period_1_90d_activity$person_id

  # Baseline windows
  baseline_windows <- list(
    "-90 to -31" = c(-90, -31),
    "-180 to -91" = c(-180, -91),
    "-365 to -181" = c(-365, -181)
  )

  baseline_evaluations <- list()

  for (window_name in names(baseline_windows)) {
    window <- baseline_windows[[window_name]]

    baseline_activity <- activity_data %>%
      filter(person_id %in% eligible_person_ids,
             days_from_initiation >= window[1],
             days_from_initiation <= window[2],
             is_valid_day == TRUE) %>%
      group_by(person_id) %>%
      filter(n() >= 4) %>%
      summarize(
        baseline_steps = mean(steps, na.rm = TRUE),
        baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
        baseline_MVPA = mean(coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0), na.rm = TRUE),
        baseline_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
        baseline_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
        n_baseline_days = n(),
        .groups = "drop"
      )

    baseline_weight <- weight_data %>%
      filter(person_id %in% eligible_person_ids,
             days_from_initiation >= window[1],
             days_from_initiation <= window[2],
             !is.na(weight_kg)) %>%
      group_by(person_id) %>%
      summarize(baseline_weight = max(weight_kg, na.rm = TRUE), .groups = "drop")

    baseline_combined <- baseline_activity %>%
      inner_join(baseline_weight, by = "person_id")

    baseline_evaluations[[window_name]] <- list(
      window = window,
      n_patients = nrow(baseline_combined),
      mean_weight = mean(baseline_combined$baseline_weight),
      mean_steps = mean(baseline_combined$baseline_steps),
      mean_MVPA = mean(baseline_combined$baseline_MVPA, na.rm = TRUE),
      data = baseline_combined
    )
  }

  # Select best baseline
  selected_baseline <- NULL
  max_weight <- 0
  max_activity <- 0

  for (window_name in names(baseline_evaluations)) {
    eval <- baseline_evaluations[[window_name]]
    if (eval$mean_weight > max_weight ||
        (eval$mean_weight == max_weight && eval$mean_MVPA > max_activity)) {
      max_weight <- eval$mean_weight
      max_activity <- eval$mean_MVPA
      selected_baseline <- window_name
    }
  }

  baseline_data <- baseline_evaluations[[selected_baseline]]$data
  final_cohort_ids <- baseline_data$person_id

  # Apply active treatment check with specified Rx window
  # Period midpoint for 1-90d is day 45.5
  period_midpoint_day <- 45.5

  active_treatment_status <- period_1_90d_activity %>%
    filter(person_id %in% final_cohort_ids) %>%
    select(person_id) %>%
    # Require ≥2 fills total
    inner_join(
      drug_data %>%
        group_by(person_id) %>%
        summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
        filter(n_fills >= 2),
      by = "person_id"
    ) %>%
    # Check for Rx within window of period midpoint
    left_join(initiation_data %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
    mutate(
      midpoint_date = glp1_initiation_date + period_midpoint_day,
      active_rx_cutoff = midpoint_date - rx_window_days  # DYNAMIC WINDOW
    ) %>%
    left_join(drug_data, by = "person_id", relationship = "many-to-many") %>%
    mutate(
      is_active = (drug_start_date <= midpoint_date & drug_start_date >= active_rx_cutoff) |
                  (drug_start_date <= midpoint_date & (is.na(drug_end_date) | drug_end_date >= midpoint_date))
    ) %>%
    group_by(person_id) %>%
    summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
    filter(has_active_rx) %>%
    select(person_id)

  period_1_90d_final <- period_1_90d_activity %>%
    filter(person_id %in% final_cohort_ids) %>%
    inner_join(active_treatment_status, by = "person_id")

  merged_data <- baseline_data %>%
    inner_join(period_1_90d_final, by = "person_id")

  # Run paired t-tests
  steps_test <- t.test(merged_data$period_steps, merged_data$baseline_steps, paired = TRUE)
  sedentary_test <- t.test(merged_data$period_sedentary, merged_data$baseline_sedentary, paired = TRUE)
  MVPA_test <- t.test(merged_data$period_MVPA, merged_data$baseline_MVPA, paired = TRUE)
  pct_sedentary_test <- t.test(merged_data$period_pct_sedentary, merged_data$baseline_pct_sedentary, paired = TRUE)
  pct_MVPA_test <- t.test(merged_data$period_pct_MVPA, merged_data$baseline_pct_MVPA, paired = TRUE)

  # Return summary results
  results <- tibble(
    rx_window_days = rx_window_days,
    n_patients = nrow(merged_data),
    baseline_steps_mean = mean(merged_data$baseline_steps, na.rm = TRUE),
    baseline_steps_sd = sd(merged_data$baseline_steps, na.rm = TRUE),
    period_steps_mean = mean(merged_data$period_steps, na.rm = TRUE),
    period_steps_sd = sd(merged_data$period_steps, na.rm = TRUE),
    steps_change = mean(merged_data$period_steps - merged_data$baseline_steps, na.rm = TRUE),
    steps_pct_change = 100 * mean((merged_data$period_steps - merged_data$baseline_steps) / merged_data$baseline_steps, na.rm = TRUE),
    steps_p = steps_test$p.value,
    baseline_MVPA_mean = mean(merged_data$baseline_MVPA, na.rm = TRUE),
    period_MVPA_mean = mean(merged_data$period_MVPA, na.rm = TRUE),
    MVPA_change = mean(merged_data$period_MVPA - merged_data$baseline_MVPA, na.rm = TRUE),
    MVPA_p = MVPA_test$p.value,
    baseline_pct_sedentary = mean(merged_data$baseline_pct_sedentary, na.rm = TRUE),
    period_pct_sedentary = mean(merged_data$period_pct_sedentary, na.rm = TRUE),
    pct_sedentary_change = mean(merged_data$period_pct_sedentary - merged_data$baseline_pct_sedentary, na.rm = TRUE),
    pct_sedentary_p = pct_sedentary_test$p.value,
    baseline_pct_MVPA = mean(merged_data$baseline_pct_MVPA, na.rm = TRUE),
    period_pct_MVPA = mean(merged_data$period_pct_MVPA, na.rm = TRUE),
    pct_MVPA_change = mean(merged_data$period_pct_MVPA - merged_data$baseline_pct_MVPA, na.rm = TRUE),
    pct_MVPA_p = pct_MVPA_test$p.value
  )

  cat(sprintf("Results Summary:\n"))
  cat(sprintf("  N patients: %d\n", results$n_patients))
  cat(sprintf("  Steps: %.1f → %.1f (%.1f%%, p=%.4f)\n",
              results$baseline_steps_mean, results$period_steps_mean,
              results$steps_pct_change, results$steps_p))
  cat(sprintf("  MVPA: %.1f → %.1f min (Δ=%.1f, p=%.4f)\n",
              results$baseline_MVPA_mean, results$period_MVPA_mean,
              results$MVPA_change, results$MVPA_p))
  cat(sprintf("  %% Sedentary: %.1f → %.1f%% (Δ=%.1f%%, p=%.4f)\n",
              results$baseline_pct_sedentary, results$period_pct_sedentary,
              results$pct_sedentary_change, results$pct_sedentary_p))

  return(results)
}

# Run sensitivity analysis with different Rx windows
rx_windows <- c(60, 90, 120)
sensitivity_results <- list()

for (window in rx_windows) {
  sensitivity_results[[as.character(window)]] <- run_analysis_with_rx_window(
    activity_with_glp1,
    weight_with_glp1,
    drug_glp1_clean,
    glp1_initiation,
    window
  )
}

# Combine all results
combined_results <- bind_rows(sensitivity_results)

# Print comparison table
cat("\n\n")
cat("========================================\n")
cat("COMPARISON ACROSS RX WINDOWS\n")
cat("========================================\n\n")

print(combined_results %>%
        select(rx_window_days, n_patients,
               baseline_steps_mean, period_steps_mean, steps_pct_change, steps_p,
               MVPA_change, MVPA_p) %>%
        mutate(across(where(is.numeric), ~round(., 2))))

# Save results
write.csv(combined_results, "rx_window_sensitivity_results.csv", row.names = FALSE)
cat("\n\nResults saved to: rx_window_sensitivity_results.csv\n")

# Create visualizations
cat("\nGenerating visualizations...\n")

# Plot 1: Steps change by Rx window
p1 <- ggplot(combined_results, aes(x = factor(rx_window_days), y = steps_pct_change)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%\np=%.3f", steps_pct_change, steps_p)),
            vjust = ifelse(combined_results$steps_pct_change < 0, 1.2, -0.5), size = 3) +
  labs(title = "Steps Change by Active Rx Window",
       subtitle = "1-90d period vs baseline",
       x = "Rx Window (days before period midpoint)",
       y = "% Change in Steps") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Plot 2: MVPA change by Rx window
p2 <- ggplot(combined_results, aes(x = factor(rx_window_days), y = MVPA_change)) +
  geom_col(fill = "coral") +
  geom_text(aes(label = sprintf("%.1f min\np=%.3f", MVPA_change, MVPA_p)),
            vjust = ifelse(combined_results$MVPA_change < 0, 1.2, -0.5), size = 3) +
  labs(title = "MVPA Change by Active Rx Window",
       subtitle = "1-90d period vs baseline",
       x = "Rx Window (days before period midpoint)",
       y = "Change in MVPA (minutes)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Plot 3: Sample size by Rx window
p3 <- ggplot(combined_results, aes(x = factor(rx_window_days), y = n_patients)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = n_patients), vjust = -0.5, size = 4) +
  labs(title = "Sample Size by Active Rx Window",
       subtitle = "Impact on cohort retention",
       x = "Rx Window (days before period midpoint)",
       y = "N Patients") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Plot 4: % Sedentary change by Rx window
p4 <- ggplot(combined_results, aes(x = factor(rx_window_days), y = pct_sedentary_change)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = sprintf("%.1f%%\np=%.3f", pct_sedentary_change, pct_sedentary_p)),
            vjust = ifelse(combined_results$pct_sedentary_change < 0, 1.2, -0.5), size = 3) +
  labs(title = "% Sedentary Time Change by Active Rx Window",
       subtitle = "1-90d period vs baseline (wear-adjusted)",
       x = "Rx Window (days before period midpoint)",
       y = "Change in % Sedentary Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Save plots
ggsave("rx_window_sensitivity_steps_change.png", p1, width = 8, height = 6, dpi = 300)
ggsave("rx_window_sensitivity_MVPA_change.png", p2, width = 8, height = 6, dpi = 300)
ggsave("rx_window_sensitivity_sample_size.png", p3, width = 8, height = 6, dpi = 300)
ggsave("rx_window_sensitivity_pct_sedentary.png", p4, width = 8, height = 6, dpi = 300)

cat("\nPlots saved:\n")
cat("  - rx_window_sensitivity_steps_change.png\n")
cat("  - rx_window_sensitivity_MVPA_change.png\n")
cat("  - rx_window_sensitivity_sample_size.png\n")
cat("  - rx_window_sensitivity_pct_sedentary.png\n")

cat("\n##################################################\n")
cat("SENSITIVITY ANALYSIS COMPLETE\n")
cat("##################################################\n")
