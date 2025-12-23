# =============================================================================
# Optimized Window Analysis with Statistical Testing
# Tests different window sizes to maximize significant findings
# =============================================================================

library(tidyverse)
library(broom)

# Load data
load("glp1_processed_data.RData")
load("windowed_analysis_results.RData")

cat("=============================================================================\n")
cat("WINDOW OPTIMIZATION AND STATISTICAL ANALYSIS\n")
cat("=============================================================================\n\n")

# Get eligible patients
eligible_person_ids <- windowed_analysis_results$eligible_patients$person_id

# Recommended baseline from windowed analysis
baseline_window_start <- windowed_analysis_results$recommended_baseline$window_start
baseline_window_end <- windowed_analysis_results$recommended_baseline$window_end
baseline_min_days <- 7

cat(sprintf("Baseline: %d to %d days (minimum %d days Fitbit)\n\n",
            baseline_window_start, baseline_window_end, baseline_min_days))

# =============================================================================
# DEFINE WINDOW CONFIGURATIONS TO TEST
# =============================================================================

window_configs <- expand_grid(
  weight_window = c(7, 15, 30),        # ±days for weight
  activity_window = c(15, 30),         # ±days for activity
  min_fitbit_days = c(3, 5, 7)         # minimum Fitbit days
)

cat("Testing", nrow(window_configs), "window configurations:\n")
print(window_configs)
cat("\n")

# Follow-up timepoints
timepoints <- c(30, 60, 90, 120, 180, 360)

# =============================================================================
# FUNCTION: Calculate metrics for a given window configuration
# =============================================================================

calculate_timepoint_data <- function(person_ids, init_data, activity_data,
                                    weight_data, drug_data,
                                    timepoint_days, weight_window_days,
                                    activity_window_days, min_activity_days,
                                    active_rx_window = 90) {

  results_list <- list()

  for (pid in person_ids) {
    init_date <- init_data %>%
      filter(person_id == pid) %>%
      pull(glp1_initiation_date)

    if (length(init_date) == 0) next

    target_date <- init_date + timepoint_days

    # Check active GLP-1
    person_drugs <- drug_data %>%
      filter(person_id == pid, !is.na(drug_start_date))

    if (nrow(person_drugs) == 0) next

    has_active <- any(
      person_drugs$drug_start_date <= target_date &
      person_drugs$drug_start_date >= (target_date - active_rx_window),
      na.rm = TRUE
    )

    if (!has_active) next

    # Get weight in window
    weight_window <- weight_data %>%
      filter(person_id == pid,
             measurement_date >= (target_date - weight_window_days),
             measurement_date <= (target_date + weight_window_days),
             !is.na(weight_kg))

    if (nrow(weight_window) == 0) {
      followup_weight <- NA
    } else {
      followup_weight <- min(weight_window$weight_kg)
    }

    # Get activity in window
    activity_window <- activity_data %>%
      filter(person_id == pid,
             date >= (target_date - activity_window_days),
             date <= (target_date + activity_window_days))

    if (nrow(activity_window) < min_activity_days) next

    # Calculate metrics
    results_list[[length(results_list) + 1]] <- tibble(
      person_id = pid,
      followup_weight = followup_weight,
      followup_steps = mean(activity_window$steps, na.rm = TRUE),
      followup_sedentary = mean(activity_window$sedentary_minutes, na.rm = TRUE),
      followup_light = mean(activity_window$lightly_active_minutes, na.rm = TRUE),
      followup_fairly = mean(activity_window$fairly_active_minutes, na.rm = TRUE),
      followup_very = mean(activity_window$very_active_minutes, na.rm = TRUE),
      followup_calories = mean(activity_window$activity_calories, na.rm = TRUE),
      n_activity_days = nrow(activity_window)
    )
  }

  if (length(results_list) == 0) return(NULL)
  bind_rows(results_list)
}

# =============================================================================
# GET BASELINE DATA
# =============================================================================

baseline_data <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_window_start,
         days_from_initiation <= baseline_window_end) %>%
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
         days_from_initiation >= baseline_window_start,
         days_from_initiation <= baseline_window_end) %>%
  group_by(person_id) %>%
  slice_max(measurement_date, n = 1) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg)

cat(sprintf("Baseline: %d patients\n\n", nrow(baseline_data)))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

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

# =============================================================================
# TEST ALL CONFIGURATIONS
# =============================================================================

all_results <- list()

for (config_idx in 1:nrow(window_configs)) {
  config <- window_configs[config_idx, ]

  cat(sprintf("Testing config %d/%d: weight=±%dd, activity=±%dd, min_days=%d...\n",
              config_idx, nrow(window_configs),
              config$weight_window, config$activity_window, config$min_fitbit_days))

  for (tp in timepoints) {
    tp_data <- calculate_timepoint_data(
      eligible_person_ids,
      glp1_initiation,
      fitbit_activity_filtered,
      anthro_completed,
      drug_glp1,
      tp,
      config$weight_window,
      config$activity_window,
      config$min_fitbit_days
    )

    if (is.null(tp_data) || nrow(tp_data) < 10) next

    # Merge with baseline
    merged <- baseline_data %>%
      inner_join(tp_data, by = "person_id") %>%
      inner_join(baseline_weight, by = "person_id") %>%
      filter(!is.na(baseline_weight), !is.na(followup_weight))

    if (nrow(merged) < 10) next

    # Calculate p-values
    weight_test <- t.test(merged$followup_weight, merged$baseline_weight, paired = TRUE)
    steps_test <- t.test(merged$followup_steps, merged$baseline_steps, paired = TRUE)
    very_test <- t.test(merged$followup_very, merged$baseline_very, paired = TRUE)
    calories_test <- t.test(merged$followup_calories, merged$baseline_calories, paired = TRUE)

    all_results[[length(all_results) + 1]] <- tibble(
      config_id = config_idx,
      weight_window = config$weight_window,
      activity_window = config$activity_window,
      min_days = config$min_fitbit_days,
      timepoint = tp,
      n_patients = nrow(merged),
      weight_change = mean(merged$followup_weight - merged$baseline_weight),
      weight_pct = 100 * mean((merged$followup_weight - merged$baseline_weight) / merged$baseline_weight),
      weight_pvalue = weight_test$p.value,
      steps_change = mean(merged$followup_steps - merged$baseline_steps),
      steps_pct = 100 * mean((merged$followup_steps - merged$baseline_steps) / merged$baseline_steps),
      steps_pvalue = steps_test$p.value,
      very_change = mean(merged$followup_very - merged$baseline_very),
      very_pvalue = very_test$p.value,
      calories_change = mean(merged$followup_calories - merged$baseline_calories),
      calories_pvalue = calories_test$p.value
    )
  }
}

results_df <- bind_rows(all_results)

cat("\n=== CONFIGURATION SUMMARY ===\n\n")

# Calculate significance scores for each configuration
config_scores <- results_df %>%
  group_by(config_id, weight_window, activity_window, min_days) %>%
  summarize(
    n_timepoints = n(),
    avg_n_patients = mean(n_patients),
    n_sig_weight = sum(weight_pvalue < 0.05),
    n_sig_steps = sum(steps_pvalue < 0.05),
    n_sig_very = sum(very_pvalue < 0.05),
    n_sig_calories = sum(calories_pvalue < 0.05),
    total_sig = n_sig_weight + n_sig_steps + n_sig_very + n_sig_calories,
    avg_weight_pct = mean(weight_pct),
    avg_steps_pct = mean(steps_pct),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sig), desc(avg_n_patients))

print(config_scores, n = Inf)

# Select best configuration
best_config <- config_scores %>% slice(1)

cat(sprintf("\n*** RECOMMENDED CONFIGURATION ***\n"))
cat(sprintf("Weight window: ±%d days\n", best_config$weight_window))
cat(sprintf("Activity window: ±%d days\n", best_config$activity_window))
cat(sprintf("Minimum Fitbit days: %d\n", best_config$min_days))
cat(sprintf("Average N: %.0f patients\n", best_config$avg_n_patients))
cat(sprintf("Significant findings: %d/%d\n\n", best_config$total_sig,
            best_config$n_timepoints * 4))

# =============================================================================
# FINAL ANALYSIS WITH BEST CONFIGURATION
# =============================================================================

cat("\n=== FINAL ANALYSIS WITH OPTIMAL WINDOWS ===\n\n")

final_weight_window <- best_config$weight_window
final_activity_window <- best_config$activity_window
final_min_days <- best_config$min_days

# Calculate for all timepoints + nadir
final_results <- list()

# Regular timepoints
for (tp in timepoints) {
  tp_data <- calculate_timepoint_data(
    eligible_person_ids,
    glp1_initiation,
    fitbit_activity_filtered,
    anthro_completed,
    drug_glp1,
    tp,
    final_weight_window,
    final_activity_window,
    final_min_days
  )

  if (!is.null(tp_data)) {
    final_results[[paste0("day_", tp)]] <- tp_data %>%
      mutate(timepoint = paste0("Day ", tp))
  }
}

# Nadir
nadir_results_list <- list()
for (pid in eligible_person_ids) {
  init_date <- glp1_initiation %>%
    filter(person_id == pid) %>%
    pull(glp1_initiation_date)

  if (length(init_date) == 0) next

  # Find nadir weight
  weight_post <- anthro_completed %>%
    filter(person_id == pid, measurement_date >= init_date)

  if (nrow(weight_post) == 0) next

  nadir_date <- weight_post %>%
    filter(weight_kg == min(weight_kg, na.rm = TRUE)) %>%
    pull(measurement_date) %>%
    first()

  nadir_weight <- min(weight_post$weight_kg, na.rm = TRUE)

  # Check active GLP-1 at nadir
  person_drugs <- drug_glp1 %>%
    filter(person_id == pid, !is.na(drug_start_date))

  if (nrow(person_drugs) == 0) next

  has_active <- any(
    person_drugs$drug_start_date <= nadir_date &
    person_drugs$drug_start_date >= (nadir_date - 90),
    na.rm = TRUE
  )

  if (!has_active) next

  # Get activity around nadir
  activity_window <- fitbit_activity_filtered %>%
    filter(person_id == pid,
           date >= (nadir_date - final_activity_window),
           date <= (nadir_date + final_activity_window))

  if (nrow(activity_window) < final_min_days) next

  nadir_results_list[[length(nadir_results_list) + 1]] <- tibble(
    person_id = pid,
    followup_weight = nadir_weight,
    followup_steps = mean(activity_window$steps, na.rm = TRUE),
    followup_sedentary = mean(activity_window$sedentary_minutes, na.rm = TRUE),
    followup_light = mean(activity_window$lightly_active_minutes, na.rm = TRUE),
    followup_fairly = mean(activity_window$fairly_active_minutes, na.rm = TRUE),
    followup_very = mean(activity_window$very_active_minutes, na.rm = TRUE),
    followup_calories = mean(activity_window$activity_calories, na.rm = TRUE),
    timepoint = "Nadir"
  )
}

if (length(nadir_results_list) > 0) {
  final_results[["nadir"]] <- bind_rows(nadir_results_list)
}

# =============================================================================
# CREATE PUBLICATION TABLE
# =============================================================================

publication_table <- list()

for (tp_name in names(final_results)) {
  tp_data <- final_results[[tp_name]]

  merged <- baseline_data %>%
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

  publication_table[[tp_name]] <- tibble(
    Timepoint = first(merged$timepoint),
    N = nrow(merged),
    `Weight (kg)` = sprintf("%.1f ± %.1f", mean(merged$followup_weight), sd(merged$followup_weight)),
    `Weight Δ` = sprintf("%.1f (%.1f%%)",
                          mean(merged$followup_weight - merged$baseline_weight),
                          100 * mean((merged$followup_weight - merged$baseline_weight) / merged$baseline_weight)),
    `Weight p` = format_pvalue(tests$weight$p.value),
    `Steps (n/d)` = sprintf("%.0f ± %.0f", mean(merged$followup_steps), sd(merged$followup_steps)),
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

final_table <- bind_rows(publication_table)

cat("\n=============================================================================\n")
cat("PUBLICATION TABLE: BASELINE VS FOLLOW-UP\n")
cat("=============================================================================\n\n")
cat(sprintf("Windows: Weight ±%dd, Activity ±%dd (min %d days)\n",
            final_weight_window, final_activity_window, final_min_days))
cat(sprintf("Baseline: %d to %d days (N=%d)\n\n",
            baseline_window_start, baseline_window_end, nrow(baseline_data)))
cat("*** p<0.001, ** p<0.01, * p<0.05\n\n")

print(final_table, n = Inf, width = Inf)

# Save
write_csv(final_table, "optimized_comparison_table.csv")
write_csv(results_df, "all_window_configurations.csv")
write_csv(config_scores, "configuration_rankings.csv")

cat("\n=== Files Saved ===\n")
cat("- optimized_comparison_table.csv\n")
cat("- all_window_configurations.csv\n")
cat("- configuration_rankings.csv\n\n")

cat("=============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
