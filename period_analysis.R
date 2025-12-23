# =============================================================================
# Period-Based Analysis with Random Effects Models
# Analyzes GLP-1 impact using time periods instead of specific timepoints
# =============================================================================

library(tidyverse)

# Load data
load("glp1_processed_data.RData")
load("windowed_analysis_results.RData")

cat("=============================================================================\n")
cat("PERIOD-BASED ANALYSIS WITH RANDOM EFFECTS MODELS\n")
cat("=============================================================================\n\n")

# Check for optional packages
use_mixed_models <- FALSE
if (require(lme4, quietly = TRUE) && require(lmerTest, quietly = TRUE)) {
  # Try to load broom.mixed for tidy model output
  if (require(broom.mixed, quietly = TRUE)) {
    use_mixed_models <- TRUE
    cat("Mixed effects models: ENABLED (with broom.mixed for tidy output)\n\n")
  } else {
    if (require(broom, quietly = TRUE)) {
      use_mixed_models <- TRUE
      cat("Mixed effects models: ENABLED (broom.mixed not available, using broom)\n\n")
    } else {
      use_mixed_models <- TRUE
      cat("Mixed effects models: ENABLED (no tidy output packages)\n\n")
    }
  }
} else {
  cat("WARNING: lme4 and/or lmerTest not available.\n")
  cat("Install with: install.packages(c('lme4', 'lmerTest', 'broom.mixed'))\n")
  cat("Proceeding with descriptive statistics only.\n\n")
}

# Get eligible patients
eligible_person_ids <- windowed_analysis_results$eligible_patients$person_id

# =============================================================================
# BASELINE (from optimized_window_analysis)
# =============================================================================

cat("### BASELINE SETUP ###\n\n")

# Use the baseline from windowed analysis results
baseline_window_info <- windowed_analysis_results$recommended_baseline
baseline_start <- baseline_window_info$window_start
baseline_end <- baseline_window_info$window_end

# Get baseline data
baseline_activity <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_start,
         days_from_initiation <= baseline_end) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%  # Minimum 3 days
  summarize(
    baseline_steps = mean(steps, na.rm = TRUE),
    baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    baseline_light = mean(lightly_active_minutes, na.rm = TRUE),
    baseline_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    baseline_very = mean(very_active_minutes, na.rm = TRUE),
    baseline_calories = mean(activity_calories, na.rm = TRUE),
    n_baseline_days = n(),
    .groups = "drop"
  )

baseline_weight <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_start,
         days_from_initiation <= baseline_end,
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  slice_max(measurement_date, n = 1) %>%  # Latest weight
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg)

cat(sprintf("Baseline: %d to %d days\n", baseline_start, baseline_end))
cat(sprintf("N = %d patients with baseline data\n\n", nrow(baseline_activity)))

# =============================================================================
# DEFINE TIME PERIODS
# =============================================================================

cat("### DEFINING TIME PERIODS ###\n\n")

time_periods <- list(
  "1-30d" = c(1, 30),
  "31-60d" = c(31, 60),
  "61-90d" = c(61, 90),
  "91-180d" = c(91, 180),
  "181-365d" = c(181, 365),
  "1-45d" = c(1, 45),
  "46-90d" = c(46, 90)
)

for (period_name in names(time_periods)) {
  cat(sprintf("%s: Days %d-%d post-initiation\n",
              period_name, time_periods[[period_name]][1], time_periods[[period_name]][2]))
}
cat("\n")

# =============================================================================
# PREPARE DATA FOR VECTORIZED ACTIVE TREATMENT CHECK
# =============================================================================

# Prepare drug data once (clean and convert dates)
drug_glp1_clean <- drug_glp1 %>%
  filter(!is.na(drug_start_date)) %>%
  mutate(
    drug_start_date = as.Date(drug_start_date),
    drug_end_date = if_else(!is.na(drug_end_date), as.Date(drug_end_date), as.Date(NA))
  )

# =============================================================================
# CALCULATE METRICS FOR EACH PERIOD
# =============================================================================

cat("### CALCULATING PERIOD METRICS ###\n\n")

period_data_list <- list()

for (period_name in names(time_periods)) {
  period_range <- time_periods[[period_name]]
  start_day <- period_range[1]
  end_day <- period_range[2]
  midpoint_day <- round((start_day + end_day) / 2)

  cat(sprintf("Processing %s (days %d-%d)...\n", period_name, start_day, end_day))

  # Activity data for this period
  activity_period <- activity_with_glp1 %>%
    filter(person_id %in% eligible_person_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%  # Minimum 3 days
    summarize(
      period_steps = mean(steps, na.rm = TRUE),
      period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      period_light = mean(lightly_active_minutes, na.rm = TRUE),
      period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      period_very = mean(very_active_minutes, na.rm = TRUE),
      period_calories = mean(activity_calories, na.rm = TRUE),
      n_period_days = n(),
      .groups = "drop"
    )

  # Weight data for this period - LOWEST weight
  weight_period <- weight_with_glp1 %>%
    filter(person_id %in% eligible_person_ids,
           days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(
      period_weight = min(weight_kg, na.rm = TRUE),  # LOWEST weight in period
      .groups = "drop"
    )

  # Check active treatment at midpoint of period (VECTORIZED)
  # Get initiation dates for calculating midpoint calendar date
  all_patients <- unique(c(activity_period$person_id, weight_period$person_id))

  active_treatment_status <- tibble(person_id = all_patients) %>%
    left_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
    mutate(
      midpoint_date = glp1_initiation_date + midpoint_day,
      active_rx_cutoff = midpoint_date - 90
    ) %>%
    left_join(drug_glp1_clean, by = "person_id", relationship = "many-to-many") %>%
    mutate(
      # Check if prescription within 90 days OR ongoing at midpoint
      is_active = (drug_start_date <= midpoint_date & drug_start_date >= active_rx_cutoff) |
                  (drug_start_date <= midpoint_date & (is.na(drug_end_date) | drug_end_date >= midpoint_date))
    ) %>%
    group_by(person_id) %>%
    summarize(has_active_rx = any(is_active, na.rm = TRUE), .groups = "drop") %>%
    filter(has_active_rx) %>%
    select(person_id)

  cat(sprintf("  Active treatment at midpoint: %d/%d patients\n",
              nrow(active_treatment_status), length(all_patients)))

  # Combine and filter to active treatment only
  period_combined <- activity_period %>%
    full_join(weight_period, by = "person_id") %>%
    inner_join(active_treatment_status, by = "person_id") %>%  # ONLY active treatment
    mutate(period = period_name)

  period_data_list[[period_name]] <- period_combined

  cat(sprintf("  N = %d patients with active treatment (weight: %d, activity: %d)\n",
              nrow(period_combined),
              sum(!is.na(period_combined$period_weight)),
              sum(!is.na(period_combined$period_steps))))
}

cat("\n")

# =============================================================================
# CREATE LONG FORMAT DATA FOR MIXED MODELS
# =============================================================================

cat("### PREPARING DATA FOR MIXED MODELS ###\n\n")

# Baseline data in long format
baseline_long <- baseline_activity %>%
  inner_join(baseline_weight, by = "person_id") %>%
  mutate(
    period = "Baseline",
    period_num = 0,
    weight = baseline_weight,
    steps = baseline_steps,
    sedentary = baseline_sedentary,
    light = baseline_light,
    fairly = baseline_fairly,
    very = baseline_very,
    calories = baseline_calories
  ) %>%
  select(person_id, period, period_num, weight, steps, sedentary,
         light, fairly, very, calories)

# Period data in long format
period_assignments <- tibble(
  period = names(time_periods),
  period_num = 1:length(time_periods)
)

period_long_list <- list()
for (period_name in names(period_data_list)) {
  period_df <- period_data_list[[period_name]]

  period_num_val <- period_assignments %>%
    filter(period == period_name) %>%
    pull(period_num)

  period_long <- period_df %>%
    mutate(
      period_num = period_num_val,
      weight = period_weight,
      steps = period_steps,
      sedentary = period_sedentary,
      light = period_light,
      fairly = period_fairly,
      very = period_very,
      calories = period_calories
    ) %>%
    select(person_id, period, period_num, weight, steps, sedentary,
           light, fairly, very, calories)

  period_long_list[[period_name]] <- period_long
}

# Combine all
all_data_long <- bind_rows(baseline_long, bind_rows(period_long_list))

cat(sprintf("Total observations: %d\n", nrow(all_data_long)))
cat(sprintf("Unique patients: %d\n", n_distinct(all_data_long$person_id)))
cat(sprintf("Periods: %d\n\n", n_distinct(all_data_long$period)))

# =============================================================================
# FIT RANDOM EFFECTS MODELS
# =============================================================================

if (use_mixed_models) {
  cat("### FITTING RANDOM EFFECTS MODELS ###\n\n")

  # Fit mixed models for each outcome
  models <- list()

  # Weight model
  cat("Fitting model: Weight...\n")
  models$weight <- lmer(weight ~ period_num + (1 | person_id),
                       data = all_data_long %>% filter(!is.na(weight)))

  # Steps model
  cat("Fitting model: Steps...\n")
  models$steps <- lmer(steps ~ period_num + (1 | person_id),
                      data = all_data_long %>% filter(!is.na(steps)))

  # Sedentary model
  cat("Fitting model: Sedentary minutes...\n")
  models$sedentary <- lmer(sedentary ~ period_num + (1 | person_id),
                          data = all_data_long %>% filter(!is.na(sedentary)))

  # Light active model
  cat("Fitting model: Light active minutes...\n")
  models$light <- lmer(light ~ period_num + (1 | person_id),
                      data = all_data_long %>% filter(!is.na(light)))

  # Fairly active model
  cat("Fitting model: Fairly active minutes...\n")
  models$fairly <- lmer(fairly ~ period_num + (1 | person_id),
                       data = all_data_long %>% filter(!is.na(fairly)))

  # Very active model
  cat("Fitting model: Very active minutes...\n")
  models$very <- lmer(very ~ period_num + (1 | person_id),
                     data = all_data_long %>% filter(!is.na(very)))

  # Calories model
  cat("Fitting model: Activity calories...\n")
  models$calories <- lmer(calories ~ period_num + (1 | person_id),
                         data = all_data_long %>% filter(!is.na(calories)))

  cat("\nAll models fitted.\n\n")
}

# =============================================================================
# CREATE SUMMARY TABLE
# =============================================================================

cat("### CREATING SUMMARY TABLE ###\n\n")

# Calculate descriptive statistics by period
summary_stats <- all_data_long %>%
  group_by(period, period_num) %>%
  summarize(
    n = n(),
    weight_mean = mean(weight, na.rm = TRUE),
    weight_sd = sd(weight, na.rm = TRUE),
    steps_mean = mean(steps, na.rm = TRUE),
    steps_sd = sd(steps, na.rm = TRUE),
    sedentary_mean = mean(sedentary, na.rm = TRUE),
    sedentary_sd = sd(sedentary, na.rm = TRUE),
    light_mean = mean(light, na.rm = TRUE),
    light_sd = sd(light, na.rm = TRUE),
    fairly_mean = mean(fairly, na.rm = TRUE),
    fairly_sd = sd(fairly, na.rm = TRUE),
    very_mean = mean(very, na.rm = TRUE),
    very_sd = sd(very, na.rm = TRUE),
    calories_mean = mean(calories, na.rm = TRUE),
    calories_sd = sd(calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period_num)

# Get p-values from mixed models if available
if (use_mixed_models) {
  # Extract coefficients and p-values
  model_pvalues <- tibble(
    metric = c("weight", "steps", "sedentary", "light", "fairly", "very", "calories"),
    pvalue = map_dbl(models, ~tidy(.x) %>%
                    filter(term == "period_num") %>%
                    pull(p.value))
  )

  cat("Mixed Model Results (period_num coefficient):\n")
  print(model_pvalues)
  cat("\n")
}

# Format table
format_pvalue <- function(p) {
  if (is.na(p)) return("")
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

publication_table <- summary_stats %>%
  mutate(
    Period = period,
    N = n,
    `Weight (kg)` = sprintf("%.1f ± %.1f", weight_mean, weight_sd),
    `Steps (n/day)` = sprintf("%.0f ± %.0f", steps_mean, steps_sd),
    `Sedentary (min)` = sprintf("%.0f ± %.0f", sedentary_mean, sedentary_sd),
    `Light Active (min)` = sprintf("%.0f ± %.0f", light_mean, light_sd),
    `Fairly Active (min)` = sprintf("%.0f ± %.0f", fairly_mean, fairly_sd),
    `Very Active (min)` = sprintf("%.0f ± %.0f", very_mean, very_sd),
    `Activity Cal (kcal)` = sprintf("%.0f ± %.0f", calories_mean, calories_sd)
  ) %>%
  select(Period, N, `Weight (kg)`, `Steps (n/day)`, `Sedentary (min)`,
         `Light Active (min)`, `Fairly Active (min)`, `Very Active (min)`,
         `Activity Cal (kcal)`)

# =============================================================================
# CALCULATE CHANGES FROM BASELINE
# =============================================================================

baseline_means <- summary_stats %>%
  filter(period == "Baseline") %>%
  select(weight_mean, steps_mean, sedentary_mean, light_mean,
         fairly_mean, very_mean, calories_mean)

changes_from_baseline <- summary_stats %>%
  filter(period != "Baseline") %>%
  mutate(
    weight_change = weight_mean - baseline_means$weight_mean,
    weight_pct = 100 * weight_change / baseline_means$weight_mean,
    steps_change = steps_mean - baseline_means$steps_mean,
    steps_pct = 100 * steps_change / baseline_means$steps_mean,
    sedentary_change = sedentary_mean - baseline_means$sedentary_mean,
    light_change = light_mean - baseline_means$light_mean,
    fairly_change = fairly_mean - baseline_means$fairly_mean,
    very_change = very_mean - baseline_means$very_mean,
    calories_change = calories_mean - baseline_means$calories_mean
  )

# Create change table
change_table <- changes_from_baseline %>%
  mutate(
    Period = period,
    `Weight Δ` = sprintf("%.1f kg (%.1f%%)", weight_change, weight_pct),
    `Steps Δ` = sprintf("%.0f (%.1f%%)", steps_change, steps_pct),
    `Sedentary Δ` = sprintf("%.0f min", sedentary_change),
    `Light Δ` = sprintf("%.0f min", light_change),
    `Fairly Δ` = sprintf("%.0f min", fairly_change),
    `Very Δ` = sprintf("%.0f min", very_change),
    `Calories Δ` = sprintf("%.0f kcal", calories_change)
  ) %>%
  select(Period, `Weight Δ`, `Steps Δ`, `Sedentary Δ`, `Light Δ`,
         `Fairly Δ`, `Very Δ`, `Calories Δ`)

# =============================================================================
# DISPLAY RESULTS
# =============================================================================

cat("=============================================================================\n")
cat("PERIOD-BASED ANALYSIS: DESCRIPTIVE STATISTICS\n")
cat("=============================================================================\n\n")
cat(sprintf("Baseline: Days %d to %d\n", baseline_start, baseline_end))
cat("Follow-up Periods: 1-30d, 31-60d, 61-90d, 91-180d, 181-365d, 1-45d, 46-90d\n")
cat("Weight: Lowest in period | Activity: Average in period (≥3 days)\n")
cat("Statistical Method: Random Effects Models (period_num predictor)\n\n")
cat("*** p<0.001, ** p<0.01, * p<0.05\n\n")

cat("--- Descriptive Statistics (Mean ± SD) ---\n\n")
print(publication_table, n = Inf)

cat("\n--- Changes from Baseline ---\n\n")
print(change_table, n = Inf)

if (use_mixed_models) {
  cat("\n--- Random Effects Model P-values ---\n\n")
  model_summary <- model_pvalues %>%
    mutate(
      Metric = case_when(
        metric == "weight" ~ "Weight (kg)",
        metric == "steps" ~ "Steps",
        metric == "sedentary" ~ "Sedentary (min)",
        metric == "light" ~ "Light Active (min)",
        metric == "fairly" ~ "Fairly Active (min)",
        metric == "very" ~ "Very Active (min)",
        metric == "calories" ~ "Activity Calories"
      ),
      `P-value` = sapply(pvalue, format_pvalue)
    ) %>%
    select(Metric, `P-value`)

  print(model_summary, n = Inf)
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n\n")

# Save tables
write_csv(publication_table, "period_analysis_descriptive.csv")
write_csv(change_table, "period_analysis_changes.csv")
write_csv(summary_stats, "period_analysis_raw_stats.csv")

if (use_mixed_models) {
  write_csv(model_pvalues, "period_analysis_model_pvalues.csv")

  # Save model summaries
  model_summaries <- map(names(models), ~{
    tidy(models[[.x]]) %>% mutate(outcome = .x)
  }) %>%
    bind_rows()

  write_csv(model_summaries, "period_analysis_model_coefficients.csv")
}

# Save long format data
write_csv(all_data_long, "period_analysis_long_data.csv")

cat("Files saved:\n")
cat("  - period_analysis_descriptive.csv (main table)\n")
cat("  - period_analysis_changes.csv (changes from baseline)\n")
cat("  - period_analysis_raw_stats.csv (raw statistics)\n")
if (use_mixed_models) {
  cat("  - period_analysis_model_pvalues.csv (p-values from models)\n")
  cat("  - period_analysis_model_coefficients.csv (full model results)\n")
}
cat("  - period_analysis_long_data.csv (long format data)\n\n")

cat("=============================================================================\n")
cat("PERIOD ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
