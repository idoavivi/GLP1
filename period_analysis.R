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
# BASELINE (-180 to 0 days before GLP-1 initiation)
# =============================================================================

cat("### BASELINE SETUP ###\n\n")

# FIXED baseline window: -180 to 0 days
baseline_start <- -180
baseline_end <- 0

cat(sprintf("Baseline window: %d to %d days before GLP-1 initiation\n\n", baseline_start, baseline_end))

# Get baseline ACTIVITY data - AVERAGE during baseline period
baseline_activity <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_start,
         days_from_initiation <= baseline_end) %>%
  # Calculate wear time and MVPA for each day
  mutate(
    wear_time = sedentary_minutes + lightly_active_minutes + fairly_active_minutes + very_active_minutes,
    MVPA = fairly_active_minutes + very_active_minutes
  ) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%  # Minimum 3 days
  summarize(
    baseline_steps = mean(steps, na.rm = TRUE),
    baseline_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    baseline_light = mean(lightly_active_minutes, na.rm = TRUE),
    baseline_fairly = mean(fairly_active_minutes, na.rm = TRUE),
    baseline_very = mean(very_active_minutes, na.rm = TRUE),
    baseline_MVPA = mean(MVPA, na.rm = TRUE),
    baseline_calories = mean(activity_calories, na.rm = TRUE),
    baseline_wear_time = mean(wear_time, na.rm = TRUE),
    # Calculate % of wear time
    baseline_sedentary_pct = mean(100 * sedentary_minutes / wear_time, na.rm = TRUE),
    baseline_light_pct = mean(100 * lightly_active_minutes / wear_time, na.rm = TRUE),
    baseline_MVPA_pct = mean(100 * MVPA / wear_time, na.rm = TRUE),
    n_baseline_days = n(),
    .groups = "drop"
  )

# Get baseline WEIGHT data - HIGHEST weight (starting weight)
baseline_weight <- weight_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= baseline_start,
         days_from_initiation <= baseline_end,
         !is.na(weight_kg)) %>%
  group_by(person_id) %>%
  summarize(baseline_weight = max(weight_kg, na.rm = TRUE), .groups = "drop")  # HIGHEST weight

# Create BASELINE COHORT - patients with BOTH activity AND weight at baseline
# AND who have follow-up data in first period (1-30d)
cat("Checking for follow-up data in first period (1-30d)...\n")

first_period_patients <- activity_with_glp1 %>%
  filter(person_id %in% eligible_person_ids,
         days_from_initiation >= 1,
         days_from_initiation <= 30) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%  # Minimum 3 days
  ungroup() %>%
  distinct(person_id)

cat(sprintf("  Patients with ≥3 days in period 1-30d: %d\n\n", nrow(first_period_patients)))

baseline_cohort <- baseline_activity %>%
  inner_join(baseline_weight, by = "person_id") %>%
  inner_join(first_period_patients, by = "person_id") %>%  # REQUIRE follow-up in first period
  select(person_id)

cat(sprintf("Patients with baseline activity (≥3 days): %d\n", nrow(baseline_activity)))
cat(sprintf("Patients with baseline weight: %d\n", nrow(baseline_weight)))
cat(sprintf("Patients with follow-up in 1-30d: %d\n", nrow(first_period_patients)))
cat(sprintf("BASELINE COHORT (all criteria met): %d patients\n\n", nrow(baseline_cohort)))

# Keep full baseline data for this cohort
baseline_activity_final <- baseline_activity %>%
  inner_join(baseline_cohort, by = "person_id")

baseline_weight_final <- baseline_weight %>%
  inner_join(baseline_cohort, by = "person_id")

# Report baseline statistics
cat(sprintf("Baseline cohort statistics:\n"))
cat(sprintf("  Mean weight: %.1f kg (SD: %.1f)\n",
            mean(baseline_weight_final$baseline_weight),
            sd(baseline_weight_final$baseline_weight)))
cat(sprintf("  Mean steps: %.0f (SD: %.0f)\n\n",
            mean(baseline_activity_final$baseline_steps),
            sd(baseline_activity_final$baseline_steps)))

# =============================================================================
# DEFINE TIME PERIODS
# =============================================================================

cat("### DEFINING TIME PERIODS ###\n\n")

time_periods <- list(
  "1-30d" = c(1, 30),
  "31-60d" = c(31, 60),
  "61-90d" = c(61, 90),
  "1-90d" = c(1, 90),        # Aggregated: first 3 months
  "91-180d" = c(91, 180),    # Aggregated: 3-6 months
  "181-365d" = c(181, 365),  # Aggregated: 6-12 months
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

  # Activity data for this period - ONLY baseline cohort patients
  activity_period <- activity_with_glp1 %>%
    inner_join(baseline_cohort, by = "person_id") %>%
    filter(days_from_initiation >= start_day,
           days_from_initiation <= end_day) %>%
    # Calculate wear time and MVPA for each day
    mutate(
      wear_time = sedentary_minutes + lightly_active_minutes + fairly_active_minutes + very_active_minutes,
      MVPA = fairly_active_minutes + very_active_minutes
    ) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%  # Minimum 3 days
    summarize(
      period_steps = mean(steps, na.rm = TRUE),
      period_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      period_light = mean(lightly_active_minutes, na.rm = TRUE),
      period_fairly = mean(fairly_active_minutes, na.rm = TRUE),
      period_very = mean(very_active_minutes, na.rm = TRUE),
      period_MVPA = mean(MVPA, na.rm = TRUE),
      period_calories = mean(activity_calories, na.rm = TRUE),
      period_wear_time = mean(wear_time, na.rm = TRUE),
      # Calculate % of wear time
      period_sedentary_pct = mean(100 * sedentary_minutes / wear_time, na.rm = TRUE),
      period_light_pct = mean(100 * lightly_active_minutes / wear_time, na.rm = TRUE),
      period_MVPA_pct = mean(100 * MVPA / wear_time, na.rm = TRUE),
      # MVPA diagnostics
      n_days_with_MVPA = sum(MVPA > 0, na.rm = TRUE),
      pct_days_with_MVPA = 100 * sum(MVPA > 0, na.rm = TRUE) / n(),
      n_period_days = n(),
      .groups = "drop"
    )

  # Weight data for this period - ONLY baseline cohort patients, LOWEST weight
  weight_period <- weight_with_glp1 %>%
    inner_join(baseline_cohort, by = "person_id") %>%
    filter(days_from_initiation >= start_day,
           days_from_initiation <= end_day,
           !is.na(weight_kg)) %>%
    group_by(person_id) %>%
    summarize(
      period_weight = min(weight_kg, na.rm = TRUE),  # LOWEST weight in period
      .groups = "drop"
    )

  # Check active treatment at midpoint of period (VECTORIZED)
  # REQUIREMENT: ≥2 prescription fills AND prescription within 90 days of midpoint
  all_patients <- unique(c(activity_period$person_id, weight_period$person_id))

  # First, filter to patients with ≥2 prescription fills
  patients_with_multiple_fills <- drug_glp1_clean %>%
    filter(person_id %in% all_patients) %>%
    group_by(person_id) %>%
    summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
    filter(n_fills >= 2) %>%
    select(person_id)

  # Then check active prescription at midpoint
  active_treatment_status <- tibble(person_id = all_patients) %>%
    inner_join(patients_with_multiple_fills, by = "person_id") %>%  # REQUIRE ≥2 fills
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

  cat(sprintf("  Patients with ≥2 fills: %d/%d\n", nrow(patients_with_multiple_fills), length(all_patients)))
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
baseline_long <- baseline_activity_final %>%
  inner_join(baseline_weight_final, by = "person_id") %>%
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
# PERIOD-SPECIFIC BASELINES AND PAIRED T-TESTS
# =============================================================================

cat("### CALCULATING PERIOD-SPECIFIC BASELINES & PAIRED T-TESTS ###\n\n")
cat("Note: Each period's baseline includes ONLY patients who appear in that period\n")
cat("      This ensures proper paired comparison (same patients at baseline and follow-up)\n\n")

# Get all baseline data
baseline_data_all <- all_data_long %>%
  filter(period == "Baseline") %>%
  select(person_id, baseline_weight = weight, baseline_steps = steps,
         baseline_sedentary = sedentary, baseline_light = light,
         baseline_fairly = fairly, baseline_very = very,
         baseline_calories = calories)

# Get period names
period_names <- unique(all_data_long$period)
period_names <- period_names[period_names != "Baseline"]

# Initialize results lists
period_specific_results <- list()
paired_test_results <- list()

for (pname in period_names) {
  cat(sprintf("Processing %s...\n", pname))

  # Get period data
  period_data <- all_data_long %>%
    filter(period == pname) %>%
    select(person_id, period_weight = weight, period_steps = steps,
           period_sedentary = sedentary, period_light = light,
           period_fairly = fairly, period_very = very,
           period_calories = calories)

  # Get baseline data for patients in this period
  baseline_data_period <- baseline_data_all %>%
    inner_join(period_data %>% select(person_id), by = "person_id")

  # Calculate baseline statistics for THIS period's cohort
  baseline_stats <- baseline_data_period %>%
    summarize(
      Period = paste0(pname, "_baseline"),
      N = n(),
      weight_mean = mean(baseline_weight, na.rm = TRUE),
      weight_sd = sd(baseline_weight, na.rm = TRUE),
      steps_mean = mean(baseline_steps, na.rm = TRUE),
      steps_sd = sd(baseline_steps, na.rm = TRUE),
      sedentary_mean = mean(baseline_sedentary, na.rm = TRUE),
      sedentary_sd = sd(baseline_sedentary, na.rm = TRUE),
      light_mean = mean(baseline_light, na.rm = TRUE),
      light_sd = sd(baseline_light, na.rm = TRUE),
      fairly_mean = mean(baseline_fairly, na.rm = TRUE),
      fairly_sd = sd(baseline_fairly, na.rm = TRUE),
      very_mean = mean(baseline_very, na.rm = TRUE),
      very_sd = sd(baseline_very, na.rm = TRUE),
      calories_mean = mean(baseline_calories, na.rm = TRUE),
      calories_sd = sd(baseline_calories, na.rm = TRUE)
    )

  # Calculate period statistics
  period_stats <- period_data %>%
    summarize(
      Period = pname,
      N = n(),
      weight_mean = mean(period_weight, na.rm = TRUE),
      weight_sd = sd(period_weight, na.rm = TRUE),
      steps_mean = mean(period_steps, na.rm = TRUE),
      steps_sd = sd(period_steps, na.rm = TRUE),
      sedentary_mean = mean(period_sedentary, na.rm = TRUE),
      sedentary_sd = sd(period_sedentary, na.rm = TRUE),
      light_mean = mean(period_light, na.rm = TRUE),
      light_sd = sd(period_light, na.rm = TRUE),
      fairly_mean = mean(period_fairly, na.rm = TRUE),
      fairly_sd = sd(period_fairly, na.rm = TRUE),
      very_mean = mean(period_very, na.rm = TRUE),
      very_sd = sd(period_very, na.rm = TRUE),
      calories_mean = mean(period_calories, na.rm = TRUE),
      calories_sd = sd(period_calories, na.rm = TRUE)
    )

  # Merge baseline and period data for paired analysis
  merged <- baseline_data_period %>%
    inner_join(period_data, by = "person_id")

  # Filter to complete pairs (both baseline and period data)
  merged_complete <- merged %>%
    filter(!is.na(baseline_weight), !is.na(period_weight),
           !is.na(baseline_steps), !is.na(period_steps))

  if (nrow(merged_complete) < 10) {
    cat(sprintf("  WARNING: Only %d complete pairs, skipping t-tests\n", nrow(merged_complete)))
    next
  }

  # Run paired t-tests
  weight_test <- t.test(merged_complete$period_weight, merged_complete$baseline_weight, paired = TRUE)
  steps_test <- t.test(merged_complete$period_steps, merged_complete$baseline_steps, paired = TRUE)
  sedentary_test <- t.test(merged_complete$period_sedentary, merged_complete$baseline_sedentary, paired = TRUE)
  light_test <- t.test(merged_complete$period_light, merged_complete$baseline_light, paired = TRUE)
  fairly_test <- t.test(merged_complete$period_fairly, merged_complete$baseline_fairly, paired = TRUE)
  very_test <- t.test(merged_complete$period_very, merged_complete$baseline_very, paired = TRUE)
  calories_test <- t.test(merged_complete$period_calories, merged_complete$baseline_calories, paired = TRUE)

  # Calculate changes
  weight_change <- period_stats$weight_mean - baseline_stats$weight_mean
  weight_pct <- 100 * weight_change / baseline_stats$weight_mean
  steps_change <- period_stats$steps_mean - baseline_stats$steps_mean
  steps_pct <- 100 * steps_change / baseline_stats$steps_mean
  sedentary_change <- period_stats$sedentary_mean - baseline_stats$sedentary_mean
  light_change <- period_stats$light_mean - baseline_stats$light_mean
  fairly_change <- period_stats$fairly_mean - baseline_stats$fairly_mean
  very_change <- period_stats$very_mean - baseline_stats$very_mean
  calories_change <- period_stats$calories_mean - baseline_stats$calories_mean

  # Store results
  period_specific_results[[pname]] <- list(
    baseline = baseline_stats,
    period = period_stats,
    n_paired = nrow(merged_complete)
  )

  paired_test_results[[pname]] <- tibble(
    Period = pname,
    N = period_stats$N,
    N_paired = nrow(merged_complete),
    # Baseline values
    baseline_weight = baseline_stats$weight_mean,
    baseline_weight_sd = baseline_stats$weight_sd,
    baseline_steps = baseline_stats$steps_mean,
    baseline_steps_sd = baseline_stats$steps_sd,
    baseline_sedentary = baseline_stats$sedentary_mean,
    baseline_sedentary_sd = baseline_stats$sedentary_sd,
    baseline_light = baseline_stats$light_mean,
    baseline_light_sd = baseline_stats$light_sd,
    baseline_fairly = baseline_stats$fairly_mean,
    baseline_fairly_sd = baseline_stats$fairly_sd,
    baseline_very = baseline_stats$very_mean,
    baseline_very_sd = baseline_stats$very_sd,
    baseline_calories = baseline_stats$calories_mean,
    baseline_calories_sd = baseline_stats$calories_sd,
    # Period values
    period_weight = period_stats$weight_mean,
    period_weight_sd = period_stats$weight_sd,
    period_steps = period_stats$steps_mean,
    period_steps_sd = period_stats$steps_sd,
    period_sedentary = period_stats$sedentary_mean,
    period_sedentary_sd = period_stats$sedentary_sd,
    period_light = period_stats$light_mean,
    period_light_sd = period_stats$light_sd,
    period_fairly = period_stats$fairly_mean,
    period_fairly_sd = period_stats$fairly_sd,
    period_very = period_stats$very_mean,
    period_very_sd = period_stats$very_sd,
    period_calories = period_stats$calories_mean,
    period_calories_sd = period_stats$calories_sd,
    # Changes
    weight_change = weight_change,
    weight_pct = weight_pct,
    steps_change = steps_change,
    steps_pct = steps_pct,
    sedentary_change = sedentary_change,
    light_change = light_change,
    fairly_change = fairly_change,
    very_change = very_change,
    calories_change = calories_change,
    # P-values
    weight_p = weight_test$p.value,
    steps_p = steps_test$p.value,
    sedentary_p = sedentary_test$p.value,
    light_p = light_test$p.value,
    fairly_p = fairly_test$p.value,
    very_p = very_test$p.value,
    calories_p = calories_test$p.value
  )

  cat(sprintf("  N=%d patients, %d complete pairs\n", period_stats$N, nrow(merged_complete)))
}

# Combine all results
paired_test_table <- bind_rows(paired_test_results)

# Create comprehensive publication table
comprehensive_table <- paired_test_table %>%
  mutate(
    # Format baseline values
    `Baseline Weight (kg)` = sprintf("%.1f ± %.1f", baseline_weight, baseline_weight_sd),
    `Baseline Steps` = sprintf("%.0f ± %.0f", baseline_steps, baseline_steps_sd),
    # Format period values
    `Weight (kg)` = sprintf("%.1f ± %.1f", period_weight, period_weight_sd),
    `Steps (n/day)` = sprintf("%.0f ± %.0f", period_steps, period_steps_sd),
    `Sedentary (min)` = sprintf("%.0f ± %.0f", period_sedentary, period_sedentary_sd),
    `Light Active (min)` = sprintf("%.0f ± %.0f", period_light, period_light_sd),
    `Fairly Active (min)` = sprintf("%.0f ± %.0f", period_fairly, period_fairly_sd),
    `Very Active (min)` = sprintf("%.0f ± %.0f", period_very, period_very_sd),
    `Activity Cal (kcal)` = sprintf("%.0f ± %.0f", period_calories, period_calories_sd),
    # Format changes
    `Weight Δ` = sprintf("%.1f (%.1f%%)", weight_change, weight_pct),
    `Steps Δ` = sprintf("%.0f (%.1f%%)", steps_change, steps_pct),
    `Sedentary Δ` = sprintf("%.0f", sedentary_change),
    `Light Δ` = sprintf("%.0f", light_change),
    `Fairly Δ` = sprintf("%.0f", fairly_change),
    `Very Δ` = sprintf("%.0f", very_change),
    `Calories Δ` = sprintf("%.0f", calories_change),
    # Format p-values
    `Weight p` = sapply(weight_p, format_pvalue),
    `Steps p` = sapply(steps_p, format_pvalue),
    `Sed p` = sapply(sedentary_p, format_pvalue),
    `Light p` = sapply(light_p, format_pvalue),
    `Fairly p` = sapply(fairly_p, format_pvalue),
    `Very p` = sapply(very_p, format_pvalue),
    `Cal p` = sapply(calories_p, format_pvalue)
  ) %>%
  select(Period, N, N_paired,
         `Baseline Weight (kg)`, `Weight (kg)`, `Weight Δ`, `Weight p`,
         `Baseline Steps`, `Steps (n/day)`, `Steps Δ`, `Steps p`,
         `Sedentary (min)`, `Sedentary Δ`, `Sed p`,
         `Light Active (min)`, `Light Δ`, `Light p`,
         `Fairly Active (min)`, `Fairly Δ`, `Fairly p`,
         `Very Active (min)`, `Very Δ`, `Very p`,
         `Activity Cal (kcal)`, `Calories Δ`, `Cal p`)

cat("\n")

# =============================================================================
# CREATE PERIOD SUMMARY TABLE WITH WEAR TIME & MVPA DIAGNOSTICS
# =============================================================================

cat("### CREATING PERIOD SUMMARY WITH DIAGNOSTICS ###\n\n")

# Combine all period data
period_summary_table <- bind_rows(period_data_list) %>%
  group_by(period) %>%
  summarize(
    n_patients = n(),
    mean_weight = mean(period_weight, na.rm = TRUE),
    sd_weight = sd(period_weight, na.rm = TRUE),
    mean_steps = mean(period_steps, na.rm = TRUE),
    sd_steps = sd(period_steps, na.rm = TRUE),
    mean_sedentary_min = mean(period_sedentary, na.rm = TRUE),
    sd_sedentary_min = sd(period_sedentary, na.rm = TRUE),
    mean_light_min = mean(period_light, na.rm = TRUE),
    sd_light_min = sd(period_light, na.rm = TRUE),
    mean_fairly_min = mean(period_fairly, na.rm = TRUE),
    sd_fairly_min = sd(period_fairly, na.rm = TRUE),
    mean_very_min = mean(period_very, na.rm = TRUE),
    sd_very_min = sd(period_very, na.rm = TRUE),
    mean_MVPA = mean(period_MVPA, na.rm = TRUE),
    sd_MVPA = sd(period_MVPA, na.rm = TRUE),
    mean_wear_time = mean(period_wear_time, na.rm = TRUE),
    sd_wear_time = sd(period_wear_time, na.rm = TRUE),
    mean_sedentary_pct = mean(period_sedentary_pct, na.rm = TRUE),
    sd_sedentary_pct = sd(period_sedentary_pct, na.rm = TRUE),
    mean_light_pct = mean(period_light_pct, na.rm = TRUE),
    sd_light_pct = sd(period_light_pct, na.rm = TRUE),
    mean_MVPA_pct = mean(period_MVPA_pct, na.rm = TRUE),
    sd_MVPA_pct = sd(period_MVPA_pct, na.rm = TRUE),
    mean_days_with_MVPA = mean(n_days_with_MVPA, na.rm = TRUE),
    mean_pct_days_with_MVPA = mean(pct_days_with_MVPA, na.rm = TRUE),
    .groups = "drop"
  )

# Add baseline to the summary
baseline_summary <- baseline_activity_final %>%
  inner_join(baseline_weight_final, by = "person_id") %>%
  summarize(
    period = "Baseline",
    n_patients = n(),
    mean_weight = mean(baseline_weight, na.rm = TRUE),
    sd_weight = sd(baseline_weight, na.rm = TRUE),
    mean_steps = mean(baseline_steps, na.rm = TRUE),
    sd_steps = sd(baseline_steps, na.rm = TRUE),
    mean_sedentary_min = mean(baseline_sedentary, na.rm = TRUE),
    sd_sedentary_min = sd(baseline_sedentary, na.rm = TRUE),
    mean_light_min = mean(baseline_light, na.rm = TRUE),
    sd_light_min = sd(baseline_light, na.rm = TRUE),
    mean_fairly_min = mean(baseline_fairly, na.rm = TRUE),
    sd_fairly_min = sd(baseline_fairly, na.rm = TRUE),
    mean_very_min = mean(baseline_very, na.rm = TRUE),
    sd_very_min = sd(baseline_very, na.rm = TRUE),
    mean_MVPA = mean(baseline_MVPA, na.rm = TRUE),
    sd_MVPA = sd(baseline_MVPA, na.rm = TRUE),
    mean_wear_time = mean(baseline_wear_time, na.rm = TRUE),
    sd_wear_time = sd(baseline_wear_time, na.rm = TRUE),
    mean_sedentary_pct = mean(baseline_sedentary_pct, na.rm = TRUE),
    sd_sedentary_pct = sd(baseline_sedentary_pct, na.rm = TRUE),
    mean_light_pct = mean(baseline_light_pct, na.rm = TRUE),
    sd_light_pct = sd(baseline_light_pct, na.rm = TRUE),
    mean_MVPA_pct = mean(baseline_MVPA_pct, na.rm = TRUE),
    sd_MVPA_pct = sd(baseline_MVPA_pct, na.rm = TRUE),
    mean_days_with_MVPA = NA_real_,
    mean_pct_days_with_MVPA = NA_real_
  )

period_summary_table <- bind_rows(baseline_summary, period_summary_table)

cat("\n")

# =============================================================================
# DISPLAY RESULTS
# =============================================================================

cat("=============================================================================\n")
cat("COMPREHENSIVE PUBLICATION TABLE WITH PERIOD-SPECIFIC BASELINES\n")
cat("=============================================================================\n\n")
cat(sprintf("Baseline Window: Days %d to %d before GLP-1 initiation\n", baseline_start, baseline_end))
cat("Baseline Method: HIGHEST weight, AVERAGE activity (≥3 days)\n")
cat("Follow-up Method: LOWEST weight, AVERAGE activity (≥3 days)\n")
cat("Eligibility: Active treatment (≥2 prescription fills, prescription within 90 days)\n\n")
cat("IMPORTANT: Each period has its own baseline calculated from patients in that period\n")
cat("           This ensures proper paired comparison (N_paired = same patients)\n\n")
cat("Statistical Tests: Paired t-tests (each period vs its specific baseline)\n")
cat("*** p<0.001, ** p<0.01, * p<0.05\n\n")

print(comprehensive_table, n = Inf, width = Inf)

cat("\n=============================================================================\n")
cat("INTERPRETATION GUIDE\n")
cat("=============================================================================\n")
cat("• Period: Time period after GLP-1 initiation\n")
cat("• N: Total number of patients with data in this period\n")
cat("• N_paired: Number of patients with BOTH baseline and period data\n")
cat("• Baseline columns: Statistics for patients in THIS period at baseline\n")
cat("• Period columns: Statistics for patients during follow-up period\n")
cat("• Δ: Change from baseline (absolute and % for weight/steps)\n")
cat("• p: P-value from paired t-test (period vs baseline, same patients)\n")
cat("• Each period has its own baseline cohort for proper paired analysis\n")
cat("=============================================================================\n\n")

if (use_mixed_models) {
  cat("\n--- Random Effects Model P-values (Linear Trend) ---\n\n")
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
  cat("\nNote: These p-values test for linear trend across ALL periods (period_num effect)\n")
  cat("      Paired t-test p-values in the table above test each period vs baseline\n\n")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n\n")

# Save main comprehensive table
write_csv(comprehensive_table, "period_analysis_comprehensive_table.csv")
cat("  ✓ period_analysis_comprehensive_table.csv (publication table with stats)\n")

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
    column_spec(2, width = "4em") %>%
    add_header_above(c(" " = 2, "Weight" = 3, "Steps" = 3, "Sedentary" = 2,
                       "Light" = 2, "Fairly" = 2, "Very" = 2, "Calories" = 2)) %>%
    footnote(
      general = c(
        "Baseline: Days -180 to 0 (highest weight, average activity)",
        "Follow-up: Active treatment only (≥2 prescription fills)",
        "Statistical Tests: Paired t-tests (each period vs baseline)",
        "*** p<0.001, ** p<0.01, * p<0.05"
      ),
      general_title = "Notes:"
    )

  save_kable(html_table, "period_analysis_comprehensive_table.html")
  cat("  ✓ period_analysis_comprehensive_table.html (formatted HTML table)\n")
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
    "</style>\n",
    "</head>\n<body>\n",
    "<h1>GLP-1 Period Analysis - Comprehensive Results</h1>\n",
    "<p><strong>Baseline:</strong> Days -180 to 0 (highest weight, average activity)<br>\n",
    "<strong>Follow-up:</strong> Active treatment only (≥2 prescription fills)<br>\n",
    "<strong>Statistical Tests:</strong> Paired t-tests (each period vs baseline)</p>\n"
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
    "p = P-value from paired t-test</p>\n",
    "</div>\n",
    "</body>\n</html>"
  )

  writeLines(html_output, "period_analysis_comprehensive_table.html")
  cat("  ✓ period_analysis_comprehensive_table.html (simple HTML table)\n")
}

# Save detailed tables
write_csv(publication_table, "period_analysis_descriptive.csv")
cat("  ✓ period_analysis_descriptive.csv (descriptive statistics only)\n")

write_csv(change_table, "period_analysis_changes.csv")
cat("  ✓ period_analysis_changes.csv (changes from baseline)\n")

write_csv(paired_test_table, "period_analysis_paired_tests.csv")
cat("  ✓ period_analysis_paired_tests.csv (paired t-test results)\n")

write_csv(period_summary_table, "period_analysis_summary_with_diagnostics.csv")
cat("  ✓ period_analysis_summary_with_diagnostics.csv (wear time & MVPA diagnostics)\n")

write_csv(summary_stats, "period_analysis_raw_stats.csv")
cat("  ✓ period_analysis_raw_stats.csv (raw statistics)\n")

if (use_mixed_models) {
  write_csv(model_pvalues, "period_analysis_model_pvalues.csv")
  cat("  ✓ period_analysis_model_pvalues.csv (random effects p-values)\n")

  # Save model summaries
  model_summaries <- map(names(models), ~{
    tidy(models[[.x]]) %>% mutate(outcome = .x)
  }) %>%
    bind_rows()

  write_csv(model_summaries, "period_analysis_model_coefficients.csv")
  cat("  ✓ period_analysis_model_coefficients.csv (full model results)\n")
}

# Save long format data
write_csv(all_data_long, "period_analysis_long_data.csv")
cat("  ✓ period_analysis_long_data.csv (long format data for analysis)\n")

# Save all results to RData file
save(
  comprehensive_table,
  publication_table,
  change_table,
  paired_test_table,
  period_summary_table,
  summary_stats,
  all_data_long,
  baseline_activity_final,
  baseline_weight_final,
  period_data_list,
  file = "period_analysis_results.RData"
)
cat("  ✓ period_analysis_results.RData (all results for visualization)\n")

cat("\nPRIMARY OUTPUT: period_analysis_comprehensive_table.csv / .html\n")

cat("=============================================================================\n")
cat("PERIOD ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
