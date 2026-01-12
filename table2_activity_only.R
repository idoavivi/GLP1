# =============================================================================
# TABLE 2: ACTIVITY ANALYSIS WITH FIXED BASELINE (EXACT TABLE 1 LOGIC)
# =============================================================================
# FIXED BASELINE COHORT: N=286 patients (same as Table 1)
#   - BMI ≥ 30
#   - GLP-1 initiation
#   - ≥3 valid activity days in baseline (-180 to 0)
#   - ≥3 valid activity days in 1-30d follow-up
#
# ACTIVITY ONLY ANALYSIS (NO WEIGHT)
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("TABLE 2: ACTIVITY ANALYSIS (FIXED BASELINE)\n")
cat("Exact same logic as Table 1\n")
cat("##################################################\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
load("glp1_cleaned_data.RData")

# Check initial patient counts
cat(sprintf("Initial activity_cleaned patients: %d\n", length(unique(activity_cleaned$person_id))))
cat(sprintf("Initial bmi_data patients: %d\n", length(unique(bmi_data$person_id))))
cat("\n")

# =============================================================================
# STEP 1: DEFINE FIXED BASELINE COHORT - EXACT TABLE 1 LOGIC
# =============================================================================

cat("\n========================================\n")
cat("STEP 1: Defining Fixed Baseline Cohort\n")
cat("========================================\n\n")

# Filter to BMI >= 30 (already done in primary_analysis.R)
baseline_bmi_check <- bmi_data %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop")

patients_to_exclude_bmi <- baseline_bmi_check %>%
  filter(baseline_bmi < 30) %>%
  pull(person_id)

cat(sprintf("Patients with BMI < 30 excluded: %d\n", length(patients_to_exclude_bmi)))

activity_cleaned <- activity_cleaned %>% filter(!person_id %in% patients_to_exclude_bmi)
cat(sprintf("Remaining patients after BMI filter: %d\n", length(unique(activity_cleaned$person_id))))

# EXACT REPLICATION OF primary_analysis.R lines 69-96

# Patients with baseline activity (at least 3 valid days in -180 to 0 period)
baseline_activity_patients <- activity_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_baseline_days = n(), .groups = "drop")

cat(sprintf("Patients with ≥3 valid baseline activity days: %d\n",
            nrow(baseline_activity_patients)))

# Patients with 1-30d follow-up activity (SPECIFICALLY 1-30d, not any period!)
followup_1_30d_patients <- activity_cleaned %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 30,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_followup_days = n(), .groups = "drop")

cat(sprintf("Patients with ≥3 valid 1-30d follow-up days: %d\n",
            nrow(followup_1_30d_patients)))

# FIXED BASELINE COHORT = patients with BOTH baseline AND 1-30d follow-up
baseline_cohort_fixed <- baseline_activity_patients %>%
  inner_join(followup_1_30d_patients, by = "person_id")

cat(sprintf("\n*** FIXED BASELINE COHORT: N = %d ***\n", nrow(baseline_cohort_fixed)))
cat("(Should match Table 1: N=286)\n\n")

# Filter all data to baseline cohort
activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort_fixed$person_id)

# =============================================================================
# STEP 2: CALCULATE BASELINE ACTIVITY (ALL N=286)
# =============================================================================

cat("========================================\n")
cat("STEP 2: Baseline Activity\n")
cat("========================================\n\n")

baseline_activity <- activity_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  summarize(
    steps = mean(steps, na.rm = TRUE),
    sedentary_minutes = mean(sedentary_minutes, na.rm = TRUE),
    mvpa = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
    activity_calories = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period = "Baseline")

cat(sprintf("Baseline activity: N=%d (should be 286)\n\n", nrow(baseline_activity)))

# =============================================================================
# STEP 3: CALCULATE FOLLOW-UP ACTIVITY FOR EACH PERIOD
# =============================================================================

cat("========================================\n")
cat("STEP 3: Follow-up Activity\n")
cat("========================================\n\n")

# Define time periods
time_periods <- tribble(
  ~period, ~days_min, ~days_max,
  "1-30 days", 1, 30,
  "31-90 days", 31, 90,
  "91-180 days", 91, 180,
  "181-365 days", 181, 365
)

activity_by_period_list <- list()

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  # Calculate activity for patients with ≥3 valid days in this period
  period_activity <- activity_cleaned %>%
    filter(days_from_initiation >= days_min,
           days_from_initiation <= days_max,
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    summarize(
      steps = mean(steps, na.rm = TRUE),
      sedentary_minutes = mean(sedentary_minutes, na.rm = TRUE),
      mvpa = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
      activity_calories = mean(activity_calories, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(period = period_name)

  activity_by_period_list[[period_name]] <- period_activity
  cat(sprintf("  %s: N=%d (≥3 valid days)\n", period_name, nrow(period_activity)))
}

cat("\n")

# =============================================================================
# STEP 4: COMBINE ALL ACTIVITY DATA
# =============================================================================

cat("========================================\n")
cat("STEP 4: Combining Activity Data\n")
cat("========================================\n\n")

# Activity long format (baseline + all follow-up periods)
activity_long <- bind_rows(
  baseline_activity,
  bind_rows(activity_by_period_list)
)

cat(sprintf("Activity long format: %d observations from %d patients\n\n",
            nrow(activity_long), n_distinct(activity_long$person_id)))

# =============================================================================
# STEP 5: CALCULATE SUMMARY STATISTICS
# =============================================================================

cat("========================================\n")
cat("STEP 5: Summary Statistics\n")
cat("========================================\n\n")

activity_summary <- activity_long %>%
  group_by(period) %>%
  summarize(
    n = n(),
    mean_steps = mean(steps, na.rm = TRUE),
    sd_steps = sd(steps, na.rm = TRUE),
    mean_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    sd_sedentary = sd(sedentary_minutes, na.rm = TRUE),
    mean_mvpa = mean(mvpa, na.rm = TRUE),
    sd_mvpa = sd(mvpa, na.rm = TRUE),
    mean_calories = mean(activity_calories, na.rm = TRUE),
    sd_calories = sd(activity_calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period = factor(period, levels = c("Baseline", "1-30 days", "31-90 days",
                                             "91-180 days", "181-365 days"))) %>%
  arrange(period)

cat("Activity summary:\n")
print(activity_summary)
cat("\n")

# Validation check
baseline_n <- activity_summary %>% filter(period == "Baseline") %>% pull(n)
followup_1_30_n <- activity_summary %>% filter(period == "1-30 days") %>% pull(n)

cat("Validation:\n")
cat(sprintf("  Baseline N: %d (should be 286)\n", baseline_n))
cat(sprintf("  1-30 days N: %d (should be 286)\n", followup_1_30_n))

if(baseline_n == followup_1_30_n && baseline_n == nrow(baseline_cohort_fixed)) {
  cat("  ✓ Baseline and 1-30d N match!\n\n")
} else {
  cat("  ✗ MISMATCH - check logic!\n\n")
}

# =============================================================================
# STEP 6: CALCULATE P-VALUES USING MIXED EFFECTS MODELS
# =============================================================================

cat("========================================\n")
cat("STEP 6: Mixed Effects Models\n")
cat("========================================\n\n")

# Function to calculate p-values from mixed effects model
calculate_period_pvalues_lmer <- function(data, outcome_var, period_levels) {

  data <- data %>%
    mutate(period = factor(period, levels = period_levels)) %>%
    filter(!is.na(.data[[outcome_var]]))

  if(nrow(data) < 10 || n_distinct(data$person_id) < 3) {
    warning(paste("Insufficient data for", outcome_var, "model"))
    return(tibble(
      period = period_levels[-1],
      p_value = rep(NA_real_, length(period_levels) - 1)
    ))
  }

  formula_str <- paste0(outcome_var, " ~ period + (1 | person_id)")
  model <- lmer(as.formula(formula_str), data = data)
  coef_summary <- summary(model)$coefficients

  period_names <- period_levels[-1]
  pvalues <- tibble(
    period = period_names,
    p_value = coef_summary[2:(length(period_names)+1), "Pr(>|t|)"]
  )

  return(pvalues)
}

# Activity p-values
cat("Calculating activity p-values...\n")
steps_pvalues <- calculate_period_pvalues_lmer(
  activity_long,
  "steps",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

mvpa_pvalues <- calculate_period_pvalues_lmer(
  activity_long,
  "mvpa",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

sedentary_pvalues <- calculate_period_pvalues_lmer(
  activity_long,
  "sedentary_minutes",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

calories_pvalues <- calculate_period_pvalues_lmer(
  activity_long,
  "activity_calories",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

cat("P-values calculated.\n\n")

# =============================================================================
# STEP 7: CREATE WIDE FORMAT TABLE
# =============================================================================

cat("========================================\n")
cat("STEP 7: Creating Wide Format Table\n")
cat("========================================\n\n")

# Activity tables
activity_long_for_wide <- activity_summary %>%
  pivot_longer(
    cols = c(mean_steps, mean_mvpa, mean_sedentary, mean_calories),
    names_to = "metric_type",
    values_to = "mean_value"
  ) %>%
  pivot_longer(
    cols = c(sd_steps, sd_mvpa, sd_sedentary, sd_calories),
    names_to = "sd_type",
    values_to = "sd_value"
  ) %>%
  filter(
    (metric_type == "mean_steps" & sd_type == "sd_steps") |
    (metric_type == "mean_mvpa" & sd_type == "sd_mvpa") |
    (metric_type == "mean_sedentary" & sd_type == "sd_sedentary") |
    (metric_type == "mean_calories" & sd_type == "sd_calories")
  ) %>%
  mutate(
    Parameter = case_when(
      metric_type == "mean_steps" ~ "Steps per day",
      metric_type == "mean_mvpa" ~ "MVPA (min/day)",
      metric_type == "mean_sedentary" ~ "Sedentary (min/day)",
      metric_type == "mean_calories" ~ "Activity Calories (kcal/day)"
    ),
    value = sprintf("%d (%.0f ± %.0f)", n, mean_value, sd_value)
  ) %>%
  select(Parameter, period, value)

# Steps
steps_wide <- activity_long_for_wide %>%
  filter(Parameter == "Steps per day") %>%
  pivot_wider(names_from = period, values_from = value)

steps_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", steps_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", steps_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", steps_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", steps_pvalues$p_value[4])
)

# MVPA
mvpa_wide <- activity_long_for_wide %>%
  filter(Parameter == "MVPA (min/day)") %>%
  pivot_wider(names_from = period, values_from = value)

mvpa_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", mvpa_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", mvpa_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", mvpa_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", mvpa_pvalues$p_value[4])
)

# Sedentary
sedentary_wide <- activity_long_for_wide %>%
  filter(Parameter == "Sedentary (min/day)") %>%
  pivot_wider(names_from = period, values_from = value)

sedentary_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", sedentary_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", sedentary_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", sedentary_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", sedentary_pvalues$p_value[4])
)

# Calories
calories_wide <- activity_long_for_wide %>%
  filter(Parameter == "Activity Calories (kcal/day)") %>%
  pivot_wider(names_from = period, values_from = value)

calories_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", calories_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", calories_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", calories_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", calories_pvalues$p_value[4])
)

# Combine all
table2_activity <- bind_rows(
  steps_wide,
  steps_pvalue_row,
  mvpa_wide,
  mvpa_pvalue_row,
  sedentary_wide,
  sedentary_pvalue_row,
  calories_wide,
  calories_pvalue_row
) %>%
  select(Parameter, Baseline, `1-30 days`, `31-90 days`, `91-180 days`, `181-365 days`)

print(table2_activity)

# =============================================================================
# STEP 8: SAVE OUTPUTS
# =============================================================================

write_csv(table2_activity, "table2_activity_only.csv")
cat("\nSaved: table2_activity_only.csv\n")

table2_html <- table2_activity %>%
  kable(format = "html", escape = FALSE, align = c("l", rep("c", 5))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  add_header_above(c(" " = 1, "Time Period" = 5)) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  footnote(
    general = c(
      "Values shown as N (Mean ± SD).",
      "P-values from linear mixed effects models comparing each period to baseline.",
      paste0("Fixed baseline cohort: N=", nrow(baseline_cohort_fixed), " patients."),
      "Baseline = patients with ≥3 valid days in baseline AND 1-30d follow-up.",
      "Follow-up periods = patients with ≥3 valid days in that specific period."
    ),
    general_title = "Notes:",
    footnote_as_chunk = TRUE
  )

writeLines(as.character(table2_html), "table2_activity_only.html")
cat("Saved: table2_activity_only.html\n")

cat("\n##################################################\n")
cat("TABLE 2 ACTIVITY ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat(sprintf("Fixed baseline cohort: N=%d (should be 286)\n", nrow(baseline_cohort_fixed)))
cat(sprintf("Baseline activity N: %d\n", baseline_n))
cat(sprintf("1-30d follow-up N: %d\n\n", followup_1_30_n))
