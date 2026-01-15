# =============================================================================
# TABLE 2: ACTIVITY ANALYSIS WITH ADAPTIVE STATISTICS
# =============================================================================
# Automatically uses mean ± SD or median (IQR) based on normality testing
# Still uses mixed effects models for p-values (robust to non-normality)
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("TABLE 2: ACTIVITY ANALYSIS (ADAPTIVE)\n")
cat("Adaptive statistics: mean±SD or median(IQR)\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD DATA (or use objects in memory if available)
# =============================================================================

cat("Checking for data...\n")

# Check if objects exist in memory (from comprehensive_data_cleaning.R)
has_data_in_memory <- (exists("obesity_cohort") && is.data.frame(obesity_cohort) &&
                       (exists("activity_cleaned") || exists("activity_final")) &&
                       (exists("weight_cleaned") || exists("weight_final")) &&
                       (exists("bmi_data") || exists("bmi_final")))

if (has_data_in_memory) {
  cat("✓ Using data from memory (from comprehensive_data_cleaning.R)\n")

  # Handle object name mapping (comprehensive_data_cleaning.R uses _final suffix)
  if (exists("activity_final")) {
    activity_cleaned <- activity_final
  }
  if (exists("weight_final")) {
    weight_cleaned <- weight_final
  }
  if (exists("bmi_final")) {
    bmi_data <- bmi_final
  }
  if (exists("drug_final")) {
    drug_glp1_clean <- drug_final
  }
  if (exists("glp1_initiation_final")) {
    glp1_initiation <- glp1_initiation_final
  }
} else {
  cat("Loading from file: glp1_cleaned_data.RData\n")
  load("glp1_cleaned_data.RData")
}

cat(sprintf("  Obesity cohort: %d patients\n\n", nrow(obesity_cohort)))

# Check if normality recommendation exists
if (file.exists("normality_recommendation.csv")) {
  normality_rec <- read_csv("normality_recommendation.csv", show_col_types = FALSE)
  use_median <- normality_rec$recommendation[1] == "median_iqr"
  cat(sprintf("✓ Normality recommendation found: %s\n", normality_rec$recommendation[1]))
} else {
  # Default to mean ± SD
  use_median <- FALSE
  cat("⚠ No normality recommendation found. Using mean ± SD (default).\n")
  cat("  Run test_normality.R first to check normality.\n")
}

if (use_median) {
  cat("\nUsing MEDIAN (IQR) for all activity parameters\n")
  cat("(Data not normally distributed)\n\n")
} else {
  cat("\nUsing MEAN ± SD for all activity parameters\n")
  cat("(Data normally distributed)\n\n")
}

# =============================================================================
# STEP 1: DEFINE FIXED BASELINE COHORT - EXACT TABLE 1 LOGIC
# =============================================================================

cat("========================================\n")
cat("STEP 1: Defining Fixed Baseline Cohort\n")
cat("========================================\n\n")

# Patients with baseline activity (at least 3 valid days in -180 to 0 period)
baseline_activity_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_baseline_days = n(), .groups = "drop")

cat(sprintf("Patients with ≥3 valid baseline activity days: %d\n",
            nrow(baseline_activity_patients)))

# Patients with 1-30d follow-up activity
followup_1_30d_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
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

cat(sprintf("\n*** FIXED BASELINE COHORT: N = %d ***\n\n", nrow(baseline_cohort_fixed)))

# Filter all data to baseline cohort
activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort_fixed$person_id)

# =============================================================================
# STEP 2: CALCULATE BASELINE ACTIVITY
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

cat(sprintf("Baseline activity: N=%d\n\n", nrow(baseline_activity)))

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
# STEP 5: CALCULATE SUMMARY STATISTICS (ADAPTIVE)
# =============================================================================

cat("========================================\n")
cat("STEP 5: Summary Statistics\n")
cat("========================================\n\n")

if (use_median) {
  # Calculate median and IQR
  activity_summary <- activity_long %>%
    group_by(period) %>%
    summarize(
      n = n(),
      median_steps = median(steps, na.rm = TRUE),
      q25_steps = quantile(steps, 0.25, na.rm = TRUE),
      q75_steps = quantile(steps, 0.75, na.rm = TRUE),
      median_sedentary = median(sedentary_minutes, na.rm = TRUE),
      q25_sedentary = quantile(sedentary_minutes, 0.25, na.rm = TRUE),
      q75_sedentary = quantile(sedentary_minutes, 0.75, na.rm = TRUE),
      median_mvpa = median(mvpa, na.rm = TRUE),
      q25_mvpa = quantile(mvpa, 0.25, na.rm = TRUE),
      q75_mvpa = quantile(mvpa, 0.75, na.rm = TRUE),
      median_calories = median(activity_calories, na.rm = TRUE),
      q25_calories = quantile(activity_calories, 0.25, na.rm = TRUE),
      q75_calories = quantile(activity_calories, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(period = factor(period, levels = c("Baseline", "1-30 days", "31-90 days",
                                               "91-180 days", "181-365 days"))) %>%
    arrange(period)
} else {
  # Calculate mean and SD
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
}

cat("Activity summary:\n")
print(activity_summary)
cat("\n")

# =============================================================================
# STEP 6: CALCULATE P-VALUES USING MIXED EFFECTS MODELS
# =============================================================================

cat("========================================\n")
cat("STEP 6: Mixed Effects Models\n")
cat("========================================\n\n")

cat("Note: Mixed effects models are robust to non-normality with sufficient sample size.\n")
cat("      P-values remain valid even when using median (IQR) for descriptive statistics.\n\n")

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

# Format values based on use_median
if (use_median) {
  activity_long_for_wide <- activity_summary %>%
    mutate(
      steps_value = sprintf("%d (%.0f, %.0f)", n, median_steps, q75_steps - q25_steps),
      mvpa_value = sprintf("%d (%.0f, %.0f)", n, median_mvpa, q75_mvpa - q25_mvpa),
      sedentary_value = sprintf("%d (%.0f, %.0f)", n, median_sedentary, q75_sedentary - q25_sedentary),
      calories_value = sprintf("%d (%.0f, %.0f)", n, median_calories, q75_calories - q25_calories)
    ) %>%
    select(period, n, steps_value, mvpa_value, sedentary_value, calories_value)

  value_format_note <- "Values shown as N (Median, IQR)."
} else {
  activity_long_for_wide <- activity_summary %>%
    mutate(
      steps_value = sprintf("%d (%.0f ± %.0f)", n, mean_steps, sd_steps),
      mvpa_value = sprintf("%d (%.0f ± %.0f)", n, mean_mvpa, sd_mvpa),
      sedentary_value = sprintf("%d (%.0f ± %.0f)", n, mean_sedentary, sd_sedentary),
      calories_value = sprintf("%d (%.0f ± %.0f)", n, mean_calories, sd_calories)
    ) %>%
    select(period, n, steps_value, mvpa_value, sedentary_value, calories_value)

  value_format_note <- "Values shown as N (Mean ± SD)."
}

# Steps
steps_wide <- activity_long_for_wide %>%
  select(period, value = steps_value) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(Parameter = "Steps per day", .before = 1)

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
  select(period, value = mvpa_value) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(Parameter = "MVPA (min/day)", .before = 1)

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
  select(period, value = sedentary_value) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(Parameter = "Sedentary (min/day)", .before = 1)

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
  select(period, value = calories_value) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(Parameter = "Activity Calories (kcal/day)", .before = 1)

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

write_csv(table2_activity, "table2_activity_adaptive.csv")
cat("\nSaved: table2_activity_adaptive.csv\n")

table2_html <- table2_activity %>%
  kable(format = "html", escape = FALSE, align = c("l", rep("c", 5))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  add_header_above(c(" " = 1, "Time Period" = 5)) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  footnote(
    general = c(
      value_format_note,
      "P-values from linear mixed effects models comparing each period to baseline.",
      "Mixed effects models are robust to non-normality with sufficient sample size.",
      paste0("Fixed baseline cohort: N=", nrow(baseline_cohort_fixed), " patients."),
      "Baseline = patients with ≥3 valid days in baseline AND 1-30d follow-up.",
      "Follow-up periods = patients with ≥3 valid days in that specific period."
    ),
    general_title = "Notes:",
    footnote_as_chunk = TRUE
  )

writeLines(as.character(table2_html), "table2_activity_adaptive.html")
cat("Saved: table2_activity_adaptive.html\n")

cat("\n##################################################\n")
cat("TABLE 2 ACTIVITY ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat(sprintf("Fixed baseline cohort: N=%d\n", nrow(baseline_cohort_fixed)))
cat(sprintf("Statistics used: %s\n", ifelse(use_median, "Median (IQR)", "Mean ± SD")))
