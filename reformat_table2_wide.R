# =============================================================================
# CREATE PUBLICATION-READY TABLE 2 (WIDE FORMAT) WITH FUNNEL DESIGN
# =============================================================================
# Reformats Table 2 with periods as columns and metrics as rows
# Each period shows: N, Mean ± SD
# P-values compare each period to baseline using mixed effects models
#
# FUNNEL DESIGN FOR WEIGHT: Only include patients with prior measurements
# COMPLETE CASE FOR ACTIVITY: All patients need all periods
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("REFORMATTING TABLE 2 FOR PUBLICATION\n")
cat("Wide format: Periods as columns\n")
cat("Funnel design for weight outcomes\n")
cat("##################################################\n\n")

# Load cleaned data if running standalone
if(!exists("weight_summary") || !exists("activity_summary")) {
  cat("Loading cleaned data...\n")
  load("glp1_cleaned_data.RData")

  # Need to recreate the data structures from primary_analysis.R
  cat("Note: This script should ideally be run after primary_analysis.R\n")
  cat("Creating necessary data structures...\n\n")

  # Filter to baseline cohort with BMI >= 30
  baseline_bmi_check <- bmi_data %>%
    filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
    group_by(person_id) %>%
    summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop")

  patients_to_exclude <- baseline_bmi_check %>%
    filter(baseline_bmi < 30) %>%
    pull(person_id)

  activity_cleaned <- activity_cleaned %>% filter(!person_id %in% patients_to_exclude)
  weight_cleaned <- weight_cleaned %>% filter(!person_id %in% patients_to_exclude)
}

# =============================================================================
# PART 1: WEIGHT OUTCOMES WITH FUNNEL DESIGN
# =============================================================================

cat("========================================\n")
cat("PART 1: Weight Analysis (Funnel Design)\n")
cat("========================================\n\n")

# Define time periods
time_periods <- tribble(
  ~period, ~days_min, ~days_max,
  "Baseline", -180, 0,
  "1-30 days", 1, 30,
  "31-90 days", 31, 90,
  "91-180 days", 91, 180,
  "181-365 days", 181, 365
)

# FUNNEL DESIGN: Only include patients who have weight at each successive timepoint
# Start with all patients who have baseline
patients_with_baseline <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_weight = mean(weight_kg, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(baseline_weight))

cat(sprintf("Patients with baseline weight: %d\n", nrow(patients_with_baseline)))

# Calculate weight for each period with funnel design
weight_funnel_list <- list()
eligible_patients <- patients_with_baseline$person_id

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  # Get weight for this period, restricted to eligible patients
  period_weight <- weight_cleaned %>%
    filter(person_id %in% eligible_patients) %>%
    filter(days_from_initiation >= days_min,
           days_from_initiation <= days_max) %>%
    group_by(person_id) %>%
    summarize(weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(weight_kg)) %>%
    mutate(period = period_name)

  weight_funnel_list[[period_name]] <- period_weight

  # For next period, only keep patients who had data in this period
  if(period_name != "Baseline") {
    eligible_patients <- period_weight$person_id
    cat(sprintf("  %s: N=%d\n", period_name, nrow(period_weight)))
  } else {
    cat(sprintf("  %s: N=%d\n", period_name, nrow(period_weight)))
  }
}

# Calculate nadir weight (minimum after 12 weeks) - only for patients in last period
nadir_weight_data <- weight_cleaned %>%
  filter(person_id %in% eligible_patients) %>%
  filter(days_from_initiation > 84) %>%
  group_by(person_id) %>%
  summarize(weight_kg = min(weight_kg, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(weight_kg), is.finite(weight_kg)) %>%
  mutate(period = "Nadir (>12 weeks)")

cat(sprintf("  Nadir (>12 weeks): N=%d\n\n", nrow(nadir_weight_data)))

weight_funnel_list[["Nadir (>12 weeks)"]] <- nadir_weight_data

# Combine all weight data
weight_long_funnel <- bind_rows(weight_funnel_list)

# Calculate summary statistics
weight_summary_funnel <- weight_long_funnel %>%
  group_by(period) %>%
  summarize(
    n = n(),
    mean_weight = mean(weight_kg, na.rm = TRUE),
    sd_weight = sd(weight_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period = factor(period, levels = c("Baseline", "1-30 days", "31-90 days",
                                             "91-180 days", "181-365 days", "Nadir (>12 weeks)"))) %>%
  arrange(period)

# =============================================================================
# PART 2: ACTIVITY OUTCOMES (COMPLETE CASE)
# =============================================================================

cat("========================================\n")
cat("PART 2: Activity Analysis (Complete Case)\n")
cat("========================================\n\n")

# For activity, we need patients with data in ALL periods (including baseline and all follow-ups)
# This is because activity is a requirement for cohort inclusion

activity_by_period_list <- list()

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  period_activity <- activity_cleaned %>%
    filter(days_from_initiation >= days_min,
           days_from_initiation <= days_max,
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%  # At least 3 valid days
    summarize(
      steps = mean(steps, na.rm = TRUE),
      sedentary_minutes = mean(sedentary_minutes, na.rm = TRUE),
      mvpa = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
      activity_calories = mean(activity_calories, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(period = period_name)

  activity_by_period_list[[period_name]] <- period_activity
  cat(sprintf("  %s: N=%d\n", period_name, nrow(period_activity)))
}

cat("\n")

# Combine all activity data
activity_long_all <- bind_rows(activity_by_period_list)

# Calculate summary statistics
activity_summary_calc <- activity_long_all %>%
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

# =============================================================================
# PART 3: CALCULATE P-VALUES USING MIXED EFFECTS MODELS
# =============================================================================

cat("========================================\n")
cat("PART 3: Mixed Effects Models for P-values\n")
cat("========================================\n\n")

# Function to calculate p-values from mixed effects model with period as categorical
calculate_period_pvalues_lmer <- function(data, outcome_var, period_levels) {

  # Ensure period is a factor with correct levels
  data <- data %>%
    mutate(period = factor(period, levels = period_levels))

  # Remove any NAs in outcome
  data <- data %>%
    filter(!is.na(.data[[outcome_var]]))

  if(nrow(data) < 10 || n_distinct(data$person_id) < 3) {
    warning(paste("Insufficient data for", outcome_var, "model"))
    return(tibble(
      period = period_levels[-1],
      p_value = rep(NA_real_, length(period_levels) - 1)
    ))
  }

  # Fit mixed effects model with period as categorical predictor
  formula_str <- paste0(outcome_var, " ~ period + (1 | person_id)")
  model <- lmer(as.formula(formula_str), data = data)

  # Extract p-values from model summary
  coef_summary <- summary(model)$coefficients

  # Get p-values for each period comparison to baseline
  # Rows 2:n are the period coefficients (baseline is reference, so not included)
  period_names <- period_levels[-1]

  pvalues <- tibble(
    period = period_names,
    p_value = coef_summary[2:(length(period_names)+1), "Pr(>|t|)"]
  )

  return(pvalues)
}

# Weight p-values (including nadir)
cat("Calculating weight p-values...\n")
weight_pvalues <- calculate_period_pvalues_lmer(
  weight_long_funnel,
  "weight_kg",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days", "Nadir (>12 weeks)")
)

# Activity p-values (excluding nadir - no activity data for nadir concept)
cat("Calculating activity p-values...\n")
steps_pvalues <- calculate_period_pvalues_lmer(
  activity_long_all,
  "steps",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

mvpa_pvalues <- calculate_period_pvalues_lmer(
  activity_long_all,
  "mvpa",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

sedentary_pvalues <- calculate_period_pvalues_lmer(
  activity_long_all,
  "sedentary_minutes",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

calories_pvalues <- calculate_period_pvalues_lmer(
  activity_long_all,
  "activity_calories",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days")
)

cat("P-values calculated.\n\n")

# =============================================================================
# PART 4: CREATE WIDE FORMAT TABLE
# =============================================================================

cat("========================================\n")
cat("PART 4: Creating Wide Format Table\n")
cat("========================================\n\n")

# Weight table wide
weight_table_wide <- weight_summary_funnel %>%
  mutate(
    value = sprintf("%d (%.1f ± %.1f)", n, mean_weight, sd_weight)
  ) %>%
  select(period, value) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(Parameter = "Weight (kg)") %>%
  select(Parameter, everything())

# Add p-value row for weight
weight_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", weight_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", weight_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", weight_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", weight_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = sprintf("%.4f", weight_pvalues$p_value[5])
)

weight_table_with_p <- bind_rows(weight_table_wide, weight_pvalue_row)

# Activity tables wide
# Prepare data in long format first
activity_long_for_wide <- activity_summary_calc %>%
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

# Create wide format for each metric
steps_wide <- activity_long_for_wide %>%
  filter(Parameter == "Steps per day") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`Nadir (>12 weeks)` = "—")  # No activity measure for nadir weight

steps_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", steps_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", steps_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", steps_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", steps_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = "—"
)

mvpa_wide <- activity_long_for_wide %>%
  filter(Parameter == "MVPA (min/day)") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`Nadir (>12 weeks)` = "—")

mvpa_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", mvpa_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", mvpa_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", mvpa_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", mvpa_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = "—"
)

sedentary_wide <- activity_long_for_wide %>%
  filter(Parameter == "Sedentary (min/day)") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`Nadir (>12 weeks)` = "—")

sedentary_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", sedentary_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", sedentary_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", sedentary_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", sedentary_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = "—"
)

calories_wide <- activity_long_for_wide %>%
  filter(Parameter == "Activity Calories (kcal/day)") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`Nadir (>12 weeks)` = "—")

calories_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", calories_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", calories_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", calories_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", calories_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = "—"
)

# Combine all into final table
table2_wide <- bind_rows(
  weight_table_with_p,
  steps_wide,
  steps_pvalue_row,
  mvpa_wide,
  mvpa_pvalue_row,
  sedentary_wide,
  sedentary_pvalue_row,
  calories_wide,
  calories_pvalue_row
) %>%
  select(Parameter, Baseline, `1-30 days`, `31-90 days`, `91-180 days`,
         `181-365 days`, `Nadir (>12 weeks)`)

print(table2_wide)

# =============================================================================
# PART 5: SAVE OUTPUTS
# =============================================================================

write_csv(table2_wide, "table2_longitudinal_outcomes_wide.csv")
cat("\nSaved: table2_longitudinal_outcomes_wide.csv\n")

# Create publication-quality HTML table
table2_html <- table2_wide %>%
  kable(format = "html", escape = FALSE, align = c("l", rep("c", 6))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  add_header_above(c(" " = 1, "Time Period" = 6)) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  footnote(
    general = c(
      "Values shown as N (Mean ± SD).",
      "P-values from linear mixed effects models with random intercepts comparing each follow-up period to baseline.",
      "Weight analysis uses funnel design: only patients with measurements at previous timepoints are included.",
      "Activity analysis uses complete case approach: all patients required to have measurements in all periods."
    ),
    general_title = "Notes:",
    footnote_as_chunk = TRUE
  )

writeLines(as.character(table2_html), "table2_longitudinal_outcomes_wide.html")
cat("Saved: table2_longitudinal_outcomes_wide.html\n")

cat("\n##################################################\n")
cat("TABLE 2 REFORMATTING COMPLETE\n")
cat("##################################################\n\n")

cat("Design features:\n")
cat("  - Weight: Funnel design (N decreases over time)\n")
cat("  - Activity: Complete case (consistent N)\n")
cat("  - P-values: Mixed effects models vs baseline\n")
cat("  - Format: N (Mean ± SD) for each period\n\n")
