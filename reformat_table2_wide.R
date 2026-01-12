# =============================================================================
# CREATE PUBLICATION-READY TABLE 2 (WIDE FORMAT) WITH FIXED BASELINE
# =============================================================================
# FIXED BASELINE COHORT: N=286 patients with:
#   - BMI ≥ 30
#   - GLP-1 initiation
#   - Fitbit data
#   - At least one activity follow-up period
#
# FOLLOW-UP INCLUSION:
#   - Activity: Still on GLP-1 + has ≥3 valid days
#   - Weight: Funnel design - must have weight at ALL previous consecutive periods
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("TABLE 2: FIXED BASELINE COHORT\n")
cat("N=286 baseline for all measurements\n")
cat("##################################################\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
load("glp1_cleaned_data.RData")

# =============================================================================
# STEP 1: DEFINE FIXED BASELINE COHORT (N=286)
# =============================================================================

cat("\n========================================\n")
cat("STEP 1: Defining Fixed Baseline Cohort\n")
cat("========================================\n\n")

# Filter to BMI >= 30
baseline_bmi_check <- bmi_data %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop")

patients_to_exclude_bmi <- baseline_bmi_check %>%
  filter(baseline_bmi < 30) %>%
  pull(person_id)

cat(sprintf("Patients with BMI < 30 excluded: %d\n", length(patients_to_exclude_bmi)))

# Filter datasets
activity_cleaned <- activity_cleaned %>% filter(!person_id %in% patients_to_exclude_bmi)
weight_cleaned <- weight_cleaned %>% filter(!person_id %in% patients_to_exclude_bmi)

# Patients with baseline activity (≥3 valid days in -180 to 0)
baseline_activity_patients <- activity_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  distinct(person_id)

cat(sprintf("Patients with ≥3 valid baseline activity days: %d\n", nrow(baseline_activity_patients)))

# Define time periods for follow-up
time_periods <- tribble(
  ~period, ~days_min, ~days_max,
  "1-30 days", 1, 30,
  "31-90 days", 31, 90,
  "91-180 days", 91, 180,
  "181-365 days", 181, 365
)

# Patients with at least ONE follow-up period with activity
followup_activity_patients <- activity_cleaned %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 365,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  mutate(
    period = case_when(
      days_from_initiation >= 1 & days_from_initiation <= 30 ~ "1-30 days",
      days_from_initiation >= 31 & days_from_initiation <= 90 ~ "31-90 days",
      days_from_initiation >= 91 & days_from_initiation <= 180 ~ "91-180 days",
      days_from_initiation >= 181 & days_from_initiation <= 365 ~ "181-365 days",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(person_id, period) %>%
  filter(n() >= 3) %>%  # At least 3 valid days in the period
  ungroup() %>%
  distinct(person_id)

cat(sprintf("Patients with ≥1 follow-up period with ≥3 valid days: %d\n", nrow(followup_activity_patients)))

# FIXED BASELINE COHORT = baseline activity AND at least one follow-up
baseline_cohort_fixed <- baseline_activity_patients %>%
  inner_join(followup_activity_patients, by = "person_id")

cat(sprintf("\n*** FIXED BASELINE COHORT: N = %d ***\n\n", nrow(baseline_cohort_fixed)))

# Filter all data to baseline cohort
activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort_fixed$person_id)

weight_cleaned <- weight_cleaned %>%
  filter(person_id %in% baseline_cohort_fixed$person_id)

# =============================================================================
# STEP 2: CALCULATE BASELINE MEASUREMENTS (N=286 for all)
# =============================================================================

cat("========================================\n")
cat("STEP 2: Baseline Measurements\n")
cat("========================================\n\n")

# Baseline weight for all N=286 patients
baseline_weight <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(weight_kg))

cat(sprintf("Baseline weight: N=%d\n", nrow(baseline_weight)))

# Baseline activity for all N=286 patients
baseline_activity <- activity_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(
    steps = mean(steps, na.rm = TRUE),
    sedentary_minutes = mean(sedentary_minutes, na.rm = TRUE),
    mvpa = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
    activity_calories = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("Baseline activity: N=%d\n\n", nrow(baseline_activity)))

# =============================================================================
# STEP 3: DETERMINE GLP-1 TREATMENT STATUS PER PERIOD
# =============================================================================

cat("========================================\n")
cat("STEP 3: GLP-1 Treatment Status\n")
cat("========================================\n\n")

# For each period, determine if patient is still on GLP-1
# Assume "on GLP-1" = has prescription within period or within 90 days before period start

glp1_status_by_period <- list()

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  # Patients with GLP-1 prescription during or shortly before this period
  # Look for prescriptions from 90 days before period start through period end
  on_glp1 <- drug_glp1_clean %>%
    mutate(
      days_from_init = as.numeric(difftime(drug_start_date,
                                           glp1_initiation$glp1_initiation_date[match(person_id, glp1_initiation$person_id)],
                                           units = "days"))
    ) %>%
    filter(days_from_init >= (days_min - 90),
           days_from_init <= days_max) %>%
    distinct(person_id) %>%
    mutate(period = period_name, on_glp1 = TRUE)

  glp1_status_by_period[[period_name]] <- on_glp1
  cat(sprintf("  %s: N=%d on GLP-1\n", period_name, nrow(on_glp1)))
}

cat("\n")

# =============================================================================
# STEP 4: CALCULATE FOLLOW-UP ACTIVITY (ON GLP-1 + ≥3 VALID DAYS)
# =============================================================================

cat("========================================\n")
cat("STEP 4: Follow-up Activity\n")
cat("========================================\n\n")

activity_by_period_list <- list()

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  # Patients on GLP-1 during this period
  on_glp1_this_period <- glp1_status_by_period[[period_name]]$person_id

  # Calculate activity for patients on GLP-1 with ≥3 valid days
  period_activity <- activity_cleaned %>%
    filter(person_id %in% on_glp1_this_period) %>%
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
  cat(sprintf("  %s: N=%d (on GLP-1 + ≥3 valid days)\n", period_name, nrow(period_activity)))
}

cat("\n")

# =============================================================================
# STEP 5: CALCULATE FOLLOW-UP WEIGHT (FUNNEL DESIGN)
# =============================================================================

cat("========================================\n")
cat("STEP 5: Follow-up Weight (Funnel Design)\n")
cat("========================================\n\n")

# FUNNEL: Must have weight at ALL previous consecutive periods
# Start with patients who have baseline weight

weight_by_period_list <- list()
eligible_for_weight <- baseline_weight$person_id

cat(sprintf("Starting with baseline weight: N=%d\n", length(eligible_for_weight)))

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  # Calculate weight for eligible patients
  period_weight <- weight_cleaned %>%
    filter(person_id %in% eligible_for_weight) %>%
    filter(days_from_initiation >= days_min,
           days_from_initiation <= days_max) %>%
    group_by(person_id) %>%
    summarize(weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(weight_kg)) %>%
    mutate(period = period_name)

  weight_by_period_list[[period_name]] <- period_weight

  # For NEXT period, only eligible patients are those who had weight THIS period
  eligible_for_weight <- period_weight$person_id

  cat(sprintf("  %s: N=%d (funnel: must have all previous)\n", period_name, nrow(period_weight)))
}

# Nadir weight: only for patients who made it through all periods
nadir_weight_data <- weight_cleaned %>%
  filter(person_id %in% eligible_for_weight) %>%
  filter(days_from_initiation > 84) %>%
  group_by(person_id) %>%
  summarize(weight_kg = min(weight_kg, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(weight_kg), is.finite(weight_kg)) %>%
  mutate(period = "Nadir (>12 weeks)")

cat(sprintf("  Nadir (>12 weeks): N=%d\n\n", nrow(nadir_weight_data)))

weight_by_period_list[["Nadir (>12 weeks)"]] <- nadir_weight_data

# =============================================================================
# STEP 6: PREPARE LONG FORMAT DATA FOR MIXED EFFECTS MODELS
# =============================================================================

cat("========================================\n")
cat("STEP 6: Preparing Data for Mixed Effects\n")
cat("========================================\n\n")

# Weight long format (baseline + follow-up + nadir)
weight_long <- bind_rows(
  baseline_weight %>% mutate(period = "Baseline"),
  bind_rows(weight_by_period_list)
)

cat(sprintf("Weight long format: %d observations from %d patients\n",
            nrow(weight_long), n_distinct(weight_long$person_id)))

# Activity long format (baseline + follow-up)
activity_long <- bind_rows(
  baseline_activity %>% mutate(period = "Baseline"),
  bind_rows(activity_by_period_list)
)

cat(sprintf("Activity long format: %d observations from %d patients\n\n",
            nrow(activity_long), n_distinct(activity_long$person_id)))

# =============================================================================
# STEP 7: CALCULATE SUMMARY STATISTICS
# =============================================================================

cat("========================================\n")
cat("STEP 7: Summary Statistics\n")
cat("========================================\n\n")

# Weight summary
weight_summary <- weight_long %>%
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

# Activity summary
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

cat("Weight summary:\n")
print(weight_summary)
cat("\nActivity summary:\n")
print(activity_summary)
cat("\n")

# =============================================================================
# STEP 8: CALCULATE P-VALUES USING MIXED EFFECTS MODELS
# =============================================================================

cat("========================================\n")
cat("STEP 8: Mixed Effects Models\n")
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

# Weight p-values
cat("Calculating weight p-values...\n")
weight_pvalues <- calculate_period_pvalues_lmer(
  weight_long,
  "weight_kg",
  c("Baseline", "1-30 days", "31-90 days", "91-180 days", "181-365 days", "Nadir (>12 weeks)")
)

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
# STEP 9: CREATE WIDE FORMAT TABLE
# =============================================================================

cat("========================================\n")
cat("STEP 9: Creating Wide Format Table\n")
cat("========================================\n\n")

# Weight table
weight_table_wide <- weight_summary %>%
  mutate(value = sprintf("%d (%.1f ± %.1f)", n, mean_weight, sd_weight)) %>%
  select(period, value) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(Parameter = "Weight (kg)") %>%
  select(Parameter, everything())

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
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`Nadir (>12 weeks)` = "—")

steps_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", steps_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", steps_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", steps_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", steps_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = "—"
)

# MVPA
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

# Sedentary
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

# Calories
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

# Combine all
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
# STEP 10: SAVE OUTPUTS
# =============================================================================

write_csv(table2_wide, "table2_longitudinal_outcomes_wide.csv")
cat("\nSaved: table2_longitudinal_outcomes_wide.csv\n")

table2_html <- table2_wide %>%
  kable(format = "html", escape = FALSE, align = c("l", rep("c", 6))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  add_header_above(c(" " = 1, "Time Period" = 6)) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  footnote(
    general = c(
      "Values shown as N (Mean ± SD).",
      "P-values from linear mixed effects models comparing each period to baseline.",
      paste0("Fixed baseline cohort: N=", nrow(baseline_cohort_fixed), " patients with BMI≥30, GLP-1 initiation, Fitbit data, and ≥1 activity follow-up."),
      "Follow-up activity: Patients on GLP-1 during period with ≥3 valid Fitbit days.",
      "Follow-up weight: Funnel design - patients must have weight at all previous consecutive periods."
    ),
    general_title = "Notes:",
    footnote_as_chunk = TRUE
  )

writeLines(as.character(table2_html), "table2_longitudinal_outcomes_wide.html")
cat("Saved: table2_longitudinal_outcomes_wide.html\n")

cat("\n##################################################\n")
cat("TABLE 2 COMPLETE\n")
cat("##################################################\n\n")

cat(sprintf("Fixed baseline cohort: N=%d\n", nrow(baseline_cohort_fixed)))
cat("Baseline measurements: Same N for weight and activity\n")
cat("Follow-up activity: On GLP-1 + ≥3 valid days\n")
cat("Follow-up weight: Funnel design (must have all previous)\n\n")
