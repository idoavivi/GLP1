# =============================================================================
# CREATE PUBLICATION-READY TABLE 2 (WIDE FORMAT)
# =============================================================================
# Reformats Table 2 with periods as columns and metrics as rows
# Each period shows: N, Mean ± SD
# P-values compare each period to baseline
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("REFORMATTING TABLE 2 FOR PUBLICATION\n")
cat("Wide format: Periods as columns\n")
cat("##################################################\n\n")

# This assumes you've already run primary_analysis.R
# Load the data if needed, or continue from primary_analysis.R

# =============================================================================
# CALCULATE P-VALUES FOR EACH PERIOD VS BASELINE USING MIXED EFFECTS MODELS
# =============================================================================

cat("Calculating p-values for each period vs baseline using mixed effects models...\n")

# Function to calculate p-values from mixed effects model
# Period is treated as categorical with Baseline as reference
calculate_period_pvalues_lmer <- function(data, outcome_var) {

  # Ensure period is a factor with Baseline as reference
  data <- data %>%
    mutate(period = factor(period, levels = c("Baseline", "1-30 days", "31-90 days",
                                               "91-180 days", "181-365 days", "Nadir (>12 weeks)")))

  # Fit mixed effects model with period as categorical predictor
  formula_str <- paste0(outcome_var, " ~ period + (1 | person_id)")
  model <- lmer(as.formula(formula_str), data = data)

  # Extract p-values from model summary
  coef_summary <- summary(model)$coefficients

  # Get p-values for each period comparison to baseline
  # Rows 2-6 are the period coefficients (baseline is reference, so not included)
  period_names <- c("1-30 days", "31-90 days", "91-180 days", "181-365 days", "Nadir (>12 weeks)")

  pvalues <- tibble(
    period = period_names,
    p_value = coef_summary[2:6, "Pr(>|t|)"]
  )

  return(pvalues)
}

# Weight p-values
weight_pvalues <- calculate_period_pvalues_lmer(
  weight_cleaned %>%
    inner_join(period_assignments, by = "person_id"),
  "weight_kg"
)

# Activity p-values
activity_data_for_pvalues <- activity_cleaned %>%
  inner_join(period_assignments, by = "person_id")

steps_pvalues <- calculate_period_pvalues_lmer(activity_data_for_pvalues, "steps")
mvpa_pvalues <- calculate_period_pvalues_lmer(activity_data_for_pvalues, "mvpa")
sedentary_pvalues <- calculate_period_pvalues_lmer(activity_data_for_pvalues, "sedentary_minutes")
calories_pvalues <- calculate_period_pvalues_lmer(activity_data_for_pvalues, "activity_calories")

# =============================================================================
# WEIGHT TABLE - WIDE FORMAT
# =============================================================================

cat("Creating wide-format weight table...\n")

weight_table_wide <- weight_summary %>%
  mutate(
    value = sprintf("%s (%.1f ± %.1f)", n, mean_weight, sd_weight)
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

print(weight_table_with_p)

# =============================================================================
# ACTIVITY TABLE - WIDE FORMAT
# =============================================================================

cat("\nCreating wide-format activity table...\n")

# Prepare data in long format first
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
    value = sprintf("%s (%.0f ± %.0f)", n, mean_value, sd_value)
  ) %>%
  select(Parameter, period, value)

# Create wide format for each metric
steps_wide <- activity_long_for_wide %>%
  filter(Parameter == "Steps per day") %>%
  pivot_wider(names_from = period, values_from = value)

steps_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", steps_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", steps_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", steps_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", steps_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = sprintf("%.4f", steps_pvalues$p_value[5])
)

mvpa_wide <- activity_long_for_wide %>%
  filter(Parameter == "MVPA (min/day)") %>%
  pivot_wider(names_from = period, values_from = value)

mvpa_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", mvpa_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", mvpa_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", mvpa_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", mvpa_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = sprintf("%.4f", mvpa_pvalues$p_value[5])
)

sedentary_wide <- activity_long_for_wide %>%
  filter(Parameter == "Sedentary (min/day)") %>%
  pivot_wider(names_from = period, values_from = value)

sedentary_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", sedentary_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", sedentary_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", sedentary_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", sedentary_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = sprintf("%.4f", sedentary_pvalues$p_value[5])
)

calories_wide <- activity_long_for_wide %>%
  filter(Parameter == "Activity Calories (kcal/day)") %>%
  pivot_wider(names_from = period, values_from = value)

calories_pvalue_row <- tibble(
  Parameter = "  P-value",
  Baseline = "—",
  `1-30 days` = sprintf("%.4f", calories_pvalues$p_value[1]),
  `31-90 days` = sprintf("%.4f", calories_pvalues$p_value[2]),
  `91-180 days` = sprintf("%.4f", calories_pvalues$p_value[3]),
  `181-365 days` = sprintf("%.4f", calories_pvalues$p_value[4]),
  `Nadir (>12 weeks)` = sprintf("%.4f", calories_pvalues$p_value[5])
)

# Combine weight and activity with p-value rows
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
# SAVE OUTPUTS
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
  footnote(general = "Values shown as N (Mean ± SD). P-values from linear mixed effects models with random intercepts comparing each follow-up period to baseline.",
           general_title = "Note:",
           footnote_as_chunk = TRUE)

writeLines(as.character(table2_html), "table2_longitudinal_outcomes_wide.html")
cat("Saved: table2_longitudinal_outcomes_wide.html\n")

cat("\n##################################################\n")
cat("TABLE 2 REFORMATTING COMPLETE\n")
cat("##################################################\n\n")

cat("Wide format table includes:\n")
cat("  - Periods as columns\n")
cat("  - Each period shows: N (Mean ± SD)\n")
cat("  - P-value row for each parameter (mixed effects model vs baseline)\n")
cat("  - Publication-ready formatting\n\n")
