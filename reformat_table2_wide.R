# =============================================================================
# CREATE PUBLICATION-READY TABLE 2 (WIDE FORMAT)
# =============================================================================
# Reformats Table 2 with periods as columns and metrics as rows
# Each period shows: N, Mean ± SD, p-value
# =============================================================================

library(tidyverse)
library(knitr)
library(kableExtra)

cat("\n##################################################\n")
cat("REFORMATTING TABLE 2 FOR PUBLICATION\n")
cat("Wide format: Periods as columns\n")
cat("##################################################\n\n")

# This assumes you've already run primary_analysis.R
# Load the data if needed, or continue from primary_analysis.R

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
  mutate(
    Parameter = "Weight (kg)",
    `P-value` = sprintf("%.4f", p_value_weight)
  ) %>%
  select(Parameter, everything(), `P-value`)

print(weight_table_wide)

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

# Create wide format
steps_wide <- activity_long_for_wide %>%
  filter(Parameter == "Steps per day") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`P-value` = sprintf("%.4f", p_value_steps))

mvpa_wide <- activity_long_for_wide %>%
  filter(Parameter == "MVPA (min/day)") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`P-value` = sprintf("%.4f", p_value_mvpa))

sedentary_wide <- activity_long_for_wide %>%
  filter(Parameter == "Sedentary (min/day)") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`P-value` = "—")

calories_wide <- activity_long_for_wide %>%
  filter(Parameter == "Activity Calories (kcal/day)") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(`P-value` = sprintf("%.4f", p_value_calories))

# Combine all metrics
activity_table_wide <- bind_rows(
  steps_wide,
  mvpa_wide,
  sedentary_wide,
  calories_wide
)

# Combine weight and activity
table2_wide <- bind_rows(
  weight_table_wide,
  activity_table_wide
) %>%
  select(Parameter, Baseline, `1-30 days`, `31-90 days`, `91-180 days`,
         `181-365 days`, `Nadir (>12 weeks)`, `P-value`)

print(table2_wide)

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

write_csv(table2_wide, "table2_longitudinal_outcomes_wide.csv")
cat("\nSaved: table2_longitudinal_outcomes_wide.csv\n")

# Create publication-quality HTML table
table2_html <- table2_wide %>%
  kable(format = "html", escape = FALSE, align = c("l", rep("c", 6), "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  add_header_above(c(" " = 1, "Time Period" = 6, " " = 1)) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  column_spec(8, bold = TRUE, width = "1.5cm") %>%
  footnote(general = "Values shown as N (Mean ± SD). P-values from linear mixed effects models testing for trend over time.",
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
cat("  - P-value column from mixed effects models\n")
cat("  - Publication-ready formatting\n\n")
