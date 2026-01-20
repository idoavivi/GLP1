# =============================================================================
# FORMAT MEDIAN DIFFERENCES TABLE TO HTML
# =============================================================================
# Converts analysis1_enhanced_median_differences_vs_reference.csv to HTML table
# =============================================================================

library(tidyverse)
library(knitr)

cat("Creating HTML table for median differences vs reference...\n")

# Read the median differences CSV
median_diffs <- read_csv("analysis1_enhanced_median_differences_vs_reference.csv",
                         show_col_types = FALSE)

# Read the summary statistics to get baseline values for reference group
summary_stats <- read_csv("analysis1_enhanced_summary.csv",
                          show_col_types = FALSE)

# Get reference group (BMI 18.5-25) baseline values
ref_stats <- summary_stats %>%
  filter(bmi_class == "18.5-25") %>%
  select(bmi_class, Steps_median, Steps_q25, Steps_q75,
         Sedentary_median, Sedentary_q25, Sedentary_q75,
         ActivityCal_median, ActivityCal_q25, ActivityCal_q75)

# Create reference row
ref_row <- data.frame(
  `BMI Class` = "18.5-25 (Reference)",
  `Steps/day` = sprintf("%.0f (%.0f–%.0f)",
                       ref_stats$Steps_median,
                       ref_stats$Steps_q25,
                       ref_stats$Steps_q75),
  `Sedentary (min/day)` = sprintf("%.0f (%.0f–%.0f)",
                                  ref_stats$Sedentary_median,
                                  ref_stats$Sedentary_q25,
                                  ref_stats$Sedentary_q75),
  `Activity Calories` = sprintf("%.0f (%.0f–%.0f)",
                                ref_stats$ActivityCal_median,
                                ref_stats$ActivityCal_q25,
                                ref_stats$ActivityCal_q75),
  check.names = FALSE
)

# Format the comparison rows with differences and CIs
comparison_rows <- median_diffs %>%
  mutate(
    `BMI Class` = BMI_Class,
    `Steps/day` = sprintf("%.0f (%.0f to %.0f)",
                         Steps_Diff, Steps_CI_Lower, Steps_CI_Upper),
    `Sedentary (min/day)` = sprintf("%.0f (%.0f to %.0f)",
                                    Sedentary_Diff, Sedentary_CI_Lower, Sedentary_CI_Upper),
    `Activity Calories` = sprintf("%.0f (%.0f to %.0f)",
                                  Calories_Diff, Calories_CI_Lower, Calories_CI_Upper)
  ) %>%
  select(`BMI Class`, `Steps/day`, `Sedentary (min/day)`, `Activity Calories`)

# Combine reference row with comparison rows
table_formatted <- bind_rows(ref_row, comparison_rows)

# Create HTML table
html_table <- knitr::kable(table_formatted,
                           format = "html",
                           caption = "Table 2: Median Differences vs Reference (BMI 18.5-25)",
                           align = c("l", "r", "r", "r"))

# Add footnote
footnote_text <- paste(
  "<p><strong>Note:</strong> First row shows reference group baseline values as median (IQR). ",
  "Subsequent rows show median difference (95% CI) compared to reference group (BMI 18.5-25). ",
  "95% confidence intervals calculated using 1000 bootstrap resamples. ",
  "Negative values for steps and calories indicate lower activity in higher BMI classes. ",
  "Positive values for sedentary minutes indicate more sedentary time in higher BMI classes.</p>"
)

# Combine table and footnote
html_output <- paste(
  "<style>",
  "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
  "th, td { padding: 10px; text-align: right; border: 1px solid #ddd; }",
  "th { background-color: #f2f2f2; font-weight: bold; }",
  "td:first-child, th:first-child { text-align: left; }",
  "tr:nth-child(2) { background-color: #f9f9f9; font-weight: bold; }",  # Reference row (first data row)
  "caption { font-size: 1.2em; font-weight: bold; margin-bottom: 10px; text-align: left; }",
  "</style>",
  html_table,
  footnote_text,
  sep = "\n"
)

# Save to file
writeLines(html_output, "analysis1_enhanced_median_differences_vs_reference.html")

cat("✓ Saved: analysis1_enhanced_median_differences_vs_reference.html\n\n")

# Print the table to console as well
cat("Median Differences vs Reference (BMI 18.5-25):\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(table_formatted)
cat("\nDone!\n")
