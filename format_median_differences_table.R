# =============================================================================
# FORMAT MEDIAN DIFFERENCES TABLE TO HTML
# =============================================================================
# Converts analysis1_enhanced_median_differences_vs_reference.csv to HTML table
# =============================================================================

library(tidyverse)
library(knitr)

cat("Creating HTML table for median differences vs reference...\n")

# Read the CSV file
median_diffs <- read_csv("analysis1_enhanced_median_differences_vs_reference.csv",
                         show_col_types = FALSE)

# Format the table with differences and CIs combined
table_formatted <- median_diffs %>%
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

# Create HTML table
html_table <- knitr::kable(table_formatted,
                           format = "html",
                           caption = "Table 2: Median Differences vs Reference (BMI 18.5-25)",
                           align = c("l", "r", "r", "r"))

# Add footnote
footnote_text <- paste(
  "<p><strong>Note:</strong> Values shown as median difference (95% CI) compared to reference group (BMI 18.5-25). ",
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
