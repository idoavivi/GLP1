# Investigate Low Weight Problem in GLP-1 Cohort
# Expected: BMI > 27, obesity diagnosis
# Observed: Mean weight 50 kg (110 lbs) - impossibly low!

library(tidyverse)

cat("\n##################################################\n")
cat("INVESTIGATING LOW WEIGHT PROBLEM\n")
cat("Expected: Obesity cohort (BMI > 27)\n")
cat("Observed: Mean 50 kg baseline - WAY TOO LOW\n")
cat("##################################################\n\n")

# Load cleaned data
load("glp1_cleaned_data.RData")

# ========================================
# 1. CHECK WEIGHT DISTRIBUTION
# ========================================

cat("========================================\n")
cat("WEIGHT DISTRIBUTION ANALYSIS\n")
cat("========================================\n\n")

weight_stats <- weight_cleaned %>%
  summarize(
    n_measurements = n(),
    n_patients = n_distinct(person_id),
    min_weight = min(weight_kg),
    p5 = quantile(weight_kg, 0.05),
    p25 = quantile(weight_kg, 0.25),
    median = median(weight_kg),
    mean = mean(weight_kg),
    p75 = quantile(weight_kg, 0.75),
    p95 = quantile(weight_kg, 0.95),
    max_weight = max(weight_kg)
  )

cat("Weight statistics (all cleaned measurements):\n")
print(weight_stats)

# Check baseline period specifically
baseline_weights <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(
    baseline_weight = max(weight_kg),  # Using MAX as in analysis
    .groups = "drop"
  )

baseline_stats <- baseline_weights %>%
  summarize(
    n_patients = n(),
    min_weight = min(baseline_weight),
    p25 = quantile(baseline_weight, 0.25),
    median = median(baseline_weight),
    mean = mean(baseline_weight),
    p75 = quantile(baseline_weight, 0.75),
    max_weight = max(baseline_weight)
  )

cat("\n\nBaseline weight distribution (max in -180 to 0 window):\n")
print(baseline_stats)

# ========================================
# 2. CHECK FOR UNIT CONVERSION ISSUES
# ========================================

cat("\n\n========================================\n")
cat("UNIT CONVERSION CHECK\n")
cat("========================================\n\n")

cat("Testing hypothesis: Some weights already in kg, others in lbs?\n\n")

# Bimodal distribution would suggest mixed units
p <- ggplot(weight_cleaned, aes(x = weight_kg)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = c(50, 110), linetype = "dashed", color = "red") +
  labs(title = "Weight Distribution (After Cleaning)",
       subtitle = "Red lines at 50 kg and 110 kg",
       x = "Weight (kg)",
       y = "Count") +
  theme_minimal()

ggsave("diagnostic_weight_distribution_cleaned.png", p, width = 10, height = 6, dpi = 300)
cat("Saved: diagnostic_weight_distribution_cleaned.png\n")

# Check if there's a bimodal pattern (would indicate mixed units)
weight_summary <- weight_cleaned %>%
  mutate(
    weight_category = case_when(
      weight_kg < 60 ~ "< 60 kg (suspicious for obesity cohort)",
      weight_kg >= 60 & weight_kg < 100 ~ "60-100 kg",
      weight_kg >= 100 ~ ">= 100 kg (expected for obesity)"
    )
  ) %>%
  count(weight_category)

cat("\nWeight distribution by category:\n")
print(weight_summary)

# ========================================
# 3. CHECK BMI AND OBESITY DIAGNOSES
# ========================================

cat("\n\n========================================\n")
cat("BMI AND OBESITY DIAGNOSIS CHECK\n")
cat("========================================\n\n")

cat("Checking if we're missing BMI/obesity filters...\n\n")

# Check if we have height data to calculate BMI
cat("TODO: Need to query height measurements and BMI from EHR\n")
cat("TODO: Need to query obesity/overweight diagnosis codes\n\n")

cat("GLP-1 medications are indicated for:\n")
cat("  - BMI >= 27 with comorbidities\n")
cat("  - BMI >= 30\n")
cat("  - Type 2 diabetes management\n\n")

cat("Expected baseline weight for BMI > 27:\n")
cat("  - Height 165 cm (5'5\"): weight > 73 kg (161 lbs)\n")
cat("  - Height 175 cm (5'9\"): weight > 83 kg (183 lbs)\n\n")

# ========================================
# 4. CHECK PATIENTS WITH VERY LOW WEIGHTS
# ========================================

cat("\n========================================\n")
cat("PATIENTS WITH SUSPICIOUSLY LOW WEIGHTS\n")
cat("========================================\n\n")

low_weight_patients <- baseline_weights %>%
  filter(baseline_weight < 60) %>%
  arrange(baseline_weight)

cat(sprintf("Patients with baseline < 60 kg: %d / %d (%.1f%%)\n\n",
            nrow(low_weight_patients),
            nrow(baseline_weights),
            100 * nrow(low_weight_patients) / nrow(baseline_weights)))

if (nrow(low_weight_patients) > 0) {
  cat("First 20 patients with low baseline weights:\n")
  print(head(low_weight_patients, 20))
}

# ========================================
# 5. RECOMMENDATIONS
# ========================================

cat("\n\n##################################################\n")
cat("RECOMMENDATIONS\n")
cat("##################################################\n\n")

cat("LIKELY ISSUES:\n")
cat("1. Missing BMI/obesity diagnosis filter in cohort definition\n")
cat("2. Possible mixed units (some kg, some lbs not converted)\n")
cat("3. May need minimum weight filter > 60 kg for adult obesity cohort\n\n")

cat("NEXT STEPS:\n")
cat("1. Add BMI calculation using height data\n")
cat("2. Filter to BMI >= 27 (or >= 30)\n")
cat("3. Alternative: Filter to obesity diagnosis codes\n")
cat("4. Check original data source for unit specifications\n")
cat("5. Consider minimum weight threshold (e.g., 60 kg for adults)\n\n")

cat("Would you like me to:\n")
cat("A. Query BMI/height data and add BMI filter?\n")
cat("B. Query obesity diagnosis codes and filter by that?\n")
cat("C. Simply add a minimum weight filter (e.g., >= 60 kg)?\n")
cat("D. All of the above?\n")
