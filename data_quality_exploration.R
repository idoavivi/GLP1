# Comprehensive Data Quality Exploration
# Investigate anomalies in weight and activity data
# Focus on identifying data quality issues requiring additional filters

library(bigrquery)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

cat("\n##################################################\n")
cat("DATA QUALITY EXPLORATION\n")
cat("Investigating weight gain anomaly in treatment data\n")
cat("##################################################\n\n")

# Connect to BigQuery
project_id <- "all-of-us-data-tools"
dataset_id <- "AoU_CDR_2024q2r2"
billing_project <- "idoaviv-tauber-org"

# ========================================
# 1. QUERY ALL DATA
# ========================================

cat("Step 1: Querying GLP-1 drug data...\n")

drug_query <- sprintf("
  SELECT
    de.person_id,
    de.drug_concept_id,
    de.drug_exposure_start_date,
    de.drug_exposure_end_date,
    c.concept_name
  FROM `%s.%s.drug_exposure` de
  JOIN `%s.%s.concept` c ON de.drug_concept_id = c.concept_id
  WHERE LOWER(c.concept_name) LIKE '%%semaglutide%%'
     OR LOWER(c.concept_name) LIKE '%%tirzepatide%%'
", project_id, dataset_id, project_id, dataset_id)

drug_glp1_raw <- bq_project_query(billing_project, drug_query) %>%
  bq_table_download()

drug_glp1_clean <- drug_glp1_raw %>%
  filter(!grepl("topical|recombinant|insulin|metformin|pioglitazone", concept_name, ignore.case = TRUE)) %>%
  mutate(
    drug_start_date = as.Date(drug_exposure_start_date),
    drug_end_date = as.Date(drug_exposure_end_date)
  )

glp1_initiation <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(glp1_initiation_date = min(drug_start_date, na.rm = TRUE), .groups = "drop")

cat("Step 2: Querying ALL weight measurements...\n")

# Query ALL weight data (no filters initially)
weight_query <- sprintf("
  SELECT
    person_id,
    measurement_date,
    measurement_datetime,
    value_as_number AS weight_pounds,
    value_as_number * 0.453592 AS weight_kg,
    measurement_source_value,
    measurement_source_concept_id
  FROM `%s.%s.measurement`
  WHERE measurement_concept_id = 3025315
", project_id, dataset_id)

weight_raw <- bq_project_query(billing_project, weight_query) %>%
  bq_table_download()

cat(sprintf("  Total weight measurements: %s\n", format(nrow(weight_raw), big.mark = ",")))

# Add GLP-1 context
weight_with_glp1 <- weight_raw %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(
    measurement_date = as.Date(measurement_date),
    days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days"))
  )

cat(sprintf("  Weight measurements in GLP-1 cohort: %s\n", format(nrow(weight_with_glp1), big.mark = ",")))

# ========================================
# 2. WEIGHT DATA QUALITY CHECKS
# ========================================

cat("\n========================================\n")
cat("WEIGHT DATA QUALITY CHECKS\n")
cat("========================================\n\n")

# Check 1: Overall distribution
cat("Check 1: Weight distribution summary\n")
weight_summary <- weight_with_glp1 %>%
  summarize(
    n_measurements = n(),
    n_patients = n_distinct(person_id),
    min_weight_kg = min(weight_kg, na.rm = TRUE),
    p1_weight_kg = quantile(weight_kg, 0.01, na.rm = TRUE),
    p5_weight_kg = quantile(weight_kg, 0.05, na.rm = TRUE),
    median_weight_kg = median(weight_kg, na.rm = TRUE),
    p95_weight_kg = quantile(weight_kg, 0.95, na.rm = TRUE),
    p99_weight_kg = quantile(weight_kg, 0.99, na.rm = TRUE),
    max_weight_kg = max(weight_kg, na.rm = TRUE),
    n_null = sum(is.na(weight_kg)),
    n_zero = sum(weight_kg == 0, na.rm = TRUE),
    n_negative = sum(weight_kg < 0, na.rm = TRUE)
  )

print(weight_summary)

# Check 2: Extreme values
cat("\n\nCheck 2: Extreme weight values (potential errors)\n")

extreme_weights <- weight_with_glp1 %>%
  filter(
    is.na(weight_kg) |
    weight_kg <= 0 |
    weight_kg < 30 |  # < 66 lbs - likely infants or errors
    weight_kg > 300   # > 660 lbs - likely errors
  ) %>%
  select(person_id, measurement_date, weight_pounds, weight_kg, days_from_initiation) %>%
  arrange(weight_kg)

cat(sprintf("  Found %d extreme/invalid weights (%.2f%%)\n",
            nrow(extreme_weights),
            100 * nrow(extreme_weights) / nrow(weight_with_glp1)))

if (nrow(extreme_weights) > 0) {
  cat("\nFirst 20 extreme values:\n")
  print(head(extreme_weights, 20))
}

# Check 3: Within-person weight changes
cat("\n\nCheck 3: Analyzing within-person weight changes over time\n")

# Calculate consecutive weight changes
weight_changes <- weight_with_glp1 %>%
  filter(!is.na(weight_kg), weight_kg > 0) %>%
  arrange(person_id, measurement_date) %>%
  group_by(person_id) %>%
  mutate(
    prev_weight = lag(weight_kg),
    prev_date = lag(measurement_date),
    days_between = as.numeric(difftime(measurement_date, prev_date, units = "days")),
    weight_change_kg = weight_kg - prev_weight,
    weight_change_pct = 100 * weight_change_kg / prev_weight,
    daily_change_rate = weight_change_kg / days_between
  ) %>%
  filter(!is.na(prev_weight))

# Extreme changes
extreme_changes <- weight_changes %>%
  filter(
    abs(weight_change_kg) > 20 |  # >20kg change between consecutive measurements
    abs(daily_change_rate) > 1    # >1kg/day sustained change
  ) %>%
  arrange(desc(abs(weight_change_kg)))

cat(sprintf("  Found %d extreme weight changes (%.2f%% of consecutive pairs)\n",
            nrow(extreme_changes),
            100 * nrow(extreme_changes) / nrow(weight_changes)))

if (nrow(extreme_changes) > 0) {
  cat("\nTop 20 extreme weight changes:\n")
  print(head(extreme_changes %>%
               select(person_id, prev_date, measurement_date, days_between,
                      prev_weight, weight_kg, weight_change_kg, weight_change_pct), 20))
}

# Check 4: Duplicates on same day
cat("\n\nCheck 4: Multiple measurements on same day\n")

same_day_dups <- weight_with_glp1 %>%
  filter(!is.na(weight_kg), weight_kg > 0) %>%
  group_by(person_id, measurement_date) %>%
  summarize(
    n_measurements = n(),
    min_weight = min(weight_kg),
    max_weight = max(weight_kg),
    range = max_weight - min_weight,
    .groups = "drop"
  ) %>%
  filter(n_measurements > 1)

cat(sprintf("  Found %d person-days with multiple measurements\n", nrow(same_day_dups)))

if (nrow(same_day_dups) > 0) {
  cat("\nSame-day measurement variability:\n")
  print(summary(same_day_dups$range))

  cat("\nLargest same-day discrepancies:\n")
  print(head(same_day_dups %>% arrange(desc(range)), 10))
}

# ========================================
# 3. TEMPORAL PATTERN ANALYSIS
# ========================================

cat("\n\n========================================\n")
cat("TEMPORAL PATTERN ANALYSIS\n")
cat("========================================\n\n")

# Focus on the problematic period: 91-180 days
cat("Analyzing weight at 91-180d period (where gain was observed)...\n\n")

# Clean weight data first
weight_clean <- weight_with_glp1 %>%
  filter(
    !is.na(weight_kg),
    weight_kg > 30,      # Min reasonable adult weight
    weight_kg < 300      # Max reasonable weight
  )

# Get baseline weights (using -90 to -31 as in analysis)
baseline_weights <- weight_clean %>%
  filter(days_from_initiation >= -90, days_from_initiation <= -31) %>%
  group_by(person_id) %>%
  summarize(
    baseline_weight = max(weight_kg, na.rm = TRUE),  # Using MAX as in analysis
    baseline_date = measurement_date[which.max(weight_kg)],
    n_baseline_measurements = n(),
    .groups = "drop"
  )

# Get weights in each follow-up period
period_weights <- list()

periods <- list(
  "1-30d" = c(1, 30),
  "31-90d" = c(31, 90),
  "91-180d" = c(91, 180),
  "181-365d" = c(181, 365)
)

for (period_name in names(periods)) {
  period <- periods[[period_name]]

  period_data <- weight_clean %>%
    filter(days_from_initiation >= period[1], days_from_initiation <= period[2]) %>%
    group_by(person_id) %>%
    summarize(
      !!paste0(period_name, "_weight_min") := min(weight_kg, na.rm = TRUE),
      !!paste0(period_name, "_weight_mean") := mean(weight_kg, na.rm = TRUE),
      !!paste0(period_name, "_weight_median") := median(weight_kg, na.rm = TRUE),
      !!paste0(period_name, "_weight_max") := max(weight_kg, na.rm = TRUE),
      !!paste0(period_name, "_n_measurements") := n(),
      .groups = "drop"
    )

  period_weights[[period_name]] <- period_data
}

# Merge all periods
all_periods_weight <- baseline_weights
for (period_name in names(periods)) {
  all_periods_weight <- all_periods_weight %>%
    left_join(period_weights[[period_name]], by = "person_id")
}

# Calculate nadir
nadir_weights <- weight_clean %>%
  filter(days_from_initiation > 0) %>%
  group_by(person_id) %>%
  summarize(
    nadir_weight = min(weight_kg, na.rm = TRUE),
    nadir_date = measurement_date[which.min(weight_kg)],
    nadir_days = days_from_initiation[which.min(weight_kg)],
    .groups = "drop"
  )

all_periods_weight <- all_periods_weight %>%
  left_join(nadir_weights, by = "person_id") %>%
  mutate(
    weight_loss_pct = 100 * (nadir_weight - baseline_weight) / baseline_weight
  )

# Identify patients showing weight GAIN during treatment
cat("Identifying patients with weight GAIN in 91-180d period:\n")

weight_gainers <- all_periods_weight %>%
  filter(!is.na(`91-180d_weight_mean`)) %>%
  mutate(
    gain_91_180 = `91-180d_weight_mean` - baseline_weight,
    gain_91_180_pct = 100 * gain_91_180 / baseline_weight
  ) %>%
  filter(gain_91_180 > 5) %>%  # Gained >5kg
  arrange(desc(gain_91_180))

cat(sprintf("\n  Found %d patients with >5kg weight gain in 91-180d period\n", nrow(weight_gainers)))

if (nrow(weight_gainers) > 0) {
  cat("\nTop 10 weight gainers:\n")
  print(head(weight_gainers %>%
               select(person_id, baseline_weight, `91-180d_weight_mean`,
                      gain_91_180, nadir_weight, weight_loss_pct), 10))

  # Look at individual trajectories for these patients
  cat("\n\nDetailed trajectory for top gainer:\n")
  top_gainer_id <- weight_gainers$person_id[1]

  top_gainer_trajectory <- weight_clean %>%
    filter(person_id == top_gainer_id) %>%
    arrange(measurement_date) %>%
    select(person_id, measurement_date, days_from_initiation, weight_kg)

  print(top_gainer_trajectory)
}

# ========================================
# 4. VISUALIZE PROBLEMATIC CASES
# ========================================

cat("\n\n========================================\n")
cat("CREATING DIAGNOSTIC VISUALIZATIONS\n")
cat("========================================\n\n")

# Plot 1: Distribution of weight changes 91-180d vs baseline
if (nrow(weight_gainers) > 0) {

  p1 <- ggplot(all_periods_weight %>% filter(!is.na(`91-180d_weight_mean`)) %>%
                 mutate(gain_91_180 = `91-180d_weight_mean` - baseline_weight),
               aes(x = gain_91_180)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = 5, linetype = "dashed", color = "orange", size = 1) +
    labs(title = "Weight Change Distribution: 91-180d vs Baseline",
         subtitle = sprintf("%d patients gained >5kg (orange line)", nrow(weight_gainers)),
         x = "Weight Change (kg)",
         y = "Count") +
    theme_minimal()

  ggsave("diagnostic_weight_change_distribution.png", p1, width = 10, height = 6, dpi = 300)
  cat("Saved: diagnostic_weight_change_distribution.png\n")
}

# Plot 2: Individual trajectories for weight gainers
if (nrow(weight_gainers) > 0) {

  sample_gainers <- head(weight_gainers$person_id, 20)

  gainer_trajectories <- weight_clean %>%
    filter(person_id %in% sample_gainers) %>%
    arrange(person_id, measurement_date)

  p2 <- ggplot(gainer_trajectories, aes(x = days_from_initiation, y = weight_kg,
                                         group = person_id, color = factor(person_id))) +
    geom_line(alpha = 0.6, size = 0.8) +
    geom_point(alpha = 0.4, size = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = c(91, 180), linetype = "dotted", color = "orange") +
    labs(title = "Individual Weight Trajectories: Top 20 Weight Gainers",
         subtitle = "Red line = initiation, Orange lines = 91-180d period",
         x = "Days from GLP-1 Initiation",
         y = "Weight (kg)") +
    theme_minimal() +
    theme(legend.position = "none")

  ggsave("diagnostic_weight_gainer_trajectories.png", p2, width = 12, height = 8, dpi = 300)
  cat("Saved: diagnostic_weight_gainer_trajectories.png\n")
}

# ========================================
# 5. PROPOSED FILTERS
# ========================================

cat("\n\n========================================\n")
cat("RECOMMENDED ADDITIONAL FILTERS\n")
cat("========================================\n\n")

filters_summary <- tibble(
  Filter = c(
    "Extreme weights",
    "Rapid changes (>20kg)",
    "Rapid rate (>1kg/day)",
    "Minimum weight",
    "Maximum weight"
  ),
  Criteria = c(
    "Remove weight_kg <= 0 or is.na",
    "Remove if |weight_change| > 20kg between consecutive measurements",
    "Remove if |daily_change_rate| > 1 kg/day",
    "Remove if weight_kg < 30 kg (66 lbs)",
    "Remove if weight_kg > 300 kg (660 lbs)"
  ),
  N_Flagged = c(
    sum(is.na(weight_with_glp1$weight_kg) | weight_with_glp1$weight_kg <= 0, na.rm = TRUE),
    sum(abs(weight_changes$weight_change_kg) > 20, na.rm = TRUE),
    sum(abs(weight_changes$daily_change_rate) > 1, na.rm = TRUE),
    sum(weight_with_glp1$weight_kg < 30, na.rm = TRUE),
    sum(weight_with_glp1$weight_kg > 300, na.rm = TRUE)
  )
)

print(filters_summary)

# ========================================
# 6. SAVE FLAGGED CASES
# ========================================

cat("\n\nSaving flagged data for review...\n")

# Save extreme weights
if (nrow(extreme_weights) > 0) {
  write.csv(extreme_weights, "flagged_extreme_weights.csv", row.names = FALSE)
  cat("  - flagged_extreme_weights.csv\n")
}

# Save extreme changes
if (nrow(extreme_changes) > 0) {
  write.csv(extreme_changes %>%
              select(person_id, prev_date, measurement_date, days_between,
                     prev_weight, weight_kg, weight_change_kg, weight_change_pct),
            "flagged_extreme_changes.csv", row.names = FALSE)
  cat("  - flagged_extreme_changes.csv\n")
}

# Save weight gainers
if (nrow(weight_gainers) > 0) {
  write.csv(weight_gainers, "flagged_weight_gainers_91_180d.csv", row.names = FALSE)
  cat("  - flagged_weight_gainers_91_180d.csv\n")
}

cat("\n##################################################\n")
cat("DATA QUALITY EXPLORATION COMPLETE\n")
cat("##################################################\n\n")

cat("SUMMARY:\n")
cat(sprintf("- Total weight measurements: %s\n", format(nrow(weight_with_glp1), big.mark = ",")))
cat(sprintf("- Patients with extreme values: %d\n", nrow(extreme_weights)))
cat(sprintf("- Extreme weight changes: %d\n", nrow(extreme_changes)))
cat(sprintf("- Patients gaining >5kg in 91-180d: %d\n", nrow(weight_gainers)))
cat("\nReview the CSV files and plots to determine appropriate filters.\n")
