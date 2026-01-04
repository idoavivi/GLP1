# Comprehensive Data Quality Exploration
# Investigate anomalies in weight and activity data
# Focus on identifying data quality issues requiring additional filters

library(tidyverse)
library(bigrquery)
library(lubridate)

cat("\n##################################################\n")
cat("DATA QUALITY EXPLORATION\n")
cat("Investigating weight gain anomaly in treatment data\n")
cat("##################################################\n\n")

# Helper function to read BQ exports
read_bq_export_from_workspace_bucket <- function(export_path, col_types = NULL) {
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}

# ========================================
# 1. LOAD GLP-1 DRUG DATA
# ========================================

message("Loading GLP-1 drug exposures...")

drug_sql <- paste("
    SELECT
        d_exposure.person_id,
        d_exposure.drug_concept_id,
        d_standard_concept.concept_name as standard_concept_name,
        d_exposure.drug_exposure_start_datetime,
        d_exposure.drug_exposure_end_datetime
    FROM
        ( SELECT *
        FROM `drug_exposure` d_exposure
        WHERE
            (drug_concept_id IN (SELECT DISTINCT ca.descendant_id
                FROM `cb_criteria_ancestor` ca
                JOIN (SELECT DISTINCT c.concept_id
                    FROM `cb_criteria` c
                    JOIN (SELECT CAST(cr.id as string) AS id
                        FROM `cb_criteria` cr
                        WHERE concept_id IN (779705, 793143)
                            AND full_text LIKE '%_rank1]%') a
                            ON (c.path LIKE CONCAT('%.', a.id, '.%')
                            OR c.path LIKE CONCAT('%.', a.id)
                            OR c.path LIKE CONCAT(a.id, '.%')
                            OR c.path = a.id)
                    WHERE is_standard = 1 AND is_selectable = 1) b
                        ON (ca.ancestor_id = b.concept_id)))
                    AND (d_exposure.PERSON_ID IN (SELECT distinct person_id
                FROM `cb_search_person` cb_search_person
                WHERE cb_search_person.person_id IN (SELECT person_id
                    FROM `cb_search_person` p
                    WHERE has_fitbit = 1)))
            ) d_exposure
    LEFT JOIN `concept` d_standard_concept
        ON d_exposure.drug_concept_id = d_standard_concept.concept_id", sep="")

drug_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "drug_dq",
  "drug_dq_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_path,
  destination_format = "CSV")

drug_df <- read_bq_export_from_workspace_bucket(drug_path)

message(str_glue("Loaded {nrow(drug_df)} drug exposures"))

# Clean drug data
drug_glp1_clean <- drug_df %>%
  filter(!grepl("topical|recombinant|insulin|metformin|pioglitazone",
                standard_concept_name, ignore.case = TRUE)) %>%
  mutate(
    drug_start_date = as.Date(drug_exposure_start_datetime),
    drug_end_date = as.Date(drug_exposure_end_datetime)
  )

# Define initiation
glp1_initiation <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(glp1_initiation_date = min(drug_start_date, na.rm = TRUE), .groups = "drop")

message(str_glue("Identified {nrow(glp1_initiation)} patients with GLP-1 therapy"))

# ========================================
# 2. LOAD WEIGHT DATA
# ========================================

message("Loading ALL weight measurements...")

weight_sql <- paste("
    SELECT
        measurement.person_id,
        measurement.measurement_datetime,
        measurement.value_as_number,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name
    FROM `measurement` measurement
    LEFT JOIN `concept` m_unit
        ON measurement.unit_concept_id = m_unit.concept_id
    WHERE measurement.measurement_concept_id = 3025315
        AND measurement.PERSON_ID IN (SELECT distinct person_id
            FROM `cb_search_person` cb_search_person
            WHERE cb_search_person.person_id IN (SELECT person_id
                FROM `cb_search_person` p
                WHERE has_fitbit = 1))", sep="")

weight_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "weight_dq",
  "weight_dq_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), weight_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  weight_path,
  destination_format = "CSV")

weight_raw <- read_bq_export_from_workspace_bucket(weight_path)

message(str_glue("Loaded {nrow(weight_raw)} weight measurements"))

# Process weight data
weight_with_glp1 <- weight_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    weight_kg = value_as_number * 0.453592  # Convert pounds to kg
  ) %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days")))

message(str_glue("Weight measurements in GLP-1 cohort: {nrow(weight_with_glp1)}"))

# ========================================
# 3. WEIGHT DATA QUALITY CHECKS
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
  select(person_id, measurement_date, value_as_number, weight_kg, days_from_initiation) %>%
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

extreme_changes <- weight_changes %>%
  filter(
    abs(weight_change_kg) > 20 |  # >20kg change
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
# 4. SAVE FLAGGED CASES
# ========================================

cat("\n\nSaving flagged data for review...\n")

if (nrow(extreme_weights) > 0) {
  write.csv(extreme_weights, "flagged_extreme_weights.csv", row.names = FALSE)
  cat("  - flagged_extreme_weights.csv\n")
}

if (nrow(extreme_changes) > 0) {
  write.csv(extreme_changes %>%
              select(person_id, prev_date, measurement_date, days_between,
                     prev_weight, weight_kg, weight_change_kg, weight_change_pct),
            "flagged_extreme_changes.csv", row.names = FALSE)
  cat("  - flagged_extreme_changes.csv\n")
}

# ========================================
# 5. RECOMMENDED FILTERS
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
# 6. CREATE VISUALIZATIONS
# ========================================

cat("\n\nCreating diagnostic visualizations...\n")

# Plot 1: Weight distribution
p1 <- ggplot(weight_with_glp1 %>% filter(weight_kg > 0, weight_kg < 500),
             aes(x = weight_kg)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = c(30, 300), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Weight Distribution (All Measurements)",
       subtitle = "Red lines show suggested min/max cutoffs",
       x = "Weight (kg)",
       y = "Count") +
  theme_minimal()

ggsave("diagnostic_weight_distribution.png", p1, width = 10, height = 6, dpi = 300)
cat("Saved: diagnostic_weight_distribution.png\n")

# Plot 2: Weight changes distribution
if (nrow(weight_changes) > 0) {
  p2 <- ggplot(weight_changes %>% filter(abs(weight_change_kg) < 50),
               aes(x = weight_change_kg)) +
    geom_histogram(bins = 100, fill = "coral", alpha = 0.7) +
    geom_vline(xintercept = c(-20, 20), linetype = "dashed", color = "red", size = 1) +
    labs(title = "Distribution of Consecutive Weight Changes",
         subtitle = "Red lines show ±20kg cutoff for extreme changes",
         x = "Weight Change (kg)",
         y = "Count") +
    theme_minimal()

  ggsave("diagnostic_weight_changes.png", p2, width = 10, height = 6, dpi = 300)
  cat("Saved: diagnostic_weight_changes.png\n")
}

cat("\n##################################################\n")
cat("DATA QUALITY EXPLORATION COMPLETE\n")
cat("##################################################\n\n")

cat("SUMMARY:\n")
cat(sprintf("- Total weight measurements: %s\n", format(nrow(weight_with_glp1), big.mark = ",")))
cat(sprintf("- Patients with extreme values: %d\n", nrow(extreme_weights)))
cat(sprintf("- Extreme weight changes: %d\n", nrow(extreme_changes)))
cat("\nReview the CSV files and plots to determine appropriate filters.\n")
