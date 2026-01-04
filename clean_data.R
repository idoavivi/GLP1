# Comprehensive Data Cleaning Script
# Removes data quality issues identified in exploration
# Creates clean datasets for use in all downstream analyses

library(tidyverse)
library(bigrquery)
library(lubridate)

cat("\n##################################################\n")
cat("DATA CLEANING FOR GLP-1 ACTIVITY ANALYSIS\n")
cat("Removing extreme values and rapid changes\n")
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
# 1. LOAD RAW DATA
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
  "drug_clean",
  "drug_clean_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_path,
  destination_format = "CSV")

drug_df <- read_bq_export_from_workspace_bucket(drug_path)

# Clean drug data
drug_glp1_clean <- drug_df %>%
  filter(!grepl("topical|recombinant|insulin|metformin|pioglitazone",
                standard_concept_name, ignore.case = TRUE)) %>%
  mutate(
    drug_start_date = as.Date(drug_exposure_start_datetime),
    drug_end_date = as.Date(drug_exposure_end_datetime)
  )

glp1_initiation <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(glp1_initiation_date = min(drug_start_date, na.rm = TRUE), .groups = "drop")

message(str_glue("Loaded {nrow(drug_glp1_clean)} drug exposures for {nrow(glp1_initiation)} patients"))

# ========================================
# 2. LOAD AND CLEAN WEIGHT DATA
# ========================================

message("Loading weight measurements...")

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
  "weight_clean",
  "weight_clean_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), weight_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  weight_path,
  destination_format = "CSV")

weight_raw <- read_bq_export_from_workspace_bucket(weight_path)

message(str_glue("Loaded {nrow(weight_raw)} raw weight measurements"))

# Process and clean weight data
weight_with_glp1 <- weight_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    weight_kg = value_as_number * 0.453592
  ) %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days")))

n_raw <- nrow(weight_with_glp1)

cat("\n========================================\n")
cat("CLEANING WEIGHT DATA\n")
cat("========================================\n\n")

# Step 1: Remove NA, zero, and negative weights
cat("Step 1: Removing NA, zero, and negative weights...\n")
weight_step1 <- weight_with_glp1 %>%
  filter(!is.na(weight_kg), weight_kg > 0)
cat(sprintf("  Removed: %d (%.2f%%)\n", n_raw - nrow(weight_step1), 100 * (n_raw - nrow(weight_step1)) / n_raw))

# Step 2: Remove extreme weights (< 30 kg or > 300 kg)
cat("Step 2: Removing extreme weights (< 30 kg or > 300 kg)...\n")
weight_step2 <- weight_step1 %>%
  filter(weight_kg >= 30, weight_kg <= 300)
cat(sprintf("  Removed: %d (%.2f%%)\n", nrow(weight_step1) - nrow(weight_step2), 100 * (nrow(weight_step1) - nrow(weight_step2)) / n_raw))

# Step 3: Handle same-day duplicates (take median)
cat("Step 3: Handling same-day duplicates (taking median)...\n")
n_before_dedup <- nrow(weight_step2)
weight_step3 <- weight_step2 %>%
  group_by(person_id, measurement_date) %>%
  summarize(
    weight_kg = median(weight_kg),
    days_from_initiation = first(days_from_initiation),
    .groups = "drop"
  )
cat(sprintf("  Deduplicated: %d → %d measurements\n", n_before_dedup, nrow(weight_step3)))

# Step 4: Remove rapid consecutive changes (> 20 kg)
cat("Step 4: Removing rapid consecutive changes (> 20 kg)...\n")
weight_step4 <- weight_step3 %>%
  arrange(person_id, measurement_date) %>%
  group_by(person_id) %>%
  mutate(
    prev_weight = lag(weight_kg),
    weight_change = abs(weight_kg - prev_weight),
    is_valid = is.na(weight_change) | weight_change <= 20
  ) %>%
  filter(is_valid) %>%
  select(-prev_weight, -weight_change, -is_valid) %>%
  ungroup()
cat(sprintf("  Removed: %d (%.2f%%)\n", nrow(weight_step3) - nrow(weight_step4), 100 * (nrow(weight_step3) - nrow(weight_step4)) / n_raw))

# Step 5: Remove rapid change rates (> 1 kg/day)
cat("Step 5: Removing rapid change rates (> 1 kg/day)...\n")
weight_step5 <- weight_step4 %>%
  arrange(person_id, measurement_date) %>%
  group_by(person_id) %>%
  mutate(
    prev_weight = lag(weight_kg),
    prev_date = lag(measurement_date),
    days_between = as.numeric(difftime(measurement_date, prev_date, units = "days")),
    daily_rate = abs(weight_kg - prev_weight) / pmax(days_between, 1),
    is_valid = is.na(daily_rate) | daily_rate <= 1
  ) %>%
  filter(is_valid) %>%
  select(-prev_weight, -prev_date, -days_between, -daily_rate, -is_valid) %>%
  ungroup()
cat(sprintf("  Removed: %d (%.2f%%)\n", nrow(weight_step4) - nrow(weight_step5), 100 * (nrow(weight_step4) - nrow(weight_step5)) / n_raw))

weight_cleaned <- weight_step5

cat("\n========================================\n")
cat("CLEANING SUMMARY\n")
cat("========================================\n\n")
cat(sprintf("Raw measurements: %d\n", n_raw))
cat(sprintf("Cleaned measurements: %d\n", nrow(weight_cleaned)))
cat(sprintf("Total removed: %d (%.2f%%)\n\n", n_raw - nrow(weight_cleaned), 100 * (n_raw - nrow(weight_cleaned)) / n_raw))

# ========================================
# 3. LOAD AND CLEAN ACTIVITY DATA
# ========================================

message("Loading Fitbit activity data...")

activity_sql <- paste("
    SELECT
        person_id,
        date,
        steps,
        activity_calories,
        sedentary_minutes,
        lightly_active_minutes,
        fairly_active_minutes,
        very_active_minutes
    FROM `activity_summary`
    WHERE PERSON_ID IN (SELECT distinct person_id
        FROM `cb_search_person` cb_search_person
        WHERE cb_search_person.person_id IN (SELECT person_id
            FROM `cb_search_person` p
            WHERE has_fitbit = 1))", sep="")

activity_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "activity_clean",
  "activity_clean_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), activity_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  activity_path,
  destination_format = "CSV")

activity_raw <- read_bq_export_from_workspace_bucket(activity_path)

message(str_glue("Loaded {nrow(activity_raw)} raw activity records"))

# Clean activity data
cat("\n========================================\n")
cat("CLEANING ACTIVITY DATA\n")
cat("========================================\n\n")

n_raw_activity <- nrow(activity_raw)

activity_cleaned <- activity_raw %>%
  mutate(activity_date = as.Date(date)) %>%
  filter(
    steps >= 100,
    steps <= 25000,
    !is.na(steps),
    sedentary_minutes + lightly_active_minutes + fairly_active_minutes + very_active_minutes <= 1440
  ) %>%
  # Add wear time and proportional metrics
  mutate(
    total_wear_minutes = coalesce(sedentary_minutes, 0) +
                         coalesce(lightly_active_minutes, 0) +
                         coalesce(fairly_active_minutes, 0) +
                         coalesce(very_active_minutes, 0),
    is_valid_day = total_wear_minutes >= 600,
    pct_sedentary = if_else(total_wear_minutes > 0,
                            100 * sedentary_minutes / total_wear_minutes,
                            NA_real_),
    pct_light = if_else(total_wear_minutes > 0,
                        100 * lightly_active_minutes / total_wear_minutes,
                        NA_real_),
    pct_fairly = if_else(total_wear_minutes > 0,
                         100 * fairly_active_minutes / total_wear_minutes,
                         NA_real_),
    pct_very = if_else(total_wear_minutes > 0,
                       100 * very_active_minutes / total_wear_minutes,
                       NA_real_),
    pct_MVPA = if_else(total_wear_minutes > 0,
                       100 * (coalesce(fairly_active_minutes, 0) +
                              coalesce(very_active_minutes, 0)) / total_wear_minutes,
                       NA_real_)
  ) %>%
  # Merge with initiation dates
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(activity_date, glp1_initiation_date, units = "days")))

cat(sprintf("Raw activity records: %d\n", n_raw_activity))
cat(sprintf("Cleaned activity records: %d\n", nrow(activity_cleaned)))
cat(sprintf("Total removed: %d (%.2f%%)\n\n", n_raw_activity - nrow(activity_cleaned), 100 * (n_raw_activity - nrow(activity_cleaned)) / n_raw_activity))

# ========================================
# 4. SAVE CLEANED DATA
# ========================================

cat("\n========================================\n")
cat("SAVING CLEANED DATA\n")
cat("========================================\n\n")

# Save as RData for easy loading in other scripts
save(
  drug_glp1_clean,
  glp1_initiation,
  weight_cleaned,
  activity_cleaned,
  file = "glp1_cleaned_data.RData"
)

cat("Saved: glp1_cleaned_data.RData\n")
cat("\nThis file contains:\n")
cat("  - drug_glp1_clean: GLP-1 drug exposures\n")
cat("  - glp1_initiation: First GLP-1 date per patient\n")
cat("  - weight_cleaned: Quality-filtered weight measurements\n")
cat("  - activity_cleaned: Quality-filtered activity data with wear time metrics\n")

# Also save cleaning report
cleaning_report <- tibble(
  Dataset = c("Weight", "Activity"),
  Raw = c(n_raw, n_raw_activity),
  Cleaned = c(nrow(weight_cleaned), nrow(activity_cleaned)),
  Removed = c(n_raw - nrow(weight_cleaned), n_raw_activity - nrow(activity_cleaned)),
  Pct_Removed = c(
    100 * (n_raw - nrow(weight_cleaned)) / n_raw,
    100 * (n_raw_activity - nrow(activity_cleaned)) / n_raw_activity
  )
)

write.csv(cleaning_report, "data_cleaning_report.csv", row.names = FALSE)
cat("\nCleaning report saved: data_cleaning_report.csv\n")

cat("\n##################################################\n")
cat("DATA CLEANING COMPLETE\n")
cat("##################################################\n\n")

cat("USAGE:\n")
cat("In your analysis scripts, replace data loading with:\n")
cat("  load('glp1_cleaned_data.RData')\n\n")

cat("This ensures all analyses use the same cleaned dataset.\n")
