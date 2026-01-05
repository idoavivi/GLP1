# Investigate Obesity Diagnosis Codes
# Find the correct way to query obesity diagnoses in All of Us

library(tidyverse)
library(bigrquery)
library(lubridate)

cat("\n##################################################\n")
cat("INVESTIGATING OBESITY DIAGNOSIS CODES\n")
cat("##################################################\n\n")

# Helper function
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

# =============================================================================
# METHOD 1: Check what obesity-related concepts exist
# =============================================================================

cat("========================================\n")
cat("METHOD 1: Searching concept table\n")
cat("========================================\n\n")

concept_search_sql <- paste("
    SELECT
        concept_id,
        concept_name,
        vocabulary_id,
        concept_class_id,
        standard_concept
    FROM `concept`
    WHERE LOWER(concept_name) LIKE '%obesity%'
        AND vocabulary_id IN ('SNOMED', 'ICD10CM', 'ICD9CM')
        AND standard_concept = 'S'
    ORDER BY concept_name
    LIMIT 50", sep="")

concept_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                          strftime(lubridate::now(), "%Y%m%d"), "obesity_concepts", "obesity_concepts_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), concept_search_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              concept_path, destination_format = "CSV")

obesity_concepts <- read_bq_export_from_workspace_bucket(concept_path)

cat("Found obesity-related standard concepts:\n")
print(obesity_concepts)
cat("\n\n")

# =============================================================================
# METHOD 2: Check actual condition_occurrence data
# =============================================================================

cat("========================================\n")
cat("METHOD 2: Checking condition_occurrence\n")
cat("========================================\n\n")

# Use common obesity concept IDs
obesity_concept_ids <- c(
  433736,  # Obesity (SNOMED)
  4058243, # Obesity due to excess calories (SNOMED)
  433736,  # Body mass index 30+ - obesity (SNOMED)
  4102901, # Morbid obesity (SNOMED)
  435928,  # Overweight (SNOMED)
  4059290  # Severe obesity (SNOMED)
)

cat("Testing with concept IDs:\n")
print(obesity_concept_ids)
cat("\n")

condition_test_sql <- paste("
    SELECT
        condition_concept_id,
        c.concept_name,
        COUNT(DISTINCT person_id) as n_patients,
        COUNT(*) as n_records
    FROM `condition_occurrence` co
    LEFT JOIN `concept` c ON co.condition_concept_id = c.concept_id
    WHERE condition_concept_id IN (",
    paste(obesity_concept_ids, collapse = ", "),
    ")
    AND person_id IN (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    )
    GROUP BY condition_concept_id, c.concept_name
    ORDER BY n_patients DESC", sep="")

condition_test_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                                 strftime(lubridate::now(), "%Y%m%d"), "condition_test", "condition_test_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), condition_test_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              condition_test_path, destination_format = "CSV")

condition_results <- read_bq_export_from_workspace_bucket(condition_test_path)

cat("Obesity diagnoses in Fitbit users:\n")
print(condition_results)
cat("\n\n")

# =============================================================================
# METHOD 3: Check ICD codes directly
# =============================================================================

cat("========================================\n")
cat("METHOD 3: Checking ICD-10 E66.x codes\n")
cat("========================================\n\n")

icd10_sql <- paste("
    SELECT
        co.condition_source_value,
        c.concept_name,
        COUNT(DISTINCT co.person_id) as n_patients
    FROM `condition_occurrence` co
    LEFT JOIN `concept` c ON co.condition_concept_id = c.concept_id
    WHERE (
        co.condition_source_value LIKE 'E66%' OR
        co.condition_source_value LIKE 'ICD10CM:E66%'
    )
    AND co.person_id IN (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    )
    GROUP BY co.condition_source_value, c.concept_name
    ORDER BY n_patients DESC
    LIMIT 20", sep="")

icd10_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                        strftime(lubridate::now(), "%Y%m%d"), "icd10_test", "icd10_test_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), icd10_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              icd10_path, destination_format = "CSV")

icd10_results <- read_bq_export_from_workspace_bucket(icd10_path)

cat("ICD-10 E66.x (obesity) codes found:\n")
print(icd10_results)
cat("\n\n")

# =============================================================================
# SUMMARY AND RECOMMENDATION
# =============================================================================

cat("##################################################\n")
cat("SUMMARY AND RECOMMENDATION\n")
cat("##################################################\n\n")

if (nrow(condition_results) > 0) {
  cat("✓ SUCCESS: Found obesity diagnoses!\n\n")

  total_patients <- sum(condition_results$n_patients)
  cat(sprintf("Total patients with obesity diagnoses: %d\n\n", total_patients))

  cat("Working concept IDs:\n")
  print(condition_results %>% select(condition_concept_id, concept_name, n_patients))

  cat("\n\nRECOMMENDATION:\n")
  cat("Update comprehensive_data_cleaning.R obesity query to use these concept IDs:\n")
  cat(paste0("obesity_concept_ids <- c(",
             paste(condition_results$condition_concept_id, collapse = ", "),
             ")\n\n"))

} else if (nrow(icd10_results) > 0) {
  cat("✓ Found obesity codes via ICD-10 source values!\n\n")

  total_patients <- sum(icd10_results$n_patients)
  cat(sprintf("Total patients with ICD-10 E66.x codes: %d\n\n", total_patients))

  cat("RECOMMENDATION:\n")
  cat("Use condition_source_value instead of concept_id:\n")
  cat("WHERE condition_source_value LIKE 'E66%'\n\n")

} else {
  cat("⚠ WARNING: No obesity diagnoses found!\n\n")
  cat("This could mean:\n")
  cat("1. All of Us doesn't capture diagnosis codes for Fitbit-only users\n")
  cat("2. Need different query approach\n")
  cat("3. Should rely on BMI criterion only\n\n")
}

cat("Next step: Update comprehensive_data_cleaning.R with working query\n")
