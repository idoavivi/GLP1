# =============================================================================
# PULL DEMOGRAPHICS AND DIAGNOSES FROM ALL OF US
# =============================================================================
# Queries BigQuery for person demographics and condition diagnoses
# To be run in All of Us Workbench
# =============================================================================

library(tidyverse)
library(bigrquery)
library(lubridate)

cat("\n##################################################\n")
cat("PULL DEMOGRAPHICS AND DIAGNOSES\n")
cat("All of Us Workbench\n")
cat("##################################################\n\n")

# Helper function from comprehensive_data_cleaning.R
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

# Load existing cohort to get patient IDs
cat("Loading existing cohort...\n")
load("glp1_cleaned_data.RData")

# Get person IDs from whichever object exists
if (exists("obesity_cohort") && is.data.frame(obesity_cohort)) {
  cohort_person_ids <- unique(obesity_cohort$person_id)
} else if (exists("glp1_initiation") && is.data.frame(glp1_initiation)) {
  cohort_person_ids <- unique(glp1_initiation$person_id)
} else if (exists("weight_cleaned") && is.data.frame(weight_cleaned)) {
  cohort_person_ids <- unique(weight_cleaned$person_id)
} else if (exists("activity_cleaned") && is.data.frame(activity_cleaned)) {
  cohort_person_ids <- unique(activity_cleaned$person_id)
} else {
  stop("Could not find cohort data in RData file")
}

cat(sprintf("Cohort size: %d patients\n\n", length(cohort_person_ids)))

# =============================================================================
# PART 1: PULL PERSON DEMOGRAPHICS
# =============================================================================

cat("========================================\n")
cat("PART 1: Person Demographics\n")
cat("========================================\n\n")

# Build SQL with person IDs directly in the query
person_ids_str <- paste(cohort_person_ids, collapse = ", ")

person_sql <- paste0("
  SELECT
    person_id,
    year_of_birth,
    sex_at_birth_concept_id,
    race_concept_id,
    ethnicity_concept_id
  FROM
    `person`
  WHERE
    person_id IN (", person_ids_str, ")
")

cat("Querying person table...\n")

person_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                         strftime(lubridate::now(), "%Y%m%d"), "person_demographics", "person_demographics_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              person_path, destination_format = "CSV")

person <- read_bq_export_from_workspace_bucket(person_path)

cat(sprintf("Retrieved demographics for %d patients\n", nrow(person)))

# Add age and process demographics
person <- person %>%
  mutate(
    age = lubridate::year(Sys.Date()) - year_of_birth,
    sex = case_when(
      sex_at_birth_concept_id == 45878463 ~ "Female",
      sex_at_birth_concept_id == 45880669 ~ "Male",
      TRUE ~ "Other/Unknown"
    ),
    race = case_when(
      race_concept_id == 8527 ~ "White",
      race_concept_id == 8516 ~ "Black or African American",
      race_concept_id == 8515 ~ "Asian",
      race_concept_id == 8657 ~ "American Indian or Alaska Native",
      race_concept_id == 8557 ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ "Other/Unknown"
    ),
    ethnicity = case_when(
      ethnicity_concept_id == 38003563 ~ "Hispanic or Latino",
      ethnicity_concept_id == 38003564 ~ "Not Hispanic or Latino",
      TRUE ~ "Unknown"
    )
  )

# =============================================================================
# PART 2: PULL DIAGNOSIS DATA
# =============================================================================

cat("\n========================================\n")
cat("PART 2: Diagnosis Data\n")
cat("========================================\n\n")

# Define SNOMED codes for conditions of interest
diagnosis_codes <- tribble(
  ~condition, ~concept_ids,
  "hypertension", c(320128, 201826),
  "diabetes", c(201826, 443238),
  "dyslipidemia", c(432867, 432571),
  "ihd", c(314666, 321318),
  "stroke", c(381591, 372924),
  "osteoarthritis", c(80180, 80004)
)

cat("Querying condition_occurrence table for:\n")
cat("  - Hypertension\n")
cat("  - Diabetes mellitus\n")
cat("  - Dyslipidemia\n")
cat("  - Ischemic heart disease\n")
cat("  - Stroke/CVA\n")
cat("  - Osteoarthritis\n\n")

# Build condition concept IDs list
all_condition_codes <- unlist(diagnosis_codes$concept_ids)
condition_ids_str <- paste(all_condition_codes, collapse = ", ")

conditions_sql <- paste0("
  SELECT
    co.person_id,
    co.condition_concept_id,
    c.concept_name,
    co.condition_start_date
  FROM
    `condition_occurrence` co
  JOIN
    `concept` c ON co.condition_concept_id = c.concept_id
  WHERE
    co.person_id IN (", person_ids_str, ")
    AND (
      co.condition_concept_id IN (", condition_ids_str, ")
      OR co.condition_source_concept_id IN (", condition_ids_str, ")
    )
")

conditions_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                              strftime(lubridate::now(), "%Y%m%d"), "conditions_diagnoses", "conditions_diagnoses_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), conditions_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              conditions_path, destination_format = "CSV")

conditions <- read_bq_export_from_workspace_bucket(conditions_path)

cat(sprintf("Retrieved %d condition records\n", nrow(conditions)))

# =============================================================================
# PART 3: PROCESS DIAGNOSES
# =============================================================================

cat("\n========================================\n")
cat("PART 3: Processing Diagnoses\n")
cat("========================================\n\n")

# Create diagnosis flags for each patient
diagnoses <- tibble(person_id = cohort_person_ids)

# Hypertension
htn_patients <- conditions %>%
  filter(condition_concept_id %in% diagnosis_codes$concept_ids[[1]]) %>%
  distinct(person_id) %>%
  mutate(has_hypertension = TRUE)

# Diabetes
dm_patients <- conditions %>%
  filter(condition_concept_id %in% diagnosis_codes$concept_ids[[2]]) %>%
  distinct(person_id) %>%
  mutate(has_diabetes = TRUE)

# Dyslipidemia
dyslip_patients <- conditions %>%
  filter(condition_concept_id %in% diagnosis_codes$concept_ids[[3]]) %>%
  distinct(person_id) %>%
  mutate(has_dyslipidemia = TRUE)

# IHD
ihd_patients <- conditions %>%
  filter(condition_concept_id %in% diagnosis_codes$concept_ids[[4]]) %>%
  distinct(person_id) %>%
  mutate(has_ihd = TRUE)

# Stroke
stroke_patients <- conditions %>%
  filter(condition_concept_id %in% diagnosis_codes$concept_ids[[5]]) %>%
  distinct(person_id) %>%
  mutate(has_stroke = TRUE)

# Osteoarthritis
oa_patients <- conditions %>%
  filter(condition_concept_id %in% diagnosis_codes$concept_ids[[6]]) %>%
  distinct(person_id) %>%
  mutate(has_osteoarthritis = TRUE)

# Combine all diagnoses
diagnoses <- diagnoses %>%
  left_join(htn_patients, by = "person_id") %>%
  left_join(dm_patients, by = "person_id") %>%
  left_join(dyslip_patients, by = "person_id") %>%
  left_join(ihd_patients, by = "person_id") %>%
  left_join(stroke_patients, by = "person_id") %>%
  left_join(oa_patients, by = "person_id") %>%
  mutate(
    has_hypertension = replace_na(has_hypertension, FALSE),
    has_diabetes = replace_na(has_diabetes, FALSE),
    has_dyslipidemia = replace_na(has_dyslipidemia, FALSE),
    has_ihd = replace_na(has_ihd, FALSE),
    has_stroke = replace_na(has_stroke, FALSE),
    has_osteoarthritis = replace_na(has_osteoarthritis, FALSE)
  )

# Summary
cat("Diagnosis prevalence:\n")
cat(sprintf("  Hypertension: %d (%.1f%%)\n",
            sum(diagnoses$has_hypertension),
            100 * mean(diagnoses$has_hypertension)))
cat(sprintf("  Diabetes: %d (%.1f%%)\n",
            sum(diagnoses$has_diabetes),
            100 * mean(diagnoses$has_diabetes)))
cat(sprintf("  Dyslipidemia: %d (%.1f%%)\n",
            sum(diagnoses$has_dyslipidemia),
            100 * mean(diagnoses$has_dyslipidemia)))
cat(sprintf("  IHD: %d (%.1f%%)\n",
            sum(diagnoses$has_ihd),
            100 * mean(diagnoses$has_ihd)))
cat(sprintf("  Stroke: %d (%.1f%%)\n",
            sum(diagnoses$has_stroke),
            100 * mean(diagnoses$has_stroke)))
cat(sprintf("  Osteoarthritis: %d (%.1f%%)\n\n",
            sum(diagnoses$has_osteoarthritis),
            100 * mean(diagnoses$has_osteoarthritis)))

# =============================================================================
# PART 4: SAVE UPDATED DATA
# =============================================================================

cat("========================================\n")
cat("PART 4: Saving Data\n")
cat("========================================\n\n")

# Update obesity_cohort if it exists
if(exists("obesity_cohort") && is.data.frame(obesity_cohort)) {
  # Remove old diagnosis columns if they exist
  obesity_cohort <- obesity_cohort %>%
    select(-any_of(c("has_hypertension", "has_diabetes", "has_dyslipidemia",
                     "has_ihd", "has_stroke", "has_osteoarthritis"))) %>%
    left_join(diagnoses, by = "person_id")
} else {
  obesity_cohort <- diagnoses
}

# Build list of objects to save
objects_to_save <- c("obesity_cohort", "person")

# Add other objects if they exist
if (exists("drug_glp1_clean") && is.data.frame(drug_glp1_clean)) {
  objects_to_save <- c(objects_to_save, "drug_glp1_clean")
}
if (exists("glp1_initiation") && is.data.frame(glp1_initiation)) {
  objects_to_save <- c(objects_to_save, "glp1_initiation")
}
if (exists("weight_cleaned") && is.data.frame(weight_cleaned)) {
  objects_to_save <- c(objects_to_save, "weight_cleaned")
}
if (exists("activity_cleaned") && is.data.frame(activity_cleaned)) {
  objects_to_save <- c(objects_to_save, "activity_cleaned")
}
if (exists("bmi_data") && is.data.frame(bmi_data)) {
  objects_to_save <- c(objects_to_save, "bmi_data")
}
if (exists("period_assignments") && is.data.frame(period_assignments)) {
  objects_to_save <- c(objects_to_save, "period_assignments")
}

# Save updated RData
save(list = objects_to_save, file = "glp1_cleaned_data.RData")

cat("Saved updated: glp1_cleaned_data.RData\n")
cat(sprintf("  - Saved %d objects\n", length(objects_to_save)))
cat("  - Added person demographics (age, sex, race, ethnicity)\n")
cat("  - Added diagnoses (HTN, DM, dyslipidemia, IHD, CVA, OA)\n\n")

cat("##################################################\n")
cat("DEMOGRAPHICS AND DIAGNOSES PULL COMPLETE\n")
cat("##################################################\n\n")

cat("Next steps:\n")
cat("  1. Download glp1_cleaned_data.RData from workspace\n")
cat("  2. Re-run primary_analysis.R to generate updated Table 1\n\n")
