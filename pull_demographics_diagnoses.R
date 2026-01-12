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

person_sql <- paste("
  SELECT
    person_id,
    year_of_birth,
    sex_at_birth_concept_id,
    race_concept_id,
    ethnicity_concept_id
  FROM
    `person`
  WHERE
    person_id IN UNNEST(@person_ids)
")

cat("Querying person table...\n")
person_query <- bq_dataset_query(
  Sys.getenv("WORKSPACE_CDR"),
  person_sql,
  billing = Sys.getenv("GOOGLE_PROJECT"),
  parameters = list(bq_param_array(cohort_person_ids, "INT64"))
)

person <- bq_table_download(person_query)

cat(sprintf("Retrieved demographics for %d patients\n", nrow(person)))

# =============================================================================
# PART 2: PULL DIAGNOSIS DATA
# =============================================================================

cat("\n========================================\n")
cat("PART 2: Diagnosis Data\n")
cat("========================================\n\n")

# Define SNOMED codes for conditions of interest
diagnosis_codes <- tribble(
  ~condition, ~concept_ids,
  "hypertension", c(320128, 201826),  # Essential hypertension, Hypertensive disorder
  "diabetes", c(201826, 443238),  # Type 2 diabetes, Diabetes mellitus
  "dyslipidemia", c(432867, 432571),  # Hyperlipidemia, Hypercholesterolemia
  "ihd", c(314666, 321318),  # Ischemic heart disease, Coronary artery disease
  "stroke", c(381591, 372924),  # Cerebrovascular accident, Cerebral infarction
  "osteoarthritis", c(80180, 80004)  # Osteoarthritis, Osteoarthritis of knee
)

cat("Querying condition_occurrence table for:\n")
cat("  - Hypertension\n")
cat("  - Diabetes mellitus\n")
cat("  - Dyslipidemia\n")
cat("  - Ischemic heart disease\n")
cat("  - Stroke/CVA\n")
cat("  - Osteoarthritis\n\n")

conditions_sql <- paste("
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
    co.person_id IN UNNEST(@person_ids)
    AND (
      -- Hypertension
      co.condition_concept_id IN UNNEST(@htn_codes)
      OR co.condition_source_concept_id IN UNNEST(@htn_codes)
      -- Diabetes
      OR co.condition_concept_id IN UNNEST(@dm_codes)
      OR co.condition_source_concept_id IN UNNEST(@dm_codes)
      -- Dyslipidemia
      OR co.condition_concept_id IN UNNEST(@dyslip_codes)
      OR co.condition_source_concept_id IN UNNEST(@dyslip_codes)
      -- IHD
      OR co.condition_concept_id IN UNNEST(@ihd_codes)
      OR co.condition_source_concept_id IN UNNEST(@ihd_codes)
      -- Stroke
      OR co.condition_concept_id IN UNNEST(@stroke_codes)
      OR co.condition_source_concept_id IN UNNEST(@stroke_codes)
      -- Osteoarthritis
      OR co.condition_concept_id IN UNNEST(@oa_codes)
      OR co.condition_source_concept_id IN UNNEST(@oa_codes)
    )
")

conditions_query <- bq_dataset_query(
  Sys.getenv("WORKSPACE_CDR"),
  conditions_sql,
  billing = Sys.getenv("GOOGLE_PROJECT"),
  parameters = list(
    bq_param_array(cohort_person_ids, "INT64"),
    bq_param_array(diagnosis_codes$concept_ids[[1]], "INT64"),
    bq_param_array(diagnosis_codes$concept_ids[[2]], "INT64"),
    bq_param_array(diagnosis_codes$concept_ids[[3]], "INT64"),
    bq_param_array(diagnosis_codes$concept_ids[[4]], "INT64"),
    bq_param_array(diagnosis_codes$concept_ids[[5]], "INT64"),
    bq_param_array(diagnosis_codes$concept_ids[[6]], "INT64")
  )
)

conditions <- bq_table_download(conditions_query)

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
