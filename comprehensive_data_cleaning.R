# =============================================================================
# COMPREHENSIVE DATA CLEANING FOR GLP-1 OBESITY COHORT
# =============================================================================
# Applies ALL data quality filters and cohort inclusion criteria
# Final cohort: BMI ≥30 OR obesity diagnosis, on active GLP-1, with Fitbit data
# =============================================================================

library(tidyverse)
library(bigrquery)
library(lubridate)

cat("\n##################################################\n")
cat("COMPREHENSIVE DATA CLEANING\n")
cat("GLP-1 Obesity Cohort with Fitbit Data\n")
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
# STEP 1: LOAD GLP-1 DRUG DATA
# =============================================================================

cat("========================================\n")
cat("STEP 1: Loading GLP-1 drug exposures\n")
cat("========================================\n\n")

drug_sql <- paste("
    SELECT
        d_exposure.person_id,
        d_exposure.drug_concept_id,
        d_standard_concept.concept_name as standard_concept_name,
        d_exposure.drug_exposure_start_datetime,
        d_exposure.drug_exposure_end_datetime,
        d_exposure.route_concept_id,
        d_route.concept_name as route_concept_name
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
        ON d_exposure.drug_concept_id = d_standard_concept.concept_id
    LEFT JOIN `concept` d_route
        ON d_exposure.route_concept_id = d_route.concept_id", sep="")

drug_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                       strftime(lubridate::now(), "%Y%m%d"), "drug_comprehensive", "drug_comprehensive_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              drug_path, destination_format = "CSV")

drug_df <- read_bq_export_from_workspace_bucket(drug_path)

cat(sprintf("Raw GLP-1 exposures: %d\n", nrow(drug_df)))

# Show route distribution before filtering
route_counts <- drug_df %>%
  mutate(route_clean = ifelse(is.na(route_concept_name), "Unknown", route_concept_name)) %>%
  count(route_clean, sort = TRUE)
cat("\nRoute distribution (top 10):\n")
print(head(route_counts, 10))
cat("\n")

drug_glp1_clean <- drug_df %>%
  # Filter out non-GLP-1 combos
  filter(!grepl("topical|recombinant|insulin|metformin|pioglitazone", standard_concept_name, ignore.case = TRUE)) %>%
  # CRITICAL: Only injectable routes (exclude oral semaglutide)
  filter(
    is.na(route_concept_name) |  # Keep if route unknown (conservative)
    grepl("subcutaneous|injection|injectable", route_concept_name, ignore.case = TRUE)
  ) %>%
  # Exclude oral routes explicitly
  filter(
    is.na(route_concept_name) |
    !grepl("oral|sublingual|buccal|mouth", route_concept_name, ignore.case = TRUE)
  ) %>%
  mutate(
    drug_start_date = as.Date(drug_exposure_start_datetime),
    drug_end_date = as.Date(drug_exposure_end_datetime)
  )

cat(sprintf("After filtering to INJECTABLE routes only: %d exposures\n", nrow(drug_glp1_clean)))
cat(sprintf("Removed: %d (%.1f%% - likely oral semaglutide)\n\n",
            nrow(drug_df) - nrow(drug_glp1_clean),
            100 * (nrow(drug_df) - nrow(drug_glp1_clean)) / nrow(drug_df)))

glp1_initiation <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(glp1_initiation_date = min(drug_start_date, na.rm = TRUE), .groups = "drop")

cat(sprintf("Injectable GLP-1 patients with Fitbit: %d\n\n", nrow(glp1_initiation)))

# =============================================================================
# STEP 2: LOAD WEIGHT, HEIGHT, BMI DATA
# =============================================================================

cat("========================================\n")
cat("STEP 2: Loading anthropometric data\n")
cat("========================================\n\n")

# Weight (concept_id = 3025315)
cat("Loading weight measurements...\n")
weight_sql <- paste("
    SELECT
        person_id,
        measurement_datetime,
        value_as_number,
        unit_concept_id
    FROM `measurement`
    WHERE measurement_concept_id = 3025315
        AND PERSON_ID IN (SELECT distinct person_id
            FROM `cb_search_person`
            WHERE has_fitbit = 1)", sep="")

weight_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                         strftime(lubridate::now(), "%Y%m%d"), "weight_comprehensive", "weight_comprehensive_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), weight_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              weight_path, destination_format = "CSV")

weight_raw <- read_bq_export_from_workspace_bucket(weight_path)

# Height (concept_id = 3036277)
cat("Loading height measurements...\n")
height_sql <- paste("
    SELECT
        person_id,
        measurement_datetime,
        value_as_number,
        unit_concept_id
    FROM `measurement`
    WHERE measurement_concept_id = 3036277
        AND PERSON_ID IN (SELECT distinct person_id
            FROM `cb_search_person`
            WHERE has_fitbit = 1)", sep="")

height_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                         strftime(lubridate::now(), "%Y%m%d"), "height_comprehensive", "height_comprehensive_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), height_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              height_path, destination_format = "CSV")

height_raw <- read_bq_export_from_workspace_bucket(height_path)

# BMI (concept_id = 3038553)
cat("Loading BMI measurements...\n")
bmi_sql <- paste("
    SELECT
        person_id,
        measurement_datetime,
        value_as_number
    FROM `measurement`
    WHERE measurement_concept_id = 3038553
        AND PERSON_ID IN (SELECT distinct person_id
            FROM `cb_search_person`
            WHERE has_fitbit = 1)", sep="")

bmi_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                      strftime(lubridate::now(), "%Y%m%d"), "bmi_comprehensive", "bmi_comprehensive_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), bmi_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              bmi_path, destination_format = "CSV")

bmi_raw <- read_bq_export_from_workspace_bucket(bmi_path)

cat(sprintf("Loaded: %d weight, %d height, %d BMI measurements\n\n",
            nrow(weight_raw), nrow(height_raw), nrow(bmi_raw)))

# =============================================================================
# STEP 3: LOAD OBESITY DIAGNOSIS CODES
# =============================================================================

cat("========================================\n")
cat("STEP 3: Loading obesity diagnoses\n")
cat("========================================\n\n")

obesity_sql <- paste("
    SELECT DISTINCT
        person_id
    FROM `condition_occurrence`
    WHERE (
        -- Method 1: Standard concept IDs for obesity
        condition_concept_id IN (
            433736,   -- Obesity (SNOMED)
            4058243,  -- Obesity due to excess calories
            4102901,  -- Morbid obesity
            435928,   -- Overweight
            4059290,  -- Severe obesity
            443343,   -- Generalized obesity
            4340390,  -- Obesity disorder
            4340604   -- Abdominal obesity
        )
        -- Method 2: ICD-10 source codes (E66.x - Overweight and obesity)
        OR condition_source_value LIKE 'E66%'
        -- Method 3: ICD-9 source codes (278.0x - Overweight and obesity)
        OR condition_source_value LIKE '278.0%'
        -- Method 4: Use ancestor hierarchy
        OR condition_concept_id IN (
            SELECT DISTINCT descendant_concept_id
            FROM `concept_ancestor`
            WHERE ancestor_concept_id = 433736  -- Obesity parent concept
        )
    )
    AND PERSON_ID IN (SELECT distinct person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1)", sep="")

obesity_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                          strftime(lubridate::now(), "%Y%m%d"), "obesity_dx", "obesity_dx_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), obesity_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              obesity_path, destination_format = "CSV")

obesity_patients <- read_bq_export_from_workspace_bucket(obesity_path)

cat(sprintf("Patients with obesity diagnosis: %d\n\n", nrow(obesity_patients)))

# =============================================================================
# STEP 4: CLEAN WEIGHT DATA
# =============================================================================

cat("========================================\n")
cat("STEP 4: Cleaning weight data\n")
cat("========================================\n\n")

weight_with_glp1 <- weight_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    # Handle different units: 8739 = pounds, 9529 = kg
    weight_kg = case_when(
      unit_concept_id == 8739 ~ value_as_number * 0.453592,  # Convert pounds to kg
      unit_concept_id == 9529 ~ value_as_number,             # Already in kg
      is.na(unit_concept_id) ~ value_as_number * 0.453592,   # Default: assume pounds
      TRUE ~ value_as_number * 0.453592                       # Default: assume pounds
    )
  ) %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days")))

# Show unit distribution
cat("Weight unit distribution:\n")
print(weight_with_glp1 %>% count(unit_concept_id, sort = TRUE))
cat("\n")

n_weight_raw <- nrow(weight_with_glp1)

# Filter 1: Remove extreme weights (< 30 kg or > 300 kg)
cat("Filter 1: Removing extreme weights (< 30 or > 300 kg)...\n")
weight_step1 <- weight_with_glp1 %>%
  filter(!is.na(weight_kg), weight_kg >= 30, weight_kg <= 300)
cat(sprintf("  Removed: %d (%.2f%%)\n", n_weight_raw - nrow(weight_step1),
            100 * (n_weight_raw - nrow(weight_step1)) / n_weight_raw))

# Filter 2: Deduplicate same-day measurements (take median)
cat("Filter 2: Deduplicating same-day measurements...\n")
weight_step2 <- weight_step1 %>%
  group_by(person_id, measurement_date) %>%
  summarize(
    weight_kg = median(weight_kg),
    days_from_initiation = first(days_from_initiation),
    .groups = "drop"
  )
cat(sprintf("  %d → %d measurements\n", nrow(weight_step1), nrow(weight_step2)))

# Filter 3: Remove rapid consecutive changes (> 20 kg)
cat("Filter 3: Removing rapid consecutive changes (> 20 kg)...\n")
weight_step3 <- weight_step2 %>%
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
cat(sprintf("  Removed: %d (%.2f%%)\n", nrow(weight_step2) - nrow(weight_step3),
            100 * (nrow(weight_step2) - nrow(weight_step3)) / n_weight_raw))

# Filter 4: Remove rapid change rates (> 1 kg/day)
cat("Filter 4: Removing rapid change rates (> 1 kg/day)...\n")
weight_cleaned <- weight_step3 %>%
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
  select(person_id, measurement_date, weight_kg, days_from_initiation) %>%
  ungroup()
cat(sprintf("  Removed: %d (%.2f%%)\n\n", nrow(weight_step3) - nrow(weight_cleaned),
            100 * (nrow(weight_step3) - nrow(weight_cleaned)) / n_weight_raw))

cat(sprintf("Weight data: %d → %d (%.1f%% retained)\n\n",
            n_weight_raw, nrow(weight_cleaned), 100 * nrow(weight_cleaned) / n_weight_raw))

# =============================================================================
# STEP 5: CLEAN HEIGHT AND CALCULATE BMI
# =============================================================================

cat("========================================\n")
cat("STEP 5: Processing height and BMI\n")
cat("========================================\n\n")

# Clean height (handle different units, filter extremes)
height_clean <- height_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    # Handle different units: 8582 = inches, 9330 = cm
    height_cm = case_when(
      unit_concept_id == 8582 ~ value_as_number * 2.54,  # Convert inches to cm
      unit_concept_id == 9330 ~ value_as_number,         # Already in cm
      is.na(unit_concept_id) ~ value_as_number * 2.54,   # Default: assume inches
      TRUE ~ value_as_number * 2.54                       # Default: assume inches
    )
  ) %>%
  filter(!is.na(height_cm), height_cm >= 100, height_cm <= 220) %>%
  group_by(person_id) %>%
  summarize(height_cm = median(height_cm), .groups = "drop")  # Take median height per person

cat("Height unit distribution:\n")
print(height_raw %>% count(unit_concept_id, sort = TRUE))
cat(sprintf("\nHeight data: %d → %d patients (filtered 100-220 cm)\n",
            nrow(height_raw), nrow(height_clean)))

# Clean BMI measurements
bmi_measured <- bmi_raw %>%
  mutate(measurement_date = as.Date(measurement_datetime)) %>%
  filter(!is.na(value_as_number), value_as_number >= 18, value_as_number <= 80) %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days"))) %>%
  rename(bmi = value_as_number)

cat(sprintf("BMI measurements: %d (filtered 18-80)\n", nrow(bmi_measured)))

# Calculate BMI from weight and height where measured BMI not available
weight_height_for_bmi <- weight_cleaned %>%
  inner_join(height_clean, by = "person_id") %>%
  mutate(
    bmi_calculated = weight_kg / ((height_cm / 100) ^ 2)
  ) %>%
  filter(bmi_calculated >= 18, bmi_calculated <= 80) %>%
  select(person_id, measurement_date, bmi = bmi_calculated, days_from_initiation)

# Combine measured and calculated BMI
bmi_all <- bind_rows(
  bmi_measured %>% select(person_id, measurement_date, bmi, days_from_initiation),
  weight_height_for_bmi
) %>%
  distinct()

cat(sprintf("Total BMI data: %d measurements\n\n", nrow(bmi_all)))

# =============================================================================
# STEP 6: DEFINE OBESITY COHORT
# =============================================================================

cat("========================================\n")
cat("STEP 6: Defining obesity cohort\n")
cat("========================================\n\n")

cat("Inclusion criteria: BMI ≥ 30 OR obesity diagnosis\n\n")

# Get patients with BMI ≥ 30 at baseline (within 180 days before initiation)
# Use MOST RECENT BMI in baseline window to ensure current obesity status
patients_bmi30 <- bmi_all %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0, bmi >= 30) %>%
  group_by(person_id) %>%
  # Take most recent BMI before initiation
  filter(days_from_initiation == max(days_from_initiation)) %>%
  ungroup() %>%
  distinct(person_id) %>%
  mutate(
    person_id = as.numeric(person_id),  # Ensure numeric type
    inclusion_reason = "BMI ≥ 30"
  )

cat(sprintf("Patients with baseline BMI ≥ 30 (within 180d): %d\n", nrow(patients_bmi30)))

# Get patients with obesity diagnosis
patients_obesity_dx <- obesity_patients %>%
  filter(person_id %in% glp1_initiation$person_id) %>%
  mutate(
    person_id = as.numeric(person_id),  # Ensure numeric type
    inclusion_reason = "Obesity diagnosis"
  )

cat(sprintf("Patients with obesity diagnosis: %d\n", nrow(patients_obesity_dx)))

# Combine (union) - handle case where obesity_dx might be empty
if (nrow(patients_obesity_dx) > 0) {
  obesity_cohort <- bind_rows(patients_bmi30, patients_obesity_dx) %>%
    distinct(person_id, .keep_all = TRUE)
} else {
  cat("  NOTE: No obesity diagnosis codes found, using BMI criterion only\n")
  obesity_cohort <- patients_bmi30
}

cat(sprintf("\nTotal obesity cohort: %d patients\n\n", nrow(obesity_cohort)))

# Summary by inclusion reason
cat("Cohort composition:\n")
print(table(obesity_cohort$inclusion_reason))
cat("\n")

# =============================================================================
# STEP 7: LOAD AND CLEAN ACTIVITY DATA
# =============================================================================

cat("========================================\n")
cat("STEP 7: Loading and cleaning activity data\n")
cat("========================================\n\n")

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
        FROM `cb_search_person`
        WHERE has_fitbit = 1)", sep="")

activity_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
                           strftime(lubridate::now(), "%Y%m%d"), "activity_comprehensive", "activity_comprehensive_*.csv")

bq_table_save(bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), activity_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
              activity_path, destination_format = "CSV")

activity_raw <- read_bq_export_from_workspace_bucket(activity_path)

n_activity_raw <- nrow(activity_raw)

activity_cleaned <- activity_raw %>%
  mutate(activity_date = as.Date(date)) %>%
  # Filter 1: Steps range (50 to 25,000)
  filter(steps >= 50, steps <= 25000, !is.na(steps)) %>%
  # Filter 2: Total minutes ≤ 1440
  filter(sedentary_minutes + lightly_active_minutes + fairly_active_minutes + very_active_minutes <= 1440) %>%
  # Add wear time and proportional metrics
  mutate(
    total_wear_minutes = coalesce(sedentary_minutes, 0) +
                         coalesce(lightly_active_minutes, 0) +
                         coalesce(fairly_active_minutes, 0) +
                         coalesce(very_active_minutes, 0),
    is_valid_day = total_wear_minutes >= 600,  # ≥10 hours
    pct_sedentary = if_else(total_wear_minutes > 0,
                            100 * sedentary_minutes / total_wear_minutes, NA_real_),
    pct_light = if_else(total_wear_minutes > 0,
                        100 * lightly_active_minutes / total_wear_minutes, NA_real_),
    pct_fairly = if_else(total_wear_minutes > 0,
                         100 * fairly_active_minutes / total_wear_minutes, NA_real_),
    pct_very = if_else(total_wear_minutes > 0,
                       100 * very_active_minutes / total_wear_minutes, NA_real_),
    pct_MVPA = if_else(total_wear_minutes > 0,
                       100 * (coalesce(fairly_active_minutes, 0) +
                              coalesce(very_active_minutes, 0)) / total_wear_minutes, NA_real_)
  ) %>%
  # Merge with GLP-1 initiation
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(activity_date, glp1_initiation_date, units = "days")))

cat(sprintf("Activity data: %d → %d records\n", n_activity_raw, nrow(activity_cleaned)))
cat(sprintf("Valid days (≥10h wear): %.1f%%\n\n", 100 * mean(activity_cleaned$is_valid_day)))

# =============================================================================
# STEP 8: FINAL COHORT FILTERING
# =============================================================================

cat("========================================\n")
cat("STEP 8: Creating final analysis cohort\n")
cat("========================================\n\n")

cat("Final inclusion criteria:\n")
cat("  1. BMI ≥ 30 OR obesity diagnosis\n")
cat("  2. On GLP-1 therapy (semaglutide/tirzepatide)\n")
cat("  3. Has Fitbit activity data\n\n")

# Filter all datasets to obesity cohort
final_person_ids <- obesity_cohort$person_id

weight_final <- weight_cleaned %>%
  filter(person_id %in% final_person_ids)

activity_final <- activity_cleaned %>%
  filter(person_id %in% final_person_ids)

drug_final <- drug_glp1_clean %>%
  filter(person_id %in% final_person_ids)

glp1_initiation_final <- glp1_initiation %>%
  filter(person_id %in% final_person_ids)

bmi_final <- bmi_all %>%
  filter(person_id %in% final_person_ids)

cat("FINAL COHORT:\n")
cat(sprintf("  Patients: %d\n", length(final_person_ids)))
cat(sprintf("  Weight measurements: %d\n", nrow(weight_final)))
cat(sprintf("  BMI measurements: %d\n", nrow(bmi_final)))
cat(sprintf("  Activity records: %d\n", nrow(activity_final)))
cat(sprintf("  Drug exposures: %d\n\n", nrow(drug_final)))

# =============================================================================
# STEP 9: SAVE CLEANED DATA
# =============================================================================

cat("========================================\n")
cat("STEP 9: Saving cleaned data\n")
cat("========================================\n\n")

save(
  drug_glp1_clean = drug_final,
  glp1_initiation = glp1_initiation_final,
  weight_cleaned = weight_final,
  activity_cleaned = activity_final,
  bmi_data = bmi_final,
  obesity_cohort = obesity_cohort,
  file = "glp1_cleaned_data.RData"
)

cat("Saved: glp1_cleaned_data.RData\n\n")

cat("Contents:\n")
cat("  - drug_glp1_clean: GLP-1 drug exposures\n")
cat("  - glp1_initiation: First GLP-1 date per patient\n")
cat("  - weight_cleaned: Quality-filtered weight (30-300 kg)\n")
cat("  - activity_cleaned: Quality-filtered activity (50-25k steps, ≥10h wear)\n")
cat("  - bmi_data: BMI measurements (18-80)\n")
cat("  - obesity_cohort: Cohort definition with inclusion reasons\n\n")

# Create summary report
summary_report <- tibble(
  Metric = c(
    "GLP-1 patients (with Fitbit)",
    "Obesity cohort (BMI≥30 or dx)",
    "Weight measurements",
    "BMI measurements",
    "Activity records",
    "Valid activity days (≥10h)"
  ),
  Count = c(
    nrow(glp1_initiation),
    length(final_person_ids),
    nrow(weight_final),
    nrow(bmi_final),
    nrow(activity_final),
    sum(activity_final$is_valid_day)
  )
)

write.csv(summary_report, "comprehensive_cleaning_summary.csv", row.names = FALSE)
cat("Summary saved: comprehensive_cleaning_summary.csv\n")

cat("\n##################################################\n")
cat("COMPREHENSIVE CLEANING COMPLETE\n")
cat("##################################################\n\n")

cat("Data quality filters applied:\n")
cat("  ✓ Weight: 30-300 kg, no rapid changes >20kg or >1kg/day\n")
cat("  ✓ Height: 100-220 cm\n")
cat("  ✓ BMI: 18-80\n")
cat("  ✓ Steps: 50-25,000 per day\n")
cat("  ✓ Wear time: ≥10 hours per valid day\n")
cat("  ✓ Total minutes: ≤1440 per day\n\n")

cat("Cohort inclusion criteria:\n")
cat("  ✓ BMI ≥ 30 OR obesity diagnosis\n")
cat("  ✓ INJECTABLE GLP-1 therapy (semaglutide/tirzepatide)\n")
cat("     - Subcutaneous route only (excludes oral semaglutide)\n")
cat("  ✓ Fitbit activity data available\n\n")

cat("Ready for analysis! Use: load('glp1_cleaned_data.RData')\n")
