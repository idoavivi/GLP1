# =============================================================================
# FITBIT ACTIVITY BY BMI STRATIFICATION - ANALYSIS 1 ONLY
# =============================================================================
# Runs only Analysis 1 and creates activity_final and bmi_final
# for use with the optimized Analysis 2 script
# =============================================================================

library(tidyverse)
library(bigrquery)
library(lubridate)
library(knitr)
library(patchwork)

cat("\n##################################################\n")
cat("FITBIT ACTIVITY BY BMI STRATIFICATION\n")
cat("Analysis 1 Only\n")
cat("##################################################\n\n")

# =============================================================================
# STEP 1: LOAD FITBIT ACTIVITY DATA
# =============================================================================

cat("========================================\n")
cat("STEP 1: Loading Fitbit activity data\n")
cat("========================================\n\n")

# Check if data already loaded from system-generated code
if (exists("dataset_50785095_fitbit_activity_df") && is.data.frame(dataset_50785095_fitbit_activity_df)) {
  cat("✓ Using pre-loaded activity data (dataset_50785095_fitbit_activity_df)\n")
  cat("  Copying data... (this may take a moment for large datasets)\n")
  activity_raw <- dataset_50785095_fitbit_activity_df
  cat("  ✓ Data loaded\n")
  cat(sprintf("  Records: %s rows\n", format(nrow(activity_raw), big.mark = ",")))
  cat("  Computing unique participants...\n")
  n_participants <- length(unique(activity_raw$person_id))
  cat(sprintf("  Participants: %s\n\n", format(n_participants, big.mark = ",")))
} else {
  cat("Loading Fitbit activity from BigQuery...\n")
  activity_sql <- paste("
    SELECT
        person_id,
        date,
        steps,
        sedentary_minutes,
        lightly_active_minutes,
        fairly_active_minutes,
        very_active_minutes,
        activity_calories
    FROM `activity_summary`
    WHERE person_id IN (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    )
  ")

  activity_raw <- bq_table_download(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), activity_sql,
                     billing = Sys.getenv("GOOGLE_PROJECT"))
  )
  cat(sprintf("Loaded: %s records from %s participants\n\n",
              format(nrow(activity_raw), big.mark = ","),
              format(length(unique(activity_raw$person_id)), big.mark = ",")))
}

# =============================================================================
# STEP 2: LOAD WEIGHT AND HEIGHT DATA
# =============================================================================

cat("========================================\n")
cat("STEP 2: Loading weight and height\n")
cat("========================================\n\n")

# Check if measurement data already loaded from system-generated code
if (exists("dataset_50785095_measurement_df") && is.data.frame(dataset_50785095_measurement_df)) {
  cat("✓ Using pre-loaded measurement data (dataset_50785095_measurement_df)\n")
  cat("  Extracting weight records...\n")

  # Extract weight records (concept_id 3025315 or any weight-related concepts)
  weight_raw <- dataset_50785095_measurement_df %>%
    filter(
      standard_concept_name %in% c("Body weight", "Body weight Measured") |
      measurement_concept_id == 3025315
    ) %>%
    mutate(
      measurement_date = as.Date(measurement_datetime),
      weight_kg = value_as_number
    ) %>%
    select(person_id, measurement_date, weight_kg, measurement_datetime)

  cat("  Extracting height records...\n")
  # Extract height records (concept_id 3036277 or any height-related concepts)
  height_raw <- dataset_50785095_measurement_df %>%
    filter(
      standard_concept_name %in% c("Body height", "Body height Measured") |
      measurement_concept_id == 3036277
    ) %>%
    mutate(
      measurement_date = as.Date(measurement_datetime),
      height_cm = value_as_number
    ) %>%
    select(person_id, measurement_date, height_cm, measurement_datetime)

} else {
  cat("Loading weight and height from BigQuery...\n")

  # Weight
  weight_sql <- paste("
    SELECT
        person_id,
        measurement_date,
        value_as_number as weight_kg,
        measurement_datetime
    FROM `measurement`
    WHERE measurement_concept_id = 3025315
      AND person_id IN (
          SELECT DISTINCT person_id
          FROM `cb_search_person`
          WHERE has_fitbit = 1
      )
  ")

  weight_raw <- bq_table_download(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), weight_sql,
                     billing = Sys.getenv("GOOGLE_PROJECT"))
  )

  # Height
  height_sql <- paste("
    SELECT
        person_id,
        measurement_date,
        value_as_number as height_cm,
        measurement_datetime
    FROM `measurement`
    WHERE measurement_concept_id = 3036277
      AND person_id IN (
          SELECT DISTINCT person_id
          FROM `cb_search_person`
          WHERE has_fitbit = 1
      )
  ")

  height_raw <- bq_table_download(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), height_sql,
                     billing = Sys.getenv("GOOGLE_PROJECT"))
  )
}

cat(sprintf("  Weight: %s records\n", format(nrow(weight_raw), big.mark = ",")))
cat(sprintf("  Height: %s records\n\n", format(nrow(height_raw), big.mark = ",")))

# =============================================================================
# STEP 3: CLEAN ACTIVITY DATA
# =============================================================================

cat("========================================\n")
cat("STEP 3: Cleaning activity data\n")
cat("========================================\n\n")

cat(sprintf("Before cleaning: %s records\n", format(nrow(activity_raw), big.mark = ",")))
cat("Applying validity filters... (this may take a few minutes)\n")

# Apply validity filters (same as used in GLP-1 analysis)
activity_cleaned <- activity_raw %>%
  mutate(
    # Mark valid days
    is_valid_day = (
      !is.na(steps) &
      steps >= 100 &
      steps <= 50000 &
      !is.na(sedentary_minutes) &
      sedentary_minutes >= 0 &
      sedentary_minutes <= 1440 &
      !is.na(lightly_active_minutes) &
      lightly_active_minutes >= 0 &
      lightly_active_minutes <= 1440 &
      !is.na(fairly_active_minutes) &
      fairly_active_minutes >= 0 &
      fairly_active_minutes <= 1440 &
      !is.na(very_active_minutes) &
      very_active_minutes >= 0 &
      very_active_minutes <= 1440 &
      (sedentary_minutes + lightly_active_minutes +
       fairly_active_minutes + very_active_minutes) <= 1440
    )
  ) %>%
  filter(is_valid_day == TRUE)

cat(sprintf("After validity filters: %s records\n", format(nrow(activity_cleaned), big.mark = ",")))
cat("Counting days per participant...\n")

# Filter for participants with >30 days of Fitbit data
participant_day_counts <- activity_cleaned %>%
  group_by(person_id) %>%
  summarize(n_valid_days = n(), .groups = "drop") %>%
  filter(n_valid_days > 30)

cat("Filtering to participants with >30 days...\n")
activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% participant_day_counts$person_id)

cat(sprintf("After >30 days filter: %s records from %s participants\n\n",
            format(nrow(activity_cleaned), big.mark = ","),
            format(length(unique(activity_cleaned$person_id)), big.mark = ",")))

# =============================================================================
# STEP 4: CLEAN WEIGHT AND HEIGHT DATA
# =============================================================================

cat("========================================\n")
cat("STEP 4: Cleaning weight and height\n")
cat("========================================\n\n")

# Weight filters (same as GLP-1 analysis)
cat(sprintf("Weight - before cleaning: %d records\n", nrow(weight_raw)))

weight_cleaned <- weight_raw %>%
  filter(
    !is.na(weight_kg),
    weight_kg >= 30,
    weight_kg <= 300
  )

cat(sprintf("Weight - after filters: %d records from %d participants\n\n",
            nrow(weight_cleaned), n_distinct(weight_cleaned$person_id)))

# Height filters
cat(sprintf("Height - before cleaning: %d records\n", nrow(height_raw)))

height_cleaned <- height_raw %>%
  filter(
    !is.na(height_cm),
    height_cm >= 100,
    height_cm <= 250
  ) %>%
  group_by(person_id) %>%
  # Take median height per person (should be stable)
  summarize(height_cm = median(height_cm, na.rm = TRUE), .groups = "drop")

cat(sprintf("Height - after filters: %d participants with height\n\n",
            nrow(height_cleaned)))

# =============================================================================
# STEP 5: COMPUTE BMI FROM WEIGHT AND HEIGHT
# =============================================================================

cat("========================================\n")
cat("STEP 5: Computing BMI\n")
cat("========================================\n\n")

# Merge weight with height to compute BMI
bmi_computed <- weight_cleaned %>%
  inner_join(height_cleaned, by = "person_id") %>%
  mutate(
    height_m = height_cm / 100,
    bmi = weight_kg / (height_m^2)
  ) %>%
  filter(
    !is.na(bmi),
    bmi >= 12,
    bmi <= 80
  )

cat(sprintf("Computed BMI: %d records from %d participants\n\n",
            nrow(bmi_computed), n_distinct(bmi_computed$person_id)))

# =============================================================================
# STEP 6: IDENTIFY COHORT WITH BOTH FITBIT AND BMI DATA
# =============================================================================

cat("========================================\n")
cat("STEP 6: Creating analysis cohort\n")
cat("========================================\n\n")

# Participants with both Fitbit and BMI data
cohort_ids <- intersect(
  unique(activity_cleaned$person_id),
  unique(bmi_computed$person_id)
)

cat(sprintf("Final cohort: %d participants with Fitbit + BMI data\n\n",
            length(cohort_ids)))

# Filter datasets to cohort and create FINAL versions for Analysis 2
activity_final <- activity_cleaned %>%
  filter(person_id %in% cohort_ids)

bmi_final <- bmi_computed %>%
  filter(person_id %in% cohort_ids)

cat("✓ Created activity_final and bmi_final dataframes\n")
cat("  These will be used for Analysis 1 and Analysis 2\n\n")

# =============================================================================
# ANALYSIS 1: ACTIVITY BY BMI CLASS
# =============================================================================

source_lines <- readLines("fitbit_bmi_stratification.R")

# Find where Analysis 1 starts and Analysis 2 starts
analysis1_start <- grep("# ANALYSIS 1: ACTIVITY BY BMI CLASS", source_lines, fixed = TRUE)[1]
analysis2_start <- grep("# ANALYSIS 2: BMI CLASS TRANSITIONS", source_lines, fixed = TRUE)[1]

# Extract Analysis 1 code
analysis1_code <- source_lines[analysis1_start:(analysis2_start-1)]

# Execute Analysis 1 code
cat("\nExecuting Analysis 1...\n\n")
eval(parse(text = paste(analysis1_code, collapse = "\n")))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("ANALYSIS 1 COMPLETE\n")
cat("##################################################\n\n")

cat("✓ Cleaned data ready in memory:\n")
cat(sprintf("  - activity_final: %s records\n", format(nrow(activity_final), big.mark = ",")))
cat(sprintf("  - bmi_final: %s records\n\n", format(nrow(bmi_final), big.mark = ",")))

cat("Next step: Run Analysis 2\n")
cat("  source(\"analysis2_bmi_transitions_optimized.R\")\n\n")

cat("Done!\n\n")
