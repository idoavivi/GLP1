# =============================================================================
# GLP-1 Therapy and Physical Activity Analysis
# Research Question: Do patients starting injectable GLP-1 therapy
# (semaglutide, tirzepatide) change their physical activity patterns?
# Data Source: All of Us Database (Fitbit + EHR data)
# =============================================================================

library(tidyverse)
library(bigrquery)
library(lubridate)

# =============================================================================
# SECTION 1: DATA LOADING
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 Person Demographics
# -----------------------------------------------------------------------------
message("Loading person demographics...")

dataset_41386742_person_sql <- paste("
    SELECT
        person.person_id,
        person.gender_concept_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        person.race_concept_id,
        p_race_concept.concept_name as race,
        person.ethnicity_concept_id,
        p_ethnicity_concept.concept_name as ethnicity,
        person.sex_at_birth_concept_id,
        p_sex_at_birth_concept.concept_name as sex_at_birth,
        person.self_reported_category_concept_id,
        p_self_reported_category_concept.concept_name as self_reported_category
    FROM
        `person` person
    LEFT JOIN
        `concept` p_gender_concept
            ON person.gender_concept_id = p_gender_concept.concept_id
    LEFT JOIN
        `concept` p_race_concept
            ON person.race_concept_id = p_race_concept.concept_id
    LEFT JOIN
        `concept` p_ethnicity_concept
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id
    LEFT JOIN
        `concept` p_sex_at_birth_concept
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id
    LEFT JOIN
        `concept` p_self_reported_category_concept
            ON person.self_reported_category_concept_id = p_self_reported_category_concept.concept_id
    WHERE
        person.PERSON_ID IN (SELECT
            distinct person_id
        FROM
            `cb_search_person` cb_search_person
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_fitbit = 1 ) )", sep="")

person_41386742_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "person_41386742",
  "person_41386742_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_41386742_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_41386742_path,
  destination_format = "CSV")

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

person_df <- read_bq_export_from_workspace_bucket(
  person_41386742_path,
  col_types = cols(gender = col_character(), race = col_character(),
                   ethnicity = col_character(), sex_at_birth = col_character(),
                   self_reported_category = col_character())
)

message(str_glue("Loaded {nrow(person_df)} persons"))

# -----------------------------------------------------------------------------
# 1.2 Drug Exposures (GLP-1 medications)
# -----------------------------------------------------------------------------
message("Loading drug exposures...")

dataset_41386742_drug_sql <- paste("
    SELECT
        d_exposure.person_id,
        d_exposure.drug_concept_id,
        d_standard_concept.concept_name as standard_concept_name,
        d_standard_concept.concept_code as standard_concept_code,
        d_standard_concept.vocabulary_id as standard_vocabulary,
        d_exposure.drug_exposure_start_datetime,
        d_exposure.drug_exposure_end_datetime,
        d_exposure.verbatim_end_date,
        d_exposure.drug_type_concept_id,
        d_type.concept_name as drug_type_concept_name,
        d_exposure.stop_reason,
        d_exposure.refills,
        d_exposure.quantity,
        d_exposure.days_supply,
        d_exposure.sig,
        d_exposure.route_concept_id,
        d_route.concept_name as route_concept_name,
        d_exposure.lot_number,
        d_exposure.visit_occurrence_id,
        d_visit.concept_name as visit_occurrence_concept_name,
        d_exposure.drug_source_value,
        d_exposure.drug_source_concept_id,
        d_source_concept.concept_name as source_concept_name,
        d_source_concept.concept_code as source_concept_code,
        d_source_concept.vocabulary_id as source_vocabulary,
        d_exposure.route_source_value,
        d_exposure.dose_unit_source_value
    FROM
        ( SELECT
            *
        FROM
            `drug_exposure` d_exposure
        WHERE
            (
                drug_concept_id IN (SELECT
                    DISTINCT ca.descendant_id
                FROM
                    `cb_criteria_ancestor` ca
                JOIN
                    (SELECT
                        DISTINCT c.concept_id
                    FROM
                        `cb_criteria` c
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id
                        FROM
                            `cb_criteria` cr
                        WHERE
                            concept_id IN (779705, 793143)
                            AND full_text LIKE '%_rank1]%'       ) a
                            ON (c.path LIKE CONCAT('%.', a.id, '.%')
                            OR c.path LIKE CONCAT('%.', a.id)
                            OR c.path LIKE CONCAT(a.id, '.%')
                            OR c.path = a.id)
                    WHERE
                        is_standard = 1
                        AND is_selectable = 1) b
                        ON (ca.ancestor_id = b.concept_id)))
                    AND (d_exposure.PERSON_ID IN (SELECT
                        distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_fitbit = 1 ) )
            )) d_exposure
    LEFT JOIN
        `concept` d_standard_concept
            ON d_exposure.drug_concept_id = d_standard_concept.concept_id
    LEFT JOIN
        `concept` d_type
            ON d_exposure.drug_type_concept_id = d_type.concept_id
    LEFT JOIN
        `concept` d_route
            ON d_exposure.route_concept_id = d_route.concept_id
    LEFT JOIN
        `visit_occurrence` v
            ON d_exposure.visit_occurrence_id = v.visit_occurrence_id
    LEFT JOIN
        `concept` d_visit
            ON v.visit_concept_id = d_visit.concept_id
    LEFT JOIN
        `concept` d_source_concept
            ON d_exposure.drug_source_concept_id = d_source_concept.concept_id", sep="")

drug_41386742_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "drug_41386742",
  "drug_41386742_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_41386742_drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_41386742_path,
  destination_format = "CSV")

drug_df <- read_bq_export_from_workspace_bucket(
  drug_41386742_path,
  col_types = cols(standard_concept_name = col_character(), standard_concept_code = col_character(),
                   standard_vocabulary = col_character(), drug_type_concept_name = col_character(),
                   stop_reason = col_character(), sig = col_character(), route_concept_name = col_character(),
                   lot_number = col_character(), visit_occurrence_concept_name = col_character(),
                   drug_source_value = col_character(), source_concept_name = col_character(),
                   source_concept_code = col_character(), source_vocabulary = col_character(),
                   route_source_value = col_character(), dose_unit_source_value = col_character())
)

message(str_glue("Loaded {nrow(drug_df)} drug exposures"))

# -----------------------------------------------------------------------------
# 1.3 Measurements (Weight, Height, BMI)
# -----------------------------------------------------------------------------
message("Loading measurements...")

dataset_41386742_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value
    FROM
        ( SELECT
            *
        FROM
            `measurement` measurement
        WHERE
            (
                measurement_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (3013762, 3023540, 3025315, 3036277, 3038553, 4030731, 40786050, 4099154, 4177340, 4245997)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
                OR  measurement_source_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (903117, 903121, 903123, 903124, 903125, 903127, 903128, 903133, 903134, 903135, 903136)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 0
                    AND is_selectable = 1)
            )
            AND (
                measurement.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_fitbit = 1 ) )
            )) measurement
    LEFT JOIN
        `concept` m_standard_concept
            ON measurement.measurement_concept_id = m_standard_concept.concept_id
    LEFT JOIN
        `concept` m_type
            ON measurement.measurement_type_concept_id = m_type.concept_id
    LEFT JOIN
        `concept` m_operator
            ON measurement.operator_concept_id = m_operator.concept_id
    LEFT JOIN
        `concept` m_value
            ON measurement.value_as_concept_id = m_value.concept_id
    LEFT JOIN
        `concept` m_unit
            ON measurement.unit_concept_id = m_unit.concept_id
    LEFT JOIn
        `visit_occurrence` v
            ON measurement.visit_occurrence_id = v.visit_occurrence_id
    LEFT JOIN
        `concept` m_visit
            ON v.visit_concept_id = m_visit.concept_id
    LEFT JOIN
        `concept` m_source_concept
            ON measurement.measurement_source_concept_id = m_source_concept.concept_id", sep="")

measurement_41386742_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "measurement_41386742",
  "measurement_41386742_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_41386742_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_41386742_path,
  destination_format = "CSV")

measurement_df <- read_bq_export_from_workspace_bucket(
  measurement_41386742_path,
  col_types = cols(standard_concept_name = col_character(), standard_concept_code = col_character(),
                   standard_vocabulary = col_character(), measurement_type_concept_name = col_character(),
                   operator_concept_name = col_character(), value_as_concept_name = col_character(),
                   unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(),
                   measurement_source_value = col_character(), source_concept_name = col_character(),
                   source_concept_code = col_character(), source_vocabulary = col_character(),
                   unit_source_value = col_character(), value_source_value = col_character())
)

message(str_glue("Loaded {nrow(measurement_df)} measurements"))

# -----------------------------------------------------------------------------
# 1.4 Fitbit Activity Summary
# -----------------------------------------------------------------------------
message("Loading Fitbit activity summary...")

dataset_41386742_fitbit_activity_sql <- paste("
    SELECT
        activity_summary.person_id,
        activity_summary.date,
        activity_summary.activity_calories,
        activity_summary.calories_bmr,
        activity_summary.calories_out,
        activity_summary.elevation,
        activity_summary.fairly_active_minutes,
        activity_summary.floors,
        activity_summary.lightly_active_minutes,
        activity_summary.marginal_calories,
        activity_summary.sedentary_minutes,
        activity_summary.steps,
        activity_summary.very_active_minutes
    FROM
        `activity_summary` activity_summary
    WHERE
        activity_summary.PERSON_ID IN (SELECT
            distinct person_id
        FROM
            `cb_search_person` cb_search_person
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_fitbit = 1 ) )", sep="")

fitbit_activity_41386742_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "fitbit_activity_41386742",
  "fitbit_activity_41386742_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_41386742_fitbit_activity_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_activity_41386742_path,
  destination_format = "CSV")

fitbit_activity_df <- read_bq_export_from_workspace_bucket(fitbit_activity_41386742_path)

message(str_glue("Loaded {nrow(fitbit_activity_df)} Fitbit activity records"))

# -----------------------------------------------------------------------------
# 1.5 Fitbit Heart Rate Summary
# -----------------------------------------------------------------------------
message("Loading Fitbit heart rate summary...")

dataset_41386742_fitbit_heart_rate_summary_sql <- paste("
    SELECT
        heart_rate_summary.person_id,
        heart_rate_summary.date,
        heart_rate_summary.zone_name,
        heart_rate_summary.min_heart_rate,
        heart_rate_summary.max_heart_rate,
        heart_rate_summary.minute_in_zone,
        heart_rate_summary.calorie_count
    FROM
        `heart_rate_summary` heart_rate_summary
    WHERE
        heart_rate_summary.PERSON_ID IN (SELECT
            distinct person_id
        FROM
            `cb_search_person` cb_search_person
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_fitbit = 1 ) )", sep="")

fitbit_heart_rate_summary_41386742_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "fitbit_heart_rate_summary_41386742",
  "fitbit_heart_rate_summary_41386742_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_41386742_fitbit_heart_rate_summary_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_heart_rate_summary_41386742_path,
  destination_format = "CSV")

fitbit_hr_df <- read_bq_export_from_workspace_bucket(
  fitbit_heart_rate_summary_41386742_path,
  col_types = cols(zone_name = col_character())
)

message(str_glue("Loaded {nrow(fitbit_hr_df)} Fitbit heart rate records"))

# =============================================================================
# SECTION 2: DATA CLEANING AND TRANSFORMATION
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1 Convert Weight and Height to Standard Units (kg and cm)
# -----------------------------------------------------------------------------
message("\n=== Converting measurements to standard units ===")

# Function to convert weight to kg
convert_weight_to_kg <- function(value, unit) {
  case_when(
    is.na(value) ~ NA_real_,
    is.na(unit) ~ value,  # Assume kg if no unit
    str_detect(unit, regex("pound|lb", ignore_case = TRUE)) ~ value * 0.453592,
    str_detect(unit, regex("kilogram|kg", ignore_case = TRUE)) ~ value,
    str_detect(unit, regex("gram|g", ignore_case = TRUE)) ~ value / 1000,
    TRUE ~ value  # Default to assuming kg
  )
}

# Function to convert height to cm
convert_height_to_cm <- function(value, unit) {
  case_when(
    is.na(value) ~ NA_real_,
    is.na(unit) ~ value,  # Assume cm if no unit
    str_detect(unit, regex("inch|in", ignore_case = TRUE)) ~ value * 2.54,
    str_detect(unit, regex("centimeter|cm", ignore_case = TRUE)) ~ value,
    str_detect(unit, regex("meter|m", ignore_case = TRUE)) ~ value * 100,
    str_detect(unit, regex("foot|feet|ft", ignore_case = TRUE)) ~ value * 30.48,
    TRUE ~ value  # Default to assuming cm
  )
}

# Process measurements
measurement_processed <- measurement_df %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    measurement_type = case_when(
      str_detect(standard_concept_name, regex("weight", ignore_case = TRUE)) ~ "weight",
      str_detect(standard_concept_name, regex("height", ignore_case = TRUE)) ~ "height",
      str_detect(standard_concept_name, regex("BMI|body mass", ignore_case = TRUE)) ~ "bmi",
      TRUE ~ "other"
    )
  ) %>%
  filter(measurement_type %in% c("weight", "height", "bmi"))

# Convert weight to kg
weight_df <- measurement_processed %>%
  filter(measurement_type == "weight") %>%
  mutate(
    weight_kg = convert_weight_to_kg(value_as_number, unit_concept_name)
  ) %>%
  select(person_id, measurement_date, weight_kg)

# Convert height to cm
height_df <- measurement_processed %>%
  filter(measurement_type == "height") %>%
  mutate(
    height_cm = convert_height_to_cm(value_as_number, unit_concept_name)
  ) %>%
  select(person_id, measurement_date, height_cm)

# Extract BMI
bmi_df <- measurement_processed %>%
  filter(measurement_type == "bmi") %>%
  mutate(
    bmi = value_as_number
  ) %>%
  select(person_id, measurement_date, bmi)

message(str_glue("Weight measurements: {nrow(weight_df)}"))
message(str_glue("Height measurements: {nrow(height_df)}"))
message(str_glue("BMI measurements: {nrow(bmi_df)}"))

# -----------------------------------------------------------------------------
# 2.2 Filter GLP-1 Drugs (Injectable Semaglutide and Tirzepatide Only)
# -----------------------------------------------------------------------------
message("\n=== Filtering GLP-1 drugs ===")

# Filter for injectable semaglutide and tirzepatide
glp1_keywords <- c("semaglutide", "tirzepatide", "ozempic", "wegovy", "mounjaro", "zepbound")
injectable_routes <- c("Subcutaneous", "Injection")

drug_glp1 <- drug_df %>%
  filter(
    str_detect(tolower(standard_concept_name), paste(glp1_keywords, collapse = "|")) |
    str_detect(tolower(source_concept_name), paste(glp1_keywords, collapse = "|"))
  ) %>%
  filter(
    str_detect(route_concept_name, regex(paste(injectable_routes, collapse = "|"), ignore_case = TRUE)) |
    is.na(route_concept_name)  # Include if route is not specified
  ) %>%
  mutate(
    drug_start_date = as.Date(drug_exposure_start_datetime),
    drug_end_date = as.Date(drug_exposure_end_datetime)
  )

message(str_glue("GLP-1 drug exposures (injectable): {nrow(drug_glp1)}"))
message(str_glue("Unique patients with GLP-1: {n_distinct(drug_glp1$person_id)}"))

# Display drug breakdown
drug_glp1 %>%
  count(standard_concept_name, route_concept_name) %>%
  arrange(desc(n)) %>%
  print()

# -----------------------------------------------------------------------------
# 2.3 Filter Fitbit Activity Data
# -----------------------------------------------------------------------------
message("\n=== Filtering Fitbit activity data ===")

fitbit_activity_filtered <- fitbit_activity_df %>%
  mutate(
    date = as.Date(date),
    total_activity_minutes = coalesce(very_active_minutes, 0) +
                            coalesce(fairly_active_minutes, 0) +
                            coalesce(lightly_active_minutes, 0) +
                            coalesce(sedentary_minutes, 0)
  ) %>%
  filter(
    steps >= 100,                          # Filter out < 100 steps/day
    steps <= 25000,                        # Filter out > 25000 steps/day
    total_activity_minutes <= 1440         # Filter out if total minutes > 1440 (24 hours)
  )

message(str_glue("Fitbit records before filtering: {nrow(fitbit_activity_df)}"))
message(str_glue("Fitbit records after filtering: {nrow(fitbit_activity_filtered)}"))
message(str_glue("Records removed: {nrow(fitbit_activity_df) - nrow(fitbit_activity_filtered)} ({round(100*(nrow(fitbit_activity_df) - nrow(fitbit_activity_filtered))/nrow(fitbit_activity_df), 2)}%)"))

# -----------------------------------------------------------------------------
# 2.4 Filter Anthropometric Data
# -----------------------------------------------------------------------------
message("\n=== Filtering anthropometric data ===")

# Filter weight
weight_filtered <- weight_df %>%
  filter(
    weight_kg >= 30,
    weight_kg <= 300
  )

message(str_glue("Weight records before filtering: {nrow(weight_df)}"))
message(str_glue("Weight records after filtering: {nrow(weight_filtered)}"))

# Filter height
height_filtered <- height_df %>%
  filter(
    height_cm >= 100,
    height_cm <= 220
  )

message(str_glue("Height records before filtering: {nrow(height_df)}"))
message(str_glue("Height records after filtering: {nrow(height_filtered)}"))

# Filter BMI
bmi_filtered <- bmi_df %>%
  filter(
    bmi <= 80,
    bmi > 0
  )

message(str_glue("BMI records before filtering: {nrow(bmi_df)}"))
message(str_glue("BMI records after filtering: {nrow(bmi_filtered)}"))

# =============================================================================
# SECTION 3: COMPLETE MISSING WEIGHT/BMI DATA
# =============================================================================
message("\n=== Completing missing weight/BMI data ===")

# Combine all anthropometric measurements
anthro_combined <- full_join(weight_filtered, height_filtered, by = c("person_id", "measurement_date")) %>%
  full_join(bmi_filtered, by = c("person_id", "measurement_date")) %>%
  arrange(person_id, measurement_date)

# For each person, get most recent height for each measurement date
anthro_with_height <- anthro_combined %>%
  group_by(person_id) %>%
  arrange(person_id, measurement_date) %>%
  mutate(
    # Fill forward most recent height
    most_recent_height = zoo::na.locf(height_cm, na.rm = FALSE),
    # Fill backward if no prior height available
    most_recent_height = zoo::na.locf(most_recent_height, na.rm = FALSE, fromLast = TRUE)
  ) %>%
  ungroup()

# Complete missing weight or BMI using available data and height
anthro_completed <- anthro_with_height %>%
  mutate(
    # If we have weight and height but no BMI, calculate it
    bmi_calculated = case_when(
      !is.na(weight_kg) & !is.na(most_recent_height) & is.na(bmi) ~
        weight_kg / ((most_recent_height / 100) ^ 2),
      TRUE ~ bmi
    ),
    # If we have BMI and height but no weight, calculate it
    weight_calculated = case_when(
      !is.na(bmi_calculated) & !is.na(most_recent_height) & is.na(weight_kg) ~
        bmi_calculated * ((most_recent_height / 100) ^ 2),
      TRUE ~ weight_kg
    )
  ) %>%
  select(person_id, measurement_date,
         weight_kg = weight_calculated,
         height_cm = most_recent_height,
         bmi = bmi_calculated)

message(str_glue("Records with weight: {sum(!is.na(anthro_completed$weight_kg))}"))
message(str_glue("Records with height: {sum(!is.na(anthro_completed$height_cm))}"))
message(str_glue("Records with BMI: {sum(!is.na(anthro_completed$bmi))}"))

# =============================================================================
# SECTION 4: EXTRACT GLP-1 INITIATION DATE AND DIVIDE DATA
# =============================================================================
message("\n=== Extracting GLP-1 initiation dates ===")

# Get first GLP-1 exposure date for each person
glp1_initiation <- drug_glp1 %>%
  group_by(person_id) %>%
  summarize(
    glp1_initiation_date = min(drug_start_date, na.rm = TRUE),
    n_glp1_exposures = n(),
    glp1_drugs = paste(unique(standard_concept_name), collapse = "; "),
    .groups = "drop"
  )

message(str_glue("Patients with GLP-1 initiation date: {nrow(glp1_initiation)}"))

# -----------------------------------------------------------------------------
# 4.1 Divide Activity Data into Before/After GLP-1
# -----------------------------------------------------------------------------
message("\n=== Dividing activity data by GLP-1 initiation ===")

activity_with_glp1 <- fitbit_activity_filtered %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(
    days_from_initiation = as.numeric(date - glp1_initiation_date),
    period = case_when(
      days_from_initiation < 0 ~ "before",
      days_from_initiation >= 0 ~ "after",
      TRUE ~ NA_character_
    )
  )

message(str_glue("Activity records linked to GLP-1 patients: {nrow(activity_with_glp1)}"))
message(str_glue("Records before GLP-1: {sum(activity_with_glp1$period == 'before', na.rm = TRUE)}"))
message(str_glue("Records after GLP-1: {sum(activity_with_glp1$period == 'after', na.rm = TRUE)}"))

# -----------------------------------------------------------------------------
# 4.2 Divide Weight Data into Before/After GLP-1
# -----------------------------------------------------------------------------
message("\n=== Dividing weight data by GLP-1 initiation ===")

weight_with_glp1 <- anthro_completed %>%
  filter(!is.na(weight_kg)) %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(
    days_from_initiation = as.numeric(measurement_date - glp1_initiation_date),
    period = case_when(
      days_from_initiation < 0 ~ "before",
      days_from_initiation >= 0 ~ "after",
      TRUE ~ NA_character_
    )
  )

message(str_glue("Weight records linked to GLP-1 patients: {nrow(weight_with_glp1)}"))
message(str_glue("Records before GLP-1: {sum(weight_with_glp1$period == 'before', na.rm = TRUE)}"))
message(str_glue("Records after GLP-1: {sum(weight_with_glp1$period == 'after', na.rm = TRUE)}"))

# =============================================================================
# SECTION 5: SAVE PROCESSED DATA
# =============================================================================
message("\n=== Saving processed data ===")

# Save to RData file for easy loading in analysis scripts
save(
  person_df,
  drug_glp1,
  glp1_initiation,
  anthro_completed,
  fitbit_activity_filtered,
  activity_with_glp1,
  weight_with_glp1,
  fitbit_hr_df,
  file = "glp1_processed_data.RData"
)

# Also save key datasets as CSV
write_csv(glp1_initiation, "glp1_initiation_dates.csv")
write_csv(activity_with_glp1, "activity_by_glp1_period.csv")
write_csv(weight_with_glp1, "weight_by_glp1_period.csv")

message("\n=== Data processing complete! ===")
message(str_glue("Total patients with GLP-1: {nrow(glp1_initiation)}"))
message(str_glue("Patients with activity data: {n_distinct(activity_with_glp1$person_id)}"))
message(str_glue("Patients with weight data: {n_distinct(weight_with_glp1$person_id)}"))

# =============================================================================
# SECTION 6: SUMMARY STATISTICS
# =============================================================================
message("\n=== Generating summary statistics ===")

# Summary by period
activity_summary <- activity_with_glp1 %>%
  group_by(person_id, period) %>%
  summarize(
    n_days = n(),
    mean_steps = mean(steps, na.rm = TRUE),
    median_steps = median(steps, na.rm = TRUE),
    mean_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    mean_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
    mean_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    mean_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    .groups = "drop"
  )

# Overall summary by period
overall_activity_summary <- activity_with_glp1 %>%
  group_by(period) %>%
  summarize(
    n_patients = n_distinct(person_id),
    n_days = n(),
    mean_steps = mean(steps, na.rm = TRUE),
    sd_steps = sd(steps, na.rm = TRUE),
    median_steps = median(steps, na.rm = TRUE),
    .groups = "drop"
  )

print("Overall Activity Summary by Period:")
print(overall_activity_summary)

# Weight summary
weight_summary <- weight_with_glp1 %>%
  group_by(period) %>%
  summarize(
    n_patients = n_distinct(person_id),
    n_measurements = n(),
    mean_weight = mean(weight_kg, na.rm = TRUE),
    sd_weight = sd(weight_kg, na.rm = TRUE),
    median_weight = median(weight_kg, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    .groups = "drop"
  )

print("Weight Summary by Period:")
print(weight_summary)

message("\n=== Analysis ready! ===")
message("Next steps:")
message("1. Load processed data: load('glp1_processed_data.RData')")
message("2. Run statistical analyses comparing before/after periods")
message("3. Create visualizations of activity patterns")
message("4. Consider additional covariates (age, sex, comorbidities)")
