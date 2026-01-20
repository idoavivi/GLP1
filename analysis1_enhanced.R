# =============================================================================
# ANALYSIS 1 ENHANCED: ACTIVITY BY BMI CLASS WITH TREND TESTS
# =============================================================================
# STANDALONE SCRIPT - No dependencies required (except system-generated data)
# Loads Fitbit and BMI data, then performs comprehensive BMI stratification analysis
# Includes: Demographics, Jonckheere-Terpstra trend test, Quantile regression,
#           Distribution plots, Scatter plots
# =============================================================================

library(tidyverse)
library(bigrquery)
library(knitr)
library(patchwork)
library(quantreg)  # For quantile regression

cat("\n##################################################\n")
cat("ANALYSIS 1 ENHANCED: ACTIVITY BY BMI CLASS\n")
cat("Standalone version with all data loading\n")
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
} else if (exists("dataset_98104042_fitbit_activity_df") && is.data.frame(dataset_98104042_fitbit_activity_df)) {
  cat("✓ Using pre-loaded activity data (dataset_98104042_fitbit_activity_df)\n")
  activity_raw <- dataset_98104042_fitbit_activity_df
  cat(sprintf("  Records: %s rows\n", format(nrow(activity_raw), big.mark = ",")))
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

} else if (exists("dataset_98104042_measurement_df") && is.data.frame(dataset_98104042_measurement_df)) {
  cat("✓ Using pre-loaded measurement data (dataset_98104042_measurement_df)\n")
  cat("  Extracting weight records...\n")

  weight_raw <- dataset_98104042_measurement_df %>%
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
  height_raw <- dataset_98104042_measurement_df %>%
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
# STEP 6: CREATE ANALYSIS COHORT
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

# Filter datasets to cohort
activity_final <- activity_cleaned %>%
  filter(person_id %in% cohort_ids)

bmi_final <- bmi_computed %>%
  filter(person_id %in% cohort_ids)

# =============================================================================
# STEP 7: LOAD DEMOGRAPHICS
# =============================================================================

cat("========================================\n")
cat("STEP 7: Loading demographics\n")
cat("========================================\n\n")

# Check if person data already loaded
if (exists("dataset_50785095_person_df") && is.data.frame(dataset_50785095_person_df)) {
  cat("✓ Using pre-loaded person data (dataset_50785095_person_df)\n")
  person_data <- dataset_50785095_person_df
} else if (exists("dataset_98104042_person_df") && is.data.frame(dataset_98104042_person_df)) {
  cat("✓ Using pre-loaded person data (dataset_98104042_person_df)\n")
  person_data <- dataset_98104042_person_df
} else {
  cat("Loading demographics from BigQuery...\n")
  person_sql <- paste("
    SELECT
        person_id,
        birth_datetime,
        gender_concept_id,
        sex_at_birth_concept_id
    FROM `person`
    WHERE person_id IN (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    )
  ")

  person_data <- bq_table_download(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), person_sql,
                     billing = Sys.getenv("GOOGLE_PROJECT"))
  )
}

# Calculate age
person_data <- person_data %>%
  mutate(
    age = as.numeric(difftime(Sys.Date(), as.Date(birth_datetime), units = "days")) / 365.25,
    sex = case_when(
      sex_at_birth_concept_id == 45878463 ~ "Female",
      sex_at_birth_concept_id == 45880669 ~ "Male",
      TRUE ~ "Unknown"
    )
  )

cat("✓ Demographics loaded\n\n")

# =============================================================================
# ANALYSIS 1: PREPARE DATA
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 1: BMI CLASS STRATIFICATION\n")
cat("========================================\n\n")

cat("Computing average BMI and activity per participant...\n")

# Average BMI per participant
participant_avg_bmi <- bmi_final %>%
  group_by(person_id) %>%
  summarize(
    avg_bmi = mean(bmi, na.rm = TRUE),
    n_bmi_measures = n(),
    .groups = "drop"
  ) %>%
  mutate(
    bmi_class = case_when(
      avg_bmi >= 18.5 & avg_bmi < 25 ~ "18.5-25",
      avg_bmi >= 25 & avg_bmi < 30 ~ "25-30",
      avg_bmi >= 30 & avg_bmi < 35 ~ "30-35",
      avg_bmi >= 35 & avg_bmi < 40 ~ "35-40",
      avg_bmi >= 40 ~ "≥40",
      TRUE ~ NA_character_  # Exclude BMI < 18.5
    ),
    bmi_class = factor(bmi_class, levels = c("18.5-25", "25-30", "30-35", "35-40", "≥40")),
    bmi_class_num = as.numeric(bmi_class)  # For regression
  ) %>%
  filter(!is.na(bmi_class))  # Remove participants with BMI < 18.5

# Average activity per participant + wear days
participant_avg_activity <- activity_final %>%
  group_by(person_id) %>%
  summarize(
    avg_steps = mean(steps, na.rm = TRUE),
    avg_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    avg_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
    avg_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    avg_activity_calories = mean(activity_calories, na.rm = TRUE),
    n_wear_days = n(),
    .groups = "drop"
  )

# Merge all data
analysis1_data <- participant_avg_bmi %>%
  inner_join(participant_avg_activity, by = "person_id") %>%
  left_join(person_data %>% select(person_id, age, sex), by = "person_id")

cat(sprintf("Analysis cohort: %s participants\n\n", format(nrow(analysis1_data), big.mark = ",")))

# =============================================================================
# SUMMARY STATISTICS BY BMI CLASS
# =============================================================================

cat("Computing summary statistics by BMI class...\n")

summary_by_bmi <- analysis1_data %>%
  group_by(bmi_class) %>%
  summarize(
    N = n(),

    # Age
    Age_median = median(age, na.rm = TRUE),
    Age_q25 = quantile(age, 0.25, na.rm = TRUE),
    Age_q75 = quantile(age, 0.75, na.rm = TRUE),

    # Sex
    Female_n = sum(sex == "Female", na.rm = TRUE),
    Female_pct = 100 * mean(sex == "Female", na.rm = TRUE),

    # BMI
    BMI_median = median(avg_bmi, na.rm = TRUE),
    BMI_q25 = quantile(avg_bmi, 0.25, na.rm = TRUE),
    BMI_q75 = quantile(avg_bmi, 0.75, na.rm = TRUE),

    # Wear days
    WearDays_median = median(n_wear_days, na.rm = TRUE),
    WearDays_q25 = quantile(n_wear_days, 0.25, na.rm = TRUE),
    WearDays_q75 = quantile(n_wear_days, 0.75, na.rm = TRUE),

    # Steps
    Steps_median = median(avg_steps, na.rm = TRUE),
    Steps_q25 = quantile(avg_steps, 0.25, na.rm = TRUE),
    Steps_q75 = quantile(avg_steps, 0.75, na.rm = TRUE),

    # Sedentary
    Sedentary_median = median(avg_sedentary_min, na.rm = TRUE),
    Sedentary_q25 = quantile(avg_sedentary_min, 0.25, na.rm = TRUE),
    Sedentary_q75 = quantile(avg_sedentary_min, 0.75, na.rm = TRUE),

    # Light active
    LightActive_median = median(avg_lightly_active_min, na.rm = TRUE),
    LightActive_q25 = quantile(avg_lightly_active_min, 0.25, na.rm = TRUE),
    LightActive_q75 = quantile(avg_lightly_active_min, 0.75, na.rm = TRUE),

    # Fairly active
    FairlyActive_median = median(avg_fairly_active_min, na.rm = TRUE),
    FairlyActive_q25 = quantile(avg_fairly_active_min, 0.25, na.rm = TRUE),
    FairlyActive_q75 = quantile(avg_fairly_active_min, 0.75, na.rm = TRUE),

    # Very active
    VeryActive_median = median(avg_very_active_min, na.rm = TRUE),
    VeryActive_q25 = quantile(avg_very_active_min, 0.25, na.rm = TRUE),
    VeryActive_q75 = quantile(avg_very_active_min, 0.75, na.rm = TRUE),

    # Activity calories
    ActivityCal_median = median(avg_activity_calories, na.rm = TRUE),
    ActivityCal_q25 = quantile(avg_activity_calories, 0.25, na.rm = TRUE),
    ActivityCal_q75 = quantile(avg_activity_calories, 0.75, na.rm = TRUE),

    .groups = "drop"
  )

cat("✓ Summary statistics computed\n\n")

# =============================================================================
# STATISTICAL TESTS
# =============================================================================

cat("Running statistical tests...\n\n")

# Jonckheere-Terpstra trend test (nonparametric test for ordered alternatives)
cat("1. Jonckheere-Terpstra trend tests:\n")

# Install/load clinfun package for JT test
if (!requireNamespace("clinfun", quietly = TRUE)) {
  cat("  Installing clinfun package...\n")
  install.packages("clinfun", repos = "http://cran.us.r-project.org")
}
library(clinfun)

jt_steps <- jonckheere.test(analysis1_data$avg_steps, as.numeric(analysis1_data$bmi_class),
                            alternative = "decreasing")
jt_sedentary <- jonckheere.test(analysis1_data$avg_sedentary_min, as.numeric(analysis1_data$bmi_class),
                                alternative = "increasing")
jt_calories <- jonckheere.test(analysis1_data$avg_activity_calories, as.numeric(analysis1_data$bmi_class),
                              alternative = "decreasing")

cat(sprintf("  Steps: J-T statistic = %.2f, p = %.2e\n", jt_steps$statistic, jt_steps$p.value))
cat(sprintf("  Sedentary: J-T statistic = %.2f, p = %.2e\n", jt_sedentary$statistic, jt_sedentary$p.value))
cat(sprintf("  Activity calories: J-T statistic = %.2f, p = %.2e\n\n", jt_calories$statistic, jt_calories$p.value))

# Quantile regression (median regression with ordinal BMI class)
cat("2. Quantile regression (median) for trend:\n")

qr_steps <- rq(avg_steps ~ bmi_class_num, data = analysis1_data, tau = 0.5)
qr_sedentary <- rq(avg_sedentary_min ~ bmi_class_num, data = analysis1_data, tau = 0.5)
qr_calories <- rq(avg_activity_calories ~ bmi_class_num, data = analysis1_data, tau = 0.5)

# Get summary with confidence intervals
qr_steps_sum <- summary(qr_steps, se = "boot")
qr_sedentary_sum <- summary(qr_sedentary, se = "boot")
qr_calories_sum <- summary(qr_calories, se = "boot")

cat(sprintf("  Steps: %.0f steps/day per BMI class increase (95%% CI: %.0f to %.0f), p = %.2e\n",
            coef(qr_steps)[2],
            qr_steps_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_steps)[2],
            qr_steps_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_steps)[2],
            qr_steps_sum$coefficients[2,4]))

cat(sprintf("  Sedentary: %.0f min/day per BMI class increase (95%% CI: %.0f to %.0f), p = %.2e\n",
            coef(qr_sedentary)[2],
            qr_sedentary_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_sedentary)[2],
            qr_sedentary_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_sedentary)[2],
            qr_sedentary_sum$coefficients[2,4]))

cat(sprintf("  Activity calories: %.0f cal/day per BMI class increase (95%% CI: %.0f to %.0f), p = %.2e\n\n",
            coef(qr_calories)[2],
            qr_calories_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_calories)[2],
            qr_calories_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_calories)[2],
            qr_calories_sum$coefficients[2,4]))

# Kruskal-Wallis (overall difference test)
cat("3. Kruskal-Wallis tests (overall group differences):\n")
kw_steps <- kruskal.test(avg_steps ~ bmi_class, data = analysis1_data)
kw_sedentary <- kruskal.test(avg_sedentary_min ~ bmi_class, data = analysis1_data)
kw_calories <- kruskal.test(avg_activity_calories ~ bmi_class, data = analysis1_data)

cat(sprintf("  Steps: χ² = %.2f, p = %.2e\n", kw_steps$statistic, kw_steps$p.value))
cat(sprintf("  Sedentary: χ² = %.2f, p = %.2e\n", kw_sedentary$statistic, kw_sedentary$p.value))
cat(sprintf("  Activity calories: χ² = %.2f, p = %.2e\n\n", kw_calories$statistic, kw_calories$p.value))

# =============================================================================
# MEDIAN DIFFERENCES VS REFERENCE (18.5-25) WITH BOOTSTRAP CI
# =============================================================================

cat("4. Median differences vs reference (18.5-25 BMI) with bootstrap 95% CI:\n")

# Bootstrap function for median difference
boot_median_diff <- function(data, indices, ref_data) {
  boot_sample <- data[indices]
  ref_sample <- sample(ref_data, length(ref_data), replace = TRUE)
  return(median(boot_sample, na.rm = TRUE) - median(ref_sample, na.rm = TRUE))
}

# Reference group (18.5-25)
ref_steps <- analysis1_data %>% filter(bmi_class == "18.5-25") %>% pull(avg_steps)
ref_sedentary <- analysis1_data %>% filter(bmi_class == "18.5-25") %>% pull(avg_sedentary_min)
ref_calories <- analysis1_data %>% filter(bmi_class == "18.5-25") %>% pull(avg_activity_calories)

# Install boot package if needed
if (!requireNamespace("boot", quietly = TRUE)) {
  cat("  Installing boot package...\n")
  install.packages("boot", repos = "http://cran.us.r-project.org")
}
library(boot)

# Calculate bootstrap CIs for each BMI class vs reference
median_diffs <- data.frame()

for (bmi_level in levels(analysis1_data$bmi_class)) {
  if (bmi_level == "18.5-25") next  # Skip reference

  # Steps
  comp_steps <- analysis1_data %>% filter(bmi_class == bmi_level) %>% pull(avg_steps)
  boot_steps <- boot(comp_steps, boot_median_diff, R = 1000, ref_data = ref_steps)
  ci_steps <- boot.ci(boot_steps, type = "perc")

  # Sedentary
  comp_sedentary <- analysis1_data %>% filter(bmi_class == bmi_level) %>% pull(avg_sedentary_min)
  boot_sedentary <- boot(comp_sedentary, boot_median_diff, R = 1000, ref_data = ref_sedentary)
  ci_sedentary <- boot.ci(boot_sedentary, type = "perc")

  # Calories
  comp_calories <- analysis1_data %>% filter(bmi_class == bmi_level) %>% pull(avg_activity_calories)
  boot_calories <- boot(comp_calories, boot_median_diff, R = 1000, ref_data = ref_calories)
  ci_calories <- boot.ci(boot_calories, type = "perc")

  cat(sprintf("  %s vs 18.5-25:\n", bmi_level))
  cat(sprintf("    Steps: %.0f (95%% CI: %.0f to %.0f)\n",
              boot_steps$t0, ci_steps$percent[4], ci_steps$percent[5]))
  cat(sprintf("    Sedentary: %.0f min (95%% CI: %.0f to %.0f)\n",
              boot_sedentary$t0, ci_sedentary$percent[4], ci_sedentary$percent[5]))
  cat(sprintf("    Calories: %.0f (95%% CI: %.0f to %.0f)\n",
              boot_calories$t0, ci_calories$percent[4], ci_calories$percent[5]))

  median_diffs <- rbind(median_diffs, data.frame(
    BMI_Class = bmi_level,
    Steps_Diff = boot_steps$t0,
    Steps_CI_Lower = ci_steps$percent[4],
    Steps_CI_Upper = ci_steps$percent[5],
    Sedentary_Diff = boot_sedentary$t0,
    Sedentary_CI_Lower = ci_sedentary$percent[4],
    Sedentary_CI_Upper = ci_sedentary$percent[5],
    Calories_Diff = boot_calories$t0,
    Calories_CI_Lower = ci_calories$percent[4],
    Calories_CI_Upper = ci_calories$percent[5]
  ))
}

cat("\n")

# =============================================================================
# ADJUSTED REGRESSION (AGE, SEX, WEAR TIME)
# =============================================================================

cat("5. Adjusted quantile regression (median, adjusted for age, sex, wear time):\n")

# Prepare data for regression
reg_data <- analysis1_data %>%
  filter(!is.na(age), !is.na(sex), sex != "Unknown") %>%
  mutate(
    sex_binary = ifelse(sex == "Female", 1, 0),
    age_scaled = scale(age)[,1],
    wear_days_scaled = scale(n_wear_days)[,1]
  )

# Adjusted quantile regression
qr_steps_adj <- rq(avg_steps ~ bmi_class_num + age_scaled + sex_binary + wear_days_scaled,
                   data = reg_data, tau = 0.5)
qr_sedentary_adj <- rq(avg_sedentary_min ~ bmi_class_num + age_scaled + sex_binary + wear_days_scaled,
                       data = reg_data, tau = 0.5)
qr_calories_adj <- rq(avg_activity_calories ~ bmi_class_num + age_scaled + sex_binary + wear_days_scaled,
                      data = reg_data, tau = 0.5)

# Get summaries
qr_steps_adj_sum <- summary(qr_steps_adj, se = "boot")
qr_sedentary_adj_sum <- summary(qr_sedentary_adj, se = "boot")
qr_calories_adj_sum <- summary(qr_calories_adj, se = "boot")

cat(sprintf("  Steps: %.0f steps/day per BMI class (95%% CI: %.0f to %.0f), p = %.2e\n",
            coef(qr_steps_adj)[2],
            qr_steps_adj_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_steps_adj)[2],
            qr_steps_adj_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_steps_adj)[2],
            qr_steps_adj_sum$coefficients[2,4]))

cat(sprintf("  Sedentary: %.0f min/day per BMI class (95%% CI: %.0f to %.0f), p = %.2e\n",
            coef(qr_sedentary_adj)[2],
            qr_sedentary_adj_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_sedentary_adj)[2],
            qr_sedentary_adj_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_sedentary_adj)[2],
            qr_sedentary_adj_sum$coefficients[2,4]))

cat(sprintf("  Activity calories: %.0f cal/day per BMI class (95%% CI: %.0f to %.0f), p = %.2e\n\n",
            coef(qr_calories_adj)[2],
            qr_calories_adj_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_calories_adj)[2],
            qr_calories_adj_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_calories_adj)[2],
            qr_calories_adj_sum$coefficients[2,4]))

# =============================================================================
# CREATE FORMATTED TABLE
# =============================================================================

cat("Creating formatted summary table...\n")

table1_formatted <- summary_by_bmi %>%
  mutate(
    `Age (years)` = sprintf("%.0f (%.0f-%.0f)", Age_median, Age_q25, Age_q75),
    `Female, n (%)` = sprintf("%d (%.1f)", Female_n, Female_pct),
    `BMI` = sprintf("%.1f (%.1f-%.1f)", BMI_median, BMI_q25, BMI_q75),
    `Wear Days` = sprintf("%.0f (%.0f-%.0f)", WearDays_median, WearDays_q25, WearDays_q75),
    `Steps/day` = sprintf("%.0f (%.0f-%.0f)", Steps_median, Steps_q25, Steps_q75),
    `Sedentary (min/day)` = sprintf("%.0f (%.0f-%.0f)", Sedentary_median, Sedentary_q25, Sedentary_q75),
    `Light Active (min/day)` = sprintf("%.0f (%.0f-%.0f)", LightActive_median, LightActive_q25, LightActive_q75),
    `Fairly Active (min/day)` = sprintf("%.0f (%.0f-%.0f)", FairlyActive_median, FairlyActive_q25, FairlyActive_q75),
    `Very Active (min/day)` = sprintf("%.0f (%.0f-%.0f)", VeryActive_median, VeryActive_q25, VeryActive_q75),
    `Activity Calories` = sprintf("%.0f (%.0f-%.0f)", ActivityCal_median, ActivityCal_q25, ActivityCal_q75)
  ) %>%
  select(`BMI Class` = bmi_class, N, `Age (years)`, `Female, n (%)`, BMI, `Wear Days`,
         `Steps/day`, `Sedentary (min/day)`, `Light Active (min/day)`,
         `Fairly Active (min/day)`, `Very Active (min/day)`, `Activity Calories`)

# Add footnote about valid Fitbit days
footnote_text <- paste(
  "<p><strong>Note:</strong> Valid Fitbit days defined as days with: ",
  "steps between 100-50,000; sedentary, light, fairly, and very active minutes each 0-1,440; ",
  "total activity minutes ≤1,440 per day. Participants required to have >30 valid days. ",
  "Participants with BMI < 18.5 were excluded. ",
  "Values shown as median (IQR).</p>",
  "<p><strong>Statistical tests:</strong> ",
  sprintf("Jonckheere-Terpstra trend test for steps: p = %.2e; ", jt_steps$p.value),
  sprintf("sedentary: p = %.2e; ", jt_sedentary$p.value),
  sprintf("activity calories: p = %.2e. ", jt_calories$p.value),
  sprintf("Unadjusted quantile regression per BMI class: steps %.0f/day (p = %.2e); ",
          coef(qr_steps)[2], qr_steps_sum$coefficients[2,4]),
  sprintf("Adjusted quantile regression (age, sex, wear time): steps %.0f/day (p = %.2e).</p>",
          coef(qr_steps_adj)[2], qr_steps_adj_sum$coefficients[2,4])
)

html_table <- knitr::kable(table1_formatted,
                           format = "html",
                           caption = "Table 1: Activity Measures by BMI Class",
                           align = c("l", rep("r", ncol(table1_formatted)-1)))

# Add footnote
html_output <- paste(html_table, footnote_text, sep = "\n")

writeLines(html_output, "analysis1_enhanced_table.html")
cat("✓ Saved: analysis1_enhanced_table.html\n")

# Save raw summary
write_csv(summary_by_bmi, "analysis1_enhanced_summary.csv")
cat("✓ Saved: analysis1_enhanced_summary.csv\n\n")

# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

cat("Creating visualizations...\n\n")

# 1. Distribution of daily steps by BMI class
cat("  1. Creating step distribution plot...\n")

# Calculate medians and N per class for annotations
class_stats <- analysis1_data %>%
  group_by(bmi_class) %>%
  summarize(
    median_steps = median(avg_steps, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("%s (n=%s)", bmi_class, format(n, big.mark = ","))
  )

# Install ggridges if needed
if (!requireNamespace("ggridges", quietly = TRUE)) {
  cat("  Installing ggridges package...\n")
  install.packages("ggridges", repos = "http://cran.us.r-project.org")
}
library(ggridges)

# Create ridgeline plot for better visualization of overlapping distributions
p1_ridges <- ggplot(analysis1_data, aes(x = avg_steps, y = bmi_class, fill = bmi_class)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5, quantile_lines = TRUE, quantiles = 2) +
  geom_vline(data = class_stats, aes(xintercept = median_steps, color = bmi_class),
             linetype = "dashed", linewidth = 0.8) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1, labels = class_stats$label) +
  scale_color_brewer(palette = "RdYlBu", direction = -1, guide = "none") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 20000)) +
  labs(
    title = "Distribution of Daily Steps by BMI Class",
    subtitle = sprintf("Jonckheere-Terpstra p = %.2e (decreasing trend). Dashed lines = medians", jt_steps$p.value),
    x = "Average Daily Steps",
    y = "BMI Class",
    fill = "BMI Class (N)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("analysis1_enhanced_step_distribution.png", p1_ridges,
       width = 12, height = 7, dpi = 300, bg = "white")
ggsave("analysis1_enhanced_step_distribution.pdf", p1_ridges,
       width = 12, height = 7)

cat("  ✓ Saved: analysis1_enhanced_step_distribution.png/pdf\n")

# Also create faceted density plot as alternative
p1_facet <- ggplot(analysis1_data, aes(x = avg_steps, fill = bmi_class)) +
  geom_density(alpha = 0.7, color = NA) +
  geom_vline(data = class_stats, aes(xintercept = median_steps),
             linetype = "dashed", linewidth = 0.6) +
  geom_text(data = class_stats, aes(x = median_steps, y = Inf, label = sprintf("Median: %.0f", median_steps)),
            hjust = -0.1, vjust = 1.5, size = 3) +
  facet_wrap(~ bmi_class, ncol = 1, labeller = labeller(bmi_class = setNames(class_stats$label, class_stats$bmi_class))) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 20000)) +
  labs(
    title = "Distribution of Daily Steps by BMI Class (Faceted)",
    subtitle = sprintf("Jonckheere-Terpstra p = %.2e (decreasing trend). Dashed lines = medians", jt_steps$p.value),
    x = "Average Daily Steps",
    y = "Density"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave("analysis1_enhanced_step_distribution_faceted.png", p1_facet,
       width = 10, height = 10, dpi = 300, bg = "white")
ggsave("analysis1_enhanced_step_distribution_faceted.pdf", p1_facet,
       width = 10, height = 10)

cat("  ✓ Saved: analysis1_enhanced_step_distribution_faceted.png/pdf\n")

# 2. Scatter plot of steps by BMI with trend line
cat("  2. Creating scatter plot with trend line...\n")

p2_scatter <- ggplot(analysis1_data, aes(x = avg_bmi, y = avg_steps)) +
  geom_point(aes(color = bmi_class), alpha = 0.4, size = 1) +
  geom_smooth(method = "loess", color = "#d73027", se = TRUE, linewidth = 1.5) +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Daily Steps by BMI",
    subtitle = sprintf("Quantile regression: %.0f steps/day per BMI class (p = %.2e)",
                      coef(qr_steps)[2], qr_steps_sum$coefficients[2,4]),
    x = "BMI",
    y = "Average Daily Steps",
    color = "BMI Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("analysis1_enhanced_scatter_bmi_steps.png", p2_scatter,
       width = 10, height = 6, dpi = 300, bg = "white")
ggsave("analysis1_enhanced_scatter_bmi_steps.pdf", p2_scatter,
       width = 10, height = 6)

cat("  ✓ Saved: analysis1_enhanced_scatter_bmi_steps.png/pdf\n")

# 3. Violin plots for main outcomes
cat("  3. Creating violin plots...\n")

p3_violin_steps <- ggplot(analysis1_data, aes(x = bmi_class, y = avg_steps, fill = bmi_class)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Daily Steps by BMI Class",
    x = "BMI Class",
    y = "Average Daily Steps"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

p3_violin_sedentary <- ggplot(analysis1_data, aes(x = bmi_class, y = avg_sedentary_min, fill = bmi_class)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Sedentary Minutes by BMI Class",
    x = "BMI Class",
    y = "Sedentary Minutes/Day"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

p3_combined_violin <- p3_violin_steps | p3_violin_sedentary

ggsave("analysis1_enhanced_violin_plots.png", p3_combined_violin,
       width = 14, height = 6, dpi = 300, bg = "white")
ggsave("analysis1_enhanced_violin_plots.pdf", p3_combined_violin,
       width = 14, height = 6)

cat("  ✓ Saved: analysis1_enhanced_violin_plots.png/pdf\n\n")

# =============================================================================
# SAVE STATISTICAL TEST RESULTS
# =============================================================================

cat("Saving statistical test results...\n")

stat_results <- data.frame(
  Test = c("Jonckheere-Terpstra", "Jonckheere-Terpstra", "Jonckheere-Terpstra",
           "Quantile Regression (Unadjusted)", "Quantile Regression (Unadjusted)", "Quantile Regression (Unadjusted)",
           "Quantile Regression (Adjusted)", "Quantile Regression (Adjusted)", "Quantile Regression (Adjusted)",
           "Kruskal-Wallis", "Kruskal-Wallis", "Kruskal-Wallis"),
  Measure = rep(c("Steps", "Sedentary", "Activity Calories"), 4),
  Statistic = c(jt_steps$statistic, jt_sedentary$statistic, jt_calories$statistic,
                coef(qr_steps)[2], coef(qr_sedentary)[2], coef(qr_calories)[2],
                coef(qr_steps_adj)[2], coef(qr_sedentary_adj)[2], coef(qr_calories_adj)[2],
                kw_steps$statistic, kw_sedentary$statistic, kw_calories$statistic),
  p_value = c(jt_steps$p.value, jt_sedentary$p.value, jt_calories$p.value,
              qr_steps_sum$coefficients[2,4], qr_sedentary_sum$coefficients[2,4],
              qr_calories_sum$coefficients[2,4],
              qr_steps_adj_sum$coefficients[2,4], qr_sedentary_adj_sum$coefficients[2,4],
              qr_calories_adj_sum$coefficients[2,4],
              kw_steps$p.value, kw_sedentary$p.value, kw_calories$p.value)
) %>%
  mutate(
    Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

write_csv(stat_results, "analysis1_enhanced_statistical_tests.csv")
cat("✓ Saved: analysis1_enhanced_statistical_tests.csv\n")

# Save median differences vs reference
write_csv(median_diffs, "analysis1_enhanced_median_differences_vs_reference.csv")
cat("✓ Saved: analysis1_enhanced_median_differences_vs_reference.csv\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("ANALYSIS 1 ENHANCED COMPLETE\n")
cat("##################################################\n\n")

cat("Output files:\n")
cat("  Tables:\n")
cat("    - analysis1_enhanced_table.html (with demographics and footnotes)\n")
cat("    - analysis1_enhanced_summary.csv (raw summary statistics)\n")
cat("    - analysis1_enhanced_statistical_tests.csv (all test results)\n")
cat("    - analysis1_enhanced_median_differences_vs_reference.csv (bootstrap CIs)\n\n")
cat("  Figures:\n")
cat("    - analysis1_enhanced_step_distribution.png/pdf (ridgeline plot with medians)\n")
cat("    - analysis1_enhanced_step_distribution_faceted.png/pdf (faceted densities)\n")
cat("    - analysis1_enhanced_scatter_bmi_steps.png/pdf\n")
cat("    - analysis1_enhanced_violin_plots.png/pdf\n\n")

cat("Key findings:\n")
cat(sprintf("  Jonckheere-Terpstra trend (steps): p = %.2e\n", jt_steps$p.value))
cat(sprintf("  Unadjusted quantile regression (steps): %.0f steps/day per BMI class\n", coef(qr_steps)[2]))
cat(sprintf("  Adjusted quantile regression (steps): %.0f steps/day per BMI class (adjusted for age, sex, wear time)\n",
            coef(qr_steps_adj)[2]))
cat(sprintf("  Total participants: %s across %d BMI classes (BMI < 18.5 excluded)\n",
            format(nrow(analysis1_data), big.mark = ","),
            length(unique(analysis1_data$bmi_class))))

cat("\nDone!\n\n")
