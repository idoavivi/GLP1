# =============================================================================
# FITBIT ACTIVITY BY BMI STRATIFICATION
# =============================================================================
# All participants with Fitbit data, stratified by BMI class
# Analysis 1: Activity measures across BMI groups
# Analysis 2: Activity change in participants who transition to lower BMI class
# =============================================================================

library(tidyverse)
library(bigrquery)
library(lubridate)
library(ggalluvial)
library(knitr)
library(patchwork)

cat("\n##################################################\n")
cat("FITBIT ACTIVITY BY BMI STRATIFICATION\n")
cat("All participants with verified Fitbit data\n")
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

# Filter datasets to cohort
activity_final <- activity_cleaned %>%
  filter(person_id %in% cohort_ids)

bmi_final <- bmi_computed %>%
  filter(person_id %in% cohort_ids)

# =============================================================================
# ANALYSIS 1: ACTIVITY BY BMI CLASS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 1: ACTIVITY BY BMI CLASS\n")
cat("========================================\n\n")

cat("Computing average BMI per participant... (this may take a moment)\n")

# Calculate average BMI per participant
participant_avg_bmi <- bmi_final %>%
  group_by(person_id) %>%
  summarize(
    avg_bmi = mean(bmi, na.rm = TRUE),
    n_bmi_measures = n(),
    .groups = "drop"
  ) %>%
  mutate(
    bmi_class = case_when(
      avg_bmi < 18 ~ "<18",
      avg_bmi >= 18 & avg_bmi < 25 ~ "18-25",
      avg_bmi >= 25 & avg_bmi < 30 ~ "25-30",
      avg_bmi >= 30 & avg_bmi < 35 ~ "30-35",
      avg_bmi >= 35 & avg_bmi < 40 ~ "35-40",
      avg_bmi >= 40 ~ "≥40"
    ),
    bmi_class = factor(bmi_class, levels = c("<18", "18-25", "25-30", "30-35", "35-40", "≥40"))
  )

cat("Computing average activity per participant... (this may take a few minutes)\n")
# Calculate average activity per participant
participant_avg_activity <- activity_final %>%
  group_by(person_id) %>%
  summarize(
    avg_steps = mean(steps, na.rm = TRUE),
    avg_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    avg_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
    avg_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    avg_activity_calories = mean(activity_calories, na.rm = TRUE),
    n_activity_days = n(),
    .groups = "drop"
  )

# Merge BMI and activity
analysis1_data <- participant_avg_bmi %>%
  inner_join(participant_avg_activity, by = "person_id")

cat(sprintf("Analysis 1 cohort: %d participants\n\n", nrow(analysis1_data)))

# Summary table by BMI class
summary_by_bmi <- analysis1_data %>%
  group_by(bmi_class) %>%
  summarize(
    N = n(),

    # BMI
    BMI_mean = mean(avg_bmi, na.rm = TRUE),
    BMI_sd = sd(avg_bmi, na.rm = TRUE),

    # Steps
    Steps_median = median(avg_steps, na.rm = TRUE),
    Steps_q25 = quantile(avg_steps, 0.25, na.rm = TRUE),
    Steps_q75 = quantile(avg_steps, 0.75, na.rm = TRUE),

    # Sedentary minutes
    Sedentary_median = median(avg_sedentary_min, na.rm = TRUE),
    Sedentary_q25 = quantile(avg_sedentary_min, 0.25, na.rm = TRUE),
    Sedentary_q75 = quantile(avg_sedentary_min, 0.75, na.rm = TRUE),

    # Lightly active minutes
    LightActive_median = median(avg_lightly_active_min, na.rm = TRUE),
    LightActive_q25 = quantile(avg_lightly_active_min, 0.25, na.rm = TRUE),
    LightActive_q75 = quantile(avg_lightly_active_min, 0.75, na.rm = TRUE),

    # Fairly active minutes
    FairlyActive_median = median(avg_fairly_active_min, na.rm = TRUE),
    FairlyActive_q25 = quantile(avg_fairly_active_min, 0.25, na.rm = TRUE),
    FairlyActive_q75 = quantile(avg_fairly_active_min, 0.75, na.rm = TRUE),

    # Very active minutes
    VeryActive_median = median(avg_very_active_min, na.rm = TRUE),
    VeryActive_q25 = quantile(avg_very_active_min, 0.25, na.rm = TRUE),
    VeryActive_q75 = quantile(avg_very_active_min, 0.75, na.rm = TRUE),

    # Activity calories
    ActivityCal_median = median(avg_activity_calories, na.rm = TRUE),
    ActivityCal_q25 = quantile(avg_activity_calories, 0.25, na.rm = TRUE),
    ActivityCal_q75 = quantile(avg_activity_calories, 0.75, na.rm = TRUE),

    .groups = "drop"
  )

# Print summary
cat("\nActivity Measures by BMI Class:\n")
cat("===============================\n\n")
print(summary_by_bmi, n = Inf, width = Inf)

# Save outputs
write_csv(analysis1_data, "analysis1_individual_data.csv")
write_csv(summary_by_bmi, "analysis1_summary_by_bmi.csv")

cat("\n✓ Saved: analysis1_individual_data.csv\n")
cat("✓ Saved: analysis1_summary_by_bmi.csv\n\n")

# Statistical tests
cat("Running Kruskal-Wallis tests...\n")
kw_steps <- kruskal.test(avg_steps ~ bmi_class, data = analysis1_data)
kw_sedentary <- kruskal.test(avg_sedentary_min ~ bmi_class, data = analysis1_data)
kw_activity_cal <- kruskal.test(avg_activity_calories ~ bmi_class, data = analysis1_data)

cat(sprintf("  Steps: χ² = %.2f, p = %.2e\n", kw_steps$statistic, kw_steps$p.value))
cat(sprintf("  Sedentary: χ² = %.2f, p = %.2e\n", kw_sedentary$statistic, kw_sedentary$p.value))
cat(sprintf("  Activity calories: χ² = %.2f, p = %.2e\n\n", kw_activity_cal$statistic, kw_activity_cal$p.value))

# Create formatted summary table
cat("Creating formatted tables...\n")

summary_table_formatted <- summary_by_bmi %>%
  mutate(
    BMI = sprintf("%.1f ± %.1f", BMI_mean, BMI_sd),
    Steps = sprintf("%.0f (%.0f-%.0f)", Steps_median, Steps_q25, Steps_q75),
    Sedentary = sprintf("%.0f (%.0f-%.0f)", Sedentary_median, Sedentary_q25, Sedentary_q75),
    `Light Active` = sprintf("%.0f (%.0f-%.0f)", LightActive_median, LightActive_q25, LightActive_q75),
    `Fairly Active` = sprintf("%.0f (%.0f-%.0f)", FairlyActive_median, FairlyActive_q25, FairlyActive_q75),
    `Very Active` = sprintf("%.0f (%.0f-%.0f)", VeryActive_median, VeryActive_q25, VeryActive_q75),
    `Activity Calories` = sprintf("%.0f (%.0f-%.0f)", ActivityCal_median, ActivityCal_q25, ActivityCal_q75)
  ) %>%
  select(`BMI Class` = bmi_class, N, BMI, Steps, Sedentary,
         `Light Active`, `Fairly Active`, `Very Active`, `Activity Calories`)

# Save as HTML table
html_table <- knitr::kable(summary_table_formatted,
                           format = "html",
                           caption = "Activity Measures by BMI Class (Median and IQR)",
                           align = c("l", "r", "r", "r", "r", "r", "r", "r", "r"))

writeLines(html_table, "analysis1_summary_table.html")
cat("✓ Saved: analysis1_summary_table.html\n")

# Save statistical test results
stat_results <- data.frame(
  Measure = c("Steps", "Sedentary Minutes", "Activity Calories"),
  Chi_squared = c(kw_steps$statistic, kw_sedentary$statistic, kw_activity_cal$statistic),
  p_value = c(kw_steps$p.value, kw_sedentary$p.value, kw_activity_cal$p.value)
) %>%
  mutate(
    Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

write_csv(stat_results, "analysis1_statistical_tests.csv")
cat("✓ Saved: analysis1_statistical_tests.csv\n\n")

# Create visualizations
cat("Creating visualizations...\n")

# Violin plot for steps by BMI class
p1_steps <- ggplot(analysis1_data, aes(x = bmi_class, y = avg_steps, fill = bmi_class)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Daily Steps by BMI Class",
    subtitle = sprintf("Kruskal-Wallis χ² = %.2f, p = %.2e", kw_steps$statistic, kw_steps$p.value),
    x = "BMI Class",
    y = "Average Daily Steps"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Violin plot for sedentary minutes
p2_sedentary <- ggplot(analysis1_data, aes(x = bmi_class, y = avg_sedentary_min, fill = bmi_class)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Sedentary Minutes by BMI Class",
    subtitle = sprintf("Kruskal-Wallis χ² = %.2f, p = %.2e", kw_sedentary$statistic, kw_sedentary$p.value),
    x = "BMI Class",
    y = "Average Sedentary Minutes/Day"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Violin plot for activity calories
p3_calories <- ggplot(analysis1_data, aes(x = bmi_class, y = avg_activity_calories, fill = bmi_class)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Activity Calories by BMI Class",
    subtitle = sprintf("Kruskal-Wallis χ² = %.2f, p = %.2e", kw_activity_cal$statistic, kw_activity_cal$p.value),
    x = "BMI Class",
    y = "Average Activity Calories/Day"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Stacked bar plot for activity intensity breakdown
activity_intensity <- analysis1_data %>%
  group_by(bmi_class) %>%
  summarize(
    Sedentary = median(avg_sedentary_min, na.rm = TRUE),
    `Light Active` = median(avg_lightly_active_min, na.rm = TRUE),
    `Fairly Active` = median(avg_fairly_active_min, na.rm = TRUE),
    `Very Active` = median(avg_very_active_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -bmi_class, names_to = "Intensity", values_to = "Minutes") %>%
  mutate(
    Intensity = factor(Intensity, levels = c("Very Active", "Fairly Active", "Light Active", "Sedentary"))
  )

p4_intensity <- ggplot(activity_intensity, aes(x = bmi_class, y = Minutes, fill = Intensity)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Very Active" = "#2166ac",
                               "Fairly Active" = "#4393c3",
                               "Light Active" = "#92c5de",
                               "Sedentary" = "#d1e5f0")) +
  labs(
    title = "Activity Intensity Breakdown by BMI Class",
    subtitle = "Median minutes per day",
    x = "BMI Class",
    y = "Minutes per Day",
    fill = "Intensity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Combine plots
p_combined <- (p1_steps | p2_sedentary) / (p3_calories | p4_intensity) +
  plot_annotation(
    title = "Fitbit Activity Measures by BMI Class",
    subtitle = sprintf("N = %d participants with >30 days of Fitbit data", nrow(analysis1_data)),
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("analysis1_activity_by_bmi_combined.png", p_combined,
       width = 14, height = 10, dpi = 300, bg = "white")
ggsave("analysis1_activity_by_bmi_combined.pdf", p_combined,
       width = 14, height = 10)

cat("✓ Saved: analysis1_activity_by_bmi_combined.png\n")
cat("✓ Saved: analysis1_activity_by_bmi_combined.pdf\n\n")

# =============================================================================
# ANALYSIS 2: BMI CLASS TRANSITIONS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 2: BMI CLASS TRANSITIONS\n")
cat("========================================\n\n")

cat("Identifying participants who moved to lower BMI class...\n\n")

# Assign BMI class to each measurement
bmi_with_class <- bmi_final %>%
  mutate(
    bmi_class = case_when(
      bmi < 18 ~ "<18",
      bmi >= 18 & bmi < 25 ~ "18-25",
      bmi >= 25 & bmi < 30 ~ "25-30",
      bmi >= 30 & bmi < 35 ~ "30-35",
      bmi >= 35 & bmi < 40 ~ "35-40",
      bmi >= 40 ~ "≥40"
    ),
    bmi_class_num = case_when(
      bmi < 18 ~ 1,
      bmi >= 18 & bmi < 25 ~ 2,
      bmi >= 25 & bmi < 30 ~ 3,
      bmi >= 30 & bmi < 35 ~ 4,
      bmi >= 35 & bmi < 40 ~ 5,
      bmi >= 40 ~ 6
    )
  )

# Find baseline (earliest) and follow-up BMI for each participant
bmi_transitions <- bmi_with_class %>%
  group_by(person_id) %>%
  arrange(measurement_date) %>%
  summarize(
    baseline_date = first(measurement_date),
    baseline_bmi = first(bmi),
    baseline_class = first(bmi_class),
    baseline_class_num = first(bmi_class_num),

    followup_date = last(measurement_date),
    followup_bmi = last(bmi),
    followup_class = last(bmi_class),
    followup_class_num = last(bmi_class_num),

    .groups = "drop"
  ) %>%
  mutate(
    days_between = as.numeric(difftime(followup_date, baseline_date, units = "days")),
    class_change = followup_class_num - baseline_class_num,
    moved_to_lower_class = class_change < 0 & days_between > 30
  )

# Filter for participants who moved to lower BMI class
transitioners <- bmi_transitions %>%
  filter(moved_to_lower_class == TRUE)

cat(sprintf("Found %d participants who transitioned to lower BMI class\n", nrow(transitioners)))
cat(sprintf("  (with >30 days between measurements)\n\n"))

if (nrow(transitioners) == 0) {
  cat("⚠ No participants found with BMI class transitions. Skipping Analysis 2.\n\n")
} else {
  # For each transitioner, get activity in 60-day windows around baseline and follow-up
  cat("Extracting activity in 60-day windows around BMI measurements...\n\n")

  transitioner_activity <- transitioners %>%
    rowwise() %>%
    mutate(
      # Baseline activity window: ±30 days from baseline BMI date
      baseline_activity = list({
        activity_final %>%
          filter(
            person_id == .data$person_id,
            date >= (baseline_date - 30),
            date <= (baseline_date + 30)
          )
      }),

      # Follow-up activity window: ±30 days from follow-up BMI date
      followup_activity = list({
        activity_final %>%
          filter(
            person_id == .data$person_id,
            date >= (followup_date - 30),
            date <= (followup_date + 30)
          )
      }),

      # Count days with data
      n_baseline_days = nrow(baseline_activity),
      n_followup_days = nrow(followup_activity)
    ) %>%
    ungroup() %>%
    # Filter for ≥5 days at both timepoints
    filter(n_baseline_days >= 5, n_followup_days >= 5)

  cat(sprintf("After requiring ≥5 days Fitbit data in windows: %d participants\n\n",
              nrow(transitioner_activity)))

  if (nrow(transitioner_activity) == 0) {
    cat("⚠ No participants with sufficient Fitbit data. Skipping Analysis 2.\n\n")
  } else {
    # Calculate average steps at baseline and follow-up
    transitioner_summary <- transitioner_activity %>%
      rowwise() %>%
      mutate(
        avg_steps_baseline = mean(baseline_activity$steps, na.rm = TRUE),
        avg_steps_followup = mean(followup_activity$steps, na.rm = TRUE),

        # Step quartiles
        steps_quartile_baseline = case_when(
          avg_steps_baseline <= quantile(transitioner_activity %>%
                                            rowwise() %>%
                                            mutate(avg_steps_baseline = mean(baseline_activity$steps, na.rm = TRUE)) %>%
                                            pull(avg_steps_baseline), 0.25, na.rm = TRUE) ~ "Q1",
          avg_steps_baseline <= quantile(transitioner_activity %>%
                                            rowwise() %>%
                                            mutate(avg_steps_baseline = mean(baseline_activity$steps, na.rm = TRUE)) %>%
                                            pull(avg_steps_baseline), 0.50, na.rm = TRUE) ~ "Q2",
          avg_steps_baseline <= quantile(transitioner_activity %>%
                                            rowwise() %>%
                                            mutate(avg_steps_baseline = mean(baseline_activity$steps, na.rm = TRUE)) %>%
                                            pull(avg_steps_baseline), 0.75, na.rm = TRUE) ~ "Q3",
          TRUE ~ "Q4"
        ),
        steps_quartile_followup = case_when(
          avg_steps_followup <= quantile(transitioner_activity %>%
                                            rowwise() %>%
                                            mutate(avg_steps_followup = mean(followup_activity$steps, na.rm = TRUE)) %>%
                                            pull(avg_steps_followup), 0.25, na.rm = TRUE) ~ "Q1",
          avg_steps_followup <= quantile(transitioner_activity %>%
                                            rowwise() %>%
                                            mutate(avg_steps_followup = mean(followup_activity$steps, na.rm = TRUE)) %>%
                                            pull(avg_steps_followup), 0.50, na.rm = TRUE) ~ "Q2",
          avg_steps_followup <= quantile(transitioner_activity %>%
                                            rowwise() %>%
                                            mutate(avg_steps_followup = mean(followup_activity$steps, na.rm = TRUE)) %>%
                                            pull(avg_steps_followup), 0.75, na.rm = TRUE) ~ "Q3",
          TRUE ~ "Q4"
        )
      ) %>%
      ungroup() %>%
      select(person_id,
             baseline_date, baseline_bmi, baseline_class,
             followup_date, followup_bmi, followup_class,
             days_between,
             avg_steps_baseline, avg_steps_followup,
             steps_quartile_baseline, steps_quartile_followup,
             n_baseline_days, n_followup_days)

    # Overall summary statistics
    overall_summary <- transitioner_summary %>%
      summarize(
        N = n(),

        Baseline_steps_median = median(avg_steps_baseline, na.rm = TRUE),
        Baseline_steps_q25 = quantile(avg_steps_baseline, 0.25, na.rm = TRUE),
        Baseline_steps_q75 = quantile(avg_steps_baseline, 0.75, na.rm = TRUE),

        Followup_steps_median = median(avg_steps_followup, na.rm = TRUE),
        Followup_steps_q25 = quantile(avg_steps_followup, 0.25, na.rm = TRUE),
        Followup_steps_q75 = quantile(avg_steps_followup, 0.75, na.rm = TRUE),

        Delta_steps_median = median(avg_steps_followup - avg_steps_baseline, na.rm = TRUE)
      )

    cat("\nActivity in Participants with BMI Class Reduction:\n")
    cat("=================================================\n\n")
    print(overall_summary)
    cat("\n")

    # Save outputs
    write_csv(transitioner_summary, "analysis2_bmi_transitioners.csv")
    cat("✓ Saved: analysis2_bmi_transitioners.csv\n")

    # Create formatted summary table
    summary_table_formatted_a2 <- overall_summary %>%
      mutate(
        `Baseline Steps` = sprintf("%.0f (%.0f-%.0f)", Baseline_steps_median, Baseline_steps_q25, Baseline_steps_q75),
        `Follow-up Steps` = sprintf("%.0f (%.0f-%.0f)", Followup_steps_median, Followup_steps_q25, Followup_steps_q75),
        `Delta Steps` = sprintf("%.0f", Delta_steps_median)
      ) %>%
      select(N, `Baseline Steps`, `Follow-up Steps`, `Delta Steps`)

    html_table_a2 <- knitr::kable(summary_table_formatted_a2,
                                  format = "html",
                                  caption = "Activity in Participants Who Transitioned to Lower BMI Class (Median and IQR)",
                                  align = c("r", "r", "r", "r"))

    writeLines(html_table_a2, "analysis2_summary_table.html")
    cat("✓ Saved: analysis2_summary_table.html\n")

    # Create additional visualization: paired comparison
    transitioner_long <- transitioner_summary %>%
      select(person_id, Baseline = avg_steps_baseline, `Follow-up` = avg_steps_followup) %>%
      pivot_longer(cols = c(Baseline, `Follow-up`), names_to = "Period", values_to = "Steps") %>%
      mutate(Period = factor(Period, levels = c("Baseline", "Follow-up")))

    p_paired <- ggplot(transitioner_long, aes(x = Period, y = Steps)) +
      geom_line(aes(group = person_id), alpha = 0.2, color = "gray60") +
      geom_violin(alpha = 0.6, fill = "#4575b4", draw_quantiles = c(0.25, 0.5, 0.75)) +
      geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.5) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Step Changes in Participants Who Reduced BMI Class",
        subtitle = sprintf("N = %d participants with >30d between measurements, ≥5d Fitbit in each window",
                          nrow(transitioner_summary)),
        x = "Period",
        y = "Average Daily Steps"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank()
      )

    ggsave("analysis2_paired_steps_comparison.png", p_paired,
           width = 8, height = 6, dpi = 300, bg = "white")
    ggsave("analysis2_paired_steps_comparison.pdf", p_paired,
           width = 8, height = 6)

    cat("✓ Saved: analysis2_paired_steps_comparison.png/pdf\n\n")

    # Sankey diagram by step quartiles
    cat("Creating Sankey diagram for step quartile transitions...\n\n")

    sankey_data <- transitioner_summary %>%
      select(person_id,
             Baseline = steps_quartile_baseline,
             Followup = steps_quartile_followup) %>%
      mutate(
        Baseline = factor(Baseline, levels = c("Q1", "Q2", "Q3", "Q4")),
        Followup = factor(Followup, levels = c("Q1", "Q2", "Q3", "Q4"))
      )

    # Save transition counts
    transition_counts <- sankey_data %>%
      group_by(Baseline, Followup) %>%
      summarize(n = n(), .groups = "drop") %>%
      arrange(Baseline, Followup)

    write_csv(transition_counts, "analysis2_step_quartile_transitions.csv")
    cat("✓ Saved: analysis2_step_quartile_transitions.csv\n\n")

    # Create Sankey diagram
    if (nrow(sankey_data) >= 10) {
      alluvial_data <- to_lodes_form(sankey_data %>% select(Baseline, Followup),
                                     key = "Period",
                                     axes = 1:2)

      p_sankey <- ggplot(alluvial_data,
                         aes(x = Period, stratum = stratum, alluvium = alluvium,
                             fill = stratum, label = stratum)) +
        geom_flow(stat = "alluvium", alpha = 0.6, width = 0.3) +
        geom_stratum(alpha = 0.8, width = 0.3) +
        geom_text(stat = "stratum", size = 3.5) +
        scale_fill_manual(values = c("Q1" = "#d73027", "Q2" = "#fc8d59",
                                     "Q3" = "#91bfdb", "Q4" = "#4575b4")) +
        scale_x_discrete(limits = c("Baseline", "Followup"),
                        labels = c("Baseline\n(Higher BMI)", "Follow-up\n(Lower BMI)")) +
        labs(
          title = "Step Quartile Transitions in BMI Class Reducers",
          subtitle = sprintf("N=%d participants who moved to lower BMI class", nrow(sankey_data)),
          y = "Number of Participants"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 11, face = "bold"),
          axis.title.x = element_blank(),
          panel.grid = element_blank()
        )

      ggsave("analysis2_step_quartile_sankey.png", p_sankey,
             width = 8, height = 6, dpi = 300, bg = "white")
      ggsave("analysis2_step_quartile_sankey.pdf", p_sankey,
             width = 8, height = 6)

      cat("✓ Saved: analysis2_step_quartile_sankey.png\n")
      cat("✓ Saved: analysis2_step_quartile_sankey.pdf\n\n")
    } else {
      cat("⚠ Too few participants for Sankey diagram (N<10). Skipped visualization.\n\n")
    }
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat("Output files:\n")
cat("  Analysis 1 (BMI stratification):\n")
cat("    CSV files:\n")
cat("      - analysis1_individual_data.csv\n")
cat("      - analysis1_summary_by_bmi.csv\n")
cat("      - analysis1_statistical_tests.csv\n")
cat("    Tables:\n")
cat("      - analysis1_summary_table.html\n")
cat("    Figures:\n")
cat("      - analysis1_activity_by_bmi_combined.png/pdf (4-panel visualization)\n\n")

if (exists("transitioner_summary") && nrow(transitioner_summary) > 0) {
  cat("  Analysis 2 (BMI class transitions):\n")
  cat("    CSV files:\n")
  cat("      - analysis2_bmi_transitioners.csv\n")
  cat("      - analysis2_step_quartile_transitions.csv\n")
  cat("    Tables:\n")
  cat("      - analysis2_summary_table.html\n")
  cat("    Figures:\n")
  cat("      - analysis2_paired_steps_comparison.png/pdf\n")
  cat("      - analysis2_step_quartile_sankey.png/pdf\n\n")
}

cat("Done!\n\n")
