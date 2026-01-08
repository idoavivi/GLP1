# =============================================================================
# COMPREHENSIVE SENSITIVITY ANALYSES
# =============================================================================
# Activity calories analysis and sensitivity analyses by various subgroups
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)

cat("\n##################################################\n")
cat("COMPREHENSIVE SENSITIVITY ANALYSES\n")
cat("Activity Calories & Subgroup Analyses\n")
cat("##################################################\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
load("cleaned_glp1_cohort.RData")

# =============================================================================
# EXCLUDE PATIENTS WITH BASELINE BMI < 30
# =============================================================================

cat("\n========================================\n")
cat("Filtering: Exclude BMI < 30 patients\n")
cat("========================================\n\n")

# Calculate baseline BMI for each patient
baseline_bmi_check <- bmi_measured %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop")

# Identify patients to exclude
patients_to_exclude <- baseline_bmi_check %>%
  filter(baseline_bmi < 30) %>%
  pull(person_id)

cat(sprintf("Patients with baseline BMI < 30: %d\n", length(patients_to_exclude)))

if(length(patients_to_exclude) > 0) {
  cat("Excluding these patients from all analyses...\n")

  # Filter all datasets
  activity_all <- activity_all %>%
    filter(!person_id %in% patients_to_exclude)

  weight_all <- weight_all %>%
    filter(!person_id %in% patients_to_exclude)

  bmi_measured <- bmi_measured %>%
    filter(!person_id %in% patients_to_exclude)

  cat(sprintf("Remaining patients: %d\n", length(unique(activity_all$person_id))))
}

# =============================================================================
# PART 1: ACTIVITY CALORIES MIXED EFFECTS ANALYSIS
# =============================================================================

cat("\n========================================\n")
cat("PART 1: Activity Calories Analysis\n")
cat("========================================\n\n")

# Define time periods
activity_periods <- tribble(
  ~period_name, ~days_min, ~days_max, ~period_numeric,
  "Baseline", -180, 0, 0,
  "1-30 days", 1, 30, 15,
  "31-90 days", 31, 90, 60,
  "91-180 days", 91, 180, 135,
  "181-365 days", 181, 365, 273
)

# Calculate activity metrics per period including calories
activity_summary <- activity_periods %>%
  rowwise() %>%
  mutate(
    period_data = list({
      activity_all %>%
        filter(days_from_initiation >= days_min,
               days_from_initiation <= days_max,
               is_valid_day == TRUE) %>%
        group_by(person_id) %>%
        filter(n() >= 3) %>%  # At least 3 valid days
        summarize(
          steps = mean(steps, na.rm = TRUE),
          active_minutes = mean(active_minutes, na.rm = TRUE),
          sedentary_minutes = mean(sedentary_minutes, na.rm = TRUE),
          activity_calories = mean(activity_calories, na.rm = TRUE),
          n_days = n(),
          .groups = "drop"
        ) %>%
        mutate(period = period_name,
               period_numeric = period_numeric)
    })
  ) %>%
  ungroup()

# Combine all periods
all_activity <- activity_summary %>%
  select(period_data) %>%
  unnest(period_data)

cat("Activity data summary by period:\n")
all_activity %>%
  group_by(period) %>%
  summarize(
    n_patients = n(),
    mean_calories = mean(activity_calories, na.rm = TRUE),
    sd_calories = sd(activity_calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Mixed effects model for activity calories
cat("\nFitting mixed effects model for activity calories...\n")

activity_cal_long <- all_activity %>%
  filter(!is.na(activity_calories)) %>%
  select(person_id, period_numeric, activity_calories)

if(nrow(activity_cal_long) > 0) {
  model_calories <- lmer(activity_calories ~ period_numeric + (1 | person_id),
                        data = activity_cal_long)

  cat("\nActivity Calories Mixed Effects Model:\n")
  print(summary(model_calories))

  # Extract coefficient
  coef_cal <- fixef(model_calories)["period_numeric"]

  # Calculate change over 1 year (365 days)
  baseline_cal <- mean(activity_cal_long %>%
                        filter(period_numeric == 0) %>%
                        pull(activity_calories), na.rm = TRUE)
  one_year_change_cal <- coef_cal * 273  # Average day in 181-365 period
  pct_change_cal <- (one_year_change_cal / baseline_cal) * 100

  cat(sprintf("\nActivity Calories Change:\n"))
  cat(sprintf("  Baseline: %.1f kcal/day\n", baseline_cal))
  cat(sprintf("  Change at 1 year: %.1f kcal/day (%.1f%%)\n",
              one_year_change_cal, pct_change_cal))

  # Create trajectory plot
  cat("\nCreating activity calories trajectory plot...\n")

  activity_cal_summary <- all_activity %>%
    filter(!is.na(activity_calories)) %>%
    group_by(period, period_numeric) %>%
    summarize(
      n = n(),
      mean_cal = mean(activity_calories),
      se_cal = sd(activity_calories) / sqrt(n()),
      .groups = "drop"
    )

  p_calories <- ggplot(activity_cal_summary, aes(x = period_numeric, y = mean_cal)) +
    geom_point(aes(size = n), color = "#E69F00") +
    geom_line(color = "#E69F00", linewidth = 1) +
    geom_errorbar(aes(ymin = mean_cal - 1.96*se_cal,
                      ymax = mean_cal + 1.96*se_cal),
                  width = 15, color = "#E69F00") +
    geom_smooth(method = "lm", se = TRUE, color = "#D55E00",
                linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(breaks = activity_periods$period_numeric,
                      labels = activity_periods$period_name) +
    scale_size_continuous(range = c(3, 8)) +
    labs(
      title = "Activity Calories Over Time After GLP-1 Initiation",
      subtitle = sprintf("Slope: %.2f kcal/day per day (%.1f%% change at 1 year)",
                        coef_cal, pct_change_cal),
      x = "Time Period",
      y = "Activity Calories (kcal/day)",
      size = "N patients"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  ggsave("activity_calories_trajectory.png", p_calories,
         width = 10, height = 7, dpi = 300)
  cat("Saved: activity_calories_trajectory.png\n")

  # Save results
  write_csv(activity_cal_summary, "activity_calories_summary.csv")
  cat("Saved: activity_calories_summary.csv\n")
}

# =============================================================================
# PART 2: SENSITIVITY ANALYSIS BY WEIGHT LOSS RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("PART 2: Sensitivity by Weight Response\n")
cat("========================================\n\n")

# Calculate weight change for each patient
weight_change <- all_activity %>%
  filter(!is.na(activity_calories)) %>%
  select(person_id) %>%
  distinct() %>%
  inner_join(
    weight_all %>%
      filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
      group_by(person_id) %>%
      summarize(baseline_weight = mean(weight_kg, na.rm = TRUE), .groups = "drop"),
    by = "person_id"
  ) %>%
  left_join(
    weight_all %>%
      filter(days_from_initiation >= 181, days_from_initiation <= 365) %>%
      group_by(person_id) %>%
      summarize(followup_weight = mean(weight_kg, na.rm = TRUE), .groups = "drop"),
    by = "person_id"
  ) %>%
  filter(!is.na(baseline_weight), !is.na(followup_weight)) %>%
  mutate(
    weight_change_kg = followup_weight - baseline_weight,
    pct_weight_change = (weight_change_kg / baseline_weight) * 100,
    response_group = if_else(pct_weight_change <= -5, "Responder (≥5% loss)",
                            "Non-responder (<5% loss)")
  )

cat(sprintf("Patients with weight data: %d\n", nrow(weight_change)))
cat(sprintf("Responders (≥5%% loss): %d (%.1f%%)\n",
            sum(weight_change$response_group == "Responder (≥5% loss)"),
            100 * mean(weight_change$response_group == "Responder (≥5% loss)")))

if(nrow(weight_change) >= 20) {
  # Add response group to activity data
  activity_by_response <- all_activity %>%
    inner_join(weight_change %>% select(person_id, response_group), by = "person_id")

  # Calculate summary statistics by response group
  response_summary <- activity_by_response %>%
    group_by(period, period_numeric, response_group) %>%
    summarize(
      n = n(),
      mean_steps = mean(steps, na.rm = TRUE),
      se_steps = sd(steps, na.rm = TRUE) / sqrt(n()),
      mean_mvpa = mean(active_minutes, na.rm = TRUE),
      se_mvpa = sd(active_minutes, na.rm = TRUE) / sqrt(n()),
      mean_calories = mean(activity_calories, na.rm = TRUE),
      se_calories = sd(activity_calories, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  write_csv(response_summary, "sensitivity_weight_response.csv")
  cat("Saved: sensitivity_weight_response.csv\n")

  # Create plots
  p_steps_response <- ggplot(response_summary,
                             aes(x = period_numeric, y = mean_steps,
                                 color = response_group, group = response_group)) +
    geom_point(aes(size = n)) +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymin = mean_steps - 1.96*se_steps,
                      ymax = mean_steps + 1.96*se_steps),
                  width = 15) +
    scale_x_continuous(breaks = activity_periods$period_numeric,
                      labels = activity_periods$period_name) +
    scale_color_manual(values = c("#0072B2", "#D55E00")) +
    labs(
      title = "Steps by Weight Loss Response",
      x = "Time Period",
      y = "Steps per Day",
      color = "Response Group",
      size = "N"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  p_mvpa_response <- ggplot(response_summary,
                            aes(x = period_numeric, y = mean_mvpa,
                                color = response_group, group = response_group)) +
    geom_point(aes(size = n)) +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymin = mean_mvpa - 1.96*se_mvpa,
                      ymax = mean_mvpa + 1.96*se_mvpa),
                  width = 15) +
    scale_x_continuous(breaks = activity_periods$period_numeric,
                      labels = activity_periods$period_name) +
    scale_color_manual(values = c("#0072B2", "#D55E00")) +
    labs(
      title = "MVPA by Weight Loss Response",
      x = "Time Period",
      y = "Active Minutes per Day",
      color = "Response Group",
      size = "N"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  p_cal_response <- ggplot(response_summary,
                           aes(x = period_numeric, y = mean_calories,
                               color = response_group, group = response_group)) +
    geom_point(aes(size = n)) +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymin = mean_calories - 1.96*se_calories,
                      ymax = mean_calories + 1.96*se_calories),
                  width = 15) +
    scale_x_continuous(breaks = activity_periods$period_numeric,
                      labels = activity_periods$period_name) +
    scale_color_manual(values = c("#0072B2", "#D55E00")) +
    labs(
      title = "Activity Calories by Weight Loss Response",
      x = "Time Period",
      y = "Activity Calories (kcal/day)",
      color = "Response Group",
      size = "N"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

} else {
  cat("Insufficient patients with weight data for response analysis\n")
}

# =============================================================================
# PART 3: SENSITIVITY ANALYSIS BY BASELINE ACTIVITY LEVEL
# =============================================================================

cat("\n========================================\n")
cat("PART 3: Sensitivity by Baseline Activity\n")
cat("========================================\n\n")

# Get baseline activity for each patient
baseline_activity <- all_activity %>%
  filter(period == "Baseline") %>%
  select(person_id, baseline_steps = steps, baseline_calories = activity_calories)

# Calculate median baseline steps
median_baseline_steps <- median(baseline_activity$baseline_steps, na.rm = TRUE)

cat(sprintf("Median baseline steps: %.0f\n", median_baseline_steps))

# Add activity level group
activity_by_baseline <- all_activity %>%
  inner_join(baseline_activity, by = "person_id") %>%
  mutate(
    activity_level = if_else(baseline_steps >= median_baseline_steps,
                            "High baseline activity",
                            "Low baseline activity")
  )

# Calculate summary statistics
baseline_summary <- activity_by_baseline %>%
  group_by(period, period_numeric, activity_level) %>%
  summarize(
    n = n(),
    mean_steps = mean(steps, na.rm = TRUE),
    se_steps = sd(steps, na.rm = TRUE) / sqrt(n()),
    mean_mvpa = mean(active_minutes, na.rm = TRUE),
    se_mvpa = sd(active_minutes, na.rm = TRUE) / sqrt(n()),
    mean_calories = mean(activity_calories, na.rm = TRUE),
    se_calories = sd(activity_calories, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

write_csv(baseline_summary, "sensitivity_baseline_activity.csv")
cat("Saved: sensitivity_baseline_activity.csv\n")

# Create plots
p_steps_baseline <- ggplot(baseline_summary,
                           aes(x = period_numeric, y = mean_steps,
                               color = activity_level, group = activity_level)) +
  geom_point(aes(size = n)) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_steps - 1.96*se_steps,
                    ymax = mean_steps + 1.96*se_steps),
                width = 15) +
  scale_x_continuous(breaks = activity_periods$period_numeric,
                    labels = activity_periods$period_name) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  labs(
    title = "Steps by Baseline Activity Level",
    x = "Time Period",
    y = "Steps per Day",
    color = "Baseline Activity",
    size = "N"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# =============================================================================
# PART 4: SENSITIVITY ANALYSIS BY BASELINE BMI CATEGORY
# =============================================================================

cat("\n========================================\n")
cat("PART 4: Sensitivity by BMI Category\n")
cat("========================================\n\n")

# Get baseline BMI for each patient
baseline_bmi_data <- bmi_measured %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    bmi_category = case_when(
      baseline_bmi >= 40 ~ "Class III (≥40)",
      baseline_bmi >= 35 ~ "Class II (35-39.9)",
      baseline_bmi >= 30 ~ "Class I (30-34.9)",
      TRUE ~ "Other"
    )
  ) %>%
  filter(bmi_category != "Other")

cat("BMI category distribution:\n")
table(baseline_bmi_data$bmi_category) %>% print()

# Add BMI category to activity data
activity_by_bmi <- all_activity %>%
  inner_join(baseline_bmi_data %>% select(person_id, bmi_category), by = "person_id")

# Calculate summary statistics
bmi_summary <- activity_by_bmi %>%
  group_by(period, period_numeric, bmi_category) %>%
  summarize(
    n = n(),
    mean_steps = mean(steps, na.rm = TRUE),
    se_steps = sd(steps, na.rm = TRUE) / sqrt(n()),
    mean_mvpa = mean(active_minutes, na.rm = TRUE),
    se_mvpa = sd(active_minutes, na.rm = TRUE) / sqrt(n()),
    mean_calories = mean(activity_calories, na.rm = TRUE),
    se_calories = sd(activity_calories, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

write_csv(bmi_summary, "sensitivity_bmi_category.csv")
cat("Saved: sensitivity_bmi_category.csv\n")

# Create plots
p_steps_bmi <- ggplot(bmi_summary,
                      aes(x = period_numeric, y = mean_steps,
                          color = bmi_category, group = bmi_category)) +
  geom_point(aes(size = n)) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_steps - 1.96*se_steps,
                    ymax = mean_steps + 1.96*se_steps),
                width = 15) +
  scale_x_continuous(breaks = activity_periods$period_numeric,
                    labels = activity_periods$period_name) +
  scale_color_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  labs(
    title = "Steps by Baseline BMI Category",
    x = "Time Period",
    y = "Steps per Day",
    color = "BMI Category",
    size = "N"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# =============================================================================
# PART 5: SENSITIVITY ANALYSIS BY STEPS CHANGE
# =============================================================================

cat("\n========================================\n")
cat("PART 5: Weight Loss by Steps Change\n")
cat("========================================\n\n")

# Calculate steps change for patients with both baseline and follow-up
steps_change <- all_activity %>%
  filter(period %in% c("Baseline", "181-365 days")) %>%
  select(person_id, period, steps) %>%
  pivot_wider(names_from = period, values_from = steps, names_prefix = "steps_") %>%
  filter(!is.na(steps_Baseline), !is.na(`steps_181-365 days`)) %>%
  mutate(
    steps_change = `steps_181-365 days` - steps_Baseline,
    pct_steps_change = (steps_change / steps_Baseline) * 100,
    steps_category = case_when(
      pct_steps_change > 10 ~ "Increased (>10%)",
      pct_steps_change < -10 ~ "Decreased (>10%)",
      TRUE ~ "Stable (±10%)"
    )
  )

cat("Steps change distribution:\n")
table(steps_change$steps_category) %>% print()

# Add weight change data
steps_weight <- steps_change %>%
  inner_join(weight_change %>% select(person_id, pct_weight_change), by = "person_id")

cat(sprintf("\nPatients with both steps and weight data: %d\n", nrow(steps_weight)))

if(nrow(steps_weight) >= 20) {
  # Summary by steps change category
  steps_weight_summary <- steps_weight %>%
    group_by(steps_category) %>%
    summarize(
      n = n(),
      mean_weight_change = mean(pct_weight_change, na.rm = TRUE),
      se_weight_change = sd(pct_weight_change, na.rm = TRUE) / sqrt(n()),
      mean_steps_change = mean(pct_steps_change, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(steps_weight_summary, "sensitivity_steps_weight.csv")
  cat("Saved: sensitivity_steps_weight.csv\n")

  # Create plot
  p_weight_by_steps <- ggplot(steps_weight_summary,
                               aes(x = reorder(steps_category, mean_weight_change),
                                   y = mean_weight_change)) +
    geom_col(aes(fill = steps_category), show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean_weight_change - 1.96*se_weight_change,
                      ymax = mean_weight_change + 1.96*se_weight_change),
                  width = 0.2) +
    geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73")) +
    labs(
      title = "Weight Loss by Steps Change Category",
      subtitle = "1-year follow-up",
      x = "Steps Change Category",
      y = "Weight Change (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

} else {
  cat("Insufficient patients for steps-weight analysis\n")
}

# =============================================================================
# PART 6: COMBINE AND SAVE ALL PLOTS
# =============================================================================

cat("\n========================================\n")
cat("PART 6: Creating Combined Visualizations\n")
cat("========================================\n\n")

if(exists("p_steps_response") && exists("p_mvpa_response") && exists("p_cal_response")) {
  cat("Creating weight response sensitivity plots...\n")
  sensitivity_response <- (p_steps_response | p_mvpa_response | p_cal_response)
  ggsave("sensitivity_weight_response.png", sensitivity_response,
         width = 18, height = 6, dpi = 300)
  cat("Saved: sensitivity_weight_response.png\n")
}

if(exists("p_steps_baseline") && exists("p_steps_bmi")) {
  cat("Creating baseline characteristics sensitivity plots...\n")
  sensitivity_baseline <- (p_steps_baseline | p_steps_bmi)
  ggsave("sensitivity_baseline_characteristics.png", sensitivity_baseline,
         width = 14, height = 6, dpi = 300)
  cat("Saved: sensitivity_baseline_characteristics.png\n")
}

if(exists("p_weight_by_steps")) {
  cat("Creating steps-weight relationship plot...\n")
  ggsave("sensitivity_steps_weight_relationship.png", p_weight_by_steps,
         width = 8, height = 6, dpi = 300)
  cat("Saved: sensitivity_steps_weight_relationship.png\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("SENSITIVITY ANALYSES COMPLETE\n")
cat("##################################################\n\n")

cat("Generated files:\n")
cat("  - activity_calories_trajectory.png\n")
cat("  - activity_calories_summary.csv\n")
cat("  - sensitivity_weight_response.png\n")
cat("  - sensitivity_weight_response.csv\n")
cat("  - sensitivity_baseline_characteristics.png\n")
cat("  - sensitivity_baseline_activity.csv\n")
cat("  - sensitivity_bmi_category.csv\n")
if(exists("steps_weight_summary")) {
  cat("  - sensitivity_steps_weight_relationship.png\n")
  cat("  - sensitivity_steps_weight.csv\n")
}
cat("\n")
