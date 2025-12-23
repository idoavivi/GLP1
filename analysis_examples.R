# =============================================================================
# GLP-1 and Physical Activity: Example Statistical Analyses
# =============================================================================
# This script provides example analyses after running glp1_activity_analysis.R
# =============================================================================

library(tidyverse)
library(ggplot2)
library(broom)
library(scales)

# Optional packages - install if needed
if (!require(patchwork, quietly = TRUE)) {
  message("Installing patchwork package...")
  install.packages("patchwork")
  library(patchwork)
}

# Mixed effects modeling packages (optional)
use_mixed_models <- FALSE
if (require(lme4, quietly = TRUE)) {
  if (require(lmerTest, quietly = TRUE)) {
    use_mixed_models <- TRUE
  } else {
    message("Note: lmerTest not available. Mixed effects models will be skipped.")
  }
} else {
  message("Note: lme4 not available. Mixed effects models will be skipped.")
}

# Load processed data
load("glp1_processed_data.RData")

# =============================================================================
# ANALYSIS 1: Descriptive Statistics
# =============================================================================

# Patient characteristics
patient_characteristics <- person_df %>%
  filter(person_id %in% glp1_initiation$person_id) %>%
  left_join(glp1_initiation, by = "person_id") %>%
  mutate(
    age_at_initiation = as.numeric(difftime(glp1_initiation_date, date_of_birth, units = "days")) / 365.25
  )

# Summary table
cat("\n=== Patient Demographics ===\n")
patient_characteristics %>%
  summarize(
    n_patients = n(),
    mean_age = mean(age_at_initiation, na.rm = TRUE),
    sd_age = sd(age_at_initiation, na.rm = TRUE),
    n_female = sum(gender == "Female", na.rm = TRUE),
    pct_female = 100 * mean(gender == "Female", na.rm = TRUE)
  ) %>%
  print()

# Race/ethnicity breakdown
cat("\n=== Race/Ethnicity Distribution ===\n")
patient_characteristics %>%
  count(race, ethnicity) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  arrange(desc(n)) %>%
  print()

# =============================================================================
# ANALYSIS 2: Baseline Activity and Weight
# =============================================================================

# Get baseline (pre-GLP-1) measurements
baseline_activity <- activity_with_glp1 %>%
  filter(period == "before", days_from_initiation >= -180) %>%  # 6 months before
  group_by(person_id) %>%
  summarize(
    baseline_mean_steps = mean(steps, na.rm = TRUE),
    baseline_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    baseline_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    n_days_baseline = n()
  )

baseline_weight <- weight_with_glp1 %>%
  filter(period == "before") %>%
  group_by(person_id) %>%
  slice_max(measurement_date, n = 1) %>%  # Most recent before GLP-1
  select(person_id, baseline_weight_kg = weight_kg, baseline_bmi = bmi)

cat("\n=== Baseline Activity ===\n")
summary(baseline_activity)

cat("\n=== Baseline Weight ===\n")
summary(baseline_weight)

# =============================================================================
# ANALYSIS 3: Before vs After Comparison (Paired Analysis)
# =============================================================================

# Require at least 30 days of data in each period
patient_before_after <- activity_with_glp1 %>%
  # Limit to +/- 6 months around initiation
  filter(abs(days_from_initiation) <= 180) %>%
  group_by(person_id, period) %>%
  summarize(
    mean_steps = mean(steps, na.rm = TRUE),
    mean_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    mean_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
    mean_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(n_days >= 30) %>%  # At least 30 days in period
  pivot_wider(
    names_from = period,
    values_from = c(mean_steps, mean_very_active_min, mean_fairly_active_min,
                    mean_sedentary_min, n_days)
  ) %>%
  filter(!is.na(mean_steps_before) & !is.na(mean_steps_after)) %>%
  mutate(
    steps_change = mean_steps_after - mean_steps_before,
    steps_pct_change = 100 * steps_change / mean_steps_before,
    very_active_change = mean_very_active_min_after - mean_very_active_min_before,
    sedentary_change = mean_sedentary_min_after - mean_sedentary_min_before
  )

cat("\n=== Patients with Before/After Data ===\n")
cat(sprintf("N = %d patients\n", nrow(patient_before_after)))

cat("\n=== Steps: Before vs After ===\n")
cat(sprintf("Before - Mean: %.1f (SD: %.1f)\n",
            mean(patient_before_after$mean_steps_before),
            sd(patient_before_after$mean_steps_before)))
cat(sprintf("After - Mean: %.1f (SD: %.1f)\n",
            mean(patient_before_after$mean_steps_after),
            sd(patient_before_after$mean_steps_after)))
cat(sprintf("Change - Mean: %.1f (SD: %.1f)\n",
            mean(patient_before_after$steps_change),
            sd(patient_before_after$steps_change)))

# Paired t-test
steps_test <- t.test(patient_before_after$mean_steps_after,
                     patient_before_after$mean_steps_before,
                     paired = TRUE)
cat("\n=== Paired t-test: Steps ===\n")
print(steps_test)

# Very active minutes
very_active_test <- t.test(patient_before_after$mean_very_active_min_after,
                          patient_before_after$mean_very_active_min_before,
                          paired = TRUE)
cat("\n=== Paired t-test: Very Active Minutes ===\n")
print(very_active_test)

# Sedentary minutes
sedentary_test <- t.test(patient_before_after$mean_sedentary_min_after,
                        patient_before_after$mean_sedentary_min_before,
                        paired = TRUE)
cat("\n=== Paired t-test: Sedentary Minutes ===\n")
print(sedentary_test)

# =============================================================================
# ANALYSIS 4: Weight Change Analysis
# =============================================================================

# Compare weight before vs after
weight_before_after <- weight_with_glp1 %>%
  filter(abs(days_from_initiation) <= 180) %>%
  group_by(person_id, period) %>%
  summarize(
    mean_weight = mean(weight_kg, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    n_measurements = n(),
    .groups = "drop"
  ) %>%
  filter(n_measurements >= 2) %>%
  pivot_wider(
    names_from = period,
    values_from = c(mean_weight, mean_bmi, n_measurements)
  ) %>%
  filter(!is.na(mean_weight_before) & !is.na(mean_weight_after)) %>%
  mutate(
    weight_change_kg = mean_weight_after - mean_weight_before,
    weight_pct_change = 100 * weight_change_kg / mean_weight_before,
    bmi_change = mean_bmi_after - mean_bmi_before
  )

cat("\n=== Weight Change ===\n")
cat(sprintf("N = %d patients with weight data\n", nrow(weight_before_after)))
cat(sprintf("Before - Mean: %.1f kg (SD: %.1f)\n",
            mean(weight_before_after$mean_weight_before, na.rm = TRUE),
            sd(weight_before_after$mean_weight_before, na.rm = TRUE)))
cat(sprintf("After - Mean: %.1f kg (SD: %.1f)\n",
            mean(weight_before_after$mean_weight_after, na.rm = TRUE),
            sd(weight_before_after$mean_weight_after, na.rm = TRUE)))
cat(sprintf("Change - Mean: %.1f kg (SD: %.1f)\n",
            mean(weight_before_after$weight_change_kg, na.rm = TRUE),
            sd(weight_before_after$weight_change_kg, na.rm = TRUE)))

weight_test <- t.test(weight_before_after$mean_weight_after,
                     weight_before_after$mean_weight_before,
                     paired = TRUE)
cat("\n=== Paired t-test: Weight ===\n")
print(weight_test)

# =============================================================================
# ANALYSIS 5: Association Between Weight Loss and Activity Change
# =============================================================================

# Combine weight and activity changes
combined_changes <- patient_before_after %>%
  inner_join(weight_before_after, by = "person_id")

if (nrow(combined_changes) > 10) {
  cat("\n=== Correlation: Weight Loss vs Activity Change ===\n")
  cat(sprintf("N = %d patients with both datasets\n", nrow(combined_changes)))

  cor_test <- cor.test(combined_changes$weight_change_kg,
                      combined_changes$steps_change)
  print(cor_test)

  # Linear regression
  lm_model <- lm(steps_change ~ weight_change_kg, data = combined_changes)
  cat("\n=== Regression: Activity Change ~ Weight Change ===\n")
  print(summary(lm_model))
}

# =============================================================================
# ANALYSIS 6: Time Trends (Mixed Effects Model)
# =============================================================================

# Prepare data for mixed model (weekly averages)
activity_weekly <- activity_with_glp1 %>%
  filter(abs(days_from_initiation) <= 180) %>%
  mutate(
    week_from_initiation = floor(days_from_initiation / 7),
    post_glp1 = as.numeric(days_from_initiation >= 0)
  ) %>%
  group_by(person_id, week_from_initiation, post_glp1) %>%
  summarize(
    mean_steps = mean(steps, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(n_days >= 3)  # At least 3 days in the week

# Fit mixed effects model
if (use_mixed_models && nrow(activity_weekly) > 100) {
  cat("\n=== Mixed Effects Model: Steps over Time ===\n")

  # Simple model: intercept + time + post-GLP-1 indicator + random intercept per person
  mixed_model <- lmer(
    mean_steps ~ week_from_initiation + post_glp1 + (1 | person_id),
    data = activity_weekly
  )

  print(summary(mixed_model))

  # Get coefficients
  cat("\n=== Model Coefficients ===\n")
  print(tidy(mixed_model))
} else if (nrow(activity_weekly) > 100) {
  cat("\n=== Mixed Effects Model: SKIPPED (lme4/lmerTest not available) ===\n")
  cat("To enable mixed models, install packages:\n")
  cat("  install.packages(c('lme4', 'lmerTest'))\n")
}

# =============================================================================
# VISUALIZATION 1: Individual Trajectories
# =============================================================================

# Select a random sample of patients for visualization
set.seed(42)
sample_patients <- sample(unique(activity_with_glp1$person_id), min(20, n_distinct(activity_with_glp1$person_id)))

p1 <- activity_with_glp1 %>%
  filter(person_id %in% sample_patients,
         abs(days_from_initiation) <= 180) %>%
  ggplot(aes(x = days_from_initiation, y = steps, group = person_id, color = person_id)) +
  geom_line(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(aes(group = 1, color = NULL), method = "loess", color = "black", size = 1.5) +
  scale_color_viridis_d(guide = "none") +
  labs(
    title = "Daily Steps Around GLP-1 Initiation",
    subtitle = sprintf("Sample of %d patients", length(sample_patients)),
    x = "Days from GLP-1 Initiation",
    y = "Daily Steps"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p1)
ggsave("figure1_step_trajectories.png", p1, width = 10, height = 6, dpi = 300)

# =============================================================================
# VISUALIZATION 2: Before/After Comparison
# =============================================================================

# Prepare data for paired plot
steps_comparison_long <- patient_before_after %>%
  select(person_id, before = mean_steps_before, after = mean_steps_after) %>%
  pivot_longer(cols = c(before, after), names_to = "period", values_to = "mean_steps") %>%
  mutate(period = factor(period, levels = c("before", "after")))

p2 <- ggplot(steps_comparison_long, aes(x = period, y = mean_steps)) +
  geom_line(aes(group = person_id), alpha = 0.3, color = "gray") +
  geom_boxplot(alpha = 0.5, fill = "steelblue", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, color = "darkblue") +
  labs(
    title = "Mean Daily Steps: Before vs After GLP-1",
    subtitle = sprintf("N = %d patients with ≥30 days in each period", nrow(patient_before_after)),
    x = "Period",
    y = "Mean Daily Steps"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p2)
ggsave("figure2_steps_before_after.png", p2, width = 8, height = 6, dpi = 300)

# =============================================================================
# VISUALIZATION 3: Activity Metrics Panel
# =============================================================================

# Create panel of different activity metrics
activity_metrics_long <- activity_with_glp1 %>%
  filter(abs(days_from_initiation) <= 180) %>%
  select(person_id, days_from_initiation, steps,
         very_active_minutes, fairly_active_minutes,
         lightly_active_minutes, sedentary_minutes) %>%
  pivot_longer(
    cols = c(steps, very_active_minutes, fairly_active_minutes,
             lightly_active_minutes, sedentary_minutes),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, levels = c("steps", "very_active_minutes",
                                       "fairly_active_minutes", "lightly_active_minutes",
                                       "sedentary_minutes"),
                    labels = c("Steps", "Very Active Min", "Fairly Active Min",
                              "Lightly Active Min", "Sedentary Min"))
  )

p3 <- ggplot(activity_metrics_long, aes(x = days_from_initiation, y = value)) +
  geom_smooth(method = "loess", color = "steelblue", fill = "lightblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Activity Metrics Around GLP-1 Initiation",
    x = "Days from GLP-1 Initiation",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(p3)
ggsave("figure3_activity_metrics_panel.png", p3, width = 12, height = 10, dpi = 300)

# =============================================================================
# VISUALIZATION 4: Weight and Steps Combined
# =============================================================================

if (exists("combined_changes") && nrow(combined_changes) > 10) {
  p4 <- ggplot(combined_changes, aes(x = weight_change_kg, y = steps_change)) +
    geom_point(alpha = 0.6, size = 3, color = "steelblue") +
    geom_smooth(method = "lm", color = "red", fill = "pink") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = "Association Between Weight Change and Activity Change",
      subtitle = sprintf("N = %d patients", nrow(combined_changes)),
      x = "Weight Change (kg)",
      y = "Change in Mean Daily Steps"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  print(p4)
  ggsave("figure4_weight_steps_association.png", p4, width = 8, height = 6, dpi = 300)
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

# Compile all results into a list
analysis_results <- list(
  patient_characteristics = patient_characteristics,
  baseline_activity = baseline_activity,
  baseline_weight = baseline_weight,
  patient_before_after = patient_before_after,
  weight_before_after = weight_before_after,
  steps_test = steps_test,
  very_active_test = very_active_test,
  sedentary_test = sedentary_test,
  weight_test = weight_test
)

if (exists("combined_changes")) {
  analysis_results$combined_changes <- combined_changes
  analysis_results$correlation_test <- cor_test
  analysis_results$regression_model <- lm_model
}

if (exists("mixed_model")) {
  analysis_results$mixed_model <- mixed_model
}

# Save results
save(analysis_results, file = "glp1_analysis_results.RData")

cat("\n=== Analysis Complete! ===\n")
cat("Results saved to: glp1_analysis_results.RData\n")
cat("Figures saved as PNG files\n")
