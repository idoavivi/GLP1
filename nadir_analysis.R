# =============================================================================
# NADIR WEIGHT ANALYSIS
# =============================================================================
# Finds each patient's lowest weight (nadir) after >12 weeks of GLP-1
# Uses nadir for sensitivity analyses by activity changes
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)

cat("\n##################################################\n")
cat("NADIR WEIGHT ANALYSIS\n")
cat("Maximum Weight Loss Regardless of Timing\n")
cat("##################################################\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
load("glp1_cleaned_data.RData")

# Prepare data with days from initiation
cat("Preparing datasets with days from initiation...\n")

activity_all <- activity_cleaned %>%
  inner_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(date, glp1_initiation_date, units = "days")))

weight_all <- weight_cleaned %>%
  inner_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days")))

bmi_measured <- bmi_data %>%
  inner_join(glp1_initiation %>% select(person_id, glp1_initiation_date), by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(measurement_date, glp1_initiation_date, units = "days")))

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
} else {
  cat("All patients have baseline BMI ≥ 30. No exclusions needed.\n")
}

# =============================================================================
# PART 1: CALCULATE NADIR WEIGHT FOR EACH PATIENT
# =============================================================================

cat("\n========================================\n")
cat("PART 1: Calculate Nadir Weight\n")
cat("========================================\n\n")

# Get baseline weight (average from -180 to 0 days)
baseline_weight <- weight_all %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(
    baseline_weight = mean(weight_kg, na.rm = TRUE),
    baseline_n = n(),
    .groups = "drop"
  )

cat(sprintf("Patients with baseline weight: %d\n", nrow(baseline_weight)))

# Get nadir weight (minimum weight after day 84, which is ~12 weeks)
nadir_weight <- weight_all %>%
  filter(days_from_initiation > 84) %>%  # After 12 weeks
  group_by(person_id) %>%
  summarize(
    nadir_weight = min(weight_kg, na.rm = TRUE),
    nadir_day = days_from_initiation[which.min(weight_kg)],
    nadir_n = n(),
    .groups = "drop"
  )

cat(sprintf("Patients with post-12-week weight data: %d\n", nrow(nadir_weight)))

# Combine baseline and nadir
weight_nadir <- baseline_weight %>%
  inner_join(nadir_weight, by = "person_id") %>%
  mutate(
    nadir_change_kg = nadir_weight - baseline_weight,
    nadir_pct_change = (nadir_change_kg / baseline_weight) * 100,
    nadir_response = if_else(nadir_pct_change <= -5,
                            "Nadir Responder (≥5% loss)",
                            "Nadir Non-responder (<5% loss)"),
    nadir_response_10 = if_else(nadir_pct_change <= -10,
                               "Nadir ≥10% loss",
                               "Nadir <10% loss")
  )

cat(sprintf("\nPatients with both baseline and nadir weight: %d\n", nrow(weight_nadir)))

# Summary statistics
cat("\nNadir weight loss summary:\n")
summary_stats <- weight_nadir %>%
  summarize(
    mean_baseline = mean(baseline_weight),
    mean_nadir = mean(nadir_weight),
    mean_change_kg = mean(nadir_change_kg),
    mean_pct_change = mean(nadir_pct_change),
    median_pct_change = median(nadir_pct_change),
    sd_pct_change = sd(nadir_pct_change),
    n_responders_5pct = sum(nadir_pct_change <= -5),
    pct_responders_5pct = 100 * mean(nadir_pct_change <= -5),
    n_responders_10pct = sum(nadir_pct_change <= -10),
    pct_responders_10pct = 100 * mean(nadir_pct_change <= -10),
    mean_nadir_day = mean(nadir_day),
    median_nadir_day = median(nadir_day)
  )

print(summary_stats)

cat(sprintf("\nNadir timing: Mean %.0f days, Median %.0f days\n",
            summary_stats$mean_nadir_day, summary_stats$median_nadir_day))

# Save nadir results
write_csv(weight_nadir, "weight_nadir_results.csv")
cat("\nSaved: weight_nadir_results.csv\n")

# =============================================================================
# PART 2: NADIR DISTRIBUTION PLOTS
# =============================================================================

cat("\n========================================\n")
cat("PART 2: Nadir Distribution Plots\n")
cat("========================================\n\n")

# Histogram of nadir weight change
p_nadir_hist <- ggplot(weight_nadir, aes(x = nadir_pct_change)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white") +
  geom_vline(xintercept = -5, linetype = "dashed", color = "#D55E00", linewidth = 1) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "#CC79A7", linewidth = 1) +
  geom_vline(xintercept = median(weight_nadir$nadir_pct_change),
             color = "black", linewidth = 1) +
  annotate("text", x = -5, y = Inf, label = "5% loss", vjust = 1.5, hjust = -0.1,
           color = "#D55E00") +
  annotate("text", x = -10, y = Inf, label = "10% loss", vjust = 1.5, hjust = -0.1,
           color = "#CC79A7") +
  labs(
    title = "Distribution of Nadir Weight Loss",
    subtitle = sprintf("N=%d patients, Median=%.1f%%, Mean=%.1f%%",
                      nrow(weight_nadir),
                      summary_stats$median_pct_change,
                      summary_stats$mean_pct_change),
    x = "Nadir Weight Change (%)",
    y = "Number of Patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Scatter plot of nadir timing vs magnitude
p_nadir_timing <- ggplot(weight_nadir, aes(x = nadir_day, y = nadir_pct_change)) +
  geom_point(alpha = 0.5, color = "#0072B2") +
  geom_smooth(method = "loess", color = "#D55E00", se = TRUE) +
  geom_hline(yintercept = -5, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = -10, linetype = "dashed", color = "gray50") +
  labs(
    title = "Nadir Weight Loss Timing vs Magnitude",
    x = "Days to Nadir",
    y = "Nadir Weight Change (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Combined plot
p_nadir_combined <- p_nadir_hist / p_nadir_timing
ggsave("nadir_weight_distribution.png", p_nadir_combined,
       width = 10, height = 10, dpi = 300)
cat("Saved: nadir_weight_distribution.png\n")

# =============================================================================
# PART 3: ACTIVITY ANALYSIS BY NADIR RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("PART 3: Activity by Nadir Response\n")
cat("========================================\n\n")

# Define time periods for activity
activity_periods <- tribble(
  ~period_name, ~days_min, ~days_max, ~period_numeric,
  "Baseline", -180, 0, 0,
  "1-30 days", 1, 30, 15,
  "31-90 days", 31, 90, 60,
  "91-180 days", 91, 180, 135,
  "181-365 days", 181, 365, 273
)

# Calculate activity metrics per period
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

# Add nadir response groups (using 5% cutoff)
activity_by_nadir <- all_activity %>%
  inner_join(
    weight_nadir %>% select(person_id, nadir_response, nadir_pct_change),
    by = "person_id"
  )

cat(sprintf("Patients with both activity and nadir data: %d\n",
            length(unique(activity_by_nadir$person_id))))

# Response group distribution
cat("\nNadir response distribution:\n")
activity_by_nadir %>%
  distinct(person_id, nadir_response) %>%
  count(nadir_response) %>%
  print()

# Calculate summary by nadir response
nadir_activity_summary <- activity_by_nadir %>%
  group_by(period, period_numeric, nadir_response) %>%
  summarize(
    n = n_distinct(person_id),
    mean_steps = mean(steps, na.rm = TRUE),
    se_steps = sd(steps, na.rm = TRUE) / sqrt(n()),
    mean_mvpa = mean(active_minutes, na.rm = TRUE),
    se_mvpa = sd(active_minutes, na.rm = TRUE) / sqrt(n()),
    mean_calories = mean(activity_calories, na.rm = TRUE),
    se_calories = sd(activity_calories, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

write_csv(nadir_activity_summary, "nadir_activity_by_response.csv")
cat("Saved: nadir_activity_by_response.csv\n")

# Create plots for each metric
p_steps_nadir <- ggplot(nadir_activity_summary,
                        aes(x = period_numeric, y = mean_steps,
                            color = nadir_response, group = nadir_response)) +
  geom_point(aes(size = n)) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_steps - 1.96*se_steps,
                    ymax = mean_steps + 1.96*se_steps),
                width = 15) +
  scale_x_continuous(breaks = activity_periods$period_numeric,
                    labels = activity_periods$period_name) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_size_continuous(range = c(3, 8)) +
  labs(
    title = "Steps by Nadir Weight Response",
    subtitle = "Comparing patients by maximum weight loss achieved",
    x = "Time Period",
    y = "Steps per Day",
    color = "Nadir Response",
    size = "N patients"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

p_mvpa_nadir <- ggplot(nadir_activity_summary,
                       aes(x = period_numeric, y = mean_mvpa,
                           color = nadir_response, group = nadir_response)) +
  geom_point(aes(size = n)) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_mvpa - 1.96*se_mvpa,
                    ymax = mean_mvpa + 1.96*se_mvpa),
                width = 15) +
  scale_x_continuous(breaks = activity_periods$period_numeric,
                    labels = activity_periods$period_name) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_size_continuous(range = c(3, 8)) +
  labs(
    title = "MVPA by Nadir Weight Response",
    subtitle = "Active minutes across treatment course",
    x = "Time Period",
    y = "Active Minutes per Day",
    color = "Nadir Response",
    size = "N patients"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

p_calories_nadir <- ggplot(nadir_activity_summary,
                           aes(x = period_numeric, y = mean_calories,
                               color = nadir_response, group = nadir_response)) +
  geom_point(aes(size = n)) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_calories - 1.96*se_calories,
                    ymax = mean_calories + 1.96*se_calories),
                width = 15) +
  scale_x_continuous(breaks = activity_periods$period_numeric,
                    labels = activity_periods$period_name) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_size_continuous(range = c(3, 8)) +
  labs(
    title = "Activity Calories by Nadir Weight Response",
    subtitle = "Energy expenditure from activity",
    x = "Time Period",
    y = "Activity Calories (kcal/day)",
    color = "Nadir Response",
    size = "N patients"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Combine all activity plots
p_activity_nadir <- (p_steps_nadir | p_mvpa_nadir | p_calories_nadir)
ggsave("nadir_activity_trajectories.png", p_activity_nadir,
       width = 18, height = 6, dpi = 300)
cat("Saved: nadir_activity_trajectories.png\n")

# =============================================================================
# PART 4: CALCULATE ACTIVITY CHANGES BY NADIR RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("PART 4: Activity Changes by Nadir\n")
cat("========================================\n\n")

# Calculate baseline and follow-up activity for each patient
activity_change <- all_activity %>%
  filter(period %in% c("Baseline", "181-365 days")) %>%
  select(person_id, period, steps, active_minutes, activity_calories) %>%
  group_by(person_id, period) %>%
  summarize(
    steps = mean(steps, na.rm = TRUE),
    active_minutes = mean(active_minutes, na.rm = TRUE),
    activity_calories = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(steps, active_minutes, activity_calories)
  )

# Add nadir response
activity_change_nadir <- activity_change %>%
  inner_join(weight_nadir %>% select(person_id, nadir_response, nadir_pct_change),
             by = "person_id") %>%
  filter(!is.na(steps_Baseline), !is.na(`steps_181-365 days`)) %>%
  mutate(
    steps_change = `steps_181-365 days` - steps_Baseline,
    pct_steps_change = (steps_change / steps_Baseline) * 100,
    mvpa_change = `active_minutes_181-365 days` - active_minutes_Baseline,
    pct_mvpa_change = (mvpa_change / active_minutes_Baseline) * 100,
    calories_change = `activity_calories_181-365 days` - activity_calories_Baseline,
    pct_calories_change = (calories_change / activity_calories_Baseline) * 100
  )

cat(sprintf("Patients with baseline and 1-year activity: %d\n",
            nrow(activity_change_nadir)))

# Summary by nadir response group
activity_change_summary <- activity_change_nadir %>%
  group_by(nadir_response) %>%
  summarize(
    n = n(),
    mean_nadir_loss = mean(nadir_pct_change),
    mean_steps_change = mean(pct_steps_change, na.rm = TRUE),
    se_steps_change = sd(pct_steps_change, na.rm = TRUE) / sqrt(n()),
    mean_mvpa_change = mean(pct_mvpa_change, na.rm = TRUE),
    se_mvpa_change = sd(pct_mvpa_change, na.rm = TRUE) / sqrt(n()),
    mean_calories_change = mean(pct_calories_change, na.rm = TRUE),
    se_calories_change = sd(pct_calories_change, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

print(activity_change_summary)

write_csv(activity_change_summary, "nadir_activity_changes.csv")
cat("\nSaved: nadir_activity_changes.csv\n")

# Create bar plots comparing activity changes
p_changes_steps <- ggplot(activity_change_summary,
                          aes(x = nadir_response, y = mean_steps_change,
                              fill = nadir_response)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_steps_change - 1.96*se_steps_change,
                    ymax = mean_steps_change + 1.96*se_steps_change),
                width = 0.2) +
  geom_text(aes(label = sprintf("n=%d\n%.1f%%", n, mean_steps_change)),
            vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  labs(
    title = "Steps Change by Nadir Response",
    subtitle = "Baseline to 1 year",
    x = "",
    y = "Steps Change (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_changes_mvpa <- ggplot(activity_change_summary,
                         aes(x = nadir_response, y = mean_mvpa_change,
                             fill = nadir_response)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_mvpa_change - 1.96*se_mvpa_change,
                    ymax = mean_mvpa_change + 1.96*se_mvpa_change),
                width = 0.2) +
  geom_text(aes(label = sprintf("n=%d\n%.1f%%", n, mean_mvpa_change)),
            vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  labs(
    title = "MVPA Change by Nadir Response",
    subtitle = "Baseline to 1 year",
    x = "",
    y = "MVPA Change (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_changes_calories <- ggplot(activity_change_summary,
                             aes(x = nadir_response, y = mean_calories_change,
                                 fill = nadir_response)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_calories_change - 1.96*se_calories_change,
                    ymax = mean_calories_change + 1.96*se_calories_change),
                width = 0.2) +
  geom_text(aes(label = sprintf("n=%d\n%.1f%%", n, mean_calories_change)),
            vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  labs(
    title = "Activity Calories Change by Nadir Response",
    subtitle = "Baseline to 1 year",
    x = "",
    y = "Calories Change (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Combine change plots
p_activity_changes <- (p_changes_steps | p_changes_mvpa | p_changes_calories)
ggsave("nadir_activity_changes.png", p_activity_changes,
       width = 15, height = 5, dpi = 300)
cat("Saved: nadir_activity_changes.png\n")

# =============================================================================
# PART 5: STEPS CHANGE CATEGORIES AND NADIR WEIGHT LOSS
# =============================================================================

cat("\n========================================\n")
cat("PART 5: Nadir Weight Loss by Steps Change\n")
cat("========================================\n\n")

# Categorize by steps change
steps_categories_nadir <- activity_change_nadir %>%
  mutate(
    steps_category = case_when(
      pct_steps_change > 10 ~ "Increased (>10%)",
      pct_steps_change < -10 ~ "Decreased (>10%)",
      TRUE ~ "Stable (±10%)"
    )
  )

# Summary by steps category
steps_nadir_summary <- steps_categories_nadir %>%
  group_by(steps_category) %>%
  summarize(
    n = n(),
    mean_nadir_loss = mean(nadir_pct_change),
    se_nadir_loss = sd(nadir_pct_change) / sqrt(n()),
    mean_steps_change = mean(pct_steps_change),
    .groups = "drop"
  )

cat("Nadir weight loss by steps change category:\n")
print(steps_nadir_summary)

write_csv(steps_nadir_summary, "nadir_by_steps_category.csv")
cat("\nSaved: nadir_by_steps_category.csv\n")

# Create plot
p_nadir_by_steps <- ggplot(steps_nadir_summary,
                           aes(x = reorder(steps_category, -mean_nadir_loss),
                               y = mean_nadir_loss)) +
  geom_col(aes(fill = steps_category), show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_nadir_loss - 1.96*se_nadir_loss,
                    ymax = mean_nadir_loss + 1.96*se_nadir_loss),
                width = 0.2) +
  geom_text(aes(label = sprintf("n=%d\n%.1f%%", n, mean_nadir_loss)),
            vjust = 1.2, color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#D55E00")) +
  labs(
    title = "Nadir Weight Loss by Steps Change Category",
    subtitle = "Maximum weight loss achieved by activity change",
    x = "Steps Change Category",
    y = "Nadir Weight Change (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("nadir_by_steps_category.png", p_nadir_by_steps,
       width = 8, height = 6, dpi = 300)
cat("Saved: nadir_by_steps_category.png\n")

# =============================================================================
# PART 6: STATISTICAL TESTS
# =============================================================================

cat("\n========================================\n")
cat("PART 6: Statistical Tests\n")
cat("========================================\n\n")

# Test if nadir weight loss differs by response group
if(nrow(activity_change_nadir) >= 20) {

  # T-test: Do steps change differently between responders and non-responders?
  cat("Testing: Do nadir responders change steps differently?\n")
  responders <- activity_change_nadir %>%
    filter(nadir_response == "Nadir Responder (≥5% loss)") %>%
    pull(pct_steps_change)

  non_responders <- activity_change_nadir %>%
    filter(nadir_response == "Nadir Non-responder (<5% loss)") %>%
    pull(pct_steps_change)

  if(length(responders) >= 3 && length(non_responders) >= 3) {
    test_steps <- t.test(responders, non_responders)
    cat(sprintf("  Steps change: Responders %.1f%% vs Non-responders %.1f%% (p=%.3f)\n",
                mean(responders, na.rm = TRUE),
                mean(non_responders, na.rm = TRUE),
                test_steps$p.value))
  }

  # Test MVPA
  cat("\nTesting: Do nadir responders change MVPA differently?\n")
  responders_mvpa <- activity_change_nadir %>%
    filter(nadir_response == "Nadir Responder (≥5% loss)") %>%
    pull(pct_mvpa_change)

  non_responders_mvpa <- activity_change_nadir %>%
    filter(nadir_response == "Nadir Non-responder (<5% loss)") %>%
    pull(pct_mvpa_change)

  if(length(responders_mvpa) >= 3 && length(non_responders_mvpa) >= 3) {
    test_mvpa <- t.test(responders_mvpa, non_responders_mvpa)
    cat(sprintf("  MVPA change: Responders %.1f%% vs Non-responders %.1f%% (p=%.3f)\n",
                mean(responders_mvpa, na.rm = TRUE),
                mean(non_responders_mvpa, na.rm = TRUE),
                test_mvpa$p.value))
  }

  # ANOVA: Does nadir weight loss differ by steps change category?
  cat("\nTesting: Does nadir weight loss differ by steps change category?\n")
  aov_result <- aov(nadir_pct_change ~ steps_category, data = steps_categories_nadir)
  cat("ANOVA results:\n")
  print(summary(aov_result))

  # Post-hoc pairwise comparisons
  if(summary(aov_result)[[1]]$`Pr(>F)`[1] < 0.05) {
    cat("\nPost-hoc pairwise comparisons (Tukey HSD):\n")
    posthoc <- TukeyHSD(aov_result)
    print(posthoc)
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("NADIR ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat("Key findings:\n")
cat(sprintf("  - %d patients with nadir data\n", nrow(weight_nadir)))
cat(sprintf("  - Mean nadir weight loss: %.1f%% (SD=%.1f%%)\n",
            summary_stats$mean_pct_change, summary_stats$sd_pct_change))
cat(sprintf("  - Median nadir weight loss: %.1f%%\n",
            summary_stats$median_pct_change))
cat(sprintf("  - ≥5%% responders: %d (%.1f%%)\n",
            summary_stats$n_responders_5pct,
            summary_stats$pct_responders_5pct))
cat(sprintf("  - ≥10%% responders: %d (%.1f%%)\n",
            summary_stats$n_responders_10pct,
            summary_stats$pct_responders_10pct))
cat(sprintf("  - Mean time to nadir: %.0f days (%.1f months)\n",
            summary_stats$mean_nadir_day,
            summary_stats$mean_nadir_day / 30.4))

cat("\nGenerated files:\n")
cat("  - weight_nadir_results.csv\n")
cat("  - nadir_weight_distribution.png\n")
cat("  - nadir_activity_by_response.csv\n")
cat("  - nadir_activity_trajectories.png\n")
cat("  - nadir_activity_changes.csv\n")
cat("  - nadir_activity_changes.png\n")
cat("  - nadir_by_steps_category.csv\n")
cat("  - nadir_by_steps_category.png\n")
cat("\n")
