# =============================================================================
# SENSITIVITY ANALYSES: STRATIFIED BY BMI CLASS AND BASELINE STEPS
# =============================================================================
# 1. By baseline BMI class: Change in step count and activity calories
# 2. By baseline step count: Change in nadir weight
# =============================================================================

library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

cat("\n##################################################\n")
cat("SENSITIVITY ANALYSES\n")
cat("Stratified by BMI Class and Baseline Steps\n")
cat("##################################################\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
load("glp1_cleaned_data.RData")

# Filter to BMI >= 30
baseline_bmi_check <- bmi_data %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop")

patients_to_exclude <- baseline_bmi_check %>%
  filter(baseline_bmi < 30) %>%
  pull(person_id)

activity_cleaned <- activity_cleaned %>% filter(!person_id %in% patients_to_exclude)
weight_cleaned <- weight_cleaned %>% filter(!person_id %in% patients_to_exclude)

# =============================================================================
# PART 1: STRATIFY BY BASELINE BMI CLASS
# =============================================================================

cat("\n========================================\n")
cat("PART 1: Stratification by BMI Class\n")
cat("========================================\n\n")

# Calculate baseline BMI with categories
baseline_bmi_categories <- bmi_data %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_bmi = mean(bmi, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    bmi_category = case_when(
      baseline_bmi >= 40 ~ "Class III (≥40)",
      baseline_bmi >= 35 ~ "Class II (35-39.9)",
      baseline_bmi >= 30 ~ "Class I (30-34.9)",
      TRUE ~ "Other"
    ),
    bmi_category = factor(bmi_category, levels = c("Class I (30-34.9)", "Class II (35-39.9)", "Class III (≥40)"))
  )

cat("BMI Class distribution:\n")
print(table(baseline_bmi_categories$bmi_category))
cat("\n")

# Calculate baseline activity
baseline_activity <- activity_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  summarize(
    baseline_steps = mean(steps, na.rm = TRUE),
    baseline_calories = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate follow-up activity by period
time_periods <- tribble(
  ~period, ~days_min, ~days_max,
  "1-30 days", 1, 30,
  "31-90 days", 31, 90,
  "91-180 days", 91, 180,
  "181-365 days", 181, 365
)

# Calculate change in steps and calories by BMI class
activity_change_by_bmi <- list()

for(i in 1:nrow(time_periods)) {
  period_name <- time_periods$period[i]
  days_min <- time_periods$days_min[i]
  days_max <- time_periods$days_max[i]

  period_activity <- activity_cleaned %>%
    filter(days_from_initiation >= days_min,
           days_from_initiation <= days_max,
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    summarize(
      followup_steps = mean(steps, na.rm = TRUE),
      followup_calories = mean(activity_calories, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    inner_join(baseline_activity, by = "person_id") %>%
    inner_join(baseline_bmi_categories, by = "person_id") %>%
    mutate(
      period = period_name,
      change_steps = followup_steps - baseline_steps,
      change_calories = followup_calories - baseline_calories
    )

  activity_change_by_bmi[[period_name]] <- period_activity
}

activity_change_all <- bind_rows(activity_change_by_bmi)

# Summarize by BMI class and period
activity_summary_by_bmi <- activity_change_all %>%
  group_by(bmi_category, period) %>%
  summarize(
    n = n(),
    mean_change_steps = mean(change_steps, na.rm = TRUE),
    se_change_steps = sd(change_steps, na.rm = TRUE) / sqrt(n()),
    mean_change_calories = mean(change_calories, na.rm = TRUE),
    se_change_calories = sd(change_calories, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(period = factor(period, levels = c("1-30 days", "31-90 days", "91-180 days", "181-365 days")))

cat("Activity change summary by BMI class:\n")
print(activity_summary_by_bmi)
cat("\n")

# =============================================================================
# PLOT 1: Change in Steps by BMI Class
# =============================================================================

cat("Creating plot: Change in steps by BMI class...\n")

plot_steps_by_bmi <- ggplot(activity_summary_by_bmi, aes(x = period, y = mean_change_steps, color = bmi_category, group = bmi_category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_change_steps - se_change_steps,
                    ymax = mean_change_steps + se_change_steps),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Class I (30-34.9)" = "#2E7D32",
                                "Class II (35-39.9)" = "#F57C00",
                                "Class III (≥40)" = "#C62828")) +
  labs(
    title = "Change in Daily Steps from Baseline by BMI Class",
    subtitle = "Mean ± SE across follow-up periods",
    x = "Follow-up Period",
    y = "Change in Steps per Day",
    color = "BMI Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("sensitivity_steps_by_bmi.png", plot_steps_by_bmi, width = 10, height = 7, dpi = 300)
cat("Saved: sensitivity_steps_by_bmi.png\n")

# =============================================================================
# PLOT 2: Change in Activity Calories by BMI Class
# =============================================================================

cat("Creating plot: Change in activity calories by BMI class...\n")

plot_calories_by_bmi <- ggplot(activity_summary_by_bmi, aes(x = period, y = mean_change_calories, color = bmi_category, group = bmi_category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_change_calories - se_change_calories,
                    ymax = mean_change_calories + se_change_calories),
                width = 0.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Class I (30-34.9)" = "#2E7D32",
                                "Class II (35-39.9)" = "#F57C00",
                                "Class III (≥40)" = "#C62828")) +
  labs(
    title = "Change in Activity Calories from Baseline by BMI Class",
    subtitle = "Mean ± SE across follow-up periods",
    x = "Follow-up Period",
    y = "Change in Activity Calories (kcal/day)",
    color = "BMI Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("sensitivity_calories_by_bmi.png", plot_calories_by_bmi, width = 10, height = 7, dpi = 300)
cat("Saved: sensitivity_calories_by_bmi.png\n\n")

# =============================================================================
# PART 2: STRATIFY BY BASELINE STEP COUNT
# =============================================================================

cat("========================================\n")
cat("PART 2: Stratification by Baseline Steps\n")
cat("========================================\n\n")

# Calculate baseline weight
baseline_weight <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  summarize(baseline_weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop")

# Calculate nadir weight (minimum after 12 weeks)
nadir_weight <- weight_cleaned %>%
  filter(days_from_initiation > 84) %>%
  group_by(person_id) %>%
  summarize(nadir_weight_kg = min(weight_kg, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(nadir_weight_kg))

# Combine with baseline steps and categorize
weight_change_data <- nadir_weight %>%
  inner_join(baseline_weight, by = "person_id") %>%
  inner_join(baseline_activity, by = "person_id") %>%
  mutate(
    weight_change_kg = nadir_weight_kg - baseline_weight_kg,
    weight_change_pct = 100 * weight_change_kg / baseline_weight_kg
  )

# Create step count tertiles
step_tertiles <- quantile(weight_change_data$baseline_steps, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

weight_change_data <- weight_change_data %>%
  mutate(
    step_category = case_when(
      baseline_steps <= step_tertiles[2] ~ paste0("Low (<", round(step_tertiles[2]), ")"),
      baseline_steps <= step_tertiles[3] ~ paste0("Medium (", round(step_tertiles[2]), "-", round(step_tertiles[3]), ")"),
      TRUE ~ paste0("High (≥", round(step_tertiles[3]), ")")
    ),
    step_category = factor(step_category, levels = c(
      paste0("Low (<", round(step_tertiles[2]), ")"),
      paste0("Medium (", round(step_tertiles[2]), "-", round(step_tertiles[3]), ")"),
      paste0("High (≥", round(step_tertiles[3]), ")")
    ))
  )

cat("Baseline step count tertiles:\n")
print(step_tertiles)
cat("\n")

cat("Step category distribution:\n")
print(table(weight_change_data$step_category))
cat("\n")

# Summarize weight change by step category
weight_summary_by_steps <- weight_change_data %>%
  group_by(step_category) %>%
  summarize(
    n = n(),
    mean_baseline_weight = mean(baseline_weight_kg, na.rm = TRUE),
    sd_baseline_weight = sd(baseline_weight_kg, na.rm = TRUE),
    mean_nadir_weight = mean(nadir_weight_kg, na.rm = TRUE),
    sd_nadir_weight = sd(nadir_weight_kg, na.rm = TRUE),
    mean_weight_change = mean(weight_change_kg, na.rm = TRUE),
    sd_weight_change = sd(weight_change_kg, na.rm = TRUE),
    mean_weight_change_pct = mean(weight_change_pct, na.rm = TRUE),
    sd_weight_change_pct = sd(weight_change_pct, na.rm = TRUE),
    .groups = "drop"
  )

cat("Weight change summary by baseline step category:\n")
print(weight_summary_by_steps)
cat("\n")

# =============================================================================
# PLOT 3: Nadir Weight Change by Baseline Step Count
# =============================================================================

cat("Creating plot: Nadir weight change by baseline step count...\n")

plot_weight_by_steps <- ggplot(weight_change_data, aes(x = step_category, y = weight_change_kg, fill = step_category)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("#E57373", "#FFB74D", "#81C784")) +
  labs(
    title = "Nadir Weight Change by Baseline Step Count Category",
    subtitle = "Box plots show median and IQR; diamonds show mean",
    x = "Baseline Step Count Category",
    y = "Weight Change at Nadir (kg)",
    fill = "Step Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("sensitivity_weight_by_baseline_steps.png", plot_weight_by_steps, width = 10, height = 7, dpi = 300)
cat("Saved: sensitivity_weight_by_baseline_steps.png\n\n")

# =============================================================================
# PLOT 4: Scatter plot - Weight change vs baseline steps
# =============================================================================

cat("Creating plot: Scatter plot of weight change vs baseline steps...\n")

# Add trend line
plot_scatter_weight_steps <- ggplot(weight_change_data, aes(x = baseline_steps, y = weight_change_kg)) +
  geom_point(alpha = 0.4, size = 2, color = "#1976D2") +
  geom_smooth(method = "lm", se = TRUE, color = "#C62828", fill = "#C62828", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Nadir Weight Change vs Baseline Step Count",
    subtitle = "Linear regression with 95% CI",
    x = "Baseline Steps per Day",
    y = "Weight Change at Nadir (kg)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Calculate correlation
cor_result <- cor.test(weight_change_data$baseline_steps, weight_change_data$weight_change_kg)

cat(sprintf("Correlation between baseline steps and weight change: r = %.3f, p = %.4f\n",
            cor_result$estimate, cor_result$p.value))

# Add correlation annotation to plot
plot_scatter_weight_steps <- plot_scatter_weight_steps +
  annotate("text", x = Inf, y = Inf,
           label = sprintf("r = %.3f, p = %.4f", cor_result$estimate, cor_result$p.value),
           hjust = 1.1, vjust = 1.5, size = 5, fontface = "italic")

ggsave("sensitivity_scatter_weight_steps.png", plot_scatter_weight_steps, width = 10, height = 7, dpi = 300)
cat("Saved: sensitivity_scatter_weight_steps.png\n\n")

# =============================================================================
# SUMMARY TABLE: Weight change by step category
# =============================================================================

cat("Creating summary table...\n")

summary_table <- weight_summary_by_steps %>%
  mutate(
    `Baseline Weight` = sprintf("%.1f ± %.1f", mean_baseline_weight, sd_baseline_weight),
    `Nadir Weight` = sprintf("%.1f ± %.1f", mean_nadir_weight, sd_nadir_weight),
    `Weight Change (kg)` = sprintf("%.1f ± %.1f", mean_weight_change, sd_weight_change),
    `Weight Change (%)` = sprintf("%.1f ± %.1f", mean_weight_change_pct, sd_weight_change_pct),
    `N` = as.character(n)
  ) %>%
  select(
    `Step Category` = step_category,
    N,
    `Baseline Weight`,
    `Nadir Weight`,
    `Weight Change (kg)`,
    `Weight Change (%)`
  )

print(summary_table)

write_csv(summary_table, "sensitivity_weight_by_steps_table.csv")
cat("\nSaved: sensitivity_weight_by_steps_table.csv\n\n")

# =============================================================================
# COMBINED FIGURE FOR PUBLICATION
# =============================================================================

cat("Creating combined figure...\n")

combined_plot <- (plot_steps_by_bmi / plot_calories_by_bmi | plot_weight_by_steps) +
  plot_annotation(
    title = "Sensitivity Analyses: Activity Changes by BMI and Weight Loss by Baseline Activity",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  ) +
  plot_layout(widths = c(1, 1))

ggsave("sensitivity_combined_figure.png", combined_plot, width = 16, height = 10, dpi = 300)
cat("Saved: sensitivity_combined_figure.png\n\n")

cat("##################################################\n")
cat("SENSITIVITY ANALYSES COMPLETE\n")
cat("##################################################\n\n")

cat("Output files:\n")
cat("  - sensitivity_steps_by_bmi.png\n")
cat("  - sensitivity_calories_by_bmi.png\n")
cat("  - sensitivity_weight_by_baseline_steps.png\n")
cat("  - sensitivity_scatter_weight_steps.png\n")
cat("  - sensitivity_combined_figure.png\n")
cat("  - sensitivity_weight_by_steps_table.csv\n\n")
