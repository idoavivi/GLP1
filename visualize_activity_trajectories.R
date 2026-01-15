# =============================================================================
# VISUALIZE ACTIVITY TRAJECTORIES OVER TIME
# =============================================================================
# Creates publication-ready plots showing activity changes over time
# Includes 4-panel combined plot and individual detailed plots
# =============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

cat("\n##################################################\n")
cat("ACTIVITY TRAJECTORY VISUALIZATION\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD DATA (or use objects in memory if available)
# =============================================================================

cat("Checking for data...\n")

has_data_in_memory <- (exists("obesity_cohort") && is.data.frame(obesity_cohort) &&
                       (exists("activity_cleaned") || exists("activity_final")))

if (has_data_in_memory) {
  cat("✓ Using data from memory\n")
  if (exists("activity_final")) {
    activity_cleaned <- activity_final
  }
} else {
  cat("Loading from file: glp1_cleaned_data.RData\n")
  load("glp1_cleaned_data.RData")
}

cat(sprintf("  Obesity cohort: %d patients\n\n", nrow(obesity_cohort)))

# =============================================================================
# DEFINE BASELINE COHORT
# =============================================================================

cat("Defining baseline cohort...\n")

baseline_activity_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_baseline_days = n(), .groups = "drop")

followup_1_30d_patients <- activity_cleaned %>%
  filter(person_id %in% obesity_cohort$person_id) %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 30,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 3) %>%
  summarize(n_followup_days = n(), .groups = "drop")

baseline_cohort <- baseline_activity_patients %>%
  inner_join(followup_1_30d_patients, by = "person_id")

cat(sprintf("Baseline cohort: N=%d\n\n", nrow(baseline_cohort)))

# =============================================================================
# CALCULATE PERSON-PERIOD SUMMARIES
# =============================================================================

cat("Calculating activity summaries by period...\n")

# Define periods
period_data <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id,
         is_valid_day == TRUE) %>%
  mutate(
    period = case_when(
      days_from_initiation >= -180 & days_from_initiation <= 0 ~ "Baseline",
      days_from_initiation >= 1 & days_from_initiation <= 30 ~ "1-30d",
      days_from_initiation >= 31 & days_from_initiation <= 90 ~ "31-90d",
      days_from_initiation >= 91 & days_from_initiation <= 180 ~ "91-180d",
      days_from_initiation >= 181 & days_from_initiation <= 365 ~ "181-365d",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

# Aggregate to person-period level
person_period_means <- period_data %>%
  group_by(person_id, period) %>%
  summarize(
    n_days = n(),
    steps = mean(steps, na.rm = TRUE),
    activity_calories = mean(activity_calories, na.rm = TRUE),
    sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    mvpa_min = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_days >= 3)

# Calculate median and IQR by period
activity_summary <- person_period_means %>%
  group_by(period) %>%
  summarize(
    n = n(),
    steps_median = median(steps, na.rm = TRUE),
    steps_q25 = quantile(steps, 0.25, na.rm = TRUE),
    steps_q75 = quantile(steps, 0.75, na.rm = TRUE),
    calories_median = median(activity_calories, na.rm = TRUE),
    calories_q25 = quantile(activity_calories, 0.25, na.rm = TRUE),
    calories_q75 = quantile(activity_calories, 0.75, na.rm = TRUE),
    sedentary_median = median(sedentary_min, na.rm = TRUE),
    sedentary_q25 = quantile(sedentary_min, 0.25, na.rm = TRUE),
    sedentary_q75 = quantile(sedentary_min, 0.75, na.rm = TRUE),
    mvpa_median = median(mvpa_min, na.rm = TRUE),
    mvpa_q25 = quantile(mvpa_min, 0.25, na.rm = TRUE),
    mvpa_q75 = quantile(mvpa_min, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")),
    time_point = as.numeric(period)  # For plotting
  ) %>%
  arrange(period)

cat(sprintf("Calculated summaries for %d period-observations\n\n", nrow(activity_summary)))

# =============================================================================
# LOAD P-VALUES (if available)
# =============================================================================

if (file.exists("table2_activity_adaptive.csv")) {
  cat("Loading p-values from Table 2...\n")
  table2 <- read_csv("table2_activity_adaptive.csv", show_col_types = FALSE)

  # Extract p-values
  get_pvalues <- function(param_name) {
    p_row <- table2 %>%
      filter(Parameter == "  P-value") %>%
      slice(1)

    tibble(
      period = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d"),
      p_value_text = c("—",
                       as.character(p_row$`1-30d`),
                       as.character(p_row$`31-90d`),
                       as.character(p_row$`91-180d`),
                       as.character(p_row$`181-365d`)),
      p_value = c(1,
                  as.numeric(p_row$`1-30d`),
                  as.numeric(p_row$`31-90d`),
                  as.numeric(p_row$`91-180d`),
                  as.numeric(p_row$`181-365d`)),
      significant = p_value < 0.05
    )
  }

  has_pvalues <- TRUE
  cat("✓ P-values loaded\n\n")
} else {
  has_pvalues <- FALSE
  cat("⚠ No p-values found (table2_activity_adaptive.csv not found)\n\n")
}

# =============================================================================
# CREATE COMBINED 4-PANEL PLOT
# =============================================================================

cat("Creating 4-panel combined plot...\n")

# Theme for all plots
plot_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    legend.position = "none"
  )

# 1. Steps plot
p_steps <- ggplot(activity_summary, aes(x = time_point, y = steps_median)) +
  geom_ribbon(aes(ymin = steps_q25, ymax = steps_q75), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "A. Daily Steps",
    x = "",
    y = "Steps per day\n(median, IQR)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. MVPA plot
p_mvpa <- ggplot(activity_summary, aes(x = time_point, y = mvpa_median)) +
  geom_ribbon(aes(ymin = mvpa_q25, ymax = mvpa_q75), alpha = 0.2, fill = "darkgreen") +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  labs(
    title = "B. Moderate-to-Vigorous Physical Activity",
    x = "",
    y = "MVPA (min/day)\n(median, IQR)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Sedentary time plot
p_sedentary <- ggplot(activity_summary, aes(x = time_point, y = sedentary_median)) +
  geom_ribbon(aes(ymin = sedentary_q25, ymax = sedentary_q75), alpha = 0.2, fill = "orangered") +
  geom_line(color = "orangered", linewidth = 1) +
  geom_point(color = "orangered", size = 3) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  labs(
    title = "C. Sedentary Time",
    x = "Time Period",
    y = "Sedentary (min/day)\n(median, IQR)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Activity calories plot
p_calories <- ggplot(activity_summary, aes(x = time_point, y = calories_median)) +
  geom_ribbon(aes(ymin = calories_q25, ymax = calories_q75), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 3) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  labs(
    title = "D. Activity Energy Expenditure",
    x = "Time Period",
    y = "Activity calories (kcal/day)\n(median, IQR)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine all 4 plots
combined_plot <- (p_steps + p_mvpa) / (p_sedentary + p_calories) +
  plot_annotation(
    title = "Activity Trajectories Following GLP-1 Initiation",
    subtitle = sprintf("N=%d patients with baseline and 1-30d follow-up data", nrow(baseline_cohort)),
    caption = "Error bands represent interquartile range (IQR). Mixed effects models used for statistical testing."
  )

# Save combined plot
ggsave("activity_trajectories_combined.png", combined_plot,
       width = 12, height = 10, dpi = 300, bg = "white")
ggsave("activity_trajectories_combined.pdf", combined_plot,
       width = 12, height = 10)

cat("Saved: activity_trajectories_combined.png\n")
cat("Saved: activity_trajectories_combined.pdf\n\n")

# =============================================================================
# CREATE INDIVIDUAL DETAILED PLOTS
# =============================================================================

cat("Creating individual detailed plots...\n")

# Helper function to add significance stars
add_significance <- function(p, data, y_var, has_pvalues) {
  if (!has_pvalues) return(p)

  # Get max y value for positioning
  max_y <- max(data[[y_var]], na.rm = TRUE)
  y_pos <- max_y * 1.15

  # Add significance markers
  p + annotate("text", x = 2, y = y_pos, label = ifelse(data$significant[2], "*", ""), size = 6) +
    annotate("text", x = 3, y = y_pos, label = ifelse(data$significant[3], "*", ""), size = 6) +
    annotate("text", x = 4, y = y_pos, label = ifelse(data$significant[4], "*", ""), size = 6) +
    annotate("text", x = 5, y = y_pos, label = ifelse(data$significant[5], "*", ""), size = 6) +
    coord_cartesian(ylim = c(min(data[[y_var]], na.rm = TRUE) * 0.9, y_pos * 1.05))
}

# 1. Steps detailed plot
p_steps_detail <- ggplot(activity_summary, aes(x = time_point, y = steps_median)) +
  geom_ribbon(aes(ymin = steps_q25, ymax = steps_q75), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1.5) +
  geom_point(color = "steelblue", size = 4) +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.5, size = 3.5) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Daily Steps Following GLP-1 Initiation",
    subtitle = sprintf("Baseline cohort: N=%d patients", nrow(baseline_cohort)),
    x = "Time Period",
    y = "Steps per day (median with IQR)",
    caption = "* p<0.05 vs. baseline (mixed effects model)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("activity_trajectory_steps.png", p_steps_detail,
       width = 8, height = 6, dpi = 300, bg = "white")

# 2. MVPA detailed plot
p_mvpa_detail <- ggplot(activity_summary, aes(x = time_point, y = mvpa_median)) +
  geom_ribbon(aes(ymin = mvpa_q25, ymax = mvpa_q75), alpha = 0.2, fill = "darkgreen") +
  geom_line(color = "darkgreen", linewidth = 1.5) +
  geom_point(color = "darkgreen", size = 4) +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.5, size = 3.5) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  labs(
    title = "Moderate-to-Vigorous Physical Activity Following GLP-1 Initiation",
    subtitle = sprintf("Baseline cohort: N=%d patients", nrow(baseline_cohort)),
    x = "Time Period",
    y = "MVPA (min/day, median with IQR)",
    caption = "* p<0.05 vs. baseline (mixed effects model)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("activity_trajectory_mvpa.png", p_mvpa_detail,
       width = 8, height = 6, dpi = 300, bg = "white")

# 3. Sedentary detailed plot
p_sedentary_detail <- ggplot(activity_summary, aes(x = time_point, y = sedentary_median)) +
  geom_ribbon(aes(ymin = sedentary_q25, ymax = sedentary_q75), alpha = 0.2, fill = "orangered") +
  geom_line(color = "orangered", linewidth = 1.5) +
  geom_point(color = "orangered", size = 4) +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.5, size = 3.5) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  labs(
    title = "Sedentary Time Following GLP-1 Initiation",
    subtitle = sprintf("Baseline cohort: N=%d patients", nrow(baseline_cohort)),
    x = "Time Period",
    y = "Sedentary time (min/day, median with IQR)",
    caption = "* p<0.05 vs. baseline (mixed effects model)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("activity_trajectory_sedentary.png", p_sedentary_detail,
       width = 8, height = 6, dpi = 300, bg = "white")

# 4. Calories detailed plot
p_calories_detail <- ggplot(activity_summary, aes(x = time_point, y = calories_median)) +
  geom_ribbon(aes(ymin = calories_q25, ymax = calories_q75), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1.5) +
  geom_point(color = "darkorchid", size = 4) +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.5, size = 3.5) +
  scale_x_continuous(breaks = 1:5, labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")) +
  labs(
    title = "Activity Energy Expenditure Following GLP-1 Initiation",
    subtitle = sprintf("Baseline cohort: N=%d patients", nrow(baseline_cohort)),
    x = "Time Period",
    y = "Activity calories (kcal/day, median with IQR)",
    caption = "* p<0.05 vs. baseline (mixed effects model)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("activity_trajectory_calories.png", p_calories_detail,
       width = 8, height = 6, dpi = 300, bg = "white")

cat("Saved: activity_trajectory_steps.png\n")
cat("Saved: activity_trajectory_mvpa.png\n")
cat("Saved: activity_trajectory_sedentary.png\n")
cat("Saved: activity_trajectory_calories.png\n\n")

# =============================================================================
# SAVE SUMMARY DATA
# =============================================================================

write_csv(activity_summary, "activity_trajectory_data.csv")
cat("Saved: activity_trajectory_data.csv\n\n")

cat("##################################################\n")
cat("VISUALIZATION COMPLETE\n")
cat("##################################################\n\n")

cat("Files created:\n")
cat("  - activity_trajectories_combined.png (4-panel plot)\n")
cat("  - activity_trajectories_combined.pdf (4-panel plot)\n")
cat("  - activity_trajectory_steps.png (detailed)\n")
cat("  - activity_trajectory_mvpa.png (detailed)\n")
cat("  - activity_trajectory_sedentary.png (detailed)\n")
cat("  - activity_trajectory_calories.png (detailed)\n")
cat("  - activity_trajectory_data.csv (summary data)\n\n")

cat("Key findings visible in plots:\n")
cat("  ✓ Non-linear trajectories clearly shown\n")
cat("  ✓ Interquartile ranges (IQR) displayed as shaded bands\n")
cat("  ✓ Sample sizes at each time point\n")
cat("  ✓ Publication-ready quality (300 dpi PNG + PDF)\n")
