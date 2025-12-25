# =============================================================================
# Sensitivity Analysis Visualizations - Optimized Version
# =============================================================================
# Creates publication-quality figures using optimized period timepoints
# Shows trajectories across all timepoints: Baseline, 1-90d, 91-180d, 181-365d, Nadir
# =============================================================================

library(tidyverse)
library(patchwork)

cat("=============================================================================\n")
cat("SENSITIVITY ANALYSIS VISUALIZATIONS (OPTIMIZED TIMEPOINTS)\n")
cat("=============================================================================\n\n")

# Load period analysis optimized results
if (!file.exists("period_analysis_optimized_results.RData")) {
  stop("ERROR: period_analysis_optimized_results.RData not found. Run period_analysis_optimized.R first.")
}

load("period_analysis_optimized_results.RData")
cat("Loaded period_analysis_optimized_results.RData\n\n")

# =============================================================================
# PREPARE DATA: CREATE WEIGHT LOSS CATEGORIES BASED ON NADIR
# =============================================================================

cat("### PREPARING DATA WITH WEIGHT LOSS CATEGORIES ###\n\n")

# Get baseline data
baseline_cohort <- baseline_data %>%
  select(person_id, baseline_weight, baseline_steps, baseline_calories = baseline_calories)

# Get nadir data
nadir_cohort <- nadir_data %>%
  select(person_id, nadir_weight, nadir_steps, nadir_calories, days_to_nadir)

# Calculate weight loss categories based on baseline to nadir
weight_loss_categories <- baseline_cohort %>%
  inner_join(nadir_cohort, by = "person_id") %>%
  mutate(
    weight_change = nadir_weight - baseline_weight,
    weight_pct_change = 100 * weight_change / baseline_weight,
    weight_loss_cat = case_when(
      weight_pct_change > -5 ~ "< 5% loss",
      weight_pct_change <= -5 & weight_pct_change > -10 ~ "5-10% loss",
      weight_pct_change <= -10 ~ "> 10% loss"
    )
  ) %>%
  select(person_id, weight_loss_cat)

cat(sprintf("Patients categorized by weight loss (baseline to nadir): %d\n", nrow(weight_loss_categories)))

# Count by category
cat("\nWeight loss category distribution:\n")
print(table(weight_loss_categories$weight_loss_cat))
cat("\n")

# =============================================================================
# CREATE LONG FORMAT DATA WITH ALL TIMEPOINTS
# =============================================================================

cat("### CREATING LONG FORMAT DATA ###\n\n")

# Baseline
baseline_long <- baseline_cohort %>%
  mutate(
    timepoint = "Baseline",
    timepoint_num = 0,
    weight = baseline_weight,
    steps = baseline_steps,
    calories = baseline_calories
  ) %>%
  select(person_id, timepoint, timepoint_num, weight, steps, calories)

# 1-90d
period_1_90d <- period_data_list[["1-90d"]] %>%
  select(person_id, period_weight, period_steps, period_calories) %>%
  mutate(
    timepoint = "1-90d",
    timepoint_num = 1,
    weight = period_weight,
    steps = period_steps,
    calories = period_calories
  ) %>%
  select(person_id, timepoint, timepoint_num, weight, steps, calories)

# 91-180d
period_91_180d <- period_data_list[["91-180d"]] %>%
  select(person_id, period_weight, period_steps, period_calories) %>%
  mutate(
    timepoint = "91-180d",
    timepoint_num = 2,
    weight = period_weight,
    steps = period_steps,
    calories = period_calories
  ) %>%
  select(person_id, timepoint, timepoint_num, weight, steps, calories)

# 181-365d
period_181_365d <- period_data_list[["181-365d"]] %>%
  select(person_id, period_weight, period_steps, period_calories) %>%
  mutate(
    timepoint = "181-365d",
    timepoint_num = 3,
    weight = period_weight,
    steps = period_steps,
    calories = period_calories
  ) %>%
  select(person_id, timepoint, timepoint_num, weight, steps, calories)

# Nadir
nadir_long <- nadir_cohort %>%
  mutate(
    timepoint = "Nadir",
    timepoint_num = 4,
    weight = nadir_weight,
    steps = nadir_steps,
    calories = nadir_calories
  ) %>%
  select(person_id, timepoint, timepoint_num, weight, steps, calories)

# Combine all timepoints
all_timepoints <- bind_rows(
  baseline_long,
  period_1_90d,
  period_91_180d,
  period_181_365d,
  nadir_long
) %>%
  inner_join(weight_loss_categories, by = "person_id") %>%
  mutate(
    weight_loss_cat = factor(weight_loss_cat,
                              levels = c("< 5% loss", "5-10% loss", "> 10% loss")),
    timepoint = factor(timepoint,
                       levels = c("Baseline", "1-90d", "91-180d", "181-365d", "Nadir"))
  )

cat(sprintf("Total observations across all timepoints: %d\n\n", nrow(all_timepoints)))

# =============================================================================
# FIGURE 1: WEIGHT TRAJECTORIES BY WEIGHT LOSS CATEGORY
# =============================================================================

cat("### CREATING FIGURE 1: Weight Trajectories by Weight Loss Category ###\n\n")

# Calculate summary statistics
weight_summary <- all_timepoints %>%
  group_by(weight_loss_cat, timepoint, timepoint_num) %>%
  summarize(
    n = n(),
    mean_weight = mean(weight, na.rm = TRUE),
    se_weight = sd(weight, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create plot
fig1 <- ggplot(weight_summary,
               aes(x = timepoint, y = mean_weight, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight),
                width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = sprintf("n=%d", n)),
            vjust = -1.5, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60"),
    name = "Weight Loss Category\n(Baseline to Nadir)"
  ) +
  labs(
    title = "Weight Trajectories by Weight Loss Category",
    subtitle = "Patients categorized by total weight loss from baseline to nadir",
    x = "Timepoint",
    y = "Weight (kg, mean ± SE)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggsave("sensitivity_figure1_weight_trajectories_by_category.png", fig1,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure1_weight_trajectories_by_category.png\n\n")

# =============================================================================
# FIGURE 2: STEPS TRAJECTORIES BY WEIGHT LOSS CATEGORY
# =============================================================================

cat("### CREATING FIGURE 2: Steps Trajectories by Weight Loss Category ###\n\n")

# Calculate summary statistics
steps_summary <- all_timepoints %>%
  group_by(weight_loss_cat, timepoint, timepoint_num) %>%
  summarize(
    n = n(),
    mean_steps = mean(steps, na.rm = TRUE),
    se_steps = sd(steps, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create plot
fig2 <- ggplot(steps_summary,
               aes(x = timepoint, y = mean_steps, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_steps - se_steps, ymax = mean_steps + se_steps),
                width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = sprintf("n=%d", n)),
            vjust = -1.5, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60"),
    name = "Weight Loss Category\n(Baseline to Nadir)"
  ) +
  labs(
    title = "Steps Trajectories by Weight Loss Category",
    subtitle = "Patients categorized by total weight loss from baseline to nadir",
    x = "Timepoint",
    y = "Daily Steps (mean ± SE)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggsave("sensitivity_figure2_steps_trajectories_by_category.png", fig2,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure2_steps_trajectories_by_category.png\n\n")

# =============================================================================
# FIGURE 3: CALORIES TRAJECTORIES BY WEIGHT LOSS CATEGORY
# =============================================================================

cat("### CREATING FIGURE 3: Calories Trajectories by Weight Loss Category ###\n\n")

# Calculate summary statistics
calories_summary <- all_timepoints %>%
  group_by(weight_loss_cat, timepoint, timepoint_num) %>%
  summarize(
    n = n(),
    mean_calories = mean(calories, na.rm = TRUE),
    se_calories = sd(calories, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create plot
fig3 <- ggplot(calories_summary,
               aes(x = timepoint, y = mean_calories, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_calories - se_calories, ymax = mean_calories + se_calories),
                width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = sprintf("n=%d", n)),
            vjust = -1.5, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60"),
    name = "Weight Loss Category\n(Baseline to Nadir)"
  ) +
  labs(
    title = "Activity Calories Trajectories by Weight Loss Category",
    subtitle = "Patients categorized by total weight loss from baseline to nadir",
    x = "Timepoint",
    y = "Activity Calories (kcal, mean ± SE)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggsave("sensitivity_figure3_calories_trajectories_by_category.png", fig3,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure3_calories_trajectories_by_category.png\n\n")

# =============================================================================
# FIGURE 4: COMBINED PANEL - WEIGHT, STEPS, CALORIES
# =============================================================================

cat("### CREATING FIGURE 4: Combined Panel Figure ###\n\n")

# Create individual panels without legends
p4a <- ggplot(weight_summary,
              aes(x = timepoint, y = mean_weight, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight),
                width = 0.2, linewidth = 0.7) +
  geom_text(aes(label = sprintf("n=%d", n)),
            vjust = -1.2, size = 2.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60")
  ) +
  labs(
    title = "A. Weight",
    x = "",
    y = "Weight (kg)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p4b <- ggplot(steps_summary,
              aes(x = timepoint, y = mean_steps, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_steps - se_steps, ymax = mean_steps + se_steps),
                width = 0.2, linewidth = 0.7) +
  geom_text(aes(label = sprintf("n=%d", n)),
            vjust = -1.2, size = 2.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60")
  ) +
  labs(
    title = "B. Daily Steps",
    x = "",
    y = "Steps (n/day)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p4c <- ggplot(calories_summary,
              aes(x = timepoint, y = mean_calories, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_calories - se_calories, ymax = mean_calories + se_calories),
                width = 0.2, linewidth = 0.7) +
  geom_text(aes(label = sprintf("n=%d", n)),
            vjust = -1.2, size = 2.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60"),
    name = "Weight Loss Category"
  ) +
  labs(
    title = "C. Activity Calories",
    x = "",
    y = "Calories (kcal)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine panels
fig4 <- (p4a + p4b) / p4c +
  plot_layout(heights = c(1, 1.2), guides = "collect") &
  theme(legend.position = "bottom")

ggsave("sensitivity_figure4_combined_trajectories.png", fig4,
       width = 12, height = 10, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure4_combined_trajectories.png\n\n")

# =============================================================================
# FIGURE 5: SPAGHETTI PLOT - WEIGHT TRAJECTORIES
# =============================================================================

cat("### CREATING FIGURE 5: Spaghetti Plot - Weight Trajectories ###\n\n")

# Sample patients for clearer visualization (max 30 per category)
set.seed(123)
sampled_patients <- all_timepoints %>%
  group_by(weight_loss_cat) %>%
  distinct(person_id) %>%
  {
    group_split(.) %>%
      map_dfr(~ slice_sample(.x, n = min(30, nrow(.x))))
  }

weight_trajectory_sample <- all_timepoints %>%
  inner_join(sampled_patients, by = c("person_id", "weight_loss_cat"))

# Create spaghetti plot
fig5 <- ggplot() +
  # Individual trajectories
  geom_line(data = weight_trajectory_sample,
            aes(x = timepoint_num, y = weight, group = person_id),
            alpha = 0.2, linewidth = 0.3) +
  # Mean trajectory
  geom_line(data = weight_summary,
            aes(x = timepoint_num, y = mean_weight, group = 1),
            color = "black", linewidth = 1.5) +
  geom_ribbon(data = weight_summary,
              aes(x = timepoint_num,
                  ymin = mean_weight - se_weight,
                  ymax = mean_weight + se_weight,
                  group = 1),
              alpha = 0.2, fill = "black") +
  facet_wrap(~weight_loss_cat, ncol = 3) +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4),
    labels = c("Baseline", "1-90d", "91-180d", "181-365d", "Nadir")
  ) +
  labs(
    title = "Individual Weight Trajectories by Weight Loss Category",
    subtitle = "Up to 30 patients per group; bold line = mean ± SE",
    x = "Timepoint",
    y = "Weight (kg)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("sensitivity_figure5_spaghetti_weight.png", fig5,
       width = 14, height = 5, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure5_spaghetti_weight.png\n\n")

# =============================================================================
# FIGURE 6: SPAGHETTI PLOT - STEPS TRAJECTORIES
# =============================================================================

cat("### CREATING FIGURE 6: Spaghetti Plot - Steps Trajectories ###\n\n")

steps_trajectory_sample <- all_timepoints %>%
  inner_join(sampled_patients, by = c("person_id", "weight_loss_cat"))

# Create spaghetti plot
fig6 <- ggplot() +
  # Individual trajectories
  geom_line(data = steps_trajectory_sample,
            aes(x = timepoint_num, y = steps, group = person_id),
            alpha = 0.2, linewidth = 0.3) +
  # Mean trajectory
  geom_line(data = steps_summary,
            aes(x = timepoint_num, y = mean_steps, group = 1),
            color = "blue", linewidth = 1.5) +
  geom_ribbon(data = steps_summary,
              aes(x = timepoint_num,
                  ymin = mean_steps - se_steps,
                  ymax = mean_steps + se_steps,
                  group = 1),
              alpha = 0.2, fill = "blue") +
  facet_wrap(~weight_loss_cat, ncol = 3) +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4),
    labels = c("Baseline", "1-90d", "91-180d", "181-365d", "Nadir")
  ) +
  labs(
    title = "Individual Steps Trajectories by Weight Loss Category",
    subtitle = "Up to 30 patients per group; bold line = mean ± SE",
    x = "Timepoint",
    y = "Daily Steps"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("sensitivity_figure6_spaghetti_steps.png", fig6,
       width = 14, height = 5, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure6_spaghetti_steps.png\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("SENSITIVITY VISUALIZATIONS COMPLETE\n")
cat("=============================================================================\n\n")

cat("Created figures:\n")
cat("  1. sensitivity_figure1_weight_trajectories_by_category.png\n")
cat("     - Weight across all timepoints by weight loss category\n")
cat("     - Shows: Baseline, 1-90d, 91-180d, 181-365d, Nadir\n")
cat("     - Sample sizes displayed at each timepoint\n\n")

cat("  2. sensitivity_figure2_steps_trajectories_by_category.png\n")
cat("     - Steps across all timepoints by weight loss category\n")
cat("     - Sample sizes displayed at each timepoint\n\n")

cat("  3. sensitivity_figure3_calories_trajectories_by_category.png\n")
cat("     - Calories across all timepoints by weight loss category\n")
cat("     - Sample sizes displayed at each timepoint\n\n")

cat("  4. sensitivity_figure4_combined_trajectories.png\n")
cat("     - 3-panel figure: Weight, Steps, Calories\n")
cat("     - All timepoints in one comprehensive view\n\n")

cat("  5. sensitivity_figure5_spaghetti_weight.png\n")
cat("     - Individual patient weight trajectories\n")
cat("     - Up to 30 patients per weight loss category\n\n")

cat("  6. sensitivity_figure6_spaghetti_steps.png\n")
cat("     - Individual patient steps trajectories\n")
cat("     - Up to 30 patients per weight loss category\n\n")

cat("Weight loss categories based on baseline to nadir:\n")
cat("  - < 5% loss (red)\n")
cat("  - 5-10% loss (orange)\n")
cat("  - > 10% loss (green)\n\n")

cat("=============================================================================\n")
