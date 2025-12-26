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

# =============================================================================
# =============================================================================
# SHORT PERIODS VISUALIZATIONS (1-30d, 31-90d, 91-180d, 181-365d, Nadir)
# =============================================================================
# =============================================================================

if (file.exists("period_analysis_short_results.RData")) {

  cat("\n\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n")
  cat("SHORT PERIODS SENSITIVITY VISUALIZATIONS\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n\n")

  load("period_analysis_short_results.RData")
  cat("Loaded period_analysis_short_results.RData\n\n")

  # =============================================================================
  # CREATE WEIGHT LOSS CATEGORIES FOR SHORT PERIODS (SEPARATE FROM MAIN)
  # =============================================================================

  cat("### CREATING WEIGHT LOSS CATEGORIES (SHORT PERIODS) ###\n\n")

  # Get baseline and nadir from SHORT periods analysis
  baseline_cohort_short <- baseline_data_short %>%
    select(person_id, baseline_weight, baseline_steps, baseline_calories)

  nadir_cohort_short <- nadir_data_short %>%
    select(person_id, nadir_weight, nadir_steps, nadir_calories, days_to_nadir)

  # Calculate weight loss categories based on SHORT periods baseline to nadir
  weight_loss_categories_short <- baseline_cohort_short %>%
    inner_join(nadir_cohort_short, by = "person_id") %>%
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

  cat(sprintf("Patients categorized by weight loss (short periods baseline to nadir): %d\n", nrow(weight_loss_categories_short)))

  # Count by category
  cat("\nWeight loss category distribution (short periods):\n")
  print(table(weight_loss_categories_short$weight_loss_cat))
  cat("\n")

  # =============================================================================
  # CREATE LONG FORMAT DATA WITH SHORT PERIODS
  # =============================================================================

  cat("### CREATING LONG FORMAT DATA (SHORT PERIODS) ###\n\n")

  # Use same baseline
  baseline_long_short_vis <- baseline_data_short %>%
    mutate(
      timepoint = "Baseline",
      timepoint_num = 0,
      weight = baseline_weight,
      steps = baseline_steps,
      calories = baseline_calories
    ) %>%
    select(person_id, timepoint, timepoint_num, weight, steps, calories)

  # 1-30d
  period_1_30d <- period_data_list_short[["1-30d"]] %>%
    select(person_id, period_weight, period_steps, period_calories) %>%
    mutate(
      timepoint = "1-30d",
      timepoint_num = 1,
      weight = period_weight,
      steps = period_steps,
      calories = period_calories
    ) %>%
    select(person_id, timepoint, timepoint_num, weight, steps, calories)

  # 31-90d
  period_31_90d <- period_data_list_short[["31-90d"]] %>%
    select(person_id, period_weight, period_steps, period_calories) %>%
    mutate(
      timepoint = "31-90d",
      timepoint_num = 2,
      weight = period_weight,
      steps = period_steps,
      calories = period_calories
    ) %>%
    select(person_id, timepoint, timepoint_num, weight, steps, calories)

  # 91-180d
  period_91_180d_short <- period_data_list_short[["91-180d"]] %>%
    select(person_id, period_weight, period_steps, period_calories) %>%
    mutate(
      timepoint = "91-180d",
      timepoint_num = 3,
      weight = period_weight,
      steps = period_steps,
      calories = period_calories
    ) %>%
    select(person_id, timepoint, timepoint_num, weight, steps, calories)

  # 181-365d
  period_181_365d_short <- period_data_list_short[["181-365d"]] %>%
    select(person_id, period_weight, period_steps, period_calories) %>%
    mutate(
      timepoint = "181-365d",
      timepoint_num = 4,
      weight = period_weight,
      steps = period_steps,
      calories = period_calories
    ) %>%
    select(person_id, timepoint, timepoint_num, weight, steps, calories)

  # Nadir (use from short analysis)
  nadir_long_short <- nadir_data_short %>%
    select(person_id, nadir_weight, nadir_steps, nadir_calories) %>%
    mutate(
      timepoint = "Nadir",
      timepoint_num = 5,
      weight = nadir_weight,
      steps = nadir_steps,
      calories = nadir_calories
    ) %>%
    select(person_id, timepoint, timepoint_num, weight, steps, calories)

  # Combine all timepoints
  all_timepoints_short <- bind_rows(
    baseline_long_short_vis,
    period_1_30d,
    period_31_90d,
    period_91_180d_short,
    period_181_365d_short,
    nadir_long_short
  ) %>%
    inner_join(weight_loss_categories_short, by = "person_id") %>%
    mutate(
      weight_loss_cat = factor(weight_loss_cat,
                                levels = c("< 5% loss", "5-10% loss", "> 10% loss")),
      timepoint = factor(timepoint,
                         levels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d", "Nadir"))
    )

  cat(sprintf("Total observations across all timepoints (short): %d\n\n", nrow(all_timepoints_short)))

  # =============================================================================
  # FIGURE 1 SHORT: WEIGHT TRAJECTORIES BY WEIGHT LOSS CATEGORY
  # =============================================================================

  cat("### CREATING FIGURE 1 (SHORT): Weight Trajectories ###\n\n")

  weight_summary_short <- all_timepoints_short %>%
    group_by(weight_loss_cat, timepoint, timepoint_num) %>%
    summarize(
      n = n(),
      mean_weight = mean(weight, na.rm = TRUE),
      se_weight = sd(weight, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  fig1_short <- ggplot(weight_summary_short,
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
      title = "Weight Trajectories by Weight Loss Category (Short Periods)",
      subtitle = "Includes early response period (1-30d)",
      x = "Timepoint",
      y = "Weight (kg, mean ± SE)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave("sensitivity_figure1_weight_trajectories_by_category_short.png", fig1_short,
         width = 11, height = 7, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure1_weight_trajectories_by_category_short.png\n\n")

  # =============================================================================
  # FIGURE 2 SHORT: STEPS TRAJECTORIES BY WEIGHT LOSS CATEGORY
  # =============================================================================

  cat("### CREATING FIGURE 2 (SHORT): Steps Trajectories ###\n\n")

  steps_summary_short <- all_timepoints_short %>%
    group_by(weight_loss_cat, timepoint, timepoint_num) %>%
    summarize(
      n = n(),
      mean_steps = mean(steps, na.rm = TRUE),
      se_steps = sd(steps, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  fig2_short <- ggplot(steps_summary_short,
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
      title = "Steps Trajectories by Weight Loss Category (Short Periods)",
      subtitle = "Includes early response period (1-30d)",
      x = "Timepoint",
      y = "Steps (mean ± SE)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave("sensitivity_figure2_steps_trajectories_by_category_short.png", fig2_short,
         width = 11, height = 7, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure2_steps_trajectories_by_category_short.png\n\n")

  # =============================================================================
  # FIGURE 3 SHORT: CALORIES TRAJECTORIES BY WEIGHT LOSS CATEGORY
  # =============================================================================

  cat("### CREATING FIGURE 3 (SHORT): Calories Trajectories ###\n\n")

  calories_summary_short <- all_timepoints_short %>%
    group_by(weight_loss_cat, timepoint, timepoint_num) %>%
    summarize(
      n = n(),
      mean_calories = mean(calories, na.rm = TRUE),
      se_calories = sd(calories, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  fig3_short <- ggplot(calories_summary_short,
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
      title = "Calories Trajectories by Weight Loss Category (Short Periods)",
      subtitle = "Includes early response period (1-30d)",
      x = "Timepoint",
      y = "Activity Calories (mean ± SE)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave("sensitivity_figure3_calories_trajectories_by_category_short.png", fig3_short,
         width = 11, height = 7, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure3_calories_trajectories_by_category_short.png\n\n")

  # =============================================================================
  # FIGURE 4 SHORT: COMBINED TRAJECTORIES (3-PANEL)
  # =============================================================================

  cat("### CREATING FIGURE 4 (SHORT): Combined 3-Panel Figure ###\n\n")

  # Weight panel
  p1_short <- ggplot(weight_summary_short,
                     aes(x = timepoint, y = mean_weight, color = weight_loss_cat, group = weight_loss_cat)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight),
                  width = 0.2, linewidth = 0.7) +
    scale_color_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "A. Weight",
      x = NULL,
      y = "Weight (kg, mean ± SE)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # Steps panel
  p2_short <- ggplot(steps_summary_short,
                     aes(x = timepoint, y = mean_steps, color = weight_loss_cat, group = weight_loss_cat)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = mean_steps - se_steps, ymax = mean_steps + se_steps),
                  width = 0.2, linewidth = 0.7) +
    scale_color_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "B. Steps",
      x = NULL,
      y = "Steps (mean ± SE)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # Calories panel
  p3_short <- ggplot(calories_summary_short,
                     aes(x = timepoint, y = mean_calories, color = weight_loss_cat, group = weight_loss_cat)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = mean_calories - se_calories, ymax = mean_calories + se_calories),
                  width = 0.2, linewidth = 0.7) +
    scale_color_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60"),
      name = "Weight Loss Category\n(Baseline to Nadir)"
    ) +
    labs(
      title = "C. Activity Calories",
      x = "Timepoint",
      y = "Calories (mean ± SE)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  fig4_short <- (p1_short / p2_short / p3_short) +
    plot_annotation(
      title = "Combined Trajectories by Weight Loss Category (Short Periods)",
      subtitle = "Includes early response period (1-30d)",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  ggsave("sensitivity_figure4_combined_trajectories_short.png", fig4_short,
         width = 10, height = 12, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure4_combined_trajectories_short.png\n\n")

  # =============================================================================
  # FIGURE 5 SHORT: SPAGHETTI PLOT - WEIGHT
  # =============================================================================

  cat("### CREATING FIGURE 5 (SHORT): Spaghetti Plot - Weight ###\n\n")

  # Sample patients for readability
  set.seed(123)
  sampled_patients_short <- all_timepoints_short %>%
    group_by(weight_loss_cat) %>%
    distinct(person_id) %>%
    {
      group_split(.) %>%
        map_dfr(~ slice_sample(.x, n = min(30, nrow(.x))))
    }

  spaghetti_data_weight_short <- all_timepoints_short %>%
    semi_join(sampled_patients_short, by = c("person_id", "weight_loss_cat"))

  fig5_short <- ggplot(spaghetti_data_weight_short,
                       aes(x = timepoint, y = weight, group = person_id, color = weight_loss_cat)) +
    geom_line(alpha = 0.3, linewidth = 0.5) +
    geom_point(alpha = 0.4, size = 1.5) +
    facet_wrap(~weight_loss_cat, ncol = 1) +
    scale_color_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60")
    ) +
    labs(
      title = "Individual Patient Weight Trajectories (Short Periods)",
      subtitle = "Up to 30 patients per weight loss category",
      x = "Timepoint",
      y = "Weight (kg)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold", size = 12)
    )

  ggsave("sensitivity_figure5_spaghetti_weight_short.png", fig5_short,
         width = 10, height = 10, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure5_spaghetti_weight_short.png\n\n")

  # =============================================================================
  # FIGURE 6 SHORT: SPAGHETTI PLOT - STEPS
  # =============================================================================

  cat("### CREATING FIGURE 6 (SHORT): Spaghetti Plot - Steps ###\n\n")

  spaghetti_data_steps_short <- all_timepoints_short %>%
    semi_join(sampled_patients_short, by = c("person_id", "weight_loss_cat"))

  fig6_short <- ggplot(spaghetti_data_steps_short,
                       aes(x = timepoint, y = steps, group = person_id, color = weight_loss_cat)) +
    geom_line(alpha = 0.3, linewidth = 0.5) +
    geom_point(alpha = 0.4, size = 1.5) +
    facet_wrap(~weight_loss_cat, ncol = 1) +
    scale_color_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60")
    ) +
    labs(
      title = "Individual Patient Steps Trajectories (Short Periods)",
      subtitle = "Up to 30 patients per weight loss category",
      x = "Timepoint",
      y = "Steps"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold", size = 12)
    )

  ggsave("sensitivity_figure6_spaghetti_steps_short.png", fig6_short,
         width = 10, height = 10, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure6_spaghetti_steps_short.png\n\n")

  # =============================================================================
  # SUMMARY - SHORT PERIODS
  # =============================================================================

  cat("=============================================================================\n")
  cat("SHORT PERIODS SENSITIVITY VISUALIZATIONS COMPLETE\n")
  cat("=============================================================================\n\n")

  cat("Created short periods figures:\n")
  cat("  1. sensitivity_figure1_weight_trajectories_by_category_short.png\n")
  cat("     - Weight across: Baseline, 1-30d, 31-90d, 91-180d, 181-365d, Nadir\n\n")

  cat("  2. sensitivity_figure2_steps_trajectories_by_category_short.png\n")
  cat("     - Steps across all short period timepoints\n\n")

  cat("  3. sensitivity_figure3_calories_trajectories_by_category_short.png\n")
  cat("     - Calories across all short period timepoints\n\n")

  cat("  4. sensitivity_figure4_combined_trajectories_short.png\n")
  cat("     - 3-panel figure: Weight, Steps, Calories (short periods)\n\n")

  cat("  5. sensitivity_figure5_spaghetti_weight_short.png\n")
  cat("     - Individual patient weight trajectories (short periods)\n\n")

  cat("  6. sensitivity_figure6_spaghetti_steps_short.png\n")
  cat("     - Individual patient steps trajectories (short periods)\n\n")

  cat("=============================================================================\n\n")

} else {
  cat("\n\nShort periods results not found.\n")
  cat("Run period_analysis_optimized.R to generate short periods data.\n\n")
}

# =============================================================================
# =============================================================================
# SENSITIVITY COMPARISON PLOTS (MAIN ANALYSIS)
# =============================================================================
# =============================================================================

if (file.exists("sensitivity_analysis_results.RData")) {

  cat("\n\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n")
  cat("SENSITIVITY COMPARISON PLOTS (MAIN ANALYSIS)\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n\n")

  load("sensitivity_analysis_results.RData")
  cat("Loaded sensitivity_analysis_results.RData\n\n")

  # =============================================================================
  # FIGURE 7: STEPS & CALORIES CHANGE BY WEIGHT LOSS (2-CATEGORY)
  # =============================================================================

  cat("### CREATING FIGURE 7: Activity Changes by Weight Loss (2-Category) ###\n\n")

  # Combine all periods for 2-category analysis
  activity_by_weight_2cat <- bind_rows(
    weight_loss_results[["1-90d"]]$summary_2cat %>% mutate(period = "1-90d"),
    weight_loss_results[["91-180d"]]$summary_2cat %>% mutate(period = "91-180d"),
    weight_loss_results[["181-365d"]]$summary_2cat %>% mutate(period = "181-365d")
  ) %>%
    mutate(
      period = factor(period, levels = c("1-90d", "91-180d", "181-365d")),
      weight_loss_cat2 = factor(weight_loss_cat2, levels = c("< 7.5% loss", "≥ 7.5% loss"))
    )

  # Steps change plot
  p_steps_2cat <- ggplot(activity_by_weight_2cat,
                         aes(x = period, y = mean_steps_change, fill = weight_loss_cat2)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_steps_change - sd_steps_change,
                      ymax = mean_steps_change + sd_steps_change),
                  position = position_dodge(width = 0.8), width = 0.3) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 7.5% loss" = "#E74C3C", "≥ 7.5% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "A. Steps Change by Weight Loss Category",
      subtitle = "< 7.5% vs ≥ 7.5% weight loss",
      x = "Period",
      y = "Steps Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  # Calories change plot
  p_calories_2cat <- ggplot(activity_by_weight_2cat,
                            aes(x = period, y = mean_calories_change, fill = weight_loss_cat2)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_calories_change - sd_calories_change,
                      ymax = mean_calories_change + sd_calories_change),
                  position = position_dodge(width = 0.8), width = 0.3) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 7.5% loss" = "#E74C3C", "≥ 7.5% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "B. Calories Change by Weight Loss Category",
      subtitle = "< 7.5% vs ≥ 7.5% weight loss",
      x = "Period",
      y = "Calories Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  fig7 <- p_steps_2cat / p_calories_2cat +
    plot_annotation(
      title = "Activity Changes by Weight Loss Category (2-Category)",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  ggsave("sensitivity_figure7_activity_by_weightloss_2cat.png", fig7,
         width = 10, height = 10, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure7_activity_by_weightloss_2cat.png\n\n")

  # =============================================================================
  # FIGURE 8: STEPS & CALORIES CHANGE BY WEIGHT LOSS (3-CATEGORY)
  # =============================================================================

  cat("### CREATING FIGURE 8: Activity Changes by Weight Loss (3-Category) ###\n\n")

  # Combine all periods for 3-category analysis
  activity_by_weight_3cat <- bind_rows(
    weight_loss_results[["1-90d"]]$summary_3cat %>% mutate(period = "1-90d"),
    weight_loss_results[["91-180d"]]$summary_3cat %>% mutate(period = "91-180d"),
    weight_loss_results[["181-365d"]]$summary_3cat %>% mutate(period = "181-365d")
  ) %>%
    mutate(
      period = factor(period, levels = c("1-90d", "91-180d", "181-365d")),
      weight_loss_cat3 = factor(weight_loss_cat3,
                                levels = c("< 5% loss", "5-10% loss", "> 10% loss"))
    )

  # Steps change plot
  p_steps_3cat <- ggplot(activity_by_weight_3cat,
                         aes(x = period, y = mean_steps_change, fill = weight_loss_cat3)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_steps_change - sd_steps_change,
                      ymax = mean_steps_change + sd_steps_change),
                  position = position_dodge(width = 0.8), width = 0.25) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 2.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "A. Steps Change by Weight Loss Category",
      subtitle = "< 5%, 5-10%, > 10% weight loss",
      x = "Period",
      y = "Steps Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  # Calories change plot
  p_calories_3cat <- ggplot(activity_by_weight_3cat,
                            aes(x = period, y = mean_calories_change, fill = weight_loss_cat3)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_calories_change - sd_calories_change,
                      ymax = mean_calories_change + sd_calories_change),
                  position = position_dodge(width = 0.8), width = 0.25) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 2.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "B. Calories Change by Weight Loss Category",
      subtitle = "< 5%, 5-10%, > 10% weight loss",
      x = "Period",
      y = "Calories Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  fig8 <- p_steps_3cat / p_calories_3cat +
    plot_annotation(
      title = "Activity Changes by Weight Loss Category (3-Category)",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  ggsave("sensitivity_figure8_activity_by_weightloss_3cat.png", fig8,
         width = 10, height = 10, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure8_activity_by_weightloss_3cat.png\n\n")

  # =============================================================================
  # FIGURE 9: WEIGHT CHANGE BY STEP CHANGE CATEGORY
  # =============================================================================

  cat("### CREATING FIGURE 9: Weight Change by Step Change Category ###\n\n")

  # Combine all periods for step change analysis
  weight_by_steps <- bind_rows(
    step_change_results[["1-90d"]]$summary %>% mutate(period = "1-90d"),
    step_change_results[["91-180d"]]$summary %>% mutate(period = "91-180d"),
    step_change_results[["181-365d"]]$summary %>% mutate(period = "181-365d")
  ) %>%
    mutate(
      period = factor(period, levels = c("1-90d", "91-180d", "181-365d")),
      step_change_cat = factor(step_change_cat,
                               levels = c("Decreased > 5%", "No change", "Increased > 5%"))
    )

  fig9 <- ggplot(weight_by_steps,
                 aes(x = period, y = mean_weight_change, fill = step_change_cat)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_weight_change - sd_weight_change,
                      ymax = mean_weight_change + sd_weight_change),
                  position = position_dodge(width = 0.8), width = 0.25) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("Decreased > 5%" = "#E74C3C",
                 "No change" = "#95A5A6",
                 "Increased > 5%" = "#27AE60"),
      name = "Step Change Category"
    ) +
    labs(
      title = "Weight Change by Step Change Category",
      subtitle = "Comparison across periods",
      x = "Period",
      y = "Weight Change from Baseline (kg, mean ± SD)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    )

  ggsave("sensitivity_figure9_weight_by_stepchange.png", fig9,
         width = 10, height = 7, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure9_weight_by_stepchange.png\n\n")

  cat("=============================================================================\n")
  cat("MAIN SENSITIVITY COMPARISON PLOTS COMPLETE\n")
  cat("=============================================================================\n\n")

} else {
  cat("\n\nMain sensitivity results not found.\n")
  cat("Run sensitivity_analysis.R first.\n\n")
}

# =============================================================================
# =============================================================================
# SENSITIVITY COMPARISON PLOTS (SHORT PERIODS)
# =============================================================================
# =============================================================================

if (file.exists("sensitivity_analysis_short_results.RData")) {

  cat("\n\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n")
  cat("SENSITIVITY COMPARISON PLOTS (SHORT PERIODS)\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n\n")

  load("sensitivity_analysis_short_results.RData")
  cat("Loaded sensitivity_analysis_short_results.RData\n\n")

  # =============================================================================
  # FIGURE 10: STEPS & CALORIES CHANGE BY WEIGHT LOSS (2-CATEGORY, SHORT)
  # =============================================================================

  cat("### CREATING FIGURE 10: Activity Changes by Weight Loss (2-Cat, Short) ###\n\n")

  # Combine all periods for 2-category analysis
  activity_by_weight_2cat_short <- bind_rows(
    weight_loss_results_short[["1-30d"]]$summary_2cat %>% mutate(period = "1-30d"),
    weight_loss_results_short[["31-90d"]]$summary_2cat %>% mutate(period = "31-90d"),
    weight_loss_results_short[["91-180d"]]$summary_2cat %>% mutate(period = "91-180d"),
    weight_loss_results_short[["181-365d"]]$summary_2cat %>% mutate(period = "181-365d")
  ) %>%
    mutate(
      period = factor(period, levels = c("1-30d", "31-90d", "91-180d", "181-365d")),
      weight_loss_cat2 = factor(weight_loss_cat2, levels = c("< 7.5% loss", "≥ 7.5% loss"))
    )

  # Steps change plot
  p_steps_2cat_short <- ggplot(activity_by_weight_2cat_short,
                                aes(x = period, y = mean_steps_change, fill = weight_loss_cat2)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_steps_change - sd_steps_change,
                      ymax = mean_steps_change + sd_steps_change),
                  position = position_dodge(width = 0.8), width = 0.3) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 2.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 7.5% loss" = "#E74C3C", "≥ 7.5% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "A. Steps Change by Weight Loss Category",
      subtitle = "< 7.5% vs ≥ 7.5% weight loss (Short Periods)",
      x = "Period",
      y = "Steps Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0)
    )

  # Calories change plot
  p_calories_2cat_short <- ggplot(activity_by_weight_2cat_short,
                                   aes(x = period, y = mean_calories_change, fill = weight_loss_cat2)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_calories_change - sd_calories_change,
                      ymax = mean_calories_change + sd_calories_change),
                  position = position_dodge(width = 0.8), width = 0.3) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 2.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 7.5% loss" = "#E74C3C", "≥ 7.5% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "B. Calories Change by Weight Loss Category",
      subtitle = "< 7.5% vs ≥ 7.5% weight loss (Short Periods)",
      x = "Period",
      y = "Calories Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0)
    )

  fig10 <- p_steps_2cat_short / p_calories_2cat_short +
    plot_annotation(
      title = "Activity Changes by Weight Loss Category (2-Category, Short Periods)",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  ggsave("sensitivity_figure10_activity_by_weightloss_2cat_short.png", fig10,
         width = 11, height = 10, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure10_activity_by_weightloss_2cat_short.png\n\n")

  # =============================================================================
  # FIGURE 11: STEPS & CALORIES CHANGE BY WEIGHT LOSS (3-CATEGORY, SHORT)
  # =============================================================================

  cat("### CREATING FIGURE 11: Activity Changes by Weight Loss (3-Cat, Short) ###\n\n")

  # Combine all periods for 3-category analysis
  activity_by_weight_3cat_short <- bind_rows(
    weight_loss_results_short[["1-30d"]]$summary_3cat %>% mutate(period = "1-30d"),
    weight_loss_results_short[["31-90d"]]$summary_3cat %>% mutate(period = "31-90d"),
    weight_loss_results_short[["91-180d"]]$summary_3cat %>% mutate(period = "91-180d"),
    weight_loss_results_short[["181-365d"]]$summary_3cat %>% mutate(period = "181-365d")
  ) %>%
    mutate(
      period = factor(period, levels = c("1-30d", "31-90d", "91-180d", "181-365d")),
      weight_loss_cat3 = factor(weight_loss_cat3,
                                levels = c("< 5% loss", "5-10% loss", "> 10% loss"))
    )

  # Steps change plot
  p_steps_3cat_short <- ggplot(activity_by_weight_3cat_short,
                                aes(x = period, y = mean_steps_change, fill = weight_loss_cat3)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_steps_change - sd_steps_change,
                      ymax = mean_steps_change + sd_steps_change),
                  position = position_dodge(width = 0.8), width = 0.2) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "A. Steps Change by Weight Loss Category",
      subtitle = "< 5%, 5-10%, > 10% weight loss (Short Periods)",
      x = "Period",
      y = "Steps Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0)
    )

  # Calories change plot
  p_calories_3cat_short <- ggplot(activity_by_weight_3cat_short,
                                   aes(x = period, y = mean_calories_change, fill = weight_loss_cat3)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_calories_change - sd_calories_change,
                      ymax = mean_calories_change + sd_calories_change),
                  position = position_dodge(width = 0.8), width = 0.2) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("< 5% loss" = "#E74C3C",
                 "5-10% loss" = "#F39C12",
                 "> 10% loss" = "#27AE60"),
      name = "Weight Loss Category"
    ) +
    labs(
      title = "B. Calories Change by Weight Loss Category",
      subtitle = "< 5%, 5-10%, > 10% weight loss (Short Periods)",
      x = "Period",
      y = "Calories Change from Baseline (mean ± SD)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0)
    )

  fig11 <- p_steps_3cat_short / p_calories_3cat_short +
    plot_annotation(
      title = "Activity Changes by Weight Loss Category (3-Category, Short Periods)",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  ggsave("sensitivity_figure11_activity_by_weightloss_3cat_short.png", fig11,
         width = 11, height = 10, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure11_activity_by_weightloss_3cat_short.png\n\n")

  # =============================================================================
  # FIGURE 12: WEIGHT CHANGE BY STEP CHANGE CATEGORY (SHORT)
  # =============================================================================

  cat("### CREATING FIGURE 12: Weight Change by Step Change Category (Short) ###\n\n")

  # Combine all periods for step change analysis
  weight_by_steps_short <- bind_rows(
    step_change_results_short[["1-30d"]]$summary %>% mutate(period = "1-30d"),
    step_change_results_short[["31-90d"]]$summary %>% mutate(period = "31-90d"),
    step_change_results_short[["91-180d"]]$summary %>% mutate(period = "91-180d"),
    step_change_results_short[["181-365d"]]$summary %>% mutate(period = "181-365d")
  ) %>%
    mutate(
      period = factor(period, levels = c("1-30d", "31-90d", "91-180d", "181-365d")),
      step_change_cat = factor(step_change_cat,
                               levels = c("Decreased > 5%", "No change", "Increased > 5%"))
    )

  fig12 <- ggplot(weight_by_steps_short,
                  aes(x = period, y = mean_weight_change, fill = step_change_cat)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_weight_change - sd_weight_change,
                      ymax = mean_weight_change + sd_weight_change),
                  position = position_dodge(width = 0.8), width = 0.25) +
    geom_text(aes(label = sprintf("n=%d", n)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 2.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("Decreased > 5%" = "#E74C3C",
                 "No change" = "#95A5A6",
                 "Increased > 5%" = "#27AE60"),
      name = "Step Change Category"
    ) +
    labs(
      title = "Weight Change by Step Change Category (Short Periods)",
      subtitle = "Comparison across short periods including early response (1-30d)",
      x = "Period",
      y = "Weight Change from Baseline (kg, mean ± SD)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    )

  ggsave("sensitivity_figure12_weight_by_stepchange_short.png", fig12,
         width = 11, height = 7, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure12_weight_by_stepchange_short.png\n\n")

  cat("=============================================================================\n")
  cat("SHORT PERIODS SENSITIVITY COMPARISON PLOTS COMPLETE\n")
  cat("=============================================================================\n\n")

} else {
  cat("\n\nShort periods sensitivity results not found.\n")
  cat("Run sensitivity_analysis.R first.\n\n")
}

cat("\n=============================================================================\n")
cat("ALL SENSITIVITY VISUALIZATIONS COMPLETE\n")
cat("=============================================================================\n")
