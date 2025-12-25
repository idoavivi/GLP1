# =============================================================================
# Sensitivity Analysis Visualizations
# =============================================================================
# Creates publication-quality figures for weight loss and step change analyses
# =============================================================================

library(tidyverse)
library(patchwork)

cat("=============================================================================\n")
cat("SENSITIVITY ANALYSIS VISUALIZATIONS\n")
cat("=============================================================================\n\n")

# Load sensitivity analysis results
if (!file.exists("sensitivity_analysis_results.RData")) {
  stop("ERROR: sensitivity_analysis_results.RData not found. Run sensitivity_analysis.R first.")
}

load("sensitivity_analysis_results.RData")

cat("Loaded sensitivity_analysis_results.RData\n\n")

# =============================================================================
# FIGURE 1: ACTIVITY BY WEIGHT LOSS CATEGORY (HIGHEST DIFFERENCE PERIOD)
# =============================================================================

cat("### CREATING FIGURE 1: Activity by Weight Loss Category ###\n\n")

# Find period with highest difference in steps between weight loss groups
max_diff_period <- NULL
max_diff_value <- 0

for (pname in names(weight_loss_results)) {
  summary_3cat <- weight_loss_results[[pname]]$summary_3cat
  if (nrow(summary_3cat) >= 2) {
    diff <- max(summary_3cat$mean_steps_change) - min(summary_3cat$mean_steps_change)
    if (diff > max_diff_value) {
      max_diff_value <- diff
      max_diff_period <- pname
    }
  }
}

cat(sprintf("Period with highest step difference: %s (difference: %.0f steps)\n\n",
            max_diff_period, max_diff_value))

# Get data for the selected period
merged_data <- weight_loss_results[[max_diff_period]]$merged_data

# Prepare data for plotting
plot_data_weight <- merged_data %>%
  mutate(
    weight_loss_cat = factor(weight_loss_cat3,
                              levels = c("< 5% loss", "5-10% loss", "> 10% loss"))
  ) %>%
  select(person_id, weight_loss_cat,
         baseline_steps, period_steps = period_steps,
         baseline_calories, period_calories = period_calories) %>%
  pivot_longer(
    cols = c(baseline_steps, period_steps, baseline_calories, period_calories),
    names_to = "metric_time",
    values_to = "value"
  ) %>%
  separate(metric_time, into = c("time", "metric"), sep = "_", extra = "merge") %>%
  mutate(
    time = factor(time, levels = c("baseline", "period")),
    metric = factor(metric, levels = c("steps", "calories"))
  )

# Calculate summary statistics
summary_data_weight <- plot_data_weight %>%
  group_by(weight_loss_cat, time, metric) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    se_value = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot 1A: Steps by weight loss category
p1a <- ggplot(summary_data_weight %>% filter(metric == "steps"),
              aes(x = time, y = mean_value, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.1, linewidth = 0.8) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60")
  ) +
  labs(
    title = "A. Daily Steps by Weight Loss Category",
    subtitle = sprintf("Period: %s", max_diff_period),
    x = "",
    y = "Daily Steps (mean ± SE)",
    color = "Weight Loss"
  ) +
  scale_x_discrete(labels = c("Baseline", "Follow-up")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Plot 1B: Calories by weight loss category
p1b <- ggplot(summary_data_weight %>% filter(metric == "calories"),
              aes(x = time, y = mean_value, color = weight_loss_cat, group = weight_loss_cat)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.1, linewidth = 0.8) +
  scale_color_manual(
    values = c("< 5% loss" = "#E74C3C",
               "5-10% loss" = "#F39C12",
               "> 10% loss" = "#27AE60")
  ) +
  labs(
    title = "B. Activity Calories by Weight Loss Category",
    subtitle = sprintf("Period: %s", max_diff_period),
    x = "",
    y = "Activity Calories (mean ± SE)",
    color = "Weight Loss"
  ) +
  scale_x_discrete(labels = c("Baseline", "Follow-up")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Combine plots
fig1 <- p1a + p1b +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("sensitivity_figure1_activity_by_weight_loss.png", fig1,
       width = 12, height = 6, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure1_activity_by_weight_loss.png\n\n")

# =============================================================================
# FIGURE 2: WEIGHT BY STEP CHANGE CATEGORY (HIGHEST DIFFERENCE PERIOD)
# =============================================================================

cat("### CREATING FIGURE 2: Weight by Step Change Category ###\n\n")

# Find period with highest difference in weight change between step change groups
max_diff_period_steps <- NULL
max_diff_value_steps <- 0

for (pname in names(step_change_results)) {
  summary_step <- step_change_results[[pname]]$summary
  if (nrow(summary_step) >= 2) {
    diff <- max(summary_step$mean_weight_change) - min(summary_step$mean_weight_change)
    if (abs(diff) > max_diff_value_steps) {
      max_diff_value_steps <- abs(diff)
      max_diff_period_steps <- pname
    }
  }
}

cat(sprintf("Period with highest weight difference: %s (difference: %.1f kg)\n\n",
            max_diff_period_steps, max_diff_value_steps))

# Get data for the selected period
merged_data_steps <- step_change_results[[max_diff_period_steps]]$merged_data

# Prepare data for plotting
plot_data_steps <- merged_data_steps %>%
  mutate(
    step_change_cat = factor(step_change_cat,
                              levels = c("Decrease > 5%", "No change (-5% to +5%)", "Increase > 5%"))
  ) %>%
  select(person_id, step_change_cat, baseline_weight, period_weight) %>%
  pivot_longer(
    cols = c(baseline_weight, period_weight),
    names_to = "time",
    values_to = "weight"
  ) %>%
  mutate(
    time = factor(time, levels = c("baseline_weight", "period_weight"))
  )

# Calculate summary statistics
summary_data_steps <- plot_data_steps %>%
  group_by(step_change_cat, time) %>%
  summarize(
    mean_weight = mean(weight, na.rm = TRUE),
    se_weight = sd(weight, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot 2: Weight by step change category
fig2 <- ggplot(summary_data_steps,
               aes(x = time, y = mean_weight, color = step_change_cat, group = step_change_cat)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight),
                width = 0.1, linewidth = 0.8) +
  scale_color_manual(
    values = c("Decrease > 5%" = "#E74C3C",
               "No change (-5% to +5%)" = "#95A5A6",
               "Increase > 5%" = "#3498DB")
  ) +
  labs(
    title = "Weight Change by Step Change Category",
    subtitle = sprintf("Period: %s", max_diff_period_steps),
    x = "",
    y = "Weight (kg, mean ± SE)",
    color = "Step Change"
  ) +
  scale_x_discrete(labels = c("Baseline", "Follow-up")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("sensitivity_figure2_weight_by_step_change.png", fig2,
       width = 8, height = 6, dpi = 300, bg = "white")

cat("  ✓ sensitivity_figure2_weight_by_step_change.png\n\n")

# =============================================================================
# FIGURE 3: WEIGHT BY STEP CHANGE CATEGORY - NADIR VERSION
# =============================================================================

cat("### CREATING FIGURE 3: Weight by Step Change Category (Baseline to Nadir) ###\n\n")

# Load period analysis results for nadir data
if (!file.exists("period_analysis_results.RData")) {
  cat("WARNING: period_analysis_results.RData not found. Skipping nadir analysis.\n\n")
} else {
  load("period_analysis_results.RData")

  # Get baseline data from all_data_long
  baseline_data_all <- all_data_long %>%
    filter(period == "Baseline") %>%
    select(person_id, baseline_weight = weight, baseline_steps = steps)

  # Get all period data and find nadir for each patient
  nadir_data <- all_data_long %>%
    filter(period != "Baseline") %>%
    select(person_id, period, weight, steps) %>%
    filter(!is.na(weight)) %>%
    group_by(person_id) %>%
    slice_min(weight, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(nadir_weight = weight, nadir_steps = steps) %>%
    select(person_id, nadir_weight)

  # Merge with baseline and calculate step change to nadir
  nadir_analysis <- baseline_data_all %>%
    inner_join(nadir_data, by = "person_id") %>%
    filter(!is.na(baseline_weight), !is.na(nadir_weight),
           !is.na(baseline_steps)) %>%
    mutate(
      weight_change = nadir_weight - baseline_weight,
      steps_pct_change = 100 * (baseline_steps - baseline_steps) / baseline_steps  # Will calculate actual from period data
    )

  # Get average steps for each patient across all periods
  avg_period_steps <- all_data_long %>%
    filter(period != "Baseline") %>%
    group_by(person_id) %>%
    summarize(avg_period_steps = mean(steps, na.rm = TRUE), .groups = "drop")

  nadir_analysis <- nadir_analysis %>%
    left_join(avg_period_steps, by = "person_id") %>%
    mutate(
      steps_pct_change = 100 * (avg_period_steps - baseline_steps) / baseline_steps,
      step_change_cat = case_when(
        steps_pct_change < -5 ~ "Decrease > 5%",
        steps_pct_change >= -5 & steps_pct_change <= 5 ~ "No change (-5% to +5%)",
        steps_pct_change > 5 ~ "Increase > 5%"
      )
    ) %>%
    filter(!is.na(step_change_cat))

  # Prepare data for plotting
  plot_data_nadir <- nadir_analysis %>%
    mutate(
      step_change_cat = factor(step_change_cat,
                                levels = c("Decrease > 5%", "No change (-5% to +5%)", "Increase > 5%"))
    ) %>%
    select(person_id, step_change_cat, baseline_weight, nadir_weight) %>%
    pivot_longer(
      cols = c(baseline_weight, nadir_weight),
      names_to = "time",
      values_to = "weight"
    ) %>%
    mutate(
      time = factor(time, levels = c("baseline_weight", "nadir_weight"))
    )

  # Calculate summary statistics
  summary_data_nadir <- plot_data_nadir %>%
    group_by(step_change_cat, time) %>%
    summarize(
      mean_weight = mean(weight, na.rm = TRUE),
      se_weight = sd(weight, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )

  # Plot 3: Weight by step change category (nadir)
  fig3 <- ggplot(summary_data_nadir,
                 aes(x = time, y = mean_weight, color = step_change_cat, group = step_change_cat)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight),
                  width = 0.1, linewidth = 0.8) +
    geom_text(aes(label = sprintf("n=%d", n)),
              vjust = -1.5, size = 3, show.legend = FALSE) +
    scale_color_manual(
      values = c("Decrease > 5%" = "#E74C3C",
                 "No change (-5% to +5%)" = "#95A5A6",
                 "Increase > 5%" = "#3498DB")
    ) +
    labs(
      title = "Weight Change by Step Change Category",
      subtitle = "Baseline to Nadir Weight",
      x = "",
      y = "Weight (kg, mean ± SE)",
      color = "Step Change"
    ) +
    scale_x_discrete(labels = c("Baseline", "Nadir")) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    )

  ggsave("sensitivity_figure3_weight_by_step_change_nadir.png", fig3,
         width = 8, height = 6, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure3_weight_by_step_change_nadir.png\n\n")
}

# =============================================================================
# FIGURE 4: SPAGHETTI PLOTS - WEIGHT TRAJECTORIES BY CATEGORY
# =============================================================================

cat("### CREATING FIGURE 4: Spaghetti Plots - Weight Trajectories ###\n\n")

if (exists("all_data_long")) {
  # Get weight loss categories for the best period
  best_period_data <- weight_loss_results[[max_diff_period]]$merged_data %>%
    select(person_id, weight_loss_cat = weight_loss_cat3)

  # Get all period data with weight
  weight_trajectory_data <- all_data_long %>%
    select(person_id, period, period_num, weight) %>%
    filter(!is.na(weight)) %>%
    inner_join(best_period_data, by = "person_id") %>%
    mutate(
      weight_loss_cat = factor(weight_loss_cat,
                                levels = c("< 5% loss", "5-10% loss", "> 10% loss"))
    )

  # Calculate mean trajectories
  mean_trajectories <- weight_trajectory_data %>%
    group_by(weight_loss_cat, period, period_num) %>%
    summarize(
      mean_weight = mean(weight, na.rm = TRUE),
      se_weight = sd(weight, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Sample patients for clearer visualization (max 30 per category)
  set.seed(123)
  sampled_patients <- weight_trajectory_data %>%
    group_by(weight_loss_cat) %>%
    distinct(person_id) %>%
    slice_sample(n = min(30, n())) %>%
    ungroup()

  weight_trajectory_sample <- weight_trajectory_data %>%
    inner_join(sampled_patients, by = c("person_id", "weight_loss_cat"))

  # Create spaghetti plot
  fig4 <- ggplot() +
    # Individual trajectories
    geom_line(data = weight_trajectory_sample,
              aes(x = period_num, y = weight, group = person_id),
              alpha = 0.2, linewidth = 0.3) +
    # Mean trajectory
    geom_line(data = mean_trajectories,
              aes(x = period_num, y = mean_weight),
              color = "black", linewidth = 1.5) +
    geom_ribbon(data = mean_trajectories,
                aes(x = period_num,
                    ymin = mean_weight - se_weight,
                    ymax = mean_weight + se_weight),
                alpha = 0.2, fill = "black") +
    facet_wrap(~weight_loss_cat, ncol = 3) +
    labs(
      title = "Individual Weight Trajectories by Weight Loss Category",
      subtitle = sprintf("Categorization based on %s; showing up to 30 patients per group", max_diff_period),
      x = "Period (0 = Baseline)",
      y = "Weight (kg)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank()
    )

  ggsave("sensitivity_figure4_spaghetti_weight_by_category.png", fig4,
         width = 14, height = 5, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure4_spaghetti_weight_by_category.png\n\n")
}

# =============================================================================
# FIGURE 5: SPAGHETTI PLOTS - STEPS TRAJECTORIES BY CATEGORY
# =============================================================================

cat("### CREATING FIGURE 5: Spaghetti Plots - Steps Trajectories ###\n\n")

if (exists("all_data_long")) {
  # Get steps trajectories
  steps_trajectory_data <- all_data_long %>%
    select(person_id, period, period_num, steps) %>%
    filter(!is.na(steps)) %>%
    inner_join(best_period_data, by = "person_id") %>%
    mutate(
      weight_loss_cat = factor(weight_loss_cat,
                                levels = c("< 5% loss", "5-10% loss", "> 10% loss"))
    )

  # Calculate mean trajectories
  mean_steps_trajectories <- steps_trajectory_data %>%
    group_by(weight_loss_cat, period, period_num) %>%
    summarize(
      mean_steps = mean(steps, na.rm = TRUE),
      se_steps = sd(steps, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Sample patients
  steps_trajectory_sample <- steps_trajectory_data %>%
    inner_join(sampled_patients, by = c("person_id", "weight_loss_cat"))

  # Create spaghetti plot
  fig5 <- ggplot() +
    # Individual trajectories
    geom_line(data = steps_trajectory_sample,
              aes(x = period_num, y = steps, group = person_id),
              alpha = 0.2, linewidth = 0.3) +
    # Mean trajectory
    geom_line(data = mean_steps_trajectories,
              aes(x = period_num, y = mean_steps),
              color = "blue", linewidth = 1.5) +
    geom_ribbon(data = mean_steps_trajectories,
                aes(x = period_num,
                    ymin = mean_steps - se_steps,
                    ymax = mean_steps + se_steps),
                alpha = 0.2, fill = "blue") +
    facet_wrap(~weight_loss_cat, ncol = 3) +
    labs(
      title = "Individual Steps Trajectories by Weight Loss Category",
      subtitle = sprintf("Categorization based on %s; showing up to 30 patients per group", max_diff_period),
      x = "Period (0 = Baseline)",
      y = "Daily Steps"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank()
    )

  ggsave("sensitivity_figure5_spaghetti_steps_by_category.png", fig5,
         width = 14, height = 5, dpi = 300, bg = "white")

  cat("  ✓ sensitivity_figure5_spaghetti_steps_by_category.png\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("SENSITIVITY VISUALIZATIONS COMPLETE\n")
cat("=============================================================================\n\n")

cat("Created figures:\n")
cat("  1. sensitivity_figure1_activity_by_weight_loss.png\n")
cat("     - Steps and calories at baseline vs follow-up by weight loss category\n")
cat(sprintf("     - Period: %s (highest difference between groups)\n", max_diff_period))
cat("  2. sensitivity_figure2_weight_by_step_change.png\n")
cat("     - Weight at baseline vs follow-up by step change category\n")
cat(sprintf("     - Period: %s (highest difference between groups)\n", max_diff_period_steps))
if (exists("fig3")) {
  cat("  3. sensitivity_figure3_weight_by_step_change_nadir.png\n")
  cat("     - Weight at baseline vs nadir by step change category\n")
}
if (exists("fig4")) {
  cat("  4. sensitivity_figure4_spaghetti_weight_by_category.png\n")
  cat("     - Individual weight trajectories by weight loss category\n")
}
if (exists("fig5")) {
  cat("  5. sensitivity_figure5_spaghetti_steps_by_category.png\n")
  cat("     - Individual steps trajectories by weight loss category\n")
}
cat("\n=============================================================================\n")
