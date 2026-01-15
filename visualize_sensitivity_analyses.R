# =============================================================================
# VISUALIZE SENSITIVITY ANALYSES
# =============================================================================
# Creates plots for all four sensitivity analyses:
# 1. Steps by BMI class
# 2. Steps by baseline tertiles
# 3. Steps by sex
# 4. Steps by weight response (nadir cohort)
# =============================================================================

library(tidyverse)
library(patchwork)

cat("\n##################################################\n")
cat("VISUALIZING SENSITIVITY ANALYSES\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD SUMMARY DATA
# =============================================================================

cat("Loading sensitivity analysis results...\n")

# Check if files exist
files_needed <- c(
  "sensitivity_bmi_class_summary.csv",
  "sensitivity_baseline_tertile_summary.csv",
  "sensitivity_weight_response_summary.csv"
)

missing_files <- files_needed[!file.exists(files_needed)]

if (length(missing_files) > 0) {
  stop(paste(
    "\n❌ ERROR: Missing required files. Please run sensitivity_analyses_steps.R first.\n",
    "Missing files:\n",
    paste("  -", missing_files, collapse = "\n"),
    "\n"
  ))
}

# Load data
bmi_summary <- read_csv("sensitivity_bmi_class_summary.csv", show_col_types = FALSE)
tertile_summary <- read_csv("sensitivity_baseline_tertile_summary.csv", show_col_types = FALSE)
weight_response_summary <- read_csv("sensitivity_weight_response_summary.csv", show_col_types = FALSE)

# Sex summary (optional)
if (file.exists("sensitivity_sex_summary.csv")) {
  sex_summary <- read_csv("sensitivity_sex_summary.csv", show_col_types = FALSE)
  has_sex_data <- TRUE
  cat("✓ All 4 analyses loaded (including sex)\n\n")
} else {
  has_sex_data <- FALSE
  cat("✓ 3 analyses loaded (sex data not available)\n\n")
}

# =============================================================================
# HELPER FUNCTION: CREATE TRAJECTORY PLOT
# =============================================================================

create_trajectory_plot <- function(data, group_var, title, y_label = "Steps per day\n(median, IQR)", colors = NULL) {

  # Prepare data
  plot_data <- data %>%
    mutate(
      period = factor(period, levels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")),
      time_point = as.numeric(period)
    )

  # Default colors if not provided
  if (is.null(colors)) {
    n_groups <- n_distinct(plot_data[[group_var]])
    colors <- scales::hue_pal()(n_groups)
  }

  # Create plot
  p <- ggplot(plot_data, aes(x = time_point, y = median_steps,
                               color = .data[[group_var]],
                               fill = .data[[group_var]],
                               group = .data[[group_var]])) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_x_continuous(
      breaks = 1:5,
      labels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d")
    ) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(
      title = title,
      x = "Time Period",
      y = y_label,
      color = "",
      fill = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

# =============================================================================
# PLOT 1: STEPS BY BMI CLASS
# =============================================================================

cat("Creating BMI class plot...\n")

# Order BMI classes
bmi_summary <- bmi_summary %>%
  mutate(bmi_class = factor(bmi_class,
                             levels = c("Class I (30-34.9)", "Class II (35-39.9)", "Class III (≥40)")))

p_bmi <- create_trajectory_plot(
  data = bmi_summary,
  group_var = "bmi_class",
  title = "A. Steps Trajectory by Baseline BMI Class",
  colors = c("#2E7D32", "#F57C00", "#C62828")  # Green, Orange, Red
)

# Add sample size annotations
bmi_n <- bmi_summary %>%
  filter(period == "Baseline") %>%
  mutate(label = sprintf("%s: N=%d", bmi_class, n))

p_bmi <- p_bmi +
  labs(caption = paste(bmi_n$label, collapse = " | "))

# =============================================================================
# PLOT 2: STEPS BY BASELINE TERTILES
# =============================================================================

cat("Creating baseline tertiles plot...\n")

# Order tertiles
tertile_summary <- tertile_summary %>%
  mutate(steps_tertile = factor(steps_tertile,
                                  levels = c("Low (T1)", "Medium (T2)", "High (T3)")))

p_tertile <- create_trajectory_plot(
  data = tertile_summary,
  group_var = "steps_tertile",
  title = "B. Steps Trajectory by Baseline Activity Level",
  colors = c("#D32F2F", "#FFA000", "#388E3C")  # Red, Amber, Green
)

# Add sample size annotations
tertile_n <- tertile_summary %>%
  filter(period == "Baseline") %>%
  mutate(label = sprintf("%s: N=%d", steps_tertile, n))

p_tertile <- p_tertile +
  labs(caption = paste(tertile_n$label, collapse = " | "))

# =============================================================================
# PLOT 3: STEPS BY SEX (if available)
# =============================================================================

if (has_sex_data) {
  cat("Creating sex-stratified plot...\n")

  p_sex <- create_trajectory_plot(
    data = sex_summary,
    group_var = "sex",
    title = "C. Steps Trajectory by Sex",
    colors = c("#1976D2", "#C2185B")  # Blue (Male), Pink (Female)
  )

  # Add sample size annotations
  sex_n <- sex_summary %>%
    filter(period == "Baseline") %>%
    mutate(label = sprintf("%s: N=%d", sex, n))

  p_sex <- p_sex +
    labs(caption = paste(sex_n$label, collapse = " | "))
}

# =============================================================================
# PLOT 4: STEPS BY WEIGHT RESPONSE (nadir cohort)
# =============================================================================

cat("Creating weight response plot...\n")

# Order weight response categories
weight_response_summary <- weight_response_summary %>%
  mutate(weight_response = factor(weight_response,
                                   levels = c("<5% loss", "5-10% loss", ">10% loss")))

p_weight <- create_trajectory_plot(
  data = weight_response_summary,
  group_var = "weight_response",
  title = ifelse(has_sex_data, "D. Steps Trajectory by Weight Response (Nadir Cohort)",
                 "C. Steps Trajectory by Weight Response (Nadir Cohort)"),
  colors = c("#EF5350", "#FFA726", "#66BB6A")  # Light Red, Orange, Green
)

# Add sample size annotations
weight_n <- weight_response_summary %>%
  filter(period == "Baseline") %>%
  mutate(label = sprintf("%s: N=%d", weight_response, n))

p_weight <- p_weight +
  labs(caption = paste(weight_n$label, collapse = " | "))

# =============================================================================
# COMBINE AND SAVE PLOTS
# =============================================================================

cat("\nCombining and saving plots...\n")

if (has_sex_data) {
  # 2x2 grid with all 4 analyses
  combined_plot <- (p_bmi + p_tertile) / (p_sex + p_weight) +
    plot_annotation(
      title = "Sensitivity Analyses: Steps Trajectories by Subgroup",
      subtitle = "Following GLP-1 Initiation",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )

  ggsave("sensitivity_analyses_combined.png", combined_plot,
         width = 14, height = 12, dpi = 300)
  ggsave("sensitivity_analyses_combined.pdf", combined_plot,
         width = 14, height = 12)

} else {
  # 3-panel layout without sex
  combined_plot <- (p_bmi + p_tertile) / (p_weight + plot_spacer()) +
    plot_annotation(
      title = "Sensitivity Analyses: Steps Trajectories by Subgroup",
      subtitle = "Following GLP-1 Initiation",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )

  ggsave("sensitivity_analyses_combined.png", combined_plot,
         width = 14, height = 9, dpi = 300)
  ggsave("sensitivity_analyses_combined.pdf", combined_plot,
         width = 14, height = 9)
}

cat("Saved: sensitivity_analyses_combined.png\n")
cat("Saved: sensitivity_analyses_combined.pdf\n\n")

# Save individual plots
ggsave("sensitivity_bmi_class.png", p_bmi, width = 8, height = 6, dpi = 300)
ggsave("sensitivity_bmi_class.pdf", p_bmi, width = 8, height = 6)

ggsave("sensitivity_baseline_tertile.png", p_tertile, width = 8, height = 6, dpi = 300)
ggsave("sensitivity_baseline_tertile.pdf", p_tertile, width = 8, height = 6)

if (has_sex_data) {
  ggsave("sensitivity_sex.png", p_sex, width = 8, height = 6, dpi = 300)
  ggsave("sensitivity_sex.pdf", p_sex, width = 8, height = 6)
}

ggsave("sensitivity_weight_response.png", p_weight, width = 8, height = 6, dpi = 300)
ggsave("sensitivity_weight_response.pdf", p_weight, width = 8, height = 6)

cat("Saved individual plots:\n")
cat("  - sensitivity_bmi_class.png/pdf\n")
cat("  - sensitivity_baseline_tertile.png/pdf\n")
if (has_sex_data) {
  cat("  - sensitivity_sex.png/pdf\n")
}
cat("  - sensitivity_weight_response.png/pdf\n\n")

# =============================================================================
# CREATE SUMMARY TABLE
# =============================================================================

cat("Creating summary comparison table...\n")

# Load model results
bmi_results <- read_csv("sensitivity_bmi_class_results.csv", show_col_types = FALSE)
tertile_results <- read_csv("sensitivity_baseline_tertile_results.csv", show_col_types = FALSE)
weight_response_results <- read_csv("sensitivity_weight_response_results.csv", show_col_types = FALSE)

if (has_sex_data) {
  sex_results <- read_csv("sensitivity_sex_results.csv", show_col_types = FALSE)
}

# Function to format results
format_results <- function(results, analysis_name) {
  results %>%
    mutate(
      change = sprintf("%.0f (%.0f)", estimate, se),
      p = sprintf("%.4f", p_value),
      analysis = analysis_name
    ) %>%
    select(analysis, strata, period, n_patients, change, p)
}

# Combine all results
summary_table <- bind_rows(
  format_results(bmi_results, "BMI Class"),
  format_results(tertile_results, "Baseline Steps"),
  if (has_sex_data) format_results(sex_results, "Sex") else NULL,
  format_results(weight_response_results, "Weight Response")
)

write_csv(summary_table, "sensitivity_analyses_summary_table.csv")
cat("Saved: sensitivity_analyses_summary_table.csv\n\n")

cat("##################################################\n")
cat("VISUALIZATION COMPLETE\n")
cat("##################################################\n\n")

cat("Generated files:\n")
cat("  - sensitivity_analyses_combined.png/pdf (main figure)\n")
cat("  - Individual plots for each analysis\n")
cat("  - sensitivity_analyses_summary_table.csv (results table)\n\n")
