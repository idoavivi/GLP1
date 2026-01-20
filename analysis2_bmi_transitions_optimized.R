# =============================================================================
# ANALYSIS 2: BMI CLASS TRANSITIONS (OPTIMIZED)
# =============================================================================
# Identifies participants who transitioned to lower BMI class
# Compares activity at baseline (higher BMI) vs follow-up (lower BMI)
# Optimized to avoid memory crashes with large datasets
# =============================================================================

library(tidyverse)
library(ggalluvial)
library(knitr)

cat("\n##################################################\n")
cat("ANALYSIS 2: BMI CLASS TRANSITIONS\n")
cat("Optimized for large datasets\n")
cat("##################################################\n\n")

# =============================================================================
# PREREQUISITES
# =============================================================================

cat("Checking for required data...\n")

# Check if data exists from previous analysis or system-generated code
if (!exists("activity_final") || !exists("bmi_final")) {
  stop(paste(
    "\n❌ ERROR: Required data not found.\n\n",
    "This script needs:\n",
    "  - activity_final (cleaned Fitbit activity data)\n",
    "  - bmi_final (cleaned BMI data)\n\n",
    "Please run one of these first:\n",
    "  1. System-generated export code + fitbit_bmi_stratification.R (Analysis 1)\n",
    "  2. Or load data manually\n"
  ))
}

cat(sprintf("✓ Found activity data: %s records\n", format(nrow(activity_final), big.mark = ",")))
cat(sprintf("✓ Found BMI data: %s records\n\n", format(nrow(bmi_final), big.mark = ",")))

# =============================================================================
# STEP 1: IDENTIFY BMI CLASS TRANSITIONS
# =============================================================================

cat("========================================\n")
cat("STEP 1: Identifying BMI transitions\n")
cat("========================================\n\n")

cat("Assigning BMI classes to each measurement...\n")

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

cat("Finding baseline and follow-up BMI for each participant...\n")

# Find baseline (earliest) and follow-up (latest) BMI for each participant
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

cat(sprintf("Found %s participants who transitioned to lower BMI class\n",
            format(nrow(transitioners), big.mark = ",")))
cat(sprintf("  (with >30 days between measurements)\n\n"))

if (nrow(transitioners) == 0) {
  cat("⚠ No participants found with BMI class transitions. Analysis complete.\n\n")
  quit(save = "no", status = 0)
}

# =============================================================================
# STEP 2: EXTRACT ACTIVITY IN TIME WINDOWS (OPTIMIZED)
# =============================================================================

cat("========================================\n")
cat("STEP 2: Extracting activity data\n")
cat("========================================\n\n")

cat("Creating 60-day time windows around BMI measurements...\n")

# Create time windows for baseline and follow-up
transitioners_windows <- transitioners %>%
  mutate(
    baseline_window_start = baseline_date - 30,
    baseline_window_end = baseline_date + 30,
    followup_window_start = followup_date - 30,
    followup_window_end = followup_date + 30
  )

cat("Extracting baseline activity... (this may take a few minutes)\n")

# Extract baseline activity using efficient joins instead of rowwise
baseline_activity <- activity_final %>%
  inner_join(
    transitioners_windows %>%
      select(person_id, baseline_window_start, baseline_window_end),
    by = "person_id"
  ) %>%
  filter(date >= baseline_window_start, date <= baseline_window_end) %>%
  group_by(person_id) %>%
  summarize(
    n_baseline_days = n(),
    avg_steps_baseline = mean(steps, na.rm = TRUE),
    .groups = "drop"
  )

cat("Extracting follow-up activity...\n")

# Extract follow-up activity
followup_activity <- activity_final %>%
  inner_join(
    transitioners_windows %>%
      select(person_id, followup_window_start, followup_window_end),
    by = "person_id"
  ) %>%
  filter(date >= followup_window_start, date <= followup_window_end) %>%
  group_by(person_id) %>%
  summarize(
    n_followup_days = n(),
    avg_steps_followup = mean(steps, na.rm = TRUE),
    .groups = "drop"
  )

cat("Merging activity with transitions...\n")

# Merge activity data with transition data
transitioner_summary <- transitioners %>%
  left_join(baseline_activity, by = "person_id") %>%
  left_join(followup_activity, by = "person_id") %>%
  # Filter for ≥5 days at both timepoints
  filter(
    !is.na(n_baseline_days), n_baseline_days >= 5,
    !is.na(n_followup_days), n_followup_days >= 5
  ) %>%
  select(person_id,
         baseline_date, baseline_bmi, baseline_class,
         followup_date, followup_bmi, followup_class,
         days_between,
         avg_steps_baseline, avg_steps_followup,
         n_baseline_days, n_followup_days)

cat(sprintf("After requiring ≥5 days Fitbit data: %s participants\n\n",
            format(nrow(transitioner_summary), big.mark = ",")))

if (nrow(transitioner_summary) == 0) {
  cat("⚠ No participants with sufficient Fitbit data. Analysis complete.\n\n")
  quit(save = "no", status = 0)
}

# =============================================================================
# STEP 3: CALCULATE STEP QUARTILES (OPTIMIZED)
# =============================================================================

cat("========================================\n")
cat("STEP 3: Calculating step quartiles\n")
cat("========================================\n\n")

# Pre-calculate quartile cutoffs (much faster than doing it repeatedly)
baseline_quartiles <- quantile(transitioner_summary$avg_steps_baseline,
                                probs = c(0.25, 0.50, 0.75),
                                na.rm = TRUE)

followup_quartiles <- quantile(transitioner_summary$avg_steps_followup,
                                probs = c(0.25, 0.50, 0.75),
                                na.rm = TRUE)

cat("Baseline step quartiles:\n")
cat(sprintf("  Q1: ≤%.0f, Q2: %.0f-%.0f, Q3: %.0f-%.0f, Q4: >%.0f\n",
            baseline_quartiles[1],
            baseline_quartiles[1], baseline_quartiles[2],
            baseline_quartiles[2], baseline_quartiles[3],
            baseline_quartiles[3]))

cat("Follow-up step quartiles:\n")
cat(sprintf("  Q1: ≤%.0f, Q2: %.0f-%.0f, Q3: %.0f-%.0f, Q4: >%.0f\n\n",
            followup_quartiles[1],
            followup_quartiles[1], followup_quartiles[2],
            followup_quartiles[2], followup_quartiles[3],
            followup_quartiles[3]))

# Assign quartiles using vectorized operations (much faster than rowwise)
transitioner_summary <- transitioner_summary %>%
  mutate(
    steps_quartile_baseline = case_when(
      avg_steps_baseline <= baseline_quartiles[1] ~ "Q1",
      avg_steps_baseline <= baseline_quartiles[2] ~ "Q2",
      avg_steps_baseline <= baseline_quartiles[3] ~ "Q3",
      TRUE ~ "Q4"
    ),
    steps_quartile_followup = case_when(
      avg_steps_followup <= followup_quartiles[1] ~ "Q1",
      avg_steps_followup <= followup_quartiles[2] ~ "Q2",
      avg_steps_followup <= followup_quartiles[3] ~ "Q3",
      TRUE ~ "Q4"
    )
  )

# =============================================================================
# STEP 4: SUMMARY STATISTICS
# =============================================================================

cat("========================================\n")
cat("STEP 4: Computing summary statistics\n")
cat("========================================\n\n")

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

    Delta_steps_median = median(avg_steps_followup - avg_steps_baseline, na.rm = TRUE),
    Delta_steps_q25 = quantile(avg_steps_followup - avg_steps_baseline, 0.25, na.rm = TRUE),
    Delta_steps_q75 = quantile(avg_steps_followup - avg_steps_baseline, 0.75, na.rm = TRUE)
  )

cat("Activity in Participants with BMI Class Reduction:\n")
cat("=================================================\n\n")
print(overall_summary)
cat("\n")

# Statistical test (paired Wilcoxon test)
wilcox_result <- wilcox.test(transitioner_summary$avg_steps_baseline,
                              transitioner_summary$avg_steps_followup,
                              paired = TRUE)

cat(sprintf("Paired Wilcoxon test: p = %.2e\n", wilcox_result$p.value))
if (wilcox_result$p.value < 0.05) {
  cat("  ✓ Significant difference in steps between baseline and follow-up\n\n")
} else {
  cat("  No significant difference in steps\n\n")
}

# =============================================================================
# STEP 5: SAVE OUTPUTS
# =============================================================================

cat("========================================\n")
cat("STEP 5: Saving outputs\n")
cat("========================================\n\n")

# Save individual-level data
write_csv(transitioner_summary, "analysis2_bmi_transitioners.csv")
cat("✓ Saved: analysis2_bmi_transitioners.csv\n")

# Save transition counts
transition_counts <- transitioner_summary %>%
  group_by(steps_quartile_baseline, steps_quartile_followup) %>%
  summarize(n = n(), .groups = "drop") %>%
  arrange(steps_quartile_baseline, steps_quartile_followup)

write_csv(transition_counts, "analysis2_step_quartile_transitions.csv")
cat("✓ Saved: analysis2_step_quartile_transitions.csv\n")

# Create formatted summary table
summary_table_formatted <- overall_summary %>%
  mutate(
    `Baseline Steps` = sprintf("%.0f (%.0f-%.0f)",
                                Baseline_steps_median, Baseline_steps_q25, Baseline_steps_q75),
    `Follow-up Steps` = sprintf("%.0f (%.0f-%.0f)",
                                 Followup_steps_median, Followup_steps_q25, Followup_steps_q75),
    `Delta Steps` = sprintf("%.0f (%.0f-%.0f)",
                             Delta_steps_median, Delta_steps_q25, Delta_steps_q75)
  ) %>%
  select(N, `Baseline Steps`, `Follow-up Steps`, `Delta Steps`)

html_table <- knitr::kable(summary_table_formatted,
                            format = "html",
                            caption = "Activity in Participants Who Transitioned to Lower BMI Class (Median and IQR)",
                            align = c("r", "r", "r", "r"))

writeLines(html_table, "analysis2_summary_table.html")
cat("✓ Saved: analysis2_summary_table.html\n\n")

# =============================================================================
# STEP 6: CREATE VISUALIZATIONS
# =============================================================================

cat("========================================\n")
cat("STEP 6: Creating visualizations\n")
cat("========================================\n\n")

# Paired comparison plot
cat("Creating paired comparison plot...\n")

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
    subtitle = sprintf("N = %s, paired Wilcoxon p = %.2e",
                      format(nrow(transitioner_summary), big.mark = ","),
                      wilcox_result$p.value),
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

cat("✓ Saved: analysis2_paired_steps_comparison.png/pdf\n")

# Sankey diagram
cat("Creating Sankey diagram...\n")

if (nrow(transitioner_summary) >= 10) {
  sankey_data <- transitioner_summary %>%
    select(person_id,
           Baseline = steps_quartile_baseline,
           Followup = steps_quartile_followup) %>%
    mutate(
      Baseline = factor(Baseline, levels = c("Q1", "Q2", "Q3", "Q4")),
      Followup = factor(Followup, levels = c("Q1", "Q2", "Q3", "Q4"))
    )

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
      subtitle = sprintf("N = %s participants who moved to lower BMI class",
                        format(nrow(sankey_data), big.mark = ",")),
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

  cat("✓ Saved: analysis2_step_quartile_sankey.png/pdf\n\n")
} else {
  cat("⚠ Too few participants for Sankey diagram (N<10). Skipped.\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("ANALYSIS 2 COMPLETE\n")
cat("##################################################\n\n")

cat("Output files:\n")
cat("  CSV files:\n")
cat("    - analysis2_bmi_transitioners.csv\n")
cat("    - analysis2_step_quartile_transitions.csv\n")
cat("  Tables:\n")
cat("    - analysis2_summary_table.html\n")
cat("  Figures:\n")
cat("    - analysis2_paired_steps_comparison.png/pdf\n")
if (nrow(transitioner_summary) >= 10) {
  cat("    - analysis2_step_quartile_sankey.png/pdf\n")
}
cat("\nDone!\n\n")
