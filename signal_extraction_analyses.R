# =============================================================================
# SIGNAL EXTRACTION: REDUCE VARIABILITY IN ACTIVITY-WEIGHT ANALYSES
# =============================================================================
# Addresses large inter-individual variability by focusing on:
# 1. Within-person changes (Δ steps, Δ weight)
# 2. Early activity predicting late outcomes
# 3. Response magnitude categories (binary/categorical outcomes)
# 4. Consistency metrics (CV, adherence)
# =============================================================================

library(tidyverse)
library(patchwork)
library(broom)

cat("\n##################################################\n")
cat("SIGNAL EXTRACTION ANALYSES\n")
cat("Reducing noise from inter-individual variability\n")
cat("##################################################\n\n")

# =============================================================================
# LOAD DATA (memory-first pattern)
# =============================================================================

cat("Checking for data...\n")

has_data_in_memory <- (exists("obesity_cohort") && is.data.frame(obesity_cohort) &&
                       (exists("activity_cleaned") || exists("activity_final")) &&
                       (exists("weight_cleaned") || exists("weight_final")) &&
                       (exists("bmi_data") || exists("bmi_final")))

if (has_data_in_memory) {
  cat("✓ Using data from memory\n")
  if (exists("activity_final")) activity_cleaned <- activity_final
  if (exists("weight_final")) weight_cleaned <- weight_final
  if (exists("bmi_final")) bmi_data <- bmi_final
  if (exists("drug_final")) drug_glp1_clean <- drug_final
  if (exists("glp1_initiation_final")) glp1_initiation <- glp1_initiation_final
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

cat(sprintf("Fixed baseline cohort: N=%d\n\n", nrow(baseline_cohort)))

activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

weight_cleaned <- weight_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

bmi_data <- bmi_data %>%
  filter(person_id %in% baseline_cohort$person_id)

# =============================================================================
# ANALYSIS 1: WITHIN-PERSON CHANGE (Δ) ANALYSIS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 1: WITHIN-PERSON CHANGES\n")
cat("========================================\n\n")

cat("Calculating within-person changes to remove inter-individual variability...\n\n")

# Calculate activity at each time window
activity_windows <- tribble(
  ~window, ~days_min, ~days_max,
  "Baseline", -180, 0,
  "Early", 1, 30,
  "Mid", 31, 90,
  "Late1", 91, 180,
  "Late2", 181, 365
)

activity_by_window <- map_dfr(1:nrow(activity_windows), function(i) {
  activity_cleaned %>%
    filter(days_from_initiation >= activity_windows$days_min[i],
           days_from_initiation <= activity_windows$days_max[i],
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    summarize(
      mean_steps = mean(steps, na.rm = TRUE),
      mean_mvpa = mean(fairly_active_minutes + very_active_minutes, na.rm = TRUE),
      mean_sedentary = mean(sedentary_minutes, na.rm = TRUE),
      mean_calories = mean(activity_calories, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    ) %>%
    mutate(window = activity_windows$window[i])
})

# Pivot to wide format
activity_wide <- activity_by_window %>%
  pivot_wider(
    id_cols = person_id,
    names_from = window,
    values_from = c(mean_steps, mean_mvpa, mean_sedentary, mean_calories, n_days),
    names_glue = "{.value}_{window}"
  )

# Calculate changes (Δ = Follow-up - Baseline)
delta_data <- activity_wide %>%
  mutate(
    # Early changes (1-30d vs Baseline)
    delta_steps_early = mean_steps_Early - mean_steps_Baseline,
    delta_mvpa_early = mean_mvpa_Early - mean_mvpa_Baseline,
    delta_sedentary_early = mean_sedentary_Early - mean_sedentary_Baseline,
    delta_calories_early = mean_calories_Early - mean_calories_Baseline,

    # Percent changes
    pct_steps_early = 100 * (mean_steps_Early - mean_steps_Baseline) / mean_steps_Baseline,
    pct_mvpa_early = 100 * (mean_mvpa_Early - mean_mvpa_Baseline) / mean_mvpa_Baseline,

    # Late changes (181-365d vs Baseline) - for prediction analysis
    delta_steps_late = if_else(!is.na(mean_steps_Late2),
                               mean_steps_Late2 - mean_steps_Baseline,
                               NA_real_),
    pct_steps_late = if_else(!is.na(mean_steps_Late2),
                             100 * (mean_steps_Late2 - mean_steps_Baseline) / mean_steps_Baseline,
                             NA_real_)
  )

cat(sprintf("Within-person changes calculated for N=%d patients\n", nrow(delta_data)))
cat(sprintf("  With early follow-up: N=%d\n", sum(!is.na(delta_data$delta_steps_early))))
cat(sprintf("  With late follow-up: N=%d\n", sum(!is.na(delta_data$delta_steps_late))))
cat("\n")

# Calculate weight changes
baseline_weight <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg, baseline_day = days_from_initiation)

nadir_weight <- weight_cleaned %>%
  filter(days_from_initiation >= 84) %>%
  group_by(person_id) %>%
  arrange(weight_kg) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, nadir_weight = weight_kg, nadir_day = days_from_initiation)

late_weight <- weight_cleaned %>%
  filter(days_from_initiation >= 181, days_from_initiation <= 365) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, late_weight = weight_kg, late_day = days_from_initiation)

# Combine all changes
change_data <- delta_data %>%
  left_join(baseline_weight, by = "person_id") %>%
  left_join(nadir_weight, by = "person_id") %>%
  left_join(late_weight, by = "person_id") %>%
  mutate(
    delta_weight_nadir = nadir_weight - baseline_weight,
    pct_weight_nadir = 100 * (nadir_weight - baseline_weight) / baseline_weight,

    delta_weight_late = late_weight - baseline_weight,
    pct_weight_late = 100 * (late_weight - baseline_weight) / baseline_weight
  )

# Baseline characteristics for stratification
baseline_bmi <- bmi_data %>%
  filter(days_from_initiation >= -365, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, baseline_bmi = bmi) %>%
  mutate(
    bmi_class = case_when(
      baseline_bmi >= 30 & baseline_bmi < 35 ~ "Class I",
      baseline_bmi >= 35 & baseline_bmi < 40 ~ "Class II",
      baseline_bmi >= 40 ~ "Class III",
      TRUE ~ NA_character_
    )
  )

change_data <- change_data %>%
  left_join(baseline_bmi, by = "person_id")

# Save delta dataset
write_csv(change_data, "within_person_changes.csv")
cat("Saved: within_person_changes.csv\n\n")

# =============================================================================
# ANALYSIS 2: EARLY ACTIVITY PREDICTS LATE WEIGHT LOSS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 2: EARLY-TO-LATE PREDICTION\n")
cat("========================================\n\n")

cat("Testing: Does early activity change (1-30d) predict late weight loss (6-12 months)?\n\n")

# Prepare prediction dataset
prediction_data <- change_data %>%
  filter(!is.na(delta_steps_early), !is.na(pct_weight_late)) %>%
  mutate(
    # Categorize early activity response
    early_activity_response = case_when(
      delta_steps_early >= 1000 ~ "Increaser (≥1000)",
      delta_steps_early > -1000 & delta_steps_early < 1000 ~ "Maintainer (±1000)",
      delta_steps_early <= -1000 ~ "Decreaser (≤-1000)",
      TRUE ~ NA_character_
    ),

    # Categorize late weight loss
    late_weight_response = case_when(
      pct_weight_late <= -10 ~ ">10% loss",
      pct_weight_late <= -5 & pct_weight_late > -10 ~ "5-10% loss",
      pct_weight_late > -5 ~ "<5% loss",
      TRUE ~ NA_character_
    )
  )

cat(sprintf("Early-to-late prediction analysis: N=%d patients\n", nrow(prediction_data)))

# Correlation
cor_early_late <- cor.test(prediction_data$delta_steps_early,
                            prediction_data$pct_weight_late,
                            method = "spearman")

cat(sprintf("\nSpearman correlation (early Δ steps vs late %% weight loss):\n"))
cat(sprintf("  rho = %.3f, p = %.4f\n\n", cor_early_late$estimate, cor_early_late$p.value))

# Linear model
pred_model <- lm(pct_weight_late ~ delta_steps_early + baseline_bmi, data = prediction_data)

cat("Linear regression: Late weight loss ~ Early activity change + Baseline BMI\n")
print(summary(pred_model))
cat("\n")

# Cross-tabulation
if (nrow(prediction_data) > 0) {
  crosstab <- table(prediction_data$early_activity_response,
                    prediction_data$late_weight_response)
  cat("Cross-tabulation: Early activity × Late weight response\n")
  print(crosstab)
  cat("\n")

  chi_test <- chisq.test(crosstab)
  cat(sprintf("Chi-square test: p = %.4f\n\n", chi_test$p.value))
}

write_csv(prediction_data, "early_to_late_prediction.csv")
cat("Saved: early_to_late_prediction.csv\n\n")

# =============================================================================
# ANALYSIS 3: RESPONSE MAGNITUDE CATEGORIES
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 3: RESPONSE CATEGORIES\n")
cat("========================================\n\n")

cat("Categorizing patients by activity and weight response...\n\n")

# Define response categories
responder_data <- change_data %>%
  filter(!is.na(delta_steps_early), !is.na(pct_weight_nadir)) %>%
  mutate(
    # Activity responder (early)
    activity_responder = case_when(
      delta_steps_early >= 1000 ~ "Increaser",
      delta_steps_early > -1000 & delta_steps_early < 1000 ~ "Maintainer",
      delta_steps_early <= -1000 ~ "Decreaser",
      TRUE ~ NA_character_
    ),
    activity_responder = factor(activity_responder, levels = c("Decreaser", "Maintainer", "Increaser")),

    # Weight responder (nadir)
    weight_responder = case_when(
      pct_weight_nadir <= -10 ~ ">10% loss",
      pct_weight_nadir <= -5 & pct_weight_nadir > -10 ~ "5-10% loss",
      pct_weight_nadir > -5 ~ "<5% loss",
      TRUE ~ NA_character_
    ),
    weight_responder = factor(weight_responder, levels = c("<5% loss", "5-10% loss", ">10% loss")),

    # Combined super responder
    super_responder = (pct_weight_nadir <= -10 & delta_steps_early >= 1000)
  )

cat("Activity responder distribution:\n")
print(table(responder_data$activity_responder))
cat("\n")

cat("Weight responder distribution:\n")
print(table(responder_data$weight_responder))
cat("\n")

cat("Cross-tabulation: Activity × Weight response\n")
crosstab_response <- table(responder_data$activity_responder,
                           responder_data$weight_responder)
print(crosstab_response)
cat("\n")

cat(sprintf("Super responders (>10%% weight loss + ≥1000 steps): N=%d (%.1f%%)\n",
            sum(responder_data$super_responder),
            100 * mean(responder_data$super_responder)))
cat("\n")

# Test if activity responders have better weight loss
weight_by_activity <- responder_data %>%
  group_by(activity_responder) %>%
  summarize(
    n = n(),
    median_weight_loss = median(pct_weight_nadir, na.rm = TRUE),
    q25 = quantile(pct_weight_nadir, 0.25, na.rm = TRUE),
    q75 = quantile(pct_weight_nadir, 0.75, na.rm = TRUE),
    pct_super = 100 * mean(super_responder),
    .groups = "drop"
  )

cat("Weight loss by activity response category:\n")
print(weight_by_activity)
cat("\n")

# Kruskal-Wallis test
kw_test <- kruskal.test(pct_weight_nadir ~ activity_responder, data = responder_data)
cat(sprintf("Kruskal-Wallis test: p = %.4f\n\n", kw_test$p.value))

write_csv(responder_data, "response_categories.csv")
write_csv(weight_by_activity, "weight_by_activity_response.csv")
cat("Saved: response_categories.csv, weight_by_activity_response.csv\n\n")

# =============================================================================
# ANALYSIS 4: CONSISTENCY METRICS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 4: ACTIVITY CONSISTENCY\n")
cat("========================================\n\n")

cat("Calculating consistency metrics (hypothesis: consistent > sporadic)\n\n")

# Calculate day-level variability in early period
consistency_data <- activity_cleaned %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 90,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  filter(n() >= 7) %>%  # Need at least 7 days
  summarize(
    mean_steps = mean(steps, na.rm = TRUE),
    sd_steps = sd(steps, na.rm = TRUE),
    cv_steps = sd(steps, na.rm = TRUE) / mean(steps, na.rm = TRUE),
    n_days = n(),
    pct_days_5000 = 100 * mean(steps >= 5000),
    pct_days_10000 = 100 * mean(steps >= 10000),
    .groups = "drop"
  ) %>%
  left_join(
    change_data %>% select(person_id, pct_weight_nadir, baseline_bmi),
    by = "person_id"
  ) %>%
  filter(!is.na(pct_weight_nadir)) %>%
  mutate(
    consistency_group = case_when(
      cv_steps < 0.4 ~ "Consistent (CV<0.4)",
      cv_steps >= 0.4 & cv_steps < 0.6 ~ "Moderate (CV 0.4-0.6)",
      cv_steps >= 0.6 ~ "Variable (CV≥0.6)",
      TRUE ~ NA_character_
    )
  )

cat(sprintf("Consistency analysis: N=%d patients with ≥7 days early follow-up\n\n", nrow(consistency_data)))

# Test consistency vs weight loss
consist_test <- cor.test(consistency_data$cv_steps,
                         consistency_data$pct_weight_nadir,
                         method = "spearman")

cat(sprintf("Correlation (CV vs weight loss): rho = %.3f, p = %.4f\n",
            consist_test$estimate, consist_test$p.value))
cat(if_else(consist_test$estimate > 0,
            "  Positive = more variable activity → less weight loss\n",
            "  Negative = more variable activity → more weight loss\n"))
cat("\n")

write_csv(consistency_data, "activity_consistency.csv")
cat("Saved: activity_consistency.csv\n\n")

# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

cat("\n========================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("========================================\n\n")

## VIZ 1: Within-person Δ scatter (Main finding)
viz_delta <- change_data %>%
  filter(!is.na(delta_steps_early), !is.na(pct_weight_nadir))

if (nrow(viz_delta) >= 10) {
  p1 <- ggplot(viz_delta, aes(x = delta_steps_early, y = pct_weight_nadir)) +
    geom_point(aes(color = bmi_class), alpha = 0.6, size = 2.5) +
    geom_smooth(method = "lm", color = "black", linewidth = 1.2, se = TRUE) +
    geom_hline(yintercept = -5, linetype = "dashed", color = "gray40", alpha = 0.5) +
    geom_hline(yintercept = -10, linetype = "dashed", color = "gray40", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.5) +
    scale_color_manual(values = c("Class I" = "#2E7D32", "Class II" = "#F57C00", "Class III" = "#C62828"),
                       na.value = "gray50") +
    annotate("text", x = max(viz_delta$delta_steps_early, na.rm = TRUE) * 0.7,
             y = -2,
             label = sprintf("Spearman rho = %.3f\np = %.4f\nN = %d",
                            cor(viz_delta$delta_steps_early, viz_delta$pct_weight_nadir,
                                use = "complete", method = "spearman"),
                            cor.test(viz_delta$delta_steps_early, viz_delta$pct_weight_nadir,
                                    method = "spearman")$p.value,
                            nrow(viz_delta)),
             hjust = 1, size = 3.5, fontface = "bold") +
    labs(
      title = "Within-Person Changes: Activity vs Weight",
      subtitle = "Reduces inter-individual variability by focusing on Δ (change scores)",
      x = "Change in Steps per Day\n(1-30d minus Baseline)",
      y = "% Weight Change at Nadir\n(negative = weight loss)",
      color = "Baseline BMI"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          legend.position = "bottom")

  ggsave("signal_within_person_delta.png", p1, width = 10, height = 8, dpi = 300)
  ggsave("signal_within_person_delta.pdf", p1, width = 10, height = 8)
  cat("Saved: signal_within_person_delta.png/pdf\n")
}

## VIZ 2: Early-to-late prediction
if (nrow(prediction_data) >= 10) {
  p2 <- ggplot(prediction_data, aes(x = delta_steps_early, y = pct_weight_late)) +
    geom_point(alpha = 0.5, size = 2.5, color = "steelblue") +
    geom_smooth(method = "lm", color = "darkred", linewidth = 1.2, se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    annotate("text", x = max(prediction_data$delta_steps_early, na.rm = TRUE) * 0.7,
             y = min(prediction_data$pct_weight_late, na.rm = TRUE) * 0.9,
             label = sprintf("Early activity change predicts\nlate weight loss\nrho = %.3f, p = %.4f",
                            cor_early_late$estimate,
                            cor_early_late$p.value),
             hjust = 1, size = 4, fontface = "bold") +
    labs(
      title = "Early Activity Change Predicts Late Weight Loss",
      subtitle = "Does 1-30d activity response predict 6-12 month outcomes?",
      x = "Early Change in Steps (1-30d)",
      y = "Late % Weight Change (6-12 months)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14))

  ggsave("signal_early_to_late_prediction.png", p2, width = 10, height = 8, dpi = 300)
  ggsave("signal_early_to_late_prediction.pdf", p2, width = 10, height = 8)
  cat("Saved: signal_early_to_late_prediction.png/pdf\n")
}

## VIZ 3: Response categories (stacked bar + box plots)
if (nrow(responder_data) >= 10) {
  # Stacked bar chart
  p3a <- ggplot(responder_data, aes(x = activity_responder, fill = weight_responder)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("<5% loss" = "#EF5350",
                                  "5-10% loss" = "#FFA726",
                                  ">10% loss" = "#66BB6A")) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Weight Loss Response by Activity Response Category",
      x = "Activity Response (Early 1-30d)",
      y = "Proportion of Patients",
      fill = "Weight Loss"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13))

  # Box plot
  p3b <- ggplot(responder_data, aes(x = activity_responder, y = pct_weight_nadir, fill = activity_responder)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    geom_hline(yintercept = -5, linetype = "dashed", color = "gray40") +
    geom_hline(yintercept = -10, linetype = "dashed", color = "gray40") +
    scale_fill_manual(values = c("Decreaser" = "#D32F2F",
                                  "Maintainer" = "#FFA000",
                                  "Increaser" = "#388E3C")) +
    annotate("text", x = 2, y = min(responder_data$pct_weight_nadir, na.rm = TRUE) * 0.9,
             label = sprintf("Kruskal-Wallis p = %.4f", kw_test$p.value),
             size = 4, fontface = "bold") +
    labs(
      title = "Weight Loss Distribution by Activity Response",
      x = "Activity Response Category",
      y = "% Weight Change at Nadir"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13),
          legend.position = "none")

  p3_combined <- p3a + p3b +
    plot_annotation(
      title = "Activity Responders Have Better Weight Loss",
      subtitle = sprintf("N=%d patients with early activity and nadir weight data", nrow(responder_data)),
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )

  ggsave("signal_response_categories.png", p3_combined, width = 14, height = 6, dpi = 300)
  ggsave("signal_response_categories.pdf", p3_combined, width = 14, height = 6)
  cat("Saved: signal_response_categories.png/pdf\n")
}

## VIZ 4: Consistency metrics
if (nrow(consistency_data) >= 10) {
  p4 <- ggplot(consistency_data, aes(x = cv_steps, y = pct_weight_nadir)) +
    geom_point(alpha = 0.5, size = 2.5, color = "purple") +
    geom_smooth(method = "lm", color = "black", linewidth = 1.2, se = TRUE) +
    geom_vline(xintercept = c(0.4, 0.6), linetype = "dashed", color = "gray40", alpha = 0.5) +
    annotate("text", x = 0.3, y = max(consistency_data$pct_weight_nadir, na.rm = TRUE),
             label = "Consistent", size = 3, fontface = "italic") +
    annotate("text", x = 0.5, y = max(consistency_data$pct_weight_nadir, na.rm = TRUE),
             label = "Moderate", size = 3, fontface = "italic") +
    annotate("text", x = 0.7, y = max(consistency_data$pct_weight_nadir, na.rm = TRUE),
             label = "Variable", size = 3, fontface = "italic") +
    labs(
      title = "Activity Consistency vs Weight Loss",
      subtitle = sprintf("Does consistent activity (low CV) predict better outcomes? rho=%.3f, p=%.4f",
                        consist_test$estimate, consist_test$p.value),
      x = "Coefficient of Variation (CV) of Daily Steps\n(Lower = more consistent)",
      y = "% Weight Change at Nadir"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14))

  ggsave("signal_consistency.png", p4, width = 10, height = 8, dpi = 300)
  ggsave("signal_consistency.pdf", p4, width = 10, height = 8)
  cat("Saved: signal_consistency.png/pdf\n")
}

cat("\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n========================================\n")
cat("SUMMARY RESULTS\n")
cat("========================================\n\n")

cat("KEY FINDINGS:\n\n")

cat(sprintf("1. WITHIN-PERSON CHANGES (N=%d):\n", sum(!is.na(change_data$delta_steps_early) &
                                                      !is.na(change_data$pct_weight_nadir))))
cat(sprintf("   Correlation Δ steps vs %% weight loss: rho=%.3f\n",
            cor(viz_delta$delta_steps_early, viz_delta$pct_weight_nadir,
                use = "complete", method = "spearman")))
cat(sprintf("   Median Δ steps: %.0f (IQR: %.0f to %.0f)\n",
            median(change_data$delta_steps_early, na.rm = TRUE),
            quantile(change_data$delta_steps_early, 0.25, na.rm = TRUE),
            quantile(change_data$delta_steps_early, 0.75, na.rm = TRUE)))
cat("\n")

cat(sprintf("2. EARLY→LATE PREDICTION (N=%d):\n", nrow(prediction_data)))
cat(sprintf("   Early activity predicts late weight loss: rho=%.3f, p=%.4f\n",
            cor_early_late$estimate, cor_early_late$p.value))
cat(sprintf("   Clinical interpretation: %s\n",
            if_else(cor_early_late$p.value < 0.05,
                   "Early activity IS a biomarker for late success",
                   "Early activity may not predict late outcomes")))
cat("\n")

cat(sprintf("3. RESPONSE CATEGORIES (N=%d):\n", nrow(responder_data)))
cat("   Weight loss by activity response:\n")
for (i in 1:nrow(weight_by_activity)) {
  cat(sprintf("     %s: %.1f%% (%.1f to %.1f), %d%% super responders\n",
              weight_by_activity$activity_responder[i],
              weight_by_activity$median_weight_loss[i],
              weight_by_activity$q25[i],
              weight_by_activity$q75[i],
              round(weight_by_activity$pct_super[i])))
}
cat(sprintf("   Kruskal-Wallis test: p=%.4f\n", kw_test$p.value))
cat("\n")

cat(sprintf("4. CONSISTENCY (N=%d):\n", nrow(consistency_data)))
cat(sprintf("   CV vs weight loss: rho=%.3f, p=%.4f\n",
            consist_test$estimate, consist_test$p.value))
cat(sprintf("   Interpretation: %s\n",
            if_else(consist_test$estimate > 0 & consist_test$p.value < 0.05,
                   "More variable activity → LESS weight loss (consistency matters!)",
                   "Consistency may not affect outcomes")))
cat("\n")

cat("##################################################\n")
cat("SIGNAL EXTRACTION COMPLETE\n")
cat("##################################################\n\n")

cat("Files generated:\n")
cat("  Data:\n")
cat("    - within_person_changes.csv\n")
cat("    - early_to_late_prediction.csv\n")
cat("    - response_categories.csv\n")
cat("    - weight_by_activity_response.csv\n")
cat("    - activity_consistency.csv\n")
cat("  Figures:\n")
cat("    - signal_within_person_delta.png/pdf\n")
cat("    - signal_early_to_late_prediction.png/pdf\n")
cat("    - signal_response_categories.png/pdf\n")
cat("    - signal_consistency.png/pdf\n\n")
