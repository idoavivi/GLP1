# =============================================================================
# Sensitivity Analysis: Weight Loss and Step Change Stratification
# =============================================================================
# Analyzes how activity changes differ by weight loss category
# and how weight changes differ by step change category
# =============================================================================

library(tidyverse)

cat("=============================================================================\n")
cat("SENSITIVITY ANALYSIS: STRATIFICATION BY WEIGHT LOSS AND STEP CHANGE\n")
cat("=============================================================================\n\n")

# Load period analysis results
if (!file.exists("period_analysis_results.RData")) {
  stop("ERROR: period_analysis_results.RData not found. Run period_analysis.R first.")
}

load("period_analysis_results.RData")

cat("Loaded period_analysis_results.RData\n")
cat(sprintf("  Periods analyzed: %d\n", nrow(comprehensive_table)))
cat(sprintf("  Patients in long data: %d\n\n", n_distinct(all_data_long$person_id)))

# =============================================================================
# ANALYSIS 1: STRATIFY BY WEIGHT LOSS CATEGORY
# =============================================================================

cat("=============================================================================\n")
cat("ANALYSIS 1: ACTIVITY CHANGES BY WEIGHT LOSS CATEGORY\n")
cat("=============================================================================\n\n")

# Get baseline data
baseline_data <- all_data_long %>%
  filter(period == "Baseline") %>%
  select(person_id, baseline_weight = weight, baseline_steps = steps,
         baseline_calories = calories)

# Focus on key aggregated periods
target_periods <- c("1-90d", "91-180d", "181-365d")

weight_loss_results <- list()

for (pname in target_periods) {
  cat(sprintf("\n### %s ###\n\n", pname))

  # Get period data
  period_data <- all_data_long %>%
    filter(period == pname) %>%
    select(person_id, period_weight = weight, period_steps = steps,
           period_calories = calories)

  # Merge baseline and period
  merged <- baseline_data %>%
    inner_join(period_data, by = "person_id") %>%
    filter(!is.na(baseline_weight), !is.na(period_weight),
           !is.na(baseline_steps), !is.na(period_steps))

  if (nrow(merged) < 20) {
    cat(sprintf("  WARNING: Only %d patients, skipping\n", nrow(merged)))
    next
  }

  # Calculate percent changes
  merged <- merged %>%
    mutate(
      weight_pct_change = 100 * (period_weight - baseline_weight) / baseline_weight,
      steps_pct_change = 100 * (period_steps - baseline_steps) / baseline_steps,
      calories_change = period_calories - baseline_calories,
      steps_change = period_steps - baseline_steps
    )

  # Categorize by weight loss (using negative values since loss is negative change)
  # Category 1: < 5% weight loss (including weight gain)
  # Category 2: 5-10% weight loss
  # Category 3: > 10% weight loss
  merged <- merged %>%
    mutate(
      weight_loss_cat3 = case_when(
        weight_pct_change > -5 ~ "< 5% loss",
        weight_pct_change <= -5 & weight_pct_change > -10 ~ "5-10% loss",
        weight_pct_change <= -10 ~ "> 10% loss"
      ),
      weight_loss_cat2 = case_when(
        weight_pct_change > -7.5 ~ "< 7.5% loss",
        weight_pct_change <= -7.5 ~ "≥ 7.5% loss"
      )
    )

  # 3-category analysis
  cat("--- 3-Category Weight Loss Analysis ---\n\n")

  summary_3cat <- merged %>%
    group_by(weight_loss_cat3) %>%
    summarize(
      n = n(),
      mean_weight_change = mean(period_weight - baseline_weight),
      mean_weight_pct = mean(weight_pct_change),
      mean_steps_change = mean(steps_change),
      sd_steps_change = sd(steps_change),
      mean_steps_pct = mean(steps_pct_change),
      mean_calories_change = mean(calories_change, na.rm = TRUE),
      sd_calories_change = sd(calories_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(weight_loss_cat3))  # Order: > 10%, 5-10%, < 5%

  print(summary_3cat, width = Inf)

  # ANOVA for steps change by weight loss category
  if (n_distinct(merged$weight_loss_cat3) >= 2) {
    anova_steps <- aov(steps_change ~ weight_loss_cat3, data = merged)
    anova_calories <- aov(calories_change ~ weight_loss_cat3, data = merged)

    cat("\nANOVA: Steps change by weight loss category\n")
    print(summary(anova_steps))

    cat("\nANOVA: Calories change by weight loss category\n")
    print(summary(anova_calories))
  }

  # 2-category analysis
  cat("\n--- 2-Category Weight Loss Analysis (7.5% cutoff) ---\n\n")

  summary_2cat <- merged %>%
    group_by(weight_loss_cat2) %>%
    summarize(
      n = n(),
      mean_weight_change = mean(period_weight - baseline_weight),
      mean_weight_pct = mean(weight_pct_change),
      mean_steps_change = mean(steps_change),
      sd_steps_change = sd(steps_change),
      mean_steps_pct = mean(steps_pct_change),
      mean_calories_change = mean(calories_change, na.rm = TRUE),
      sd_calories_change = sd(calories_change, na.rm = TRUE),
      .groups = "drop"
    )

  print(summary_2cat, width = Inf)

  # T-test for 2-category comparison
  if (n_distinct(merged$weight_loss_cat2) == 2) {
    cat1_steps <- merged %>% filter(weight_loss_cat2 == "< 7.5% loss") %>% pull(steps_change)
    cat2_steps <- merged %>% filter(weight_loss_cat2 == "≥ 7.5% loss") %>% pull(steps_change)

    cat1_calories <- merged %>% filter(weight_loss_cat2 == "< 7.5% loss") %>% pull(calories_change)
    cat2_calories <- merged %>% filter(weight_loss_cat2 == "≥ 7.5% loss") %>% pull(calories_change)

    ttest_steps <- t.test(cat1_steps, cat2_steps)
    ttest_calories <- t.test(cat1_calories, cat2_calories, na.rm = TRUE)

    cat(sprintf("\nT-test: Steps change (< 7.5%% vs ≥ 7.5%% weight loss)\n"))
    cat(sprintf("  Mean difference: %.0f steps\n", mean(cat1_steps) - mean(cat2_steps)))
    cat(sprintf("  p-value: %.4f\n", ttest_steps$p.value))

    cat(sprintf("\nT-test: Calories change (< 7.5%% vs ≥ 7.5%% weight loss)\n"))
    cat(sprintf("  Mean difference: %.0f kcal\n", mean(cat1_calories, na.rm = TRUE) - mean(cat2_calories, na.rm = TRUE)))
    cat(sprintf("  p-value: %.4f\n", ttest_calories$p.value))
  }

  # Store results
  weight_loss_results[[pname]] <- list(
    summary_3cat = summary_3cat,
    summary_2cat = summary_2cat,
    merged_data = merged
  )
}

# =============================================================================
# ANALYSIS 2: STRATIFY BY STEP CHANGE CATEGORY
# =============================================================================

cat("\n\n=============================================================================\n")
cat("ANALYSIS 2: WEIGHT CHANGES BY STEP CHANGE CATEGORY\n")
cat("=============================================================================\n\n")

step_change_results <- list()

for (pname in target_periods) {
  cat(sprintf("\n### %s ###\n\n", pname))

  # Get period data
  period_data <- all_data_long %>%
    filter(period == pname) %>%
    select(person_id, period_weight = weight, period_steps = steps)

  # Merge baseline and period
  merged <- baseline_data %>%
    inner_join(period_data, by = "person_id") %>%
    filter(!is.na(baseline_weight), !is.na(period_weight),
           !is.na(baseline_steps), !is.na(period_steps))

  if (nrow(merged) < 20) {
    cat(sprintf("  WARNING: Only %d patients, skipping\n", nrow(merged)))
    next
  }

  # Calculate percent changes
  merged <- merged %>%
    mutate(
      weight_pct_change = 100 * (period_weight - baseline_weight) / baseline_weight,
      weight_change = period_weight - baseline_weight,
      steps_pct_change = 100 * (period_steps - baseline_steps) / baseline_steps,
      steps_change = period_steps - baseline_steps
    )

  # Categorize by step change
  merged <- merged %>%
    mutate(
      step_change_cat = case_when(
        steps_pct_change < -5 ~ "Decrease > 5%",
        steps_pct_change >= -5 & steps_pct_change <= 5 ~ "No change (-5% to +5%)",
        steps_pct_change > 5 ~ "Increase > 5%"
      )
    )

  # Summary by step change category
  summary_step_cat <- merged %>%
    group_by(step_change_cat) %>%
    summarize(
      n = n(),
      mean_steps_change = mean(steps_change),
      mean_steps_pct = mean(steps_pct_change),
      mean_weight_change = mean(weight_change),
      sd_weight_change = sd(weight_change),
      mean_weight_pct = mean(weight_pct_change),
      sd_weight_pct = sd(weight_pct_change),
      .groups = "drop"
    ) %>%
    arrange(step_change_cat)

  print(summary_step_cat, width = Inf)

  # ANOVA for weight change by step change category
  if (n_distinct(merged$step_change_cat) >= 2) {
    anova_weight <- aov(weight_change ~ step_change_cat, data = merged)

    cat("\nANOVA: Weight change by step change category\n")
    print(summary(anova_weight))

    # Post-hoc pairwise comparisons if ANOVA is significant
    anova_p <- summary(anova_weight)[[1]]$`Pr(>F)`[1]
    if (!is.na(anova_p) && anova_p < 0.05) {
      cat("\nPost-hoc pairwise comparisons (Tukey HSD):\n")
      posthoc <- TukeyHSD(anova_weight)
      print(posthoc)
    }
  }

  # Store results
  step_change_results[[pname]] <- list(
    summary = summary_step_cat,
    merged_data = merged
  )
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n\n=== SAVING RESULTS ===\n\n")

# Save weight loss stratification results
for (pname in names(weight_loss_results)) {
  # 3-category results
  write_csv(
    weight_loss_results[[pname]]$summary_3cat,
    sprintf("sensitivity_weight_loss_3cat_%s.csv", pname)
  )

  # 2-category results
  write_csv(
    weight_loss_results[[pname]]$summary_2cat,
    sprintf("sensitivity_weight_loss_2cat_%s.csv", pname)
  )

  cat(sprintf("  ✓ sensitivity_weight_loss_3cat_%s.csv\n", pname))
  cat(sprintf("  ✓ sensitivity_weight_loss_2cat_%s.csv\n", pname))
}

# Save step change stratification results
for (pname in names(step_change_results)) {
  write_csv(
    step_change_results[[pname]]$summary,
    sprintf("sensitivity_step_change_%s.csv", pname)
  )

  cat(sprintf("  ✓ sensitivity_step_change_%s.csv\n", pname))
}

# Save all results to RData
save(
  weight_loss_results,
  step_change_results,
  file = "sensitivity_analysis_results.RData"
)
cat("  ✓ sensitivity_analysis_results.RData\n")

cat("\n=============================================================================\n")
cat("MAIN ANALYSIS (1-90d, 91-180d, 181-365d) SENSITIVITY COMPLETE\n")
cat("=============================================================================\n")

# =============================================================================
# =============================================================================
# SHORT PERIODS SENSITIVITY ANALYSIS (1-30d, 31-90d, 91-180d, 181-365d)
# =============================================================================
# =============================================================================

if (file.exists("period_analysis_short_results.RData")) {

  cat("\n\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n")
  cat("SHORT PERIODS SENSITIVITY ANALYSIS\n")
  cat("=============================================================================\n")
  cat("=============================================================================\n\n")

  load("period_analysis_short_results.RData")

  cat("Loaded period_analysis_short_results.RData\n")
  cat(sprintf("  Periods analyzed: %d\n", nrow(comprehensive_table_short)))
  cat(sprintf("  Patients in long data: %d\n\n", n_distinct(all_data_long_short$person_id)))

  # =============================================================================
  # ANALYSIS 1: STRATIFY BY WEIGHT LOSS CATEGORY (SHORT PERIODS)
  # =============================================================================

  cat("=============================================================================\n")
  cat("ANALYSIS 1: ACTIVITY CHANGES BY WEIGHT LOSS CATEGORY (SHORT PERIODS)\n")
  cat("=============================================================================\n\n")

  # Get baseline data
  baseline_data_sens <- all_data_long_short %>%
    filter(period == "Baseline") %>%
    select(person_id, baseline_weight = weight, baseline_steps = steps,
           baseline_calories = calories)

  # Focus on key aggregated periods including 1-30d
  target_periods_short <- c("1-30d", "31-90d", "91-180d", "181-365d")

  weight_loss_results_short <- list()

  for (pname in target_periods_short) {
    cat(sprintf("\n### %s ###\n\n", pname))

    # Get period data
    period_data <- all_data_long_short %>%
      filter(period == pname) %>%
      select(person_id, period_weight = weight, period_steps = steps,
             period_calories = calories)

    # Merge baseline and period
    merged <- baseline_data_sens %>%
      inner_join(period_data, by = "person_id") %>%
      filter(!is.na(baseline_weight), !is.na(period_weight),
             !is.na(baseline_steps), !is.na(period_steps))

    if (nrow(merged) < 20) {
      cat(sprintf("  WARNING: Only %d patients, skipping\n", nrow(merged)))
      next
    }

    # Calculate percent changes
    merged <- merged %>%
      mutate(
        weight_pct_change = 100 * (period_weight - baseline_weight) / baseline_weight,
        steps_pct_change = 100 * (period_steps - baseline_steps) / baseline_steps,
        calories_change = period_calories - baseline_calories,
        steps_change = period_steps - baseline_steps
      )

    # Categorize by weight loss
    merged <- merged %>%
      mutate(
        weight_loss_cat3 = case_when(
          weight_pct_change > -5 ~ "< 5% loss",
          weight_pct_change <= -5 & weight_pct_change > -10 ~ "5-10% loss",
          weight_pct_change <= -10 ~ "> 10% loss"
        ),
        weight_loss_cat2 = case_when(
          weight_pct_change > -7.5 ~ "< 7.5% loss",
          weight_pct_change <= -7.5 ~ "≥ 7.5% loss"
        )
      )

    # 3-category analysis
    cat("--- 3-Category Weight Loss Analysis ---\n\n")

    summary_3cat <- merged %>%
      group_by(weight_loss_cat3) %>%
      summarize(
        n = n(),
        mean_weight_change = mean(period_weight - baseline_weight),
        mean_weight_pct = mean(weight_pct_change),
        mean_steps_change = mean(steps_change),
        sd_steps_change = sd(steps_change),
        mean_calories_change = mean(calories_change),
        sd_calories_change = sd(calories_change),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_weight_pct))

    print(summary_3cat)

    # ANOVA for steps
    if (all(table(merged$weight_loss_cat3) >= 3)) {
      anova_steps <- aov(steps_change ~ weight_loss_cat3, data = merged)
      p_val_steps <- summary(anova_steps)[[1]]$`Pr(>F)`[1]
      cat(sprintf("\nANOVA for steps change: p = %.4f\n", p_val_steps))
    }

    # ANOVA for calories
    if (all(table(merged$weight_loss_cat3) >= 3)) {
      anova_cal <- aov(calories_change ~ weight_loss_cat3, data = merged)
      p_val_cal <- summary(anova_cal)[[1]]$`Pr(>F)`[1]
      cat(sprintf("ANOVA for calories change: p = %.4f\n\n", p_val_cal))
    }

    # 2-category analysis
    cat("--- 2-Category Weight Loss Analysis (< 7.5% vs ≥ 7.5%) ---\n\n")

    summary_2cat <- merged %>%
      group_by(weight_loss_cat2) %>%
      summarize(
        n = n(),
        mean_weight_change = mean(period_weight - baseline_weight),
        mean_weight_pct = mean(weight_pct_change),
        mean_steps_change = mean(steps_change),
        sd_steps_change = sd(steps_change),
        mean_calories_change = mean(calories_change),
        sd_calories_change = sd(calories_change),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_weight_pct))

    print(summary_2cat)

    # T-test for steps
    if (all(table(merged$weight_loss_cat2) >= 3)) {
      t_test_steps <- t.test(steps_change ~ weight_loss_cat2, data = merged)
      cat(sprintf("\nT-test for steps change: p = %.4f\n", t_test_steps$p.value))
    }

    # T-test for calories
    if (all(table(merged$weight_loss_cat2) >= 3)) {
      t_test_cal <- t.test(calories_change ~ weight_loss_cat2, data = merged)
      cat(sprintf("T-test for calories change: p = %.4f\n", t_test_cal$p.value))
    }

    weight_loss_results_short[[pname]] <- list(
      summary_3cat = summary_3cat,
      summary_2cat = summary_2cat
    )
  }

  # =============================================================================
  # ANALYSIS 2: STRATIFY BY STEP CHANGE CATEGORY (SHORT PERIODS)
  # =============================================================================

  cat("\n\n=============================================================================\n")
  cat("ANALYSIS 2: WEIGHT CHANGES BY STEP CHANGE CATEGORY (SHORT PERIODS)\n")
  cat("=============================================================================\n\n")

  step_change_results_short <- list()

  for (pname in target_periods_short) {
    cat(sprintf("\n### %s ###\n\n", pname))

    # Get period data
    period_data <- all_data_long_short %>%
      filter(period == pname) %>%
      select(person_id, period_weight = weight, period_steps = steps)

    # Merge baseline and period
    merged <- baseline_data_sens %>%
      inner_join(period_data, by = "person_id") %>%
      filter(!is.na(baseline_weight), !is.na(period_weight),
             !is.na(baseline_steps), !is.na(period_steps))

    if (nrow(merged) < 20) {
      cat(sprintf("  WARNING: Only %d patients, skipping\n", nrow(merged)))
      next
    }

    # Calculate changes
    merged <- merged %>%
      mutate(
        weight_change = period_weight - baseline_weight,
        steps_change = period_steps - baseline_steps,
        steps_pct_change = 100 * steps_change / baseline_steps
      )

    # Categorize by step change
    merged <- merged %>%
      mutate(
        step_change_cat = case_when(
          steps_pct_change < -5 ~ "Decreased > 5%",
          steps_pct_change > 5 ~ "Increased > 5%",
          TRUE ~ "No change"
        )
      )

    summary_by_steps <- merged %>%
      group_by(step_change_cat) %>%
      summarize(
        n = n(),
        mean_steps_change = mean(steps_change),
        sd_steps_change = sd(steps_change),
        mean_steps_pct = mean(steps_pct_change),
        mean_weight_change = mean(weight_change),
        sd_weight_change = sd(weight_change),
        .groups = "drop"
      )

    print(summary_by_steps)

    # ANOVA for weight change
    if (all(table(merged$step_change_cat) >= 3)) {
      anova_weight <- aov(weight_change ~ step_change_cat, data = merged)
      p_val <- summary(anova_weight)[[1]]$`Pr(>F)`[1]
      cat(sprintf("\nANOVA for weight change: p = %.4f\n", p_val))
    }

    step_change_results_short[[pname]] <- list(
      summary = summary_by_steps
    )
  }

  # =============================================================================
  # SAVE SHORT PERIODS SENSITIVITY RESULTS
  # =============================================================================

  cat("\n\n=== SAVING SHORT PERIODS SENSITIVITY RESULTS ===\n\n")

  # Save weight loss stratification results
  for (pname in names(weight_loss_results_short)) {
    # 3-category results
    write_csv(
      weight_loss_results_short[[pname]]$summary_3cat,
      sprintf("sensitivity_weight_loss_3cat_short_%s.csv", pname)
    )

    # 2-category results
    write_csv(
      weight_loss_results_short[[pname]]$summary_2cat,
      sprintf("sensitivity_weight_loss_2cat_short_%s.csv", pname)
    )

    cat(sprintf("  ✓ sensitivity_weight_loss_3cat_short_%s.csv\n", pname))
    cat(sprintf("  ✓ sensitivity_weight_loss_2cat_short_%s.csv\n", pname))
  }

  # Save step change stratification results
  for (pname in names(step_change_results_short)) {
    write_csv(
      step_change_results_short[[pname]]$summary,
      sprintf("sensitivity_step_change_short_%s.csv", pname)
    )

    cat(sprintf("  ✓ sensitivity_step_change_short_%s.csv\n", pname))
  }

  # Save all results to RData
  save(
    weight_loss_results_short,
    step_change_results_short,
    file = "sensitivity_analysis_short_results.RData"
  )
  cat("  ✓ sensitivity_analysis_short_results.RData\n")

  cat("\n=============================================================================\n")
  cat("SHORT PERIODS SENSITIVITY ANALYSIS COMPLETE\n")
  cat("=============================================================================\n\n")

} else {
  cat("\n\nShort periods analysis results not found.\n")
  cat("Run period_analysis_optimized.R to generate short periods data.\n\n")
}

cat("\n=============================================================================\n")
cat("ALL SENSITIVITY ANALYSES COMPLETE\n")
cat("=============================================================================\n")
cat("\nKEY FINDINGS:\n")
cat("1. Analysis 1 shows how steps and calories change for patients with\n")
cat("   different levels of weight loss (< 5%, 5-10%, > 10% or < 7.5%, ≥ 7.5%)\n")
cat("2. Analysis 2 shows how weight changes for patients with different\n")
cat("   levels of step change (decrease > 5%, no change, increase > 5%)\n")
cat("3. Statistical tests (ANOVA, t-tests) assess whether differences are significant\n")
cat("4. SHORT PERIODS analysis includes early response (1-30d) for more granular insights\n")
cat("=============================================================================\n")
