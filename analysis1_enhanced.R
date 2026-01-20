# =============================================================================
# ANALYSIS 1 ENHANCED: ACTIVITY BY BMI CLASS WITH TREND TESTS
# =============================================================================
# Run this after fitbit_bmi_stratification.R creates activity_final and bmi_final
# Adds: Demographics, Jonckheere-Terpstra trend test, Quantile regression,
#       Distribution plots, Scatter plots
# =============================================================================

library(tidyverse)
library(knitr)
library(patchwork)
library(quantreg)  # For quantile regression

cat("\n##################################################\n")
cat("ANALYSIS 1 ENHANCED: ACTIVITY BY BMI CLASS\n")
cat("With trend tests and demographics\n")
cat("##################################################\n\n")

# =============================================================================
# CHECK FOR REQUIRED DATA
# =============================================================================

if (!exists("activity_final") || !exists("bmi_final")) {
  stop("ERROR: Run fitbit_bmi_stratification.R first to create activity_final and bmi_final")
}

# =============================================================================
# LOAD DEMOGRAPHICS
# =============================================================================

cat("Loading demographics...\n")

# Check if person data already loaded
if (exists("dataset_50785095_person_df") && is.data.frame(dataset_50785095_person_df)) {
  cat("✓ Using pre-loaded person data\n")
  person_data <- dataset_50785095_person_df
} else if (exists("dataset_98104042_person_df") && is.data.frame(dataset_98104042_person_df)) {
  cat("✓ Using pre-loaded person data (dataset_98104042_person_df)\n")
  person_data <- dataset_98104042_person_df
} else {
  cat("Loading demographics from BigQuery...\n")
  person_sql <- paste("
    SELECT
        person_id,
        birth_datetime,
        gender_concept_id,
        sex_at_birth_concept_id
    FROM `person`
    WHERE person_id IN (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    )
  ")

  person_data <- bq_table_download(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), person_sql,
                     billing = Sys.getenv("GOOGLE_PROJECT"))
  )
}

# Calculate age
person_data <- person_data %>%
  mutate(
    age = as.numeric(difftime(Sys.Date(), as.Date(birth_datetime), units = "days")) / 365.25,
    sex = case_when(
      sex_at_birth_concept_id == 45878463 ~ "Female",
      sex_at_birth_concept_id == 45880669 ~ "Male",
      TRUE ~ "Unknown"
    )
  )

cat("✓ Demographics loaded\n\n")

# =============================================================================
# PREPARE ANALYSIS DATA
# =============================================================================

cat("Computing average BMI and activity per participant...\n")

# Average BMI per participant
participant_avg_bmi <- bmi_final %>%
  group_by(person_id) %>%
  summarize(
    avg_bmi = mean(bmi, na.rm = TRUE),
    n_bmi_measures = n(),
    .groups = "drop"
  ) %>%
  mutate(
    bmi_class = case_when(
      avg_bmi < 18 ~ "<18",
      avg_bmi >= 18 & avg_bmi < 25 ~ "18-25",
      avg_bmi >= 25 & avg_bmi < 30 ~ "25-30",
      avg_bmi >= 30 & avg_bmi < 35 ~ "30-35",
      avg_bmi >= 35 & avg_bmi < 40 ~ "35-40",
      avg_bmi >= 40 ~ "≥40"
    ),
    bmi_class = factor(bmi_class, levels = c("<18", "18-25", "25-30", "30-35", "35-40", "≥40")),
    bmi_class_num = as.numeric(bmi_class)  # For regression
  )

# Average activity per participant + wear days
participant_avg_activity <- activity_final %>%
  group_by(person_id) %>%
  summarize(
    avg_steps = mean(steps, na.rm = TRUE),
    avg_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    avg_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
    avg_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    avg_activity_calories = mean(activity_calories, na.rm = TRUE),
    n_wear_days = n(),
    .groups = "drop"
  )

# Merge all data
analysis1_data <- participant_avg_bmi %>%
  inner_join(participant_avg_activity, by = "person_id") %>%
  left_join(person_data %>% select(person_id, age, sex), by = "person_id")

cat(sprintf("Analysis cohort: %s participants\n\n", format(nrow(analysis1_data), big.mark = ",")))

# =============================================================================
# SUMMARY STATISTICS BY BMI CLASS
# =============================================================================

cat("Computing summary statistics by BMI class...\n")

summary_by_bmi <- analysis1_data %>%
  group_by(bmi_class) %>%
  summarize(
    N = n(),

    # Age
    Age_median = median(age, na.rm = TRUE),
    Age_q25 = quantile(age, 0.25, na.rm = TRUE),
    Age_q75 = quantile(age, 0.75, na.rm = TRUE),

    # Sex
    Female_n = sum(sex == "Female", na.rm = TRUE),
    Female_pct = 100 * mean(sex == "Female", na.rm = TRUE),

    # BMI
    BMI_median = median(avg_bmi, na.rm = TRUE),
    BMI_q25 = quantile(avg_bmi, 0.25, na.rm = TRUE),
    BMI_q75 = quantile(avg_bmi, 0.75, na.rm = TRUE),

    # Wear days
    WearDays_median = median(n_wear_days, na.rm = TRUE),
    WearDays_q25 = quantile(n_wear_days, 0.25, na.rm = TRUE),
    WearDays_q75 = quantile(n_wear_days, 0.75, na.rm = TRUE),

    # Steps
    Steps_median = median(avg_steps, na.rm = TRUE),
    Steps_q25 = quantile(avg_steps, 0.25, na.rm = TRUE),
    Steps_q75 = quantile(avg_steps, 0.75, na.rm = TRUE),

    # Sedentary
    Sedentary_median = median(avg_sedentary_min, na.rm = TRUE),
    Sedentary_q25 = quantile(avg_sedentary_min, 0.25, na.rm = TRUE),
    Sedentary_q75 = quantile(avg_sedentary_min, 0.75, na.rm = TRUE),

    # Light active
    LightActive_median = median(avg_lightly_active_min, na.rm = TRUE),
    LightActive_q25 = quantile(avg_lightly_active_min, 0.25, na.rm = TRUE),
    LightActive_q75 = quantile(avg_lightly_active_min, 0.75, na.rm = TRUE),

    # Fairly active
    FairlyActive_median = median(avg_fairly_active_min, na.rm = TRUE),
    FairlyActive_q25 = quantile(avg_fairly_active_min, 0.25, na.rm = TRUE),
    FairlyActive_q75 = quantile(avg_fairly_active_min, 0.75, na.rm = TRUE),

    # Very active
    VeryActive_median = median(avg_very_active_min, na.rm = TRUE),
    VeryActive_q25 = quantile(avg_very_active_min, 0.25, na.rm = TRUE),
    VeryActive_q75 = quantile(avg_very_active_min, 0.75, na.rm = TRUE),

    # Activity calories
    ActivityCal_median = median(avg_activity_calories, na.rm = TRUE),
    ActivityCal_q25 = quantile(avg_activity_calories, 0.25, na.rm = TRUE),
    ActivityCal_q75 = quantile(avg_activity_calories, 0.75, na.rm = TRUE),

    .groups = "drop"
  )

cat("✓ Summary statistics computed\n\n")

# =============================================================================
# STATISTICAL TESTS
# =============================================================================

cat("Running statistical tests...\n\n")

# Jonckheere-Terpstra trend test (nonparametric test for ordered alternatives)
cat("1. Jonckheere-Terpstra trend tests:\n")

# Install/load clinfun package for JT test
if (!requireNamespace("clinfun", quietly = TRUE)) {
  cat("  Installing clinfun package...\n")
  install.packages("clinfun", repos = "http://cran.us.r-project.org")
}
library(clinfun)

jt_steps <- jonckheere.test(analysis1_data$avg_steps, as.numeric(analysis1_data$bmi_class),
                            alternative = "decreasing")
jt_sedentary <- jonckheere.test(analysis1_data$avg_sedentary_min, as.numeric(analysis1_data$bmi_class),
                                alternative = "increasing")
jt_calories <- jonckheere.test(analysis1_data$avg_activity_calories, as.numeric(analysis1_data$bmi_class),
                              alternative = "decreasing")

cat(sprintf("  Steps: J-T statistic = %.2f, p = %.2e\n", jt_steps$statistic, jt_steps$p.value))
cat(sprintf("  Sedentary: J-T statistic = %.2f, p = %.2e\n", jt_sedentary$statistic, jt_sedentary$p.value))
cat(sprintf("  Activity calories: J-T statistic = %.2f, p = %.2e\n\n", jt_calories$statistic, jt_calories$p.value))

# Quantile regression (median regression with ordinal BMI class)
cat("2. Quantile regression (median) for trend:\n")

qr_steps <- rq(avg_steps ~ bmi_class_num, data = analysis1_data, tau = 0.5)
qr_sedentary <- rq(avg_sedentary_min ~ bmi_class_num, data = analysis1_data, tau = 0.5)
qr_calories <- rq(avg_activity_calories ~ bmi_class_num, data = analysis1_data, tau = 0.5)

# Get summary with confidence intervals
qr_steps_sum <- summary(qr_steps, se = "boot")
qr_sedentary_sum <- summary(qr_sedentary, se = "boot")
qr_calories_sum <- summary(qr_calories, se = "boot")

cat(sprintf("  Steps: %.0f steps/day per BMI class increase (95%% CI: %.0f to %.0f), p = %.2e\n",
            coef(qr_steps)[2],
            qr_steps_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_steps)[2],
            qr_steps_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_steps)[2],
            qr_steps_sum$coefficients[2,4]))

cat(sprintf("  Sedentary: %.0f min/day per BMI class increase (95%% CI: %.0f to %.0f), p = %.2e\n",
            coef(qr_sedentary)[2],
            qr_sedentary_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_sedentary)[2],
            qr_sedentary_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_sedentary)[2],
            qr_sedentary_sum$coefficients[2,4]))

cat(sprintf("  Activity calories: %.0f cal/day per BMI class increase (95%% CI: %.0f to %.0f), p = %.2e\n\n",
            coef(qr_calories)[2],
            qr_calories_sum$coefficients[2,2] * qnorm(0.025) + coef(qr_calories)[2],
            qr_calories_sum$coefficients[2,2] * qnorm(0.975) + coef(qr_calories)[2],
            qr_calories_sum$coefficients[2,4]))

# Kruskal-Wallis (overall difference test)
cat("3. Kruskal-Wallis tests (overall group differences):\n")
kw_steps <- kruskal.test(avg_steps ~ bmi_class, data = analysis1_data)
kw_sedentary <- kruskal.test(avg_sedentary_min ~ bmi_class, data = analysis1_data)
kw_calories <- kruskal.test(avg_activity_calories ~ bmi_class, data = analysis1_data)

cat(sprintf("  Steps: χ² = %.2f, p = %.2e\n", kw_steps$statistic, kw_steps$p.value))
cat(sprintf("  Sedentary: χ² = %.2f, p = %.2e\n", kw_sedentary$statistic, kw_sedentary$p.value))
cat(sprintf("  Activity calories: χ² = %.2f, p = %.2e\n\n", kw_calories$statistic, kw_calories$p.value))

# =============================================================================
# CREATE FORMATTED TABLE
# =============================================================================

cat("Creating formatted summary table...\n")

table1_formatted <- summary_by_bmi %>%
  mutate(
    `Age (years)` = sprintf("%.0f (%.0f-%.0f)", Age_median, Age_q25, Age_q75),
    `Female, n (%)` = sprintf("%d (%.1f)", Female_n, Female_pct),
    `BMI` = sprintf("%.1f (%.1f-%.1f)", BMI_median, BMI_q25, BMI_q75),
    `Wear Days` = sprintf("%.0f (%.0f-%.0f)", WearDays_median, WearDays_q25, WearDays_q75),
    `Steps/day` = sprintf("%.0f (%.0f-%.0f)", Steps_median, Steps_q25, Steps_q75),
    `Sedentary (min/day)` = sprintf("%.0f (%.0f-%.0f)", Sedentary_median, Sedentary_q25, Sedentary_q75),
    `Light Active (min/day)` = sprintf("%.0f (%.0f-%.0f)", LightActive_median, LightActive_q25, LightActive_q75),
    `Fairly Active (min/day)` = sprintf("%.0f (%.0f-%.0f)", FairlyActive_median, FairlyActive_q25, FairlyActive_q75),
    `Very Active (min/day)` = sprintf("%.0f (%.0f-%.0f)", VeryActive_median, VeryActive_q25, VeryActive_q75),
    `Activity Calories` = sprintf("%.0f (%.0f-%.0f)", ActivityCal_median, ActivityCal_q25, ActivityCal_q75)
  ) %>%
  select(`BMI Class` = bmi_class, N, `Age (years)`, `Female, n (%)`, BMI, `Wear Days`,
         `Steps/day`, `Sedentary (min/day)`, `Light Active (min/day)`,
         `Fairly Active (min/day)`, `Very Active (min/day)`, `Activity Calories`)

# Add footnote about valid Fitbit days
footnote_text <- paste(
  "<p><strong>Note:</strong> Valid Fitbit days defined as days with: ",
  "steps between 100-50,000; sedentary, light, fairly, and very active minutes each 0-1,440; ",
  "total activity minutes ≤1,440 per day. Participants required to have >30 valid days. ",
  "Values shown as median (IQR).</p>",
  "<p><strong>Statistical tests:</strong> ",
  sprintf("Jonckheere-Terpstra trend test for steps: p = %.2e; ", jt_steps$p.value),
  sprintf("sedentary: p = %.2e; ", jt_sedentary$p.value),
  sprintf("activity calories: p = %.2e. ", jt_calories$p.value),
  sprintf("Quantile regression per BMI class: steps %.0f/day (p = %.2e); ",
          coef(qr_steps)[2], qr_steps_sum$coefficients[2,4]),
  sprintf("sedentary %.0f min/day (p = %.2e); ",
          coef(qr_sedentary)[2], qr_sedentary_sum$coefficients[2,4]),
  sprintf("calories %.0f cal/day (p = %.2e).</p>",
          coef(qr_calories)[2], qr_calories_sum$coefficients[2,4])
)

html_table <- knitr::kable(table1_formatted,
                           format = "html",
                           caption = "Table 1: Activity Measures by BMI Class",
                           align = c("l", rep("r", ncol(table1_formatted)-1)))

# Add footnote
html_output <- paste(html_table, footnote_text, sep = "\n")

writeLines(html_output, "analysis1_enhanced_table.html")
cat("✓ Saved: analysis1_enhanced_table.html\n")

# Save raw summary
write_csv(summary_by_bmi, "analysis1_enhanced_summary.csv")
cat("✓ Saved: analysis1_enhanced_summary.csv\n\n")

# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

cat("Creating visualizations...\n\n")

# 1. Distribution of daily steps by BMI class
cat("  1. Creating step distribution plot...\n")

p1_dist <- ggplot(analysis1_data, aes(x = avg_steps, fill = bmi_class, color = bmi_class)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 20000)) +
  labs(
    title = "Distribution of Daily Steps by BMI Class",
    subtitle = sprintf("Jonckheere-Terpstra p = %.2e (decreasing trend)", jt_steps$p.value),
    x = "Average Daily Steps",
    y = "Density",
    fill = "BMI Class",
    color = "BMI Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("analysis1_enhanced_step_distribution.png", p1_dist,
       width = 10, height = 6, dpi = 300, bg = "white")
ggsave("analysis1_enhanced_step_distribution.pdf", p1_dist,
       width = 10, height = 6)

cat("  ✓ Saved: analysis1_enhanced_step_distribution.png/pdf\n")

# 2. Scatter plot of steps by BMI with trend line
cat("  2. Creating scatter plot with trend line...\n")

p2_scatter <- ggplot(analysis1_data, aes(x = avg_bmi, y = avg_steps)) +
  geom_point(aes(color = bmi_class), alpha = 0.4, size = 1) +
  geom_smooth(method = "loess", color = "#d73027", se = TRUE, linewidth = 1.5) +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Daily Steps by BMI",
    subtitle = sprintf("Quantile regression: %.0f steps/day per BMI class (p = %.2e)",
                      coef(qr_steps)[2], qr_steps_sum$coefficients[2,4]),
    x = "BMI",
    y = "Average Daily Steps",
    color = "BMI Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("analysis1_enhanced_scatter_bmi_steps.png", p2_scatter,
       width = 10, height = 6, dpi = 300, bg = "white")
ggsave("analysis1_enhanced_scatter_bmi_steps.pdf", p2_scatter,
       width = 10, height = 6)

cat("  ✓ Saved: analysis1_enhanced_scatter_bmi_steps.png/pdf\n")

# 3. Violin plots for main outcomes
cat("  3. Creating violin plots...\n")

p3_violin_steps <- ggplot(analysis1_data, aes(x = bmi_class, y = avg_steps, fill = bmi_class)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Daily Steps by BMI Class",
    x = "BMI Class",
    y = "Average Daily Steps"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

p3_violin_sedentary <- ggplot(analysis1_data, aes(x = bmi_class, y = avg_sedentary_min, fill = bmi_class)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Sedentary Minutes by BMI Class",
    x = "BMI Class",
    y = "Sedentary Minutes/Day"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

p3_combined_violin <- p3_violin_steps | p3_violin_sedentary

ggsave("analysis1_enhanced_violin_plots.png", p3_combined_violin,
       width = 14, height = 6, dpi = 300, bg = "white")
ggsave("analysis1_enhanced_violin_plots.pdf", p3_combined_violin,
       width = 14, height = 6)

cat("  ✓ Saved: analysis1_enhanced_violin_plots.png/pdf\n\n")

# =============================================================================
# SAVE STATISTICAL TEST RESULTS
# =============================================================================

cat("Saving statistical test results...\n")

stat_results <- data.frame(
  Test = c("Jonckheere-Terpstra", "Jonckheere-Terpstra", "Jonckheere-Terpstra",
           "Quantile Regression", "Quantile Regression", "Quantile Regression",
           "Kruskal-Wallis", "Kruskal-Wallis", "Kruskal-Wallis"),
  Measure = rep(c("Steps", "Sedentary", "Activity Calories"), 3),
  Statistic = c(jt_steps$statistic, jt_sedentary$statistic, jt_calories$statistic,
                coef(qr_steps)[2], coef(qr_sedentary)[2], coef(qr_calories)[2],
                kw_steps$statistic, kw_sedentary$statistic, kw_calories$statistic),
  p_value = c(jt_steps$p.value, jt_sedentary$p.value, jt_calories$p.value,
              qr_steps_sum$coefficients[2,4], qr_sedentary_sum$coefficients[2,4],
              qr_calories_sum$coefficients[2,4],
              kw_steps$p.value, kw_sedentary$p.value, kw_calories$p.value)
) %>%
  mutate(
    Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

write_csv(stat_results, "analysis1_enhanced_statistical_tests.csv")
cat("✓ Saved: analysis1_enhanced_statistical_tests.csv\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("ANALYSIS 1 ENHANCED COMPLETE\n")
cat("##################################################\n\n")

cat("Output files:\n")
cat("  Tables:\n")
cat("    - analysis1_enhanced_table.html (with demographics and footnotes)\n")
cat("    - analysis1_enhanced_summary.csv (raw summary statistics)\n")
cat("    - analysis1_enhanced_statistical_tests.csv (all test results)\n\n")
cat("  Figures:\n")
cat("    - analysis1_enhanced_step_distribution.png/pdf\n")
cat("    - analysis1_enhanced_scatter_bmi_steps.png/pdf\n")
cat("    - analysis1_enhanced_violin_plots.png/pdf\n\n")

cat("Key findings:\n")
cat(sprintf("  Jonckheere-Terpstra trend (steps): p = %.2e\n", jt_steps$p.value))
cat(sprintf("  Quantile regression (steps): %.0f steps/day per BMI class\n", coef(qr_steps)[2]))
cat(sprintf("  Total participants: %s across %d BMI classes\n",
            format(nrow(analysis1_data), big.mark = ","),
            length(unique(analysis1_data$bmi_class))))

cat("\nDone!\n\n")
