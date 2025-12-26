# Interrupted Time Series (ITS) Analysis
# Tests for level and slope changes in activity at GLP-1 initiation (day 0)
# More rigorous than period comparisons - avoids cherry-picking windows
# Models weekly activity trends before/after treatment initiation

library(bigrquery)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(broom)
library(lme4)
library(lmerTest)

cat("\n##################################################\n")
cat("INTERRUPTED TIME SERIES (ITS) ANALYSIS\n")
cat("Testing level and slope change at initiation\n")
cat("##################################################\n\n")

# Connect to BigQuery
project_id <- "all-of-us-data-tools"
dataset_id <- "AoU_CDR_2024q2r2"
billing_project <- "idoaviv-tauber-org"

cat("Querying GLP-1 drug exposure data...\n")

# Query GLP-1 drug exposure
drug_query <- sprintf("
  SELECT
    de.person_id,
    de.drug_concept_id,
    de.drug_exposure_start_date,
    de.drug_exposure_end_date,
    c.concept_name
  FROM `%s.%s.drug_exposure` de
  JOIN `%s.%s.concept` c ON de.drug_concept_id = c.concept_id
  WHERE LOWER(c.concept_name) LIKE '%%semaglutide%%'
     OR LOWER(c.concept_name) LIKE '%%tirzepatide%%'
", project_id, dataset_id, project_id, dataset_id)

drug_glp1_raw <- bq_project_query(billing_project, drug_query) %>%
  bq_table_download()

# Clean drug data
drug_glp1_clean <- drug_glp1_raw %>%
  filter(!grepl("topical|recombinant|insulin|metformin|pioglitazone", concept_name, ignore.case = TRUE)) %>%
  mutate(
    drug_start_date = as.Date(drug_exposure_start_date),
    drug_end_date = as.Date(drug_exposure_end_date)
  ) %>%
  select(person_id, drug_concept_id, drug_start_date, drug_end_date, concept_name)

# Define initiation (first prescription)
glp1_initiation <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(glp1_initiation_date = min(drug_start_date, na.rm = TRUE), .groups = "drop")

# Require ≥2 fills for inclusion
patients_with_2plus_fills <- drug_glp1_clean %>%
  group_by(person_id) %>%
  summarize(n_fills = n_distinct(drug_start_date), .groups = "drop") %>%
  filter(n_fills >= 2) %>%
  pull(person_id)

glp1_initiation <- glp1_initiation %>%
  filter(person_id %in% patients_with_2plus_fills)

cat("Querying Fitbit activity data...\n")

# Query Fitbit activity data
activity_query <- sprintf("
  SELECT
    person_id,
    date AS activity_date,
    steps,
    activity_calories,
    sedentary_minutes,
    lightly_active_minutes,
    fairly_active_minutes,
    very_active_minutes
  FROM `%s.%s.activity_summary`
", project_id, dataset_id)

activity_raw <- bq_project_query(billing_project, activity_query) %>%
  bq_table_download()

# Clean activity data
activity_clean <- activity_raw %>%
  mutate(activity_date = as.Date(activity_date)) %>%
  filter(
    steps >= 100,
    steps <= 25000,
    !is.na(steps),
    sedentary_minutes + lightly_active_minutes + fairly_active_minutes + very_active_minutes <= 1440
  )

# Add wear time and proportional metrics
activity_clean <- activity_clean %>%
  mutate(
    total_wear_minutes = coalesce(sedentary_minutes, 0) +
                         coalesce(lightly_active_minutes, 0) +
                         coalesce(fairly_active_minutes, 0) +
                         coalesce(very_active_minutes, 0),
    is_valid_day = total_wear_minutes >= 600,
    MVPA = coalesce(fairly_active_minutes, 0) + coalesce(very_active_minutes, 0),
    pct_sedentary = if_else(total_wear_minutes > 0,
                            100 * sedentary_minutes / total_wear_minutes,
                            NA_real_),
    pct_MVPA = if_else(total_wear_minutes > 0,
                       100 * MVPA / total_wear_minutes,
                       NA_real_)
  )

# Merge with initiation dates
activity_with_glp1 <- activity_clean %>%
  inner_join(glp1_initiation, by = "person_id") %>%
  mutate(days_from_initiation = as.numeric(difftime(activity_date, glp1_initiation_date, units = "days")))

cat("Data loaded. Preparing time series...\n\n")

# Filter to ±365 days from initiation and valid days only
activity_its <- activity_with_glp1 %>%
  filter(days_from_initiation >= -365,
         days_from_initiation <= 365,
         is_valid_day == TRUE)

# Create week bins (7-day periods)
# Week 0 is initiation week
activity_its <- activity_its %>%
  mutate(
    week = floor(days_from_initiation / 7),
    post_intervention = if_else(days_from_initiation >= 0, 1, 0)
  )

cat("Aggregating to weekly data...\n")

# Aggregate to weekly level for each person
weekly_data <- activity_its %>%
  group_by(person_id, week) %>%
  summarize(
    n_valid_days = n(),
    mean_steps = mean(steps, na.rm = TRUE),
    mean_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    mean_MVPA = mean(MVPA, na.rm = TRUE),
    mean_pct_sedentary = mean(pct_sedentary, na.rm = TRUE),
    mean_pct_MVPA = mean(pct_MVPA, na.rm = TRUE),
    mean_wear_time = mean(total_wear_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_valid_days >= 3)  # Require ≥3 valid days per week

# Add intervention indicator and time_after_intervention
weekly_data <- weekly_data %>%
  mutate(
    post_intervention = if_else(week >= 0, 1, 0),
    time_after_intervention = if_else(week >= 0, week, 0)
  )

# Require patients with data both before and after initiation
patients_with_both_periods <- weekly_data %>%
  group_by(person_id) %>%
  summarize(
    has_pre = any(week < 0),
    has_post = any(week >= 0),
    .groups = "drop"
  ) %>%
  filter(has_pre & has_post) %>%
  pull(person_id)

weekly_data_filtered <- weekly_data %>%
  filter(person_id %in% patients_with_both_periods)

cat(sprintf("Final cohort: %d patients with weekly data before and after initiation\n\n",
            length(unique(weekly_data_filtered$person_id))))

# ========================================
# ITS MODEL 1: POOLED REGRESSION
# ========================================

cat("========================================\n")
cat("MODEL 1: POOLED ITS REGRESSION\n")
cat("========================================\n\n")

# Fit ITS model: Y = β0 + β1*week + β2*post + β3*time_after + error
# β2 = level change (immediate drop)
# β3 = slope change (change in trend)

cat("Fitting ITS models for each outcome...\n\n")

# Steps
model_steps <- lm(mean_steps ~ week + post_intervention + time_after_intervention,
                  data = weekly_data_filtered)
summary_steps <- summary(model_steps)
cat("STEPS MODEL:\n")
print(summary_steps$coefficients)
cat("\n")

# MVPA
model_mvpa <- lm(mean_MVPA ~ week + post_intervention + time_after_intervention,
                 data = weekly_data_filtered)
summary_mvpa <- summary(model_mvpa)
cat("MVPA MODEL:\n")
print(summary_mvpa$coefficients)
cat("\n")

# % Sedentary
model_pct_sed <- lm(mean_pct_sedentary ~ week + post_intervention + time_after_intervention,
                    data = weekly_data_filtered)
summary_pct_sed <- summary(model_pct_sed)
cat("% SEDENTARY MODEL:\n")
print(summary_pct_sed$coefficients)
cat("\n")

# ========================================
# ITS MODEL 2: MIXED EFFECTS
# ========================================

cat("\n========================================\n")
cat("MODEL 2: MIXED EFFECTS ITS\n")
cat("Random intercepts per patient\n")
cat("========================================\n\n")

# Steps
cat("Fitting mixed effects model for STEPS...\n")
model_steps_me <- lmer(mean_steps ~ week + post_intervention + time_after_intervention + (1 | person_id),
                       data = weekly_data_filtered)
summary_steps_me <- summary(model_steps_me)
cat("STEPS MIXED EFFECTS:\n")
print(summary_steps_me$coefficients)
cat("\n")

# MVPA
cat("Fitting mixed effects model for MVPA...\n")
model_mvpa_me <- lmer(mean_MVPA ~ week + post_intervention + time_after_intervention + (1 | person_id),
                      data = weekly_data_filtered)
summary_mvpa_me <- summary(model_mvpa_me)
cat("MVPA MIXED EFFECTS:\n")
print(summary_mvpa_me$coefficients)
cat("\n")

# % Sedentary
cat("Fitting mixed effects model for % SEDENTARY...\n")
model_pct_sed_me <- lmer(mean_pct_sedentary ~ week + post_intervention + time_after_intervention + (1 | person_id),
                         data = weekly_data_filtered)
summary_pct_sed_me <- summary(model_pct_sed_me)
cat("% SEDENTARY MIXED EFFECTS:\n")
print(summary_pct_sed_me$coefficients)
cat("\n")

# ========================================
# EXTRACT AND SAVE RESULTS
# ========================================

cat("\n========================================\n")
cat("EXTRACTING RESULTS\n")
cat("========================================\n\n")

# Extract coefficients from mixed effects models
results_its <- tibble(
  outcome = c("Steps", "MVPA", "% Sedentary"),

  # Pooled regression
  pooled_level_change = c(
    coef(model_steps)["post_intervention"],
    coef(model_mvpa)["post_intervention"],
    coef(model_pct_sed)["post_intervention"]
  ),
  pooled_level_p = c(
    summary_steps$coefficients["post_intervention", "Pr(>|t|)"],
    summary_mvpa$coefficients["post_intervention", "Pr(>|t|)"],
    summary_pct_sed$coefficients["post_intervention", "Pr(>|t|)"]
  ),
  pooled_slope_change = c(
    coef(model_steps)["time_after_intervention"],
    coef(model_mvpa)["time_after_intervention"],
    coef(model_pct_sed)["time_after_intervention"]
  ),
  pooled_slope_p = c(
    summary_steps$coefficients["time_after_intervention", "Pr(>|t|)"],
    summary_mvpa$coefficients["time_after_intervention", "Pr(>|t|)"],
    summary_pct_sed$coefficients["time_after_intervention", "Pr(>|t|)"]
  ),

  # Mixed effects
  me_level_change = c(
    fixef(model_steps_me)["post_intervention"],
    fixef(model_mvpa_me)["post_intervention"],
    fixef(model_pct_sed_me)["post_intervention"]
  ),
  me_level_p = c(
    summary_steps_me$coefficients["post_intervention", "Pr(>|t|)"],
    summary_mvpa_me$coefficients["post_intervention", "Pr(>|t|)"],
    summary_pct_sed_me$coefficients["post_intervention", "Pr(>|t|)"]
  ),
  me_slope_change = c(
    fixef(model_steps_me)["time_after_intervention"],
    fixef(model_mvpa_me)["time_after_intervention"],
    fixef(model_pct_sed_me)["time_after_intervention"]
  ),
  me_slope_p = c(
    summary_steps_me$coefficients["time_after_intervention", "Pr(>|t|)"],
    summary_mvpa_me$coefficients["time_after_intervention", "Pr(>|t|)"],
    summary_pct_sed_me$coefficients["time_after_intervention", "Pr(>|t|)"]
  )
)

print(results_its)

write.csv(results_its, "its_results.csv", row.names = FALSE)
cat("\nResults saved to: its_results.csv\n")

# ========================================
# VISUALIZATIONS
# ========================================

cat("\nGenerating visualizations...\n")

# Aggregate to mean per week across all patients
weekly_aggregate <- weekly_data_filtered %>%
  group_by(week) %>%
  summarize(
    n_patients = n_distinct(person_id),
    steps_mean = mean(mean_steps, na.rm = TRUE),
    steps_se = sd(mean_steps, na.rm = TRUE) / sqrt(n()),
    mvpa_mean = mean(mean_MVPA, na.rm = TRUE),
    mvpa_se = sd(mean_MVPA, na.rm = TRUE) / sqrt(n()),
    pct_sed_mean = mean(mean_pct_sedentary, na.rm = TRUE),
    pct_sed_se = sd(mean_pct_sedentary, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create predictions from mixed effects models
pred_data <- expand.grid(
  week = seq(-52, 52, by = 1),
  person_id = unique(weekly_data_filtered$person_id)[1]  # Use first patient as reference
) %>%
  mutate(
    post_intervention = if_else(week >= 0, 1, 0),
    time_after_intervention = if_else(week >= 0, week, 0)
  )

pred_data$steps_pred <- predict(model_steps_me, newdata = pred_data, re.form = NA)
pred_data$mvpa_pred <- predict(model_mvpa_me, newdata = pred_data, re.form = NA)
pred_data$pct_sed_pred <- predict(model_pct_sed_me, newdata = pred_data, re.form = NA)

# Plot 1: Steps ITS
p1 <- ggplot() +
  geom_point(data = weekly_aggregate, aes(x = week, y = steps_mean),
             alpha = 0.5, color = "gray40") +
  geom_line(data = pred_data, aes(x = week, y = steps_pred),
            color = "steelblue", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = 0, y = max(weekly_aggregate$steps_mean, na.rm = TRUE),
           label = "GLP-1 Initiation", color = "red", vjust = -0.5, size = 3.5) +
  labs(title = "Interrupted Time Series: Steps",
       subtitle = sprintf("Level change: %.1f steps (p=%.3f) | Slope change: %.2f/week (p=%.3f)",
                         results_its$me_level_change[1], results_its$me_level_p[1],
                         results_its$me_slope_change[1], results_its$me_slope_p[1]),
       x = "Weeks from GLP-1 Initiation",
       y = "Mean Steps per Day") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9))

# Plot 2: MVPA ITS
p2 <- ggplot() +
  geom_point(data = weekly_aggregate, aes(x = week, y = mvpa_mean),
             alpha = 0.5, color = "gray40") +
  geom_line(data = pred_data, aes(x = week, y = mvpa_pred),
            color = "coral", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = 0, y = max(weekly_aggregate$mvpa_mean, na.rm = TRUE),
           label = "GLP-1 Initiation", color = "red", vjust = -0.5, size = 3.5) +
  labs(title = "Interrupted Time Series: MVPA",
       subtitle = sprintf("Level change: %.1f min (p=%.3f) | Slope change: %.2f/week (p=%.3f)",
                         results_its$me_level_change[2], results_its$me_level_p[2],
                         results_its$me_slope_change[2], results_its$me_slope_p[2]),
       x = "Weeks from GLP-1 Initiation",
       y = "Mean MVPA (minutes/day)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9))

# Plot 3: % Sedentary ITS
p3 <- ggplot() +
  geom_point(data = weekly_aggregate, aes(x = week, y = pct_sed_mean),
             alpha = 0.5, color = "gray40") +
  geom_line(data = pred_data, aes(x = week, y = pct_sed_pred),
            color = "purple", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = 0, y = max(weekly_aggregate$pct_sed_mean, na.rm = TRUE),
           label = "GLP-1 Initiation", color = "red", vjust = -0.5, size = 3.5) +
  labs(title = "Interrupted Time Series: % Sedentary Time",
       subtitle = sprintf("Level change: %.1f%% (p=%.3f) | Slope change: %.2f/week (p=%.3f)",
                         results_its$me_level_change[3], results_its$me_level_p[3],
                         results_its$me_slope_change[3], results_its$me_slope_p[3]),
       x = "Weeks from GLP-1 Initiation",
       y = "% Sedentary Time (wear-adjusted)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9))

# Save plots
ggsave("its_steps.png", p1, width = 10, height = 6, dpi = 300)
ggsave("its_mvpa.png", p2, width = 10, height = 6, dpi = 300)
ggsave("its_pct_sedentary.png", p3, width = 10, height = 6, dpi = 300)

cat("\nPlots saved:\n")
cat("  - its_steps.png\n")
cat("  - its_mvpa.png\n")
cat("  - its_pct_sedentary.png\n")

cat("\n##################################################\n")
cat("ITS ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat("INTERPRETATION:\n")
cat("- Level change (β2): Immediate change at GLP-1 initiation\n")
cat("- Slope change (β3): Change in weekly trend after initiation\n")
cat("- Negative level change = immediate activity drop\n")
cat("- Negative slope change = accelerating decline over time\n")
cat("- Mixed effects accounts for repeated measures per patient\n\n")
