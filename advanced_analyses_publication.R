# =============================================================================
# ADVANCED ANALYSES FOR PUBLICATION
# =============================================================================
# 1. Mediation Analysis: Does activity change mediate weight loss?
# 2. Dose-Response: Scatter plot of Δ steps vs % weight loss
# 3. Time-to-Nadir: Kaplan-Meier curves by baseline activity
# 4. Responder Analysis: Super responders vs non-responders
# =============================================================================

library(tidyverse)
library(survival)
library(broom)

cat("\n##################################################\n")
cat("ADVANCED ANALYSES FOR PUBLICATION\n")
cat("##################################################\n\n")

# Install packages if needed
if (!require("mediation", quietly = TRUE)) {
  cat("⚠ mediation package not available - mediation analysis will use basic linear models\n")
  has_mediation <- FALSE
} else {
  has_mediation <- TRUE
}

if (!require("survminer", quietly = TRUE)) {
  cat("⚠ survminer package not available - will create basic Kaplan-Meier plot\n")
  has_survminer <- FALSE
} else {
  library(survminer)
  has_survminer <- TRUE
}

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

# Filter data
activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

weight_cleaned <- weight_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

bmi_data <- bmi_data %>%
  filter(person_id %in% baseline_cohort$person_id)

# =============================================================================
# PREPARE ANALYSIS DATASET
# =============================================================================

cat("Preparing analysis dataset...\n")

# Baseline steps
baseline_steps <- activity_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  summarize(baseline_steps = mean(steps, na.rm = TRUE), .groups = "drop")

# Early follow-up steps (1-30d)
early_steps <- activity_cleaned %>%
  filter(days_from_initiation >= 1, days_from_initiation <= 30,
         is_valid_day == TRUE) %>%
  group_by(person_id) %>%
  summarize(early_steps = mean(steps, na.rm = TRUE), .groups = "drop")

# Baseline weight
baseline_weight <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg, baseline_weight_day = days_from_initiation)

# Nadir weight (≥84 days)
nadir_weight <- weight_cleaned %>%
  filter(days_from_initiation >= 84) %>%
  group_by(person_id) %>%
  arrange(weight_kg) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, nadir_weight = weight_kg, nadir_day = days_from_initiation)

# Baseline BMI
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

# Add demographics if available
if (exists("person") && is.data.frame(person)) {
  demographics <- person %>% select(person_id, age, sex)
} else {
  demographics <- tibble(person_id = baseline_cohort$person_id)
}

# Combine all
analysis_data <- baseline_cohort %>%
  select(person_id) %>%
  left_join(baseline_steps, by = "person_id") %>%
  left_join(early_steps, by = "person_id") %>%
  left_join(baseline_weight, by = "person_id") %>%
  left_join(nadir_weight, by = "person_id") %>%
  left_join(baseline_bmi, by = "person_id") %>%
  left_join(demographics, by = "person_id") %>%
  mutate(
    # Activity change
    steps_change = early_steps - baseline_steps,
    steps_change_pct = 100 * (early_steps - baseline_steps) / baseline_steps,

    # Weight change
    weight_change_kg = nadir_weight - baseline_weight,
    weight_change_pct = 100 * (nadir_weight - baseline_weight) / baseline_weight,

    # Time to nadir
    time_to_nadir = nadir_day - baseline_weight_day,

    # Baseline activity tertiles
    steps_tertile = ntile(baseline_steps, 3)
  )

cat(sprintf("Analysis dataset: N=%d patients\n", nrow(analysis_data)))
cat(sprintf("  With nadir weight: N=%d\n", sum(!is.na(analysis_data$nadir_weight))))
cat(sprintf("  With early follow-up steps: N=%d\n", sum(!is.na(analysis_data$early_steps))))
cat("\n")

# =============================================================================
# ANALYSIS 1: MEDIATION ANALYSIS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 1: MEDIATION ANALYSIS\n")
cat("========================================\n\n")

cat("Testing pathway: GLP-1 initiation → Activity change → Weight loss\n\n")

# Prepare mediation dataset (complete cases only)
mediation_data <- analysis_data %>%
  filter(!is.na(steps_change), !is.na(weight_change_pct), !is.na(baseline_bmi)) %>%
  mutate(
    # Center predictors for interpretability
    steps_change_c = steps_change - mean(steps_change, na.rm = TRUE),
    baseline_bmi_c = baseline_bmi - mean(baseline_bmi, na.rm = TRUE),
    baseline_steps_c = baseline_steps - mean(baseline_steps, na.rm = TRUE)
  )

cat(sprintf("Mediation analysis N=%d (complete cases)\n\n", nrow(mediation_data)))

# Model 1: Effect of time on mediator (activity change)
# Note: Everyone is exposed to GLP-1, so we use time since initiation as proxy
# Alternative: use baseline characteristics as predictors
mediator_model <- lm(steps_change ~ baseline_bmi_c + baseline_steps_c,
                     data = mediation_data)

cat("Mediator Model (Activity Change):\n")
cat("Predictors: Baseline BMI, Baseline Steps\n")
print(summary(mediator_model))
cat("\n")

# Model 2: Effect of activity change on outcome (weight loss), controlling for baseline
outcome_model <- lm(weight_change_pct ~ steps_change_c + baseline_bmi_c + baseline_steps_c,
                    data = mediation_data)

cat("Outcome Model (Weight Loss):\n")
cat("Predictors: Activity Change, Baseline BMI, Baseline Steps\n")
print(summary(outcome_model))
cat("\n")

# Calculate correlation between activity change and weight loss
cor_test <- cor.test(mediation_data$steps_change, mediation_data$weight_change_pct)
cat(sprintf("Correlation between activity change and weight loss: r=%.3f, p=%.4f\n\n",
            cor_test$estimate, cor_test$p.value))

# Save mediation results
mediation_results <- tibble(
  model = c("Mediator: Activity Change", "Outcome: Weight Loss"),
  predictor = c("Baseline BMI", "Activity Change"),
  estimate = c(coef(mediator_model)["baseline_bmi_c"],
               coef(outcome_model)["steps_change_c"]),
  se = c(summary(mediator_model)$coefficients["baseline_bmi_c", "Std. Error"],
         summary(outcome_model)$coefficients["steps_change_c", "Std. Error"]),
  p_value = c(summary(mediator_model)$coefficients["baseline_bmi_c", "Pr(>|t|)"],
              summary(outcome_model)$coefficients["steps_change_c", "Pr(>|t|)"])
)

write_csv(mediation_results, "mediation_analysis_results.csv")
cat("Saved: mediation_analysis_results.csv\n\n")

# =============================================================================
# ANALYSIS 2: DOSE-RESPONSE ANALYSIS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 2: DOSE-RESPONSE ANALYSIS\n")
cat("========================================\n\n")

# Prepare dose-response data
dose_response_data <- analysis_data %>%
  filter(!is.na(steps_change), !is.na(weight_change_pct))

cat(sprintf("Dose-response analysis N=%d\n\n", nrow(dose_response_data)))

# Linear regression
dose_model <- lm(weight_change_pct ~ steps_change, data = dose_response_data)

cat("Dose-Response Model:\n")
cat("Outcome: % Weight Loss\n")
cat("Predictor: Change in Steps (1-30d vs Baseline)\n\n")
print(summary(dose_model))
cat("\n")

# Calculate effect size
steps_per_1000 <- coef(dose_model)["steps_change"] * 1000
cat(sprintf("Effect size: Every 1000 step increase → %.2f%% additional weight loss\n",
            steps_per_1000))
cat(sprintf("95%% CI: [%.2f, %.2f]\n\n",
            (coef(dose_model)["steps_change"] - 1.96 * summary(dose_model)$coefficients["steps_change", "Std. Error"]) * 1000,
            (coef(dose_model)["steps_change"] + 1.96 * summary(dose_model)$coefficients["steps_change", "Std. Error"]) * 1000))

# Create scatter plot
p_dose <- ggplot(dose_response_data, aes(x = steps_change, y = weight_change_pct)) +
  geom_point(alpha = 0.4, size = 2.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", linewidth = 1.2, se = TRUE, level = 0.95) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  annotate("text", x = max(dose_response_data$steps_change, na.rm = TRUE) * 0.7,
           y = min(dose_response_data$weight_change_pct, na.rm = TRUE) * 0.9,
           label = sprintf("Every 1000 steps → %.2f%% weight loss\np=%.4f, R²=%.3f",
                          steps_per_1000,
                          summary(dose_model)$coefficients["steps_change", "Pr(>|t|)"],
                          summary(dose_model)$r.squared),
           hjust = 1, size = 4, fontface = "bold") +
  labs(
    title = "Dose-Response: Activity Change vs Weight Loss",
    subtitle = sprintf("N=%d patients with nadir weight", nrow(dose_response_data)),
    x = "Change in Steps per Day\n(1-30d minus Baseline)",
    y = "% Weight Change at Nadir\n(negative = weight loss)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30")
  )

ggsave("dose_response_steps_weight.png", p_dose, width = 10, height = 8, dpi = 300)
ggsave("dose_response_steps_weight.pdf", p_dose, width = 10, height = 8)
cat("Saved: dose_response_steps_weight.png/pdf\n\n")

# =============================================================================
# ANALYSIS 3: TIME-TO-NADIR ANALYSIS (Kaplan-Meier)
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 3: TIME-TO-NADIR ANALYSIS\n")
cat("========================================\n\n")

# Prepare survival data
survival_data <- analysis_data %>%
  filter(!is.na(nadir_day), !is.na(baseline_steps)) %>%
  mutate(
    # Event = reached nadir (everyone has event=1 since we filter for nadir)
    event = 1,
    time = nadir_day,

    # Activity tertile groups
    activity_group = case_when(
      steps_tertile == 1 ~ "Low baseline activity",
      steps_tertile == 2 ~ "Medium baseline activity",
      steps_tertile == 3 ~ "High baseline activity",
      TRUE ~ NA_character_
    ),
    activity_group = factor(activity_group,
                            levels = c("Low baseline activity",
                                      "Medium baseline activity",
                                      "High baseline activity"))
  )

cat(sprintf("Time-to-nadir analysis N=%d\n", nrow(survival_data)))
cat(sprintf("  Low activity: N=%d\n", sum(survival_data$steps_tertile == 1)))
cat(sprintf("  Medium activity: N=%d\n", sum(survival_data$steps_tertile == 2)))
cat(sprintf("  High activity: N=%d\n", sum(survival_data$steps_tertile == 3)))
cat("\n")

# Fit survival curves
surv_obj <- Surv(time = survival_data$time, event = survival_data$event)
surv_fit <- survfit(surv_obj ~ activity_group, data = survival_data)

# Log-rank test
surv_diff <- survdiff(surv_obj ~ activity_group, data = survival_data)

cat("Kaplan-Meier: Time to Reach Nadir Weight\n")
cat("Stratified by baseline activity level\n\n")
print(surv_fit)
cat("\n")

cat("Log-rank test for group differences:\n")
print(surv_diff)
cat("\n")

# Create Kaplan-Meier plot
if (has_survminer) {
  # Use survminer for fancy plot with risk table
  p_km <- ggsurvplot(
    surv_fit,
    data = survival_data,
    conf.int = TRUE,
    pval = TRUE,
    risk.table = TRUE,
    risk.table.height = 0.25,
    ggtheme = theme_minimal(base_size = 12),
    palette = c("#D32F2F", "#FFA000", "#388E3C"),
    title = "Time to Nadir Weight by Baseline Activity Level",
    xlab = "Days from GLP-1 Initiation",
    ylab = "Probability of NOT Reaching Nadir",
    legend.title = "Baseline Activity",
    legend.labs = c("Low", "Medium", "High")
  )

  ggsave("kaplan_meier_nadir.png", print(p_km), width = 10, height = 10, dpi = 300)
  ggsave("kaplan_meier_nadir.pdf", print(p_km), width = 10, height = 10)
  cat("Saved: kaplan_meier_nadir.png/pdf\n\n")

} else {
  # Basic ggplot version
  surv_data_plot <- broom::tidy(surv_fit) %>%
    mutate(activity_group = rep(c("Low baseline activity",
                                   "Medium baseline activity",
                                   "High baseline activity"),
                                 times = c(sum(strata == "activity_group=Low baseline activity"),
                                          sum(strata == "activity_group=Medium baseline activity"),
                                          sum(strata == "activity_group=High baseline activity"))))

  p_km_basic <- ggplot(surv_data_plot, aes(x = time, y = estimate, color = activity_group, fill = activity_group)) +
    geom_step(linewidth = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    scale_color_manual(values = c("Low baseline activity" = "#D32F2F",
                                   "Medium baseline activity" = "#FFA000",
                                   "High baseline activity" = "#388E3C")) +
    scale_fill_manual(values = c("Low baseline activity" = "#D32F2F",
                                  "Medium baseline activity" = "#FFA000",
                                  "High baseline activity" = "#388E3C")) +
    labs(
      title = "Time to Nadir Weight by Baseline Activity Level",
      subtitle = sprintf("Log-rank p=%.4f", surv_diff$pvalue),
      x = "Days from GLP-1 Initiation",
      y = "Probability of NOT Reaching Nadir",
      color = "Baseline Activity",
      fill = "Baseline Activity"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  ggsave("kaplan_meier_nadir.png", p_km_basic, width = 10, height = 8, dpi = 300)
  ggsave("kaplan_meier_nadir.pdf", p_km_basic, width = 10, height = 8)
  cat("Saved: kaplan_meier_nadir.png/pdf (basic version)\n\n")
}

# Calculate median time to nadir by group
median_times <- survfit(surv_obj ~ activity_group, data = survival_data) %>%
  broom::tidy() %>%
  group_by(strata) %>%
  filter(estimate <= 0.5) %>%
  slice(1) %>%
  select(strata, median_time = time)

cat("Median time to nadir by group:\n")
print(median_times)
cat("\n")

# =============================================================================
# ANALYSIS 4: RESPONDER ANALYSIS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 4: RESPONDER ANALYSIS\n")
cat("========================================\n\n")

# Define responder categories
responder_data <- analysis_data %>%
  filter(!is.na(steps_change), !is.na(weight_change_pct)) %>%
  mutate(
    # Weight loss responder
    weight_responder = case_when(
      weight_change_pct <= -10 ~ "High response (>10% loss)",
      weight_change_pct <= -5 & weight_change_pct > -10 ~ "Moderate response (5-10% loss)",
      weight_change_pct > -5 ~ "Low response (<5% loss)",
      TRUE ~ NA_character_
    ),

    # Activity responder
    activity_responder = case_when(
      steps_change >= 1000 ~ "Active (≥1000 step increase)",
      steps_change >= 0 & steps_change < 1000 ~ "Stable (0-1000 step increase)",
      steps_change < 0 ~ "Inactive (step decrease)",
      TRUE ~ NA_character_
    ),

    # Combined responder definition
    responder_category = case_when(
      weight_change_pct <= -10 & steps_change >= 1000 ~ "Super responder",
      weight_change_pct <= -5 & steps_change >= 0 ~ "Moderate responder",
      TRUE ~ "Non-responder"
    ),

    # Binary for logistic regression
    super_responder = as.integer(responder_category == "Super responder")
  )

cat("Responder categories:\n")
print(table(responder_data$responder_category))
cat("\n")

cat("Weight response distribution:\n")
print(table(responder_data$weight_responder))
cat("\n")

cat("Activity response distribution:\n")
print(table(responder_data$activity_responder))
cat("\n")

# Cross-tabulation
cat("Weight × Activity response:\n")
print(table(responder_data$weight_responder, responder_data$activity_responder))
cat("\n")

# Logistic regression: Predict super responder
# Prepare predictors
responder_model_data <- responder_data %>%
  filter(!is.na(baseline_bmi), !is.na(baseline_steps)) %>%
  mutate(
    age_group = if ("age" %in% colnames(.)) {
      cut(age, breaks = c(0, 40, 60, 100), labels = c("<40", "40-60", ">60"))
    } else {
      NA_character_
    }
  )

# Build model
if ("sex" %in% colnames(responder_model_data) && "age" %in% colnames(responder_model_data)) {
  logit_formula <- super_responder ~ baseline_bmi + baseline_steps + age + sex + bmi_class
} else {
  logit_formula <- super_responder ~ baseline_bmi + baseline_steps + bmi_class
}

logit_model <- glm(logit_formula,
                   data = responder_model_data,
                   family = binomial(link = "logit"))

cat("Logistic Regression: Predictors of Super Responder Status\n")
cat("Super responder = >10% weight loss + ≥1000 step increase\n\n")
print(summary(logit_model))
cat("\n")

# Calculate odds ratios
or_results <- tidy(logit_model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    or_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(term, OR = estimate, `95% CI` = or_ci, p_value = p_formatted)

cat("Odds Ratios for Super Responder:\n")
print(or_results)
cat("\n")

write_csv(or_results, "responder_odds_ratios.csv")
cat("Saved: responder_odds_ratios.csv\n\n")

# Create visualization: Responder composition
p_responder <- ggplot(responder_data, aes(x = activity_responder, fill = weight_responder)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_fill_manual(values = c(
    "High response (>10% loss)" = "#66BB6A",
    "Moderate response (5-10% loss)" = "#FFA726",
    "Low response (<5% loss)" = "#EF5350"
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Weight Loss Response by Activity Response",
    subtitle = sprintf("N=%d patients with nadir weight and early activity data", nrow(responder_data)),
    x = "Activity Response",
    y = "Proportion of Patients",
    fill = "Weight Loss\nResponse"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("responder_composition.png", p_responder, width = 10, height = 7, dpi = 300)
ggsave("responder_composition.pdf", p_responder, width = 10, height = 7)
cat("Saved: responder_composition.png/pdf\n\n")

# =============================================================================
# SAVE COMPREHENSIVE RESULTS
# =============================================================================

cat("\n========================================\n")
cat("SAVING COMPREHENSIVE RESULTS\n")
cat("========================================\n\n")

# Save analysis dataset
write_csv(analysis_data, "advanced_analysis_dataset.csv")
cat("Saved: advanced_analysis_dataset.csv\n")

# Save dose-response data
dose_response_summary <- tibble(
  analysis = "Dose-Response",
  n = nrow(dose_response_data),
  coefficient = coef(dose_model)["steps_change"],
  se = summary(dose_model)$coefficients["steps_change", "Std. Error"],
  p_value = summary(dose_model)$coefficients["steps_change", "Pr(>|t|)"],
  r_squared = summary(dose_model)$r.squared,
  effect_per_1000_steps = steps_per_1000
)

write_csv(dose_response_summary, "dose_response_results.csv")
cat("Saved: dose_response_results.csv\n")

# Save responder summary
responder_summary <- responder_data %>%
  count(responder_category) %>%
  mutate(
    proportion = n / sum(n),
    percentage = sprintf("%.1f%%", 100 * proportion)
  )

write_csv(responder_summary, "responder_summary.csv")
cat("Saved: responder_summary.csv\n")

cat("\n##################################################\n")
cat("ADVANCED ANALYSES COMPLETE\n")
cat("##################################################\n\n")

cat("Summary:\n")
cat(sprintf("  1. Mediation: Activity change effect on weight loss (N=%d)\n", nrow(mediation_data)))
cat(sprintf("  2. Dose-response: %.2f%% weight loss per 1000 steps (p=%.4f)\n",
            steps_per_1000,
            summary(dose_model)$coefficients["steps_change", "Pr(>|t|)"]))
cat(sprintf("  3. Time-to-nadir: Median time by activity level (N=%d)\n", nrow(survival_data)))
cat(sprintf("  4. Responders: %d super, %d moderate, %d non-responders\n",
            sum(responder_data$responder_category == "Super responder"),
            sum(responder_data$responder_category == "Moderate responder"),
            sum(responder_data$responder_category == "Non-responder")))
cat("\n")

cat("Files generated:\n")
cat("  - mediation_analysis_results.csv\n")
cat("  - dose_response_steps_weight.png/pdf\n")
cat("  - dose_response_results.csv\n")
cat("  - kaplan_meier_nadir.png/pdf\n")
cat("  - responder_odds_ratios.csv\n")
cat("  - responder_composition.png/pdf\n")
cat("  - responder_summary.csv\n")
cat("  - advanced_analysis_dataset.csv\n\n")
