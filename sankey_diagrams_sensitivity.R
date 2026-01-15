# =============================================================================
# SANKEY/ALLUVIAL DIAGRAMS FOR SENSITIVITY ANALYSES
# =============================================================================
# Creates flow diagrams showing:
# 1. Activity level transitions (baseline → 1-30d → 31-90d)
# 2. BMI class → Weight response transitions
# 3. Baseline activity → Weight response
# =============================================================================

library(tidyverse)

cat("\n##################################################\n")
cat("CREATING SANKEY/ALLUVIAL DIAGRAMS\n")
cat("##################################################\n\n")

# Check if ggalluvial is installed
if (!require("ggalluvial", quietly = TRUE)) {
  cat("Installing ggalluvial package...\n")
  install.packages("ggalluvial")
  library(ggalluvial)
} else {
  library(ggalluvial)
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

activity_cleaned <- activity_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

weight_cleaned <- weight_cleaned %>%
  filter(person_id %in% baseline_cohort$person_id)

bmi_data <- bmi_data %>%
  filter(person_id %in% baseline_cohort$person_id)

# =============================================================================
# DIAGRAM 1: ACTIVITY LEVEL TRANSITIONS (BASELINE → 1-30d → 31-90d)
# =============================================================================

cat("\n========================================\n")
cat("DIAGRAM 1: ACTIVITY LEVEL TRANSITIONS\n")
cat("========================================\n\n")

# Calculate steps at each period
activity_by_period <- activity_cleaned %>%
  mutate(
    period_group = case_when(
      days_from_initiation >= -180 & days_from_initiation <= 0 ~ "Baseline",
      days_from_initiation >= 1 & days_from_initiation <= 30 ~ "Period_1",
      days_from_initiation >= 31 & days_from_initiation <= 90 ~ "Period_2",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period_group), is_valid_day == TRUE) %>%
  group_by(person_id, period_group) %>%
  filter(n() >= 3) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE), .groups = "drop")

# Define tertiles based on baseline steps
baseline_steps <- activity_by_period %>%
  filter(period_group == "Baseline")

tertile_cutoffs <- quantile(baseline_steps$mean_steps, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

cat("Steps tertile cutoffs:\n")
print(tertile_cutoffs)
cat("\n")

# Categorize activity level at each period
activity_categorized <- activity_by_period %>%
  mutate(
    activity_level = case_when(
      mean_steps < tertile_cutoffs[2] ~ "Low",
      mean_steps >= tertile_cutoffs[2] & mean_steps < tertile_cutoffs[3] ~ "Medium",
      mean_steps >= tertile_cutoffs[3] ~ "High",
      TRUE ~ NA_character_
    )
  )

# Create wide format for alluvial plot
# Try 3 periods first, fall back to 2 if not enough data
transitions_3p <- activity_categorized %>%
  pivot_wider(names_from = period_group, values_from = activity_level,
              values_fill = "Missing") %>%
  filter(Baseline != "Missing", Period_1 != "Missing", Period_2 != "Missing")

cat(sprintf("Patients with complete 3-period trajectory: N=%d\n", nrow(transitions_3p)))

# Check if we have enough patients for 3-period diagram
if (nrow(transitions_3p) >= 10) {
  # Use 3-period diagram
  transitions <- transitions_3p
  n_periods <- 3
  period_labels <- c("Baseline", "1-30 days", "31-90 days")
  cat("Using 3-period diagram\n\n")

} else {
  # Fall back to 2-period diagram (Baseline → 1-30d)
  cat("⚠ Not enough patients for 3-period diagram. Using 2-period (Baseline → 1-30d)\n\n")

  transitions <- activity_categorized %>%
    pivot_wider(names_from = period_group, values_from = activity_level,
                values_fill = "Missing") %>%
    filter(Baseline != "Missing", Period_1 != "Missing") %>%
    select(person_id, mean_steps, Baseline, Period_1)

  n_periods <- 2
  period_labels <- c("Baseline", "1-30 days")
}

# Count transitions
if (n_periods == 3) {
  transition_counts <- transitions %>%
    count(Baseline, Period_1, Period_2) %>%
    rename(Freq = n)
} else {
  transition_counts <- transitions %>%
    count(Baseline, Period_1) %>%
    rename(Freq = n)
}

cat("\nTransition counts:\n")
print(transition_counts)
cat("\n")

write_csv(transition_counts, "sankey_activity_transitions_data.csv")

# Create alluvial diagram using ggalluvial
if (nrow(transitions) == 0) {
  cat("⚠ No patients with activity transitions. Skipping diagram.\n\n")
} else {
  if (n_periods == 3) {
    alluvial_data <- to_lodes_form(transitions %>% select(Baseline, Period_1, Period_2),
                                   key = "Period",
                                   axes = 1:3)
  } else {
    alluvial_data <- to_lodes_form(transitions %>% select(Baseline, Period_1),
                                   key = "Period",
                                   axes = 1:2)
  }

  p_activity <- ggplot(alluvial_data,
                        aes(x = Period, stratum = stratum, alluvium = alluvium,
                            fill = stratum, label = stratum)) +
    geom_flow(stat = "alluvium", alpha = 0.5) +
    geom_stratum(alpha = 0.8) +
    geom_text(stat = "stratum", size = 3.5) +
    scale_fill_manual(values = c("Low" = "#D32F2F", "Medium" = "#FFA000", "High" = "#388E3C")) +
    scale_x_discrete(limits = if (n_periods == 3) c("Baseline", "Period_1", "Period_2") else c("Baseline", "Period_1"),
                     labels = period_labels) +
    labs(
      title = "Activity Level Transitions Over Time",
      subtitle = sprintf("Steps per day (low/medium/high tertiles), N=%d", nrow(transitions)),
      x = "Time Period",
      y = "Number of Patients"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14)
    )

  ggsave("sankey_activity_transitions.png", p_activity, width = 10, height = 8, dpi = 300)
  ggsave("sankey_activity_transitions.pdf", p_activity, width = 10, height = 8)
  cat("Saved: sankey_activity_transitions.png/pdf\n\n")
}

# =============================================================================
# DIAGRAM 2: BMI CLASS → WEIGHT RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("DIAGRAM 2: BMI CLASS → WEIGHT RESPONSE\n")
cat("========================================\n\n")

# Baseline BMI
baseline_bmi <- bmi_data %>%
  filter(days_from_initiation >= -365, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, baseline_bmi = bmi) %>%
  mutate(
    BMI_Class = case_when(
      baseline_bmi >= 30 & baseline_bmi < 35 ~ "Class I",
      baseline_bmi >= 35 & baseline_bmi < 40 ~ "Class II",
      baseline_bmi >= 40 ~ "Class III",
      TRUE ~ NA_character_
    )
  )

# Baseline weight
baseline_weight <- weight_cleaned %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  arrange(desc(days_from_initiation)) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg)

# Nadir weight (≥84 days)
nadir_weight <- weight_cleaned %>%
  filter(days_from_initiation >= 84) %>%
  group_by(person_id) %>%
  arrange(weight_kg) %>%
  slice(1) %>%
  ungroup() %>%
  select(person_id, nadir_weight = weight_kg)

# Weight response
weight_response_data <- baseline_bmi %>%
  inner_join(baseline_weight, by = "person_id") %>%
  inner_join(nadir_weight, by = "person_id") %>%
  filter(!is.na(baseline_weight), !is.na(nadir_weight), !is.na(BMI_Class)) %>%
  mutate(
    weight_change_pct = 100 * (nadir_weight - baseline_weight) / baseline_weight,
    Weight_Response = case_when(
      weight_change_pct > -5 ~ "<5% loss",
      weight_change_pct <= -5 & weight_change_pct > -10 ~ "5-10% loss",
      weight_change_pct <= -10 ~ ">10% loss",
      TRUE ~ NA_character_
    )
  )

cat(sprintf("Patients with BMI class and weight response: N=%d\n", nrow(weight_response_data)))

# Count transitions
bmi_weight_counts <- weight_response_data %>%
  count(BMI_Class, Weight_Response) %>%
  rename(Freq = n)

cat("\nBMI class → Weight response:\n")
print(bmi_weight_counts)
cat("\n")

write_csv(bmi_weight_counts, "sankey_bmi_weight_data.csv")

# Create alluvial diagram
bmi_alluvial <- to_lodes_form(weight_response_data %>% select(BMI_Class, Weight_Response),
                               key = "Variable",
                               axes = 1:2)

p_bmi_weight <- ggplot(bmi_alluvial,
                        aes(x = Variable, stratum = stratum, alluvium = alluvium,
                            fill = stratum, label = stratum)) +
  geom_flow(stat = "alluvium", alpha = 0.6) +
  geom_stratum(alpha = 0.8) +
  geom_text(stat = "stratum", size = 3.5) +
  scale_fill_manual(values = c(
    "Class I" = "#2E7D32",
    "Class II" = "#F57C00",
    "Class III" = "#C62828",
    "<5% loss" = "#EF5350",
    "5-10% loss" = "#FFA726",
    ">10% loss" = "#66BB6A"
  )) +
  scale_x_discrete(limits = c("BMI_Class", "Weight_Response"),
                   labels = c("Baseline BMI Class", "Weight Response")) +
  labs(
    title = "BMI Class → Weight Response",
    subtitle = "Nadir cohort (≥12 weeks from initiation)",
    x = "",
    y = "Number of Patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("sankey_bmi_weight_response.png", p_bmi_weight, width = 10, height = 8, dpi = 300)
ggsave("sankey_bmi_weight_response.pdf", p_bmi_weight, width = 10, height = 8)
cat("Saved: sankey_bmi_weight_response.png/pdf\n\n")

# =============================================================================
# DIAGRAM 3: BASELINE ACTIVITY → WEIGHT RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("DIAGRAM 3: BASELINE ACTIVITY → WEIGHT\n")
cat("========================================\n\n")

# Get baseline activity tertiles
baseline_steps_tertile <- baseline_steps %>%
  mutate(
    Activity_Level = case_when(
      mean_steps < tertile_cutoffs[2] ~ "Low activity",
      mean_steps >= tertile_cutoffs[2] & mean_steps < tertile_cutoffs[3] ~ "Medium activity",
      mean_steps >= tertile_cutoffs[3] ~ "High activity",
      TRUE ~ NA_character_
    )
  ) %>%
  select(person_id, Activity_Level)

# Combine with weight response
activity_weight_data <- baseline_steps_tertile %>%
  inner_join(weight_response_data %>% select(person_id, Weight_Response), by = "person_id") %>%
  filter(!is.na(Activity_Level), !is.na(Weight_Response))

cat(sprintf("Patients with baseline activity and weight response: N=%d\n", nrow(activity_weight_data)))

activity_weight_counts <- activity_weight_data %>%
  count(Activity_Level, Weight_Response) %>%
  rename(Freq = n)

cat("\nBaseline activity → Weight response:\n")
print(activity_weight_counts)
cat("\n")

write_csv(activity_weight_counts, "sankey_activity_weight_data.csv")

# Create alluvial diagram
activity_weight_alluvial <- to_lodes_form(activity_weight_data %>% select(Activity_Level, Weight_Response),
                                          key = "Variable",
                                          axes = 1:2)

p_activity_weight <- ggplot(activity_weight_alluvial,
                             aes(x = Variable, stratum = stratum, alluvium = alluvium,
                                 fill = stratum, label = stratum)) +
  geom_flow(stat = "alluvium", alpha = 0.6) +
  geom_stratum(alpha = 0.8) +
  geom_text(stat = "stratum", size = 3.5) +
  scale_fill_manual(values = c(
    "Low activity" = "#D32F2F",
    "Medium activity" = "#FFA000",
    "High activity" = "#388E3C",
    "<5% loss" = "#EF5350",
    "5-10% loss" = "#FFA726",
    ">10% loss" = "#66BB6A"
  )) +
  scale_x_discrete(limits = c("Activity_Level", "Weight_Response"),
                   labels = c("Baseline Activity", "Weight Response")) +
  labs(
    title = "Baseline Activity Level → Weight Response",
    subtitle = "Does baseline activity predict weight loss?",
    x = "",
    y = "Number of Patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("sankey_activity_weight_response.png", p_activity_weight, width = 10, height = 8, dpi = 300)
ggsave("sankey_activity_weight_response.pdf", p_activity_weight, width = 10, height = 8)
cat("Saved: sankey_activity_weight_response.png/pdf\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("SANKEY DIAGRAMS COMPLETE\n")
cat("##################################################\n\n")

cat("Generated diagrams:\n")
cat("  1. Activity level transitions (baseline → 1-30d → 31-90d)\n")
cat("  2. BMI class → Weight response\n")
cat("  3. Baseline activity → Weight response\n\n")

cat("Data files:\n")
cat("  - sankey_activity_transitions_data.csv\n")
cat("  - sankey_bmi_weight_data.csv\n")
cat("  - sankey_activity_weight_data.csv\n\n")

cat("All diagrams saved as PNG (300 dpi) and PDF.\n\n")
