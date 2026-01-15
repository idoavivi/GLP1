# =============================================================================
# SANKEY DIAGRAMS FOR SENSITIVITY ANALYSES
# =============================================================================
# Creates flow diagrams showing:
# 1. Patient flow through time periods by subgroup
# 2. Activity level transitions (baseline → 1-30d → later periods)
# 3. BMI class → Weight response transitions
# 4. Steps tertile transitions over time
# =============================================================================

library(tidyverse)
library(ggsankey)  # For Sankey/alluvial diagrams
library(patchwork)

cat("\n##################################################\n")
cat("CREATING SANKEY DIAGRAMS\n")
cat("##################################################\n\n")

# Check if ggsankey is installed
if (!require("ggsankey", quietly = TRUE)) {
  cat("Installing ggsankey package...\n")
  install.packages("ggsankey")
  library(ggsankey)
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
# DIAGRAM 1: PATIENT FLOW BY TIME PERIOD (ALL PATIENTS)
# =============================================================================

cat("\n========================================\n")
cat("DIAGRAM 1: PATIENT FLOW BY PERIOD\n")
cat("========================================\n\n")

# Calculate which patients have data at each period
time_periods <- tribble(
  ~period, ~period_label, ~days_min, ~days_max,
  1, "Baseline", -180, 0,
  2, "1-30d", 1, 30,
  3, "31-90d", 31, 90,
  4, "91-180d", 91, 180,
  5, "181-365d", 181, 365
)

patient_by_period <- map_dfr(1:nrow(time_periods), function(i) {
  period_data <- activity_cleaned %>%
    filter(days_from_initiation >= time_periods$days_min[i],
           days_from_initiation <= time_periods$days_max[i],
           is_valid_day == TRUE) %>%
    group_by(person_id) %>%
    filter(n() >= 3) %>%
    ungroup() %>%
    distinct(person_id) %>%
    mutate(
      period = time_periods$period[i],
      period_label = time_periods$period_label[i],
      has_data = TRUE
    )
}) %>%
  arrange(person_id, period)

# Create flow data: track patients from one period to the next
flow_data <- patient_by_period %>%
  select(person_id, period, period_label) %>%
  pivot_wider(names_from = period, values_from = period_label,
              names_prefix = "period_", values_fill = "Lost") %>%
  pivot_longer(cols = starts_with("period_"),
               names_to = "period_num",
               values_to = "status") %>%
  mutate(
    period_num = as.numeric(str_remove(period_num, "period_")),
    status = ifelse(status == "Lost", "Lost to follow-up", status)
  ) %>%
  arrange(person_id, period_num)

# Count flows between consecutive periods
flow_counts <- flow_data %>%
  group_by(person_id) %>%
  mutate(next_status = lead(status)) %>%
  ungroup() %>%
  filter(!is.na(next_status)) %>%
  count(status, next_status, period_num) %>%
  rename(from = status, to = next_status, n_patients = n)

cat("Patient flow summary:\n")
print(flow_counts)
cat("\n")

# Create Sankey diagram for patient flow
sankey_flow <- flow_data %>%
  mutate(x = period_num,
         node = status) %>%
  make_long(person_id, x, node)

p_flow <- ggplot(sankey_flow, aes(x = x,
                                   next_x = next_x,
                                   node = node,
                                   next_node = next_node,
                                   fill = factor(node),
                                   label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "gray30", smooth = 8) +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +
  labs(title = "Patient Flow Through Time Periods",
       subtitle = sprintf("Following GLP-1 Initiation (N=%d baseline cohort)", nrow(baseline_cohort)),
       x = "Time Period",
       y = "Number of Patients") +
  theme_sankey(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

ggsave("sankey_patient_flow.png", p_flow, width = 12, height = 8, dpi = 300)
ggsave("sankey_patient_flow.pdf", p_flow, width = 12, height = 8)
cat("Saved: sankey_patient_flow.png/pdf\n\n")

# =============================================================================
# DIAGRAM 2: ACTIVITY LEVEL TRANSITIONS (BASELINE → FOLLOW-UP)
# =============================================================================

cat("\n========================================\n")
cat("DIAGRAM 2: ACTIVITY LEVEL TRANSITIONS\n")
cat("========================================\n\n")

# Calculate steps at each period
activity_by_period <- activity_cleaned %>%
  mutate(
    period_group = case_when(
      days_from_initiation >= -180 & days_from_initiation <= 0 ~ "Baseline",
      days_from_initiation >= 1 & days_from_initiation <= 30 ~ "1-30d",
      days_from_initiation >= 31 & days_from_initiation <= 90 ~ "31-90d",
      days_from_initiation >= 91 & days_from_initiation <= 180 ~ "91-180d",
      days_from_initiation >= 181 & days_from_initiation <= 365 ~ "181-365d",
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
    ),
    period_group = factor(period_group, levels = c("Baseline", "1-30d", "31-90d", "91-180d", "181-365d"))
  )

# Track transitions: Baseline → 1-30d → 31-90d
transitions <- activity_categorized %>%
  filter(period_group %in% c("Baseline", "1-30d", "31-90d")) %>%
  select(person_id, period_group, activity_level) %>%
  pivot_wider(names_from = period_group, values_from = activity_level) %>%
  filter(!is.na(Baseline), !is.na(`1-30d`), !is.na(`31-90d`))

cat(sprintf("Patients with complete 3-period trajectory: N=%d\n", nrow(transitions)))

# Count transitions
transition_counts <- transitions %>%
  count(Baseline, `1-30d`, `31-90d`) %>%
  rename(n_patients = n)

cat("\nTransition counts:\n")
print(transition_counts)
cat("\n")

# Create alluvial diagram
alluvial_data <- transitions %>%
  make_long(Baseline, `1-30d`, `31-90d`)

p_activity_transitions <- ggplot(alluvial_data,
                                   aes(x = x,
                                       next_x = next_x,
                                       node = node,
                                       next_node = next_node,
                                       fill = factor(node),
                                       label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "gray30", smooth = 6) +
  geom_sankey_label(size = 3.5, color = "white", fill = "gray40") +
  scale_fill_manual(values = c("Low" = "#D32F2F", "Medium" = "#FFA000", "High" = "#388E3C")) +
  scale_x_discrete(labels = c("Baseline", "1-30 days", "31-90 days")) +
  labs(title = "Activity Level Transitions Over Time",
       subtitle = "Steps per day (low/medium/high tertiles)",
       x = "Time Period",
       y = "Number of Patients") +
  theme_sankey(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

ggsave("sankey_activity_transitions.png", p_activity_transitions, width = 10, height = 8, dpi = 300)
ggsave("sankey_activity_transitions.pdf", p_activity_transitions, width = 10, height = 8)
cat("Saved: sankey_activity_transitions.png/pdf\n\n")

# =============================================================================
# DIAGRAM 3: BMI CLASS → WEIGHT RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("DIAGRAM 3: BMI CLASS → WEIGHT RESPONSE\n")
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
    bmi_class = case_when(
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
weight_response <- baseline_bmi %>%
  inner_join(baseline_weight, by = "person_id") %>%
  inner_join(nadir_weight, by = "person_id") %>%
  filter(!is.na(baseline_weight), !is.na(nadir_weight), !is.na(bmi_class)) %>%
  mutate(
    weight_change_pct = 100 * (nadir_weight - baseline_weight) / baseline_weight,
    response = case_when(
      weight_change_pct > -5 ~ "<5% loss",
      weight_change_pct <= -5 & weight_change_pct > -10 ~ "5-10% loss",
      weight_change_pct <= -10 ~ ">10% loss",
      TRUE ~ NA_character_
    )
  )

cat(sprintf("Patients with BMI class and weight response: N=%d\n", nrow(weight_response)))

# Count transitions
bmi_weight_counts <- weight_response %>%
  count(bmi_class, response) %>%
  rename(n_patients = n)

cat("\nBMI class → Weight response:\n")
print(bmi_weight_counts)
cat("\n")

# Create Sankey diagram
bmi_weight_sankey <- weight_response %>%
  select(person_id, bmi_class, response) %>%
  make_long(bmi_class, response)

p_bmi_weight <- ggplot(bmi_weight_sankey,
                        aes(x = x,
                            next_x = next_x,
                            node = node,
                            next_node = next_node,
                            fill = factor(node),
                            label = node)) +
  geom_sankey(flow.alpha = 0.6, node.color = "gray30", smooth = 6) +
  geom_sankey_label(size = 3.5, color = "white", fill = "gray40") +
  scale_fill_manual(values = c(
    "Class I" = "#2E7D32",
    "Class II" = "#F57C00",
    "Class III" = "#C62828",
    "<5% loss" = "#EF5350",
    "5-10% loss" = "#FFA726",
    ">10% loss" = "#66BB6A"
  )) +
  scale_x_discrete(labels = c("Baseline BMI Class", "Weight Response")) +
  labs(title = "BMI Class → Weight Response",
       subtitle = "Nadir cohort (≥12 weeks from initiation)",
       x = "",
       y = "Number of Patients") +
  theme_sankey(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

ggsave("sankey_bmi_weight_response.png", p_bmi_weight, width = 10, height = 8, dpi = 300)
ggsave("sankey_bmi_weight_response.pdf", p_bmi_weight, width = 10, height = 8)
cat("Saved: sankey_bmi_weight_response.png/pdf\n\n")

# =============================================================================
# DIAGRAM 4: BASELINE ACTIVITY → WEIGHT RESPONSE
# =============================================================================

cat("\n========================================\n")
cat("DIAGRAM 4: BASELINE ACTIVITY → WEIGHT\n")
cat("========================================\n\n")

# Get baseline activity tertiles
baseline_steps_tertile <- baseline_steps %>%
  mutate(
    steps_tertile = case_when(
      mean_steps < tertile_cutoffs[2] ~ "Low activity",
      mean_steps >= tertile_cutoffs[2] & mean_steps < tertile_cutoffs[3] ~ "Medium activity",
      mean_steps >= tertile_cutoffs[3] ~ "High activity",
      TRUE ~ NA_character_
    )
  ) %>%
  select(person_id, steps_tertile)

# Combine with weight response
activity_weight <- baseline_steps_tertile %>%
  inner_join(weight_response %>% select(person_id, response), by = "person_id") %>%
  filter(!is.na(steps_tertile), !is.na(response))

cat(sprintf("Patients with baseline activity and weight response: N=%d\n", nrow(activity_weight)))

activity_weight_counts <- activity_weight %>%
  count(steps_tertile, response) %>%
  rename(n_patients = n)

cat("\nBaseline activity → Weight response:\n")
print(activity_weight_counts)
cat("\n")

# Create Sankey diagram
activity_weight_sankey <- activity_weight %>%
  make_long(steps_tertile, response)

p_activity_weight <- ggplot(activity_weight_sankey,
                             aes(x = x,
                                 next_x = next_x,
                                 node = node,
                                 next_node = next_node,
                                 fill = factor(node),
                                 label = node)) +
  geom_sankey(flow.alpha = 0.6, node.color = "gray30", smooth = 6) +
  geom_sankey_label(size = 3.5, color = "white", fill = "gray40") +
  scale_fill_manual(values = c(
    "Low activity" = "#D32F2F",
    "Medium activity" = "#FFA000",
    "High activity" = "#388E3C",
    "<5% loss" = "#EF5350",
    "5-10% loss" = "#FFA726",
    ">10% loss" = "#66BB6A"
  )) +
  scale_x_discrete(labels = c("Baseline Activity", "Weight Response")) +
  labs(title = "Baseline Activity Level → Weight Response",
       subtitle = "Does baseline activity predict weight loss?",
       x = "",
       y = "Number of Patients") +
  theme_sankey(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

ggsave("sankey_activity_weight_response.png", p_activity_weight, width = 10, height = 8, dpi = 300)
ggsave("sankey_activity_weight_response.pdf", p_activity_weight, width = 10, height = 8)
cat("Saved: sankey_activity_weight_response.png/pdf\n\n")

# =============================================================================
# SAVE TRANSITION DATA
# =============================================================================

cat("\n========================================\n")
cat("SAVING TRANSITION DATA\n")
cat("========================================\n\n")

write_csv(flow_counts, "sankey_patient_flow_data.csv")
cat("Saved: sankey_patient_flow_data.csv\n")

write_csv(transition_counts, "sankey_activity_transitions_data.csv")
cat("Saved: sankey_activity_transitions_data.csv\n")

write_csv(bmi_weight_counts, "sankey_bmi_weight_data.csv")
cat("Saved: sankey_bmi_weight_data.csv\n")

write_csv(activity_weight_counts, "sankey_activity_weight_data.csv")
cat("Saved: sankey_activity_weight_data.csv\n")

cat("\n##################################################\n")
cat("SANKEY DIAGRAMS COMPLETE\n")
cat("##################################################\n\n")

cat("Generated diagrams:\n")
cat("  1. Patient flow through time periods\n")
cat("  2. Activity level transitions (baseline → 1-30d → 31-90d)\n")
cat("  3. BMI class → Weight response\n")
cat("  4. Baseline activity → Weight response\n\n")

cat("All diagrams saved as PNG (300 dpi) and PDF.\n\n")
