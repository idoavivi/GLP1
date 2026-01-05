# =============================================================================
# INVESTIGATE LOW BASELINE WEIGHTS (<100 KG) IN OBESITY COHORT
# =============================================================================
# Check if patients with baseline weight <100 kg truly have BMI ≥30
# Identify data quality issues or very short patients
# =============================================================================

library(tidyverse)

cat("\n##################################################\n")
cat("INVESTIGATING LOW BASELINE WEIGHTS\n")
cat("##################################################\n\n")

# Load cleaned data
load("glp1_cleaned_data.RData")

# Filter to obesity cohort
final_person_ids <- obesity_cohort$person_id

weight_with_glp1 <- weight_cleaned %>%
  filter(person_id %in% final_person_ids)

# Get baseline weights
baseline_weights <- weight_with_glp1 %>%
  filter(days_from_initiation >= -180, days_from_initiation <= 0) %>%
  group_by(person_id) %>%
  filter(n() >= 2) %>%
  summarize(baseline_weight = max(weight_kg), .groups = "drop")

# Identify patients with baseline weight <100 kg
low_weight_patients <- baseline_weights %>%
  filter(baseline_weight < 100) %>%
  pull(person_id)

cat(sprintf("Patients with baseline weight <100 kg: %d\n\n", length(low_weight_patients)))

# =============================================================================
# ANALYSIS 1: BMI VERIFICATION
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 1: BMI at Baseline\n")
cat("========================================\n\n")

# Initialize bmi_baseline as empty to ensure it exists
bmi_baseline <- tibble(person_id = integer(), bmi = numeric(), measurement_date = as.Date(character()))

# Get BMI data for these patients
if (exists("bmi_data")) {
  bmi_baseline <- bmi_data %>%
    filter(person_id %in% low_weight_patients,
           days_from_initiation >= -180,
           days_from_initiation <= 0) %>%
    group_by(person_id) %>%
    filter(days_from_initiation == max(days_from_initiation)) %>%
    ungroup() %>%
    select(person_id, bmi, measurement_date)

  if (nrow(bmi_baseline) > 0) {
    cat("Most recent baseline BMI for low-weight patients:\n")
    cat(sprintf("  N with BMI data: %d / %d\n", nrow(bmi_baseline), length(low_weight_patients)))
    cat(sprintf("  Mean BMI: %.1f\n", mean(bmi_baseline$bmi)))
    cat(sprintf("  Median BMI: %.1f\n", median(bmi_baseline$bmi)))
    cat(sprintf("  Min BMI: %.1f\n", min(bmi_baseline$bmi)))
    cat(sprintf("  Max BMI: %.1f\n", max(bmi_baseline$bmi)))

    # BMI distribution
    bmi_categories <- bmi_baseline %>%
      mutate(
        bmi_category = case_when(
          bmi < 25 ~ "<25 (Normal - SHOULD NOT BE HERE!)",
          bmi < 27 ~ "25-27 (Overweight - borderline)",
          bmi < 30 ~ "27-30 (Overweight - close)",
          bmi < 35 ~ "30-35 (Class I obesity)",
          bmi < 40 ~ "35-40 (Class II obesity)",
          TRUE ~ "≥40 (Class III obesity)"
        )
      ) %>%
      count(bmi_category) %>%
      mutate(percent = 100 * n / sum(n)) %>%
      arrange(desc(percent))

    cat("\nBMI distribution for <100 kg patients:\n")
    print(bmi_categories)

    # Flag patients with BMI <30
    low_bmi_patients <- bmi_baseline %>%
      filter(bmi < 30)

    if (nrow(low_bmi_patients) > 0) {
      cat(sprintf("\n⚠️  WARNING: %d patients have BMI <30 but are in obesity cohort!\n", nrow(low_bmi_patients)))
      cat("These should be investigated:\n")
      print(low_bmi_patients %>% select(person_id, bmi))
    }
  } else {
    cat("No BMI data found for low-weight patients in baseline period.\n")
  }
} else {
  cat("WARNING: bmi_data not found in RData file\n")
  cat("Cannot perform BMI verification.\n")
}

# =============================================================================
# ANALYSIS 2: HEIGHT VERIFICATION
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 2: Height Distribution\n")
cat("========================================\n\n")

# Only run if we have BMI data
if (nrow(bmi_baseline) > 0) {
  # Calculate height from weight and BMI
  low_weight_with_bmi <- baseline_weights %>%
    filter(person_id %in% low_weight_patients) %>%
    left_join(bmi_baseline, by = "person_id") %>%
    filter(!is.na(bmi)) %>%
    mutate(
      # BMI = weight(kg) / (height(m))^2
      # height(m) = sqrt(weight / BMI)
      calculated_height_cm = 100 * sqrt(baseline_weight / bmi)
    )

  if (nrow(low_weight_with_bmi) > 0) {
    cat("Calculated height for <100 kg patients:\n")
    cat(sprintf("  Mean height: %.1f cm\n", mean(low_weight_with_bmi$calculated_height_cm, na.rm = TRUE)))
    cat(sprintf("  Median height: %.1f cm\n", median(low_weight_with_bmi$calculated_height_cm, na.rm = TRUE)))
    cat(sprintf("  Min height: %.1f cm\n", min(low_weight_with_bmi$calculated_height_cm, na.rm = TRUE)))
    cat(sprintf("  Max height: %.1f cm\n", max(low_weight_with_bmi$calculated_height_cm, na.rm = TRUE)))

    height_categories <- low_weight_with_bmi %>%
      mutate(
        height_category = case_when(
          calculated_height_cm < 145 ~ "<145 cm (Very short - dwarfism?)",
          calculated_height_cm < 155 ~ "145-155 cm (Short)",
          calculated_height_cm < 165 ~ "155-165 cm (Average for women)",
          calculated_height_cm < 175 ~ "165-175 cm (Tall for women/average for men)",
          TRUE ~ "≥175 cm (Tall)"
        )
      ) %>%
      count(height_category) %>%
      mutate(percent = 100 * n / sum(n)) %>%
      arrange(desc(percent))

    cat("\nHeight distribution:\n")
    print(height_categories)
  } else {
    cat("No BMI data available for height calculation.\n")
  }
} else {
  cat("Skipping height verification - no BMI data available.\n")
}

# =============================================================================
# ANALYSIS 3: OBESITY DIAGNOSIS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 3: Obesity Diagnosis Status\n")
cat("========================================\n\n")

# Check if these patients have obesity diagnosis or just BMI criterion
low_weight_inclusion <- obesity_cohort %>%
  filter(person_id %in% low_weight_patients)

cat("Inclusion reason for <100 kg patients:\n")
print(table(low_weight_inclusion$inclusion_reason))
cat("\n")

# =============================================================================
# ANALYSIS 4: DETAILED REVIEW OF PROBLEMATIC CASES
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 4: Detailed Review\n")
cat("========================================\n\n")

# Only run if we have BMI data
if (nrow(bmi_baseline) > 0) {
  # Combine all data for detailed review
  detailed_review <- baseline_weights %>%
    filter(person_id %in% low_weight_patients) %>%
    left_join(bmi_baseline %>% select(person_id, baseline_bmi = bmi), by = "person_id") %>%
    left_join(low_weight_inclusion %>% select(person_id, inclusion_reason), by = "person_id") %>%
    mutate(
      calculated_height_cm = 100 * sqrt(baseline_weight / baseline_bmi),
      issue_flag = case_when(
        baseline_bmi < 30 & inclusion_reason == "BMI ≥ 30" ~ "BMI <30 but included via BMI criterion!",
        baseline_weight < 70 ~ "Weight <70 kg (impossible for BMI≥30 unless <153cm)",
        baseline_weight < 80 & baseline_bmi >= 30 ~ "Very short (<164cm) for BMI≥30",
        TRUE ~ "OK (short but valid)"
      )
    ) %>%
    arrange(baseline_weight)
} else {
  # Create minimal detailed_review without BMI data
  detailed_review <- baseline_weights %>%
    filter(person_id %in% low_weight_patients) %>%
    left_join(low_weight_inclusion %>% select(person_id, inclusion_reason), by = "person_id") %>%
    mutate(
      baseline_bmi = NA_real_,
      calculated_height_cm = NA_real_,
      issue_flag = case_when(
        baseline_weight < 70 ~ "Weight <70 kg (impossible for BMI≥30 unless <153cm)",
        baseline_weight < 80 ~ "Weight 70-80 kg (very short required)",
        TRUE ~ "Cannot verify without BMI data"
      )
    ) %>%
    arrange(baseline_weight)
}

cat("Most problematic cases (sorted by weight):\n")
print(detailed_review %>%
        select(person_id, baseline_weight, baseline_bmi, calculated_height_cm, inclusion_reason, issue_flag) %>%
        head(20))

# Summary of issues
cat("\n\nIssue summary:\n")
issue_summary <- detailed_review %>%
  count(issue_flag) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(n))
print(issue_summary)

# =============================================================================
# RECOMMENDATIONS
# =============================================================================

cat("\n========================================\n")
cat("RECOMMENDATIONS\n")
cat("========================================\n\n")

# Patients that should likely be excluded
exclude_candidates <- detailed_review %>%
  filter(baseline_bmi < 30 | baseline_weight < 70 | is.na(baseline_bmi))

cat(sprintf("Patients recommended for EXCLUSION: %d\n", nrow(exclude_candidates)))
cat("Reasons:\n")
cat("  - BMI <30 at baseline (not obese)\n")
cat("  - Weight <70 kg (biologically implausible for BMI≥30)\n")
cat("  - Missing BMI data\n\n")

if (nrow(exclude_candidates) > 0) {
  cat("List of patient IDs to exclude:\n")
  print(exclude_candidates$person_id)
}

# Save results
save(
  detailed_review,
  exclude_candidates,
  file = "low_weight_investigation.RData"
)

cat("\n\nSaved: low_weight_investigation.RData\n")

cat("\n##################################################\n")
cat("INVESTIGATION COMPLETE\n")
cat("##################################################\n\n")
