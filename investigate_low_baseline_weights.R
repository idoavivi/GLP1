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
# ANALYSIS 1: CALCULATE HEIGHT FROM HISTORICAL BMI + WEIGHT
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 1: Calculate Height from Historical Data\n")
cat("========================================\n\n")

# Strategy: Use ANY historical BMI + weight measurement to calculate height
# Then use that height to verify BMI at baseline

# Initialize
patient_heights <- tibble(person_id = integer(), height_cm = numeric())

# Get ALL weight measurements for low-weight patients
all_weights <- weight_cleaned %>%
  filter(person_id %in% low_weight_patients) %>%
  select(person_id, measurement_date, weight_kg)

cat(sprintf("Total weight measurements for <100 kg patients: %d\n", nrow(all_weights)))

# Try to get BMI data
if (exists("bmi_data") && nrow(bmi_data) > 0) {
  cat("BMI data found - calculating height from BMI + weight pairs\n\n")

  # Match weight and BMI measurements by person and date
  weight_bmi_pairs <- all_weights %>%
    inner_join(
      bmi_data %>% select(person_id, measurement_date, bmi),
      by = c("person_id", "measurement_date")
    ) %>%
    filter(!is.na(bmi), bmi > 0, weight_kg > 0) %>%
    mutate(
      # BMI = weight(kg) / (height(m))^2
      # height(cm) = 100 * sqrt(weight / BMI)
      height_cm = 100 * sqrt(weight_kg / bmi)
    ) %>%
    filter(height_cm >= 100, height_cm <= 220)  # Sanity check

  cat(sprintf("Matched BMI + weight pairs: %d\n", nrow(weight_bmi_pairs)))

  if (nrow(weight_bmi_pairs) > 0) {
    # Calculate median height per person (to handle measurement errors)
    patient_heights <- weight_bmi_pairs %>%
      group_by(person_id) %>%
      summarize(
        height_cm = median(height_cm),
        n_measurements = n(),
        .groups = "drop"
      )

    cat(sprintf("Patients with calculable height: %d / %d\n", nrow(patient_heights), length(low_weight_patients)))
    cat(sprintf("  Mean height: %.1f cm\n", mean(patient_heights$height_cm)))
    cat(sprintf("  Median height: %.1f cm\n", median(patient_heights$height_cm)))
    cat(sprintf("  Range: %.1f - %.1f cm\n\n", min(patient_heights$height_cm), max(patient_heights$height_cm)))
  }
} else {
  cat("WARNING: bmi_data not found in RData file\n")
  cat("Cannot calculate height from BMI measurements.\n\n")
}

# =============================================================================
# ANALYSIS 2: VERIFY BMI AT BASELINE USING CALCULATED HEIGHT
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 2: Verify BMI at Baseline\n")
cat("========================================\n\n")

# Initialize
bmi_baseline <- tibble(person_id = integer(), baseline_bmi = numeric())

if (nrow(patient_heights) > 0) {
  # Calculate BMI at baseline using calculated height
  bmi_baseline <- baseline_weights %>%
    filter(person_id %in% low_weight_patients) %>%
    inner_join(patient_heights, by = "person_id") %>%
    mutate(
      # BMI = weight(kg) / (height(m))^2
      baseline_bmi = baseline_weight / ((height_cm / 100)^2)
    ) %>%
    select(person_id, baseline_weight, height_cm, baseline_bmi)

  cat(sprintf("Patients with verified BMI: %d / %d\n", nrow(bmi_baseline), length(low_weight_patients)))
  cat(sprintf("  Mean baseline BMI: %.1f\n", mean(bmi_baseline$baseline_bmi)))
  cat(sprintf("  Median baseline BMI: %.1f\n", median(bmi_baseline$baseline_bmi)))
  cat(sprintf("  Min baseline BMI: %.1f\n", min(bmi_baseline$baseline_bmi)))
  cat(sprintf("  Max baseline BMI: %.1f\n", max(bmi_baseline$baseline_bmi)))

  # BMI distribution
  bmi_categories <- bmi_baseline %>%
    mutate(
      bmi_category = case_when(
        baseline_bmi < 25 ~ "<25 (Normal - SHOULD NOT BE HERE!)",
        baseline_bmi < 27 ~ "25-27 (Overweight - borderline)",
        baseline_bmi < 30 ~ "27-30 (Overweight - SHOULD NOT BE HERE!)",
        baseline_bmi < 35 ~ "30-35 (Class I obesity)",
        baseline_bmi < 40 ~ "35-40 (Class II obesity)",
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
    filter(baseline_bmi < 30)

  if (nrow(low_bmi_patients) > 0) {
    cat(sprintf("\n⚠️  CRITICAL: %d patients have baseline BMI <30 but are in obesity cohort!\n", nrow(low_bmi_patients)))
    cat(sprintf("These represent %.1f%% of <100 kg patients\n\n", 100 * nrow(low_bmi_patients) / nrow(bmi_baseline)))
    cat("Top 10 patients with lowest BMI:\n")
    print(low_bmi_patients %>%
            arrange(baseline_bmi) %>%
            head(10) %>%
            select(person_id, baseline_weight, height_cm, baseline_bmi))
  } else {
    cat("\n✓ All <100 kg patients have BMI ≥30 at baseline (verified)\n")
  }
} else {
  cat("Cannot verify BMI - no height data available.\n")
}

# =============================================================================
# ANALYSIS 3: HEIGHT DISTRIBUTION
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 3: Height Distribution\n")
cat("========================================\n\n")

if (nrow(patient_heights) > 0) {
  height_categories <- patient_heights %>%
    mutate(
      height_category = case_when(
        height_cm < 145 ~ "<145 cm (Very short - dwarfism?)",
        height_cm < 155 ~ "145-155 cm (Short)",
        height_cm < 165 ~ "155-165 cm (Average for women)",
        height_cm < 175 ~ "165-175 cm (Tall for women/average for men)",
        TRUE ~ "≥175 cm (Tall)"
      )
    ) %>%
    count(height_category) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    arrange(desc(percent))

  cat("Height distribution:\n")
  print(height_categories)

  # Check for extremely short patients
  very_short <- patient_heights %>%
    filter(height_cm < 150)

  if (nrow(very_short) > 0) {
    cat(sprintf("\n⚠️  WARNING: %d patients with height <150 cm (potential dwarfism)\n", nrow(very_short)))
    cat("These patients should be reviewed:\n")
    print(very_short %>% head(10))
  }
} else {
  cat("No height data available.\n")
}

# =============================================================================
# ANALYSIS 4: OBESITY DIAGNOSIS
# =============================================================================

cat("\n========================================\n")
cat("ANALYSIS 4: Obesity Diagnosis Status\n")
cat("========================================\n\n")

# Check if these patients have obesity diagnosis or just BMI criterion
low_weight_inclusion <- obesity_cohort %>%
  filter(person_id %in% low_weight_patients)

cat("Inclusion reason for <100 kg patients:\n")
print(table(low_weight_inclusion$inclusion_reason))
cat("\n")

# =============================================================================
# ANALYSIS 5: DETAILED REVIEW OF PROBLEMATIC CASES
# =============================================================================

cat("========================================\n")
cat("ANALYSIS 5: Detailed Review\n")
cat("========================================\n\n")

# Combine all data for detailed review
if (nrow(bmi_baseline) > 0) {
  detailed_review <- bmi_baseline %>%
    left_join(low_weight_inclusion %>% select(person_id, inclusion_reason), by = "person_id") %>%
    mutate(
      issue_flag = case_when(
        baseline_bmi < 30 & inclusion_reason == "BMI ≥ 30" ~ "BMI <30 but included via BMI criterion!",
        baseline_bmi < 30 & inclusion_reason == "Obesity diagnosis" ~ "BMI <30 (included via diagnosis)",
        baseline_weight < 70 ~ "Weight <70 kg (impossible for BMI≥30 unless <153cm)",
        height_cm < 150 ~ "Very short (<150cm, possible dwarfism)",
        baseline_bmi >= 30 & baseline_bmi < 35 ~ "OK (Class I obesity)",
        baseline_bmi >= 35 ~ "OK (Class II+ obesity)",
        TRUE ~ "OK"
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
      height_cm = NA_real_,
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
        select(person_id, baseline_weight, baseline_bmi, height_cm, inclusion_reason, issue_flag) %>%
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
