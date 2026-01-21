# =============================================================================
# WEIGHT LOSS ANALYSIS WITH MEDICATION EXCLUSION (OPTIMIZED)
# =============================================================================
# Analyzes participants who achieved ≥5% weight loss
# - Compares HIGHEST weight (baseline) to LOWEST weight (nadir)
# - Requires ≥5% weight loss (clinically significant)
# - Requires ≥30 days between measurements
# - Excludes weight loss >30% (extreme/unrealistic loss)
# - Excludes GLP-1 medications (semaglutide, tirzepatide)
# - Compares activity in 90-day windows BEFORE each measurement (≥5 days required)
# =============================================================================

library(tidyverse)
library(ggalluvial)
library(knitr)

cat("\n##################################################\n")
cat("WEIGHT LOSS ANALYSIS (GLP-1 MEDICATION EXCLUSION)\n")
cat("≥5% weight loss - Activity before highest & lowest weights\n")
cat("##################################################\n\n")

# =============================================================================
# STEP 1: LOAD DRUG EXPOSURE DATA
# =============================================================================

cat("========================================\n")
cat("STEP 1: Loading drug exposure data\n")
cat("========================================\n\n")

# Check if data already loaded from system-generated code
# First, look for any drug-related dataframes
drug_vars <- ls(pattern = "drug", envir = .GlobalEnv)
cat(sprintf("Found %d drug-related objects in memory: %s\n",
            length(drug_vars), paste(drug_vars, collapse = ", ")))

# Option 1: Check for pre-loaded dataframes in memory
if (exists("dataset_98104042_drug_df") && is.data.frame(dataset_98104042_drug_df)) {
  cat("✓ Using pre-loaded drug exposure data (dataset_98104042_drug_df)\n")
  drug_raw <- dataset_98104042_drug_df
} else if (exists("dataset_23119529_drug_df") && is.data.frame(dataset_23119529_drug_df)) {
  cat("✓ Using pre-loaded drug exposure data (dataset_23119529_drug_df)\n")
  drug_raw <- dataset_23119529_drug_df
} else if (exists("dataset_50785095_drug_exposure_df") && is.data.frame(dataset_50785095_drug_exposure_df)) {
  cat("✓ Using pre-loaded drug exposure data (dataset_50785095_drug_exposure_df)\n")
  drug_raw <- dataset_50785095_drug_exposure_df
} else {
  # Option 2: Check for CSV exports in workspace bucket (GCS)
  cat("No pre-loaded dataframe found. Checking for CSV exports...\n")

  # First, check if drug_98104042_path variable exists (from export code)
  if (exists("drug_98104042_path")) {
    cat(sprintf("Found drug export path: %s\n", drug_98104042_path))

    # Extract the pattern - use gsutil to find matching CSV files
    gcs_pattern <- drug_98104042_path

    # Use gsutil to find matching CSV files
    gsutil_cmd <- sprintf("gsutil ls %s 2>/dev/null || true", shQuote(gcs_pattern))
    gcs_files <- system(gsutil_cmd, intern = TRUE)

    if (length(gcs_files) > 0 && !any(grepl("^CommandException|^BucketNotFoundException", gcs_files))) {
      cat(sprintf("✓ Found %d CSV file(s) in GCS\n", length(gcs_files)))
      cat("  Loading data from GCS...\n")

      # Create temp directory for downloading CSVs
      temp_dir <- tempdir()
      local_files <- file.path(temp_dir, basename(gcs_files))

      # Download each file
      for (i in seq_along(gcs_files)) {
        cat(sprintf("  Downloading file %d/%d...\n", i, length(gcs_files)))
        system(sprintf("gsutil -q cp %s %s", shQuote(gcs_files[i]), shQuote(local_files[i])))
      }

      # Load and combine all CSV files
      cat("  Reading CSV files...\n")
      drug_raw <- map_dfr(local_files, read_csv, show_col_types = FALSE)

      # Clean up temp files
      file.remove(local_files)

      cat(sprintf("✓ Loaded %s drug records from CSV export\n", format(nrow(drug_raw), big.mark = ",")))
    } else {
      # No files found - provide helpful error
      workspace_bucket <- Sys.getenv("WORKSPACE_BUCKET")
      owner_email <- Sys.getenv("OWNER_EMAIL")

      stop(paste(
        "\n❌ ERROR: CSV export not found at expected location.\n\n",
        "Path checked: ", drug_98104042_path, "\n\n",
        "Please run the BigQuery EXPORT code (not just define the path).\n",
        "The export code should look like:\n\n",
        "  bq extract --destination_format CSV \\\n",
        "    'project:dataset.drug_exposure' \\\n",
        "    '", drug_98104042_path, "'\n\n",
        "Or use the Workbench UI to export the drug_exposure table to this location.\n"
      ))
    }
  } else {
    # No path variable found - try to search GCS
    workspace_bucket <- Sys.getenv("WORKSPACE_BUCKET")
    owner_email <- Sys.getenv("OWNER_EMAIL")

    if (workspace_bucket != "" && owner_email != "") {
      bq_exports_base <- file.path(workspace_bucket, "bq_exports", owner_email)
      cat(sprintf("Searching for drug exports in: %s\n", bq_exports_base))

      # Search for drug_98104042 folder using gsutil
      search_pattern <- sprintf("%s/*/drug_98104042/drug_98104042_*.csv", bq_exports_base)
      gsutil_cmd <- sprintf("gsutil ls %s 2>/dev/null | head -100 || true", shQuote(search_pattern))
      gcs_files <- system(gsutil_cmd, intern = TRUE)

      if (length(gcs_files) > 0 && !any(grepl("^CommandException|^BucketNotFoundException", gcs_files))) {
        cat(sprintf("✓ Found %d CSV file(s) in GCS\n", length(gcs_files)))
        cat("  Loading data from GCS...\n")

        # Create temp directory for downloading CSVs
        temp_dir <- tempdir()
        local_files <- file.path(temp_dir, basename(gcs_files))

        # Download each file
        for (i in seq_along(gcs_files)) {
          cat(sprintf("  Downloading file %d/%d...\n", i, length(gcs_files)))
          system(sprintf("gsutil -q cp %s %s", shQuote(gcs_files[i]), shQuote(local_files[i])))
        }

        # Load and combine all CSV files
        cat("  Reading CSV files...\n")
        drug_raw <- map_dfr(local_files, read_csv, show_col_types = FALSE)

        # Clean up temp files
        file.remove(local_files)

        cat(sprintf("✓ Loaded %s drug records from CSV export\n", format(nrow(drug_raw), big.mark = ",")))
      } else {
        stop(paste(
          "\n❌ ERROR: No drug exposure data found.\n\n",
          "Option 1: Load data into memory\n",
          "  Run system-generated code to load into dataframe:\n",
          "    - dataset_98104042_drug_df\n",
          "    - dataset_23119529_drug_df\n",
          "    - dataset_50785095_drug_exposure_df\n\n",
          "Option 2: Export to CSV\n",
          "  Run the BigQuery EXPORT code (not just the path definition).\n",
          "  Expected location: ", bq_exports_base, "/YYYYMMDD/drug_98104042/\n\n",
          "Found in memory: ", paste(drug_vars, collapse = ", "), "\n"
        ))
      }
    } else {
      stop(paste(
        "\n❌ ERROR: No pre-loaded drug exposure data found.\n\n",
        "Please run the system-generated data export code first to load drug_df.\n",
        "The drug exposure table is too large (~9 million rows) to download directly.\n\n",
        "Expected variable names:\n",
        "  - dataset_98104042_drug_df\n",
          "  - dataset_23119529_drug_df\n",
        "  - dataset_50785095_drug_exposure_df\n\n",
        "Found in memory: ", paste(drug_vars, collapse = ", "), "\n\n",
        "If you have drug data with a different name, please rename it to one of the above.\n"
      ))
    }
  }
}

cat(sprintf("Loaded: %s drug records\n\n", format(nrow(drug_raw), big.mark = ",")))

# =============================================================================
# STEP 2: IDENTIFY GLP-1 MEDICATION USERS
# =============================================================================

cat("========================================\n")
cat("STEP 2: Identifying GLP-1 users\n")
cat("========================================\n\n")

# Identify injectable semaglutide and tirzepatide
glp1_injectable <- drug_raw %>%
  filter(
    # Semaglutide (Ozempic, Wegovy) or Tirzepatide (Mounjaro, Zepbound)
    grepl("semaglutide|tirzepatide", standard_concept_name, ignore.case = TRUE) &
    # Injectable routes only (exclude oral)
    grepl("injection|subcutaneous|intravenous", route_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id)

cat(sprintf("Found %s participants on injectable GLP-1 medications\n",
            format(nrow(glp1_injectable), big.mark = ",")))
cat("  (semaglutide or tirzepatide via injection)\n\n")

# =============================================================================
# STEP 3: LOAD REQUIRED DATA
# =============================================================================

cat("========================================\n")
cat("STEP 3: Checking for required data\n")
cat("========================================\n\n")

if (!exists("activity_final") || !exists("bmi_final")) {
  stop(paste(
    "\n❌ ERROR: Required data not found.\n\n",
    "This script needs:\n",
    "  - activity_final (cleaned Fitbit activity data)\n",
    "  - bmi_final (cleaned BMI data)\n\n",
    "Please run fitbit_bmi_stratification.R (Analysis 1) first.\n"
  ))
}

cat(sprintf("✓ Found activity data: %s records\n", format(nrow(activity_final), big.mark = ",")))
cat(sprintf("✓ Found BMI data: %s records\n\n", format(nrow(bmi_final), big.mark = ",")))

# Exclude GLP-1 users
cat("Excluding GLP-1 medication users from analysis...\n")
activity_final_no_glp1 <- activity_final %>%
  filter(!person_id %in% glp1_injectable$person_id)

bmi_final_no_glp1 <- bmi_final %>%
  filter(!person_id %in% glp1_injectable$person_id)

cat(sprintf("After exclusion: %s activity records from %s participants\n",
            format(nrow(activity_final_no_glp1), big.mark = ","),
            format(length(unique(activity_final_no_glp1$person_id)), big.mark = ",")))
cat(sprintf("After exclusion: %s BMI records from %s participants\n\n",
            format(nrow(bmi_final_no_glp1), big.mark = ","),
            format(length(unique(bmi_final_no_glp1$person_id)), big.mark = ",")))

# =============================================================================
# STEP 4: IDENTIFY WEIGHT LOSS PARTICIPANTS
# =============================================================================

cat("========================================\n")
cat("STEP 4: Identifying weight loss participants\n")
cat("========================================\n\n")

cat("Assigning BMI classes...\n")

bmi_with_class <- bmi_final_no_glp1 %>%
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

cat("Finding baseline (highest weight) and nadir (lowest weight)...\n")

# Find baseline (HIGHEST weight) and nadir (lowest weight) for each participant
weight_loss_participants <- bmi_with_class %>%
  group_by(person_id) %>%
  arrange(measurement_date) %>%
  summarize(
    # Baseline (HIGHEST weight)
    baseline_date = measurement_date[which.max(weight_kg)],
    baseline_bmi = bmi[which.max(weight_kg)],
    baseline_weight = max(weight_kg),
    baseline_class = bmi_class[which.max(weight_kg)],
    baseline_class_num = bmi_class_num[which.max(weight_kg)],

    # Nadir (LOWEST weight)
    nadir_date = measurement_date[which.min(weight_kg)],
    nadir_bmi = min(bmi),
    nadir_weight = min(weight_kg),
    nadir_class = bmi_class[which.min(weight_kg)],
    nadir_class_num = bmi_class_num[which.min(weight_kg)],

    .groups = "drop"
  ) %>%
  mutate(
    # Time between highest and lowest weight
    days_baseline_to_nadir = as.numeric(difftime(nadir_date, baseline_date, units = "days")),

    # Weight change calculations
    delta_weight = nadir_weight - baseline_weight,
    pct_weight_loss = 100 * (baseline_weight - nadir_weight) / baseline_weight,

    # BMI class change (tracked for descriptive purposes)
    class_change = nadir_class_num - baseline_class_num,
    moved_to_lower_bmi_class = class_change < 0,

    # Criteria for inclusion: ≥5% weight loss
    significant_weight_loss = days_baseline_to_nadir >= 30 &  # Nadir at least 30 days AFTER baseline
                              pct_weight_loss >= 5 &  # Clinically significant weight loss (≥5%)
                              pct_weight_loss <= 30  # Exclude extreme weight loss (>30%)
  )

# Filter for participants with ≥5% weight loss
weight_losers <- weight_loss_participants %>%
  filter(significant_weight_loss == TRUE)

cat(sprintf("Found %s participants with ≥5%% weight loss\n",
            format(nrow(weight_losers), big.mark = ",")))
cat(sprintf("  Criteria: highest→lowest weight ≥30 days apart, 5-30%% loss, no GLP-1\n"))

# Show how many also changed BMI class
n_bmi_class_change <- sum(weight_losers$moved_to_lower_bmi_class, na.rm = TRUE)
cat(sprintf("  Of these, %s (%.1f%%) also moved to lower BMI class\n\n",
            format(n_bmi_class_change, big.mark = ","),
            100 * n_bmi_class_change / nrow(weight_losers)))

if (nrow(weight_losers) == 0) {
  cat("⚠ No participants found with ≥5% weight loss. Analysis complete.\n\n")
  quit(save = "no", status = 0)
}

# =============================================================================
# STEP 5: EXTRACT ACTIVITY IN 90-DAY WINDOWS (OPTIMIZED)
# =============================================================================

cat("========================================\n")
cat("STEP 5: Extracting activity data\n")
cat("========================================\n\n")

cat("Creating 90-day windows BEFORE baseline and nadir weights...\n")

# Create time windows (90 days BEFORE each weight measurement)
weight_losers_windows <- weight_losers %>%
  mutate(
    baseline_window_start = baseline_date - 90,
    baseline_window_end = baseline_date - 1,  # Day before baseline
    nadir_window_start = nadir_date - 90,
    nadir_window_end = nadir_date - 1  # Day before nadir
  )

cat("Extracting baseline activity (90 days before baseline)...\n")

# Extract baseline activity
baseline_activity <- activity_final_no_glp1 %>%
  inner_join(
    weight_losers_windows %>%
      select(person_id, baseline_window_start, baseline_window_end),
    by = "person_id"
  ) %>%
  filter(date >= baseline_window_start, date <= baseline_window_end) %>%
  group_by(person_id) %>%
  summarize(
    n_baseline_days = n(),
    avg_steps_baseline = mean(steps, na.rm = TRUE),
    avg_sedentary_baseline = mean(sedentary_minutes, na.rm = TRUE),
    avg_lightly_active_baseline = mean(lightly_active_minutes, na.rm = TRUE),
    avg_fairly_active_baseline = mean(fairly_active_minutes, na.rm = TRUE),
    avg_very_active_baseline = mean(very_active_minutes, na.rm = TRUE),
    avg_activity_cal_baseline = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  )

cat("Extracting nadir activity (90 days before nadir)...\n")

# Extract nadir activity
nadir_activity <- activity_final_no_glp1 %>%
  inner_join(
    weight_losers_windows %>%
      select(person_id, nadir_window_start, nadir_window_end),
    by = "person_id"
  ) %>%
  filter(date >= nadir_window_start, date <= nadir_window_end) %>%
  group_by(person_id) %>%
  summarize(
    n_nadir_days = n(),
    avg_steps_nadir = mean(steps, na.rm = TRUE),
    avg_sedentary_nadir = mean(sedentary_minutes, na.rm = TRUE),
    avg_lightly_active_nadir = mean(lightly_active_minutes, na.rm = TRUE),
    avg_fairly_active_nadir = mean(fairly_active_minutes, na.rm = TRUE),
    avg_very_active_nadir = mean(very_active_minutes, na.rm = TRUE),
    avg_activity_cal_nadir = mean(activity_calories, na.rm = TRUE),
    .groups = "drop"
  )

cat("Merging activity with weight loss data...\n")

# Merge activity data with weight loss data
weight_loser_summary <- weight_losers %>%
  left_join(baseline_activity, by = "person_id") %>%
  left_join(nadir_activity, by = "person_id") %>%
  # Filter for ≥5 days at both timepoints (out of 90-day windows)
  filter(
    !is.na(n_baseline_days), n_baseline_days >= 5,
    !is.na(n_nadir_days), n_nadir_days >= 5
  ) %>%
  mutate(
    # Calculate step changes
    delta_steps = avg_steps_nadir - avg_steps_baseline,
    pct_change_steps = 100 * delta_steps / avg_steps_baseline
    # Note: delta_weight and pct_weight_loss already calculated in weight_loss_participants
  ) %>%
  select(person_id,
         baseline_date, baseline_bmi, baseline_weight, baseline_class,
         nadir_date, nadir_bmi, nadir_weight, nadir_class,
         days_baseline_to_nadir, delta_weight, pct_weight_loss, moved_to_lower_bmi_class,
         n_baseline_days, n_nadir_days,
         avg_steps_baseline, avg_steps_nadir, delta_steps, pct_change_steps,
         avg_sedentary_baseline, avg_sedentary_nadir,
         avg_lightly_active_baseline, avg_lightly_active_nadir,
         avg_fairly_active_baseline, avg_fairly_active_nadir,
         avg_very_active_baseline, avg_very_active_nadir,
         avg_activity_cal_baseline, avg_activity_cal_nadir)

cat(sprintf("After requiring ≥5 days Fitbit data: %s participants\n\n",
            format(nrow(weight_loser_summary), big.mark = ",")))

if (nrow(weight_loser_summary) == 0) {
  cat("⚠ No participants with sufficient Fitbit data. Analysis complete.\n\n")
  quit(save = "no", status = 0)
}

# =============================================================================
# STEP 6: CALCULATE STEP QUARTILES
# =============================================================================

cat("========================================\n")
cat("STEP 6: Calculating step quartiles\n")
cat("========================================\n\n")

# Pre-calculate quartile cutoffs
baseline_quartiles <- quantile(weight_loser_summary$avg_steps_baseline,
                                probs = c(0.25, 0.50, 0.75),
                                na.rm = TRUE)

nadir_quartiles <- quantile(weight_loser_summary$avg_steps_nadir,
                            probs = c(0.25, 0.50, 0.75),
                            na.rm = TRUE)

cat("Baseline step quartiles:\n")
cat(sprintf("  Q1: ≤%.0f, Q2: %.0f-%.0f, Q3: %.0f-%.0f, Q4: >%.0f\n",
            baseline_quartiles[1],
            baseline_quartiles[1], baseline_quartiles[2],
            baseline_quartiles[2], baseline_quartiles[3],
            baseline_quartiles[3]))

cat("Nadir step quartiles:\n")
cat(sprintf("  Q1: ≤%.0f, Q2: %.0f-%.0f, Q3: %.0f-%.0f, Q4: >%.0f\n\n",
            nadir_quartiles[1],
            nadir_quartiles[1], nadir_quartiles[2],
            nadir_quartiles[2], nadir_quartiles[3],
            nadir_quartiles[3]))

# Assign quartiles
weight_loser_summary <- weight_loser_summary %>%
  mutate(
    steps_quartile_baseline = case_when(
      avg_steps_baseline <= baseline_quartiles[1] ~ "Q1",
      avg_steps_baseline <= baseline_quartiles[2] ~ "Q2",
      avg_steps_baseline <= baseline_quartiles[3] ~ "Q3",
      TRUE ~ "Q4"
    ),
    steps_quartile_nadir = case_when(
      avg_steps_nadir <= nadir_quartiles[1] ~ "Q1",
      avg_steps_nadir <= nadir_quartiles[2] ~ "Q2",
      avg_steps_nadir <= nadir_quartiles[3] ~ "Q3",
      TRUE ~ "Q4"
    )
  )

# =============================================================================
# STEP 7: SUMMARY STATISTICS
# =============================================================================

cat("========================================\n")
cat("STEP 7: Computing summary statistics\n")
cat("========================================\n\n")

# Overall summary
overall_summary <- weight_loser_summary %>%
  summarize(
    N = n(),

    # Weight loss
    Weight_loss_median = median(pct_weight_loss, na.rm = TRUE),
    Weight_loss_q25 = quantile(pct_weight_loss, 0.25, na.rm = TRUE),
    Weight_loss_q75 = quantile(pct_weight_loss, 0.75, na.rm = TRUE),

    # Steps at baseline
    Baseline_steps_median = median(avg_steps_baseline, na.rm = TRUE),
    Baseline_steps_q25 = quantile(avg_steps_baseline, 0.25, na.rm = TRUE),
    Baseline_steps_q75 = quantile(avg_steps_baseline, 0.75, na.rm = TRUE),

    # Steps at nadir
    Nadir_steps_median = median(avg_steps_nadir, na.rm = TRUE),
    Nadir_steps_q25 = quantile(avg_steps_nadir, 0.25, na.rm = TRUE),
    Nadir_steps_q75 = quantile(avg_steps_nadir, 0.75, na.rm = TRUE),

    # Change in steps
    Delta_steps_median = median(delta_steps, na.rm = TRUE),
    Delta_steps_q25 = quantile(delta_steps, 0.25, na.rm = TRUE),
    Delta_steps_q75 = quantile(delta_steps, 0.75, na.rm = TRUE)
  )

cat("Activity in Participants with ≥5% Weight Loss:\n")
cat("(Excluding GLP-1 injectable users)\n")
cat("=================================================\n\n")
print(overall_summary)
cat("\n")

# Statistical tests
wilcox_steps <- wilcox.test(weight_loser_summary$avg_steps_baseline,
                            weight_loser_summary$avg_steps_nadir,
                            paired = TRUE)

cat(sprintf("Paired Wilcoxon test (steps): p = %.2e\n", wilcox_steps$p.value))
if (wilcox_steps$p.value < 0.05) {
  cat("  ✓ Significant difference in steps\n\n")
} else {
  cat("  No significant difference in steps\n\n")
}

# Correlation between step change and weight loss
cor_test <- cor.test(weight_loser_summary$delta_steps,
                    weight_loser_summary$pct_weight_loss,
                    method = "spearman")

cat(sprintf("Correlation (Δ steps vs %% weight loss): rho = %.3f, p = %.2e\n\n",
            cor_test$estimate, cor_test$p.value))

# =============================================================================
# STEP 8: SAVE OUTPUTS
# =============================================================================

cat("========================================\n")
cat("STEP 8: Saving outputs\n")
cat("========================================\n\n")

# Save individual data
write_csv(weight_loser_summary, "analysis2_glp1_excluded_weight_losers.csv")
cat("✓ Saved: analysis2_glp1_excluded_weight_losers.csv\n")

# Save transition counts
transition_counts <- weight_loser_summary %>%
  group_by(steps_quartile_baseline, steps_quartile_nadir) %>%
  summarize(n = n(), .groups = "drop") %>%
  arrange(steps_quartile_baseline, steps_quartile_nadir)

write_csv(transition_counts, "analysis2_glp1_excluded_step_quartile_transitions.csv")
cat("✓ Saved: analysis2_glp1_excluded_step_quartile_transitions.csv\n")

# Save formatted summary table
summary_table_formatted <- overall_summary %>%
  mutate(
    `Weight Loss (%)` = sprintf("%.1f (%.1f-%.1f)",
                                Weight_loss_median, Weight_loss_q25, Weight_loss_q75),
    `Baseline Steps` = sprintf("%.0f (%.0f-%.0f)",
                              Baseline_steps_median, Baseline_steps_q25, Baseline_steps_q75),
    `Nadir Steps` = sprintf("%.0f (%.0f-%.0f)",
                           Nadir_steps_median, Nadir_steps_q25, Nadir_steps_q75),
    `Change in Steps` = sprintf("%.0f (%.0f-%.0f)",
                               Delta_steps_median, Delta_steps_q25, Delta_steps_q75)
  ) %>%
  select(N, `Weight Loss (%)`, `Baseline Steps`, `Nadir Steps`, `Change in Steps`)

html_table <- knitr::kable(summary_table_formatted,
                           format = "html",
                           caption = "Activity in Participants with ≥5% Weight Loss (GLP-1 Excluded)",
                           align = c("r", "r", "r", "r", "r"))

writeLines(html_table, "analysis2_glp1_excluded_summary_table.html")
cat("✓ Saved: analysis2_glp1_excluded_summary_table.html\n\n")

# =============================================================================
# STEP 9: CREATE VISUALIZATIONS
# =============================================================================

cat("========================================\n")
cat("STEP 9: Creating visualizations\n")
cat("========================================\n\n")

# Paired comparison plot
cat("Creating paired comparison plot...\n")

weight_loser_long <- weight_loser_summary %>%
  select(person_id, Baseline = avg_steps_baseline, Nadir = avg_steps_nadir) %>%
  pivot_longer(cols = c(Baseline, Nadir), names_to = "Period", values_to = "Steps") %>%
  mutate(Period = factor(Period, levels = c("Baseline", "Nadir")))

p_paired <- ggplot(weight_loser_long, aes(x = Period, y = Steps)) +
  geom_line(aes(group = person_id), alpha = 0.2, color = "gray60") +
  geom_violin(alpha = 0.6, fill = "#4575b4", draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Step Changes in Participants with ≥5% Weight Loss (GLP-1 Excluded)",
    subtitle = sprintf("N = %s, paired Wilcoxon p = %.2e",
                      format(nrow(weight_loser_summary), big.mark = ","),
                      wilcox_steps$p.value),
    x = "Period",
    y = "Average Daily Steps (90-day window)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("analysis2_glp1_excluded_paired_steps.png", p_paired,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("analysis2_glp1_excluded_paired_steps.pdf", p_paired,
       width = 8, height = 6)

cat("✓ Saved: analysis2_glp1_excluded_paired_steps.png/pdf\n")

# Scatter plot: Delta steps vs weight loss
cat("Creating scatter plot...\n")

p_scatter <- ggplot(weight_loser_summary, aes(x = delta_steps, y = pct_weight_loss)) +
  geom_point(alpha = 0.4, color = "#4575b4") +
  geom_smooth(method = "lm", color = "#d73027", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Step Change vs Weight Loss (≥5% Loss, GLP-1 Excluded)",
    subtitle = sprintf("Spearman rho = %.3f, p = %.2e",
                      cor_test$estimate, cor_test$p.value),
    x = "Change in Steps (Nadir - Baseline)",
    y = "Weight Loss (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("analysis2_glp1_excluded_scatter.png", p_scatter,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("analysis2_glp1_excluded_scatter.pdf", p_scatter,
       width = 8, height = 6)

cat("✓ Saved: analysis2_glp1_excluded_scatter.png/pdf\n")

# Sankey diagram
cat("Creating Sankey diagram...\n")

if (nrow(weight_loser_summary) >= 10) {
  sankey_data <- weight_loser_summary %>%
    select(person_id,
           Baseline = steps_quartile_baseline,
           Nadir = steps_quartile_nadir) %>%
    mutate(
      Baseline = factor(Baseline, levels = c("Q1", "Q2", "Q3", "Q4")),
      Nadir = factor(Nadir, levels = c("Q1", "Q2", "Q3", "Q4"))
    )

  alluvial_data <- to_lodes_form(sankey_data %>% select(Baseline, Nadir),
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
    scale_x_discrete(limits = c("Baseline", "Nadir"),
                    labels = c("Baseline\n(Highest Weight)", "Nadir\n(Lowest Weight)")) +
    labs(
      title = "Step Quartile Transitions in ≥5% Weight Loss Participants",
      subtitle = sprintf("N = %s (GLP-1 excluded)",
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

  ggsave("analysis2_glp1_excluded_sankey.png", p_sankey,
         width = 8, height = 6, dpi = 300, bg = "white")
  ggsave("analysis2_glp1_excluded_sankey.pdf", p_sankey,
         width = 8, height = 6)

  cat("✓ Saved: analysis2_glp1_excluded_sankey.png/pdf\n\n")
} else {
  cat("⚠ Too few participants for Sankey diagram (N<10). Skipped.\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n##################################################\n")
cat("ANALYSIS COMPLETE\n")
cat("##################################################\n\n")

cat("Output files:\n")
cat("  CSV files:\n")
cat("    - analysis2_glp1_excluded_weight_losers.csv\n")
cat("    - analysis2_glp1_excluded_step_quartile_transitions.csv\n")
cat("  Tables:\n")
cat("    - analysis2_glp1_excluded_summary_table.html\n")
cat("  Figures:\n")
cat("    - analysis2_glp1_excluded_paired_steps.png/pdf\n")
cat("    - analysis2_glp1_excluded_scatter.png/pdf\n")
if (nrow(weight_loser_summary) >= 10) {
  cat("    - analysis2_glp1_excluded_sankey.png/pdf\n")
}
cat("\nDone!\n\n")
