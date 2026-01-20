# Fitbit BMI Stratification Analysis Workflow

## Quick Start

### Step 1: Load Data from All of Us (System-Generated Code)
Run the system-generated export code to load data into memory:
- `dataset_50785095_fitbit_activity_df`
- `dataset_50785095_measurement_df`

### Step 2: Run Analysis 1 (BMI Stratification)
```r
source("fitbit_bmi_stratification.R")
```

**⚠️ Important:** If the script crashes during Analysis 2, that's OK! Analysis 1 will have completed and created the required dataframes:
- `activity_final` - cleaned Fitbit data
- `bmi_final` - cleaned BMI data

### Step 3: Run Analysis 2 (Optimized BMI Transitions)
```r
source("analysis2_bmi_transitions_optimized.R")
```

This optimized version avoids memory crashes.

---

## Alternative: Run Just Analysis 1

If you only want Analysis 1 results and the combined script keeps crashing:

```r
# After running system-generated code:
source("analysis1_only.R")
```

Then optionally run Analysis 2:
```r
source("analysis2_bmi_transitions_optimized.R")
```

---

## Outputs

### Analysis 1 (BMI Stratification)
- **CSV files:**
  - `analysis1_individual_data.csv`
  - `analysis1_summary_by_bmi.csv`
  - `analysis1_statistical_tests.csv` (Kruskal-Wallis)
  - `analysis1_pairwise_comparisons.csv` (post-hoc pairwise tests)

- **Tables:**
  - `analysis1_summary_table.html`

- **Figures:**
  - `analysis1_activity_by_bmi_combined.png/pdf` (4-panel visualization)

### Analysis 2 (BMI Transitions)
- **CSV files:**
  - `analysis2_bmi_transitioners.csv`
  - `analysis2_step_quartile_transitions.csv`

- **Tables:**
  - `analysis2_summary_table.html`

- **Figures:**
  - `analysis2_paired_steps_comparison.png/pdf`
  - `analysis2_step_quartile_sankey.png/pdf`

---

## Troubleshooting

### "Kernel dies midway"
- Increase RAM to 30-52 GB in cloud compute settings
- Increase CPUs to 8 for faster processing

### "Analysis 2 keeps crashing"
- Use the optimized Analysis 2 script: `analysis2_bmi_transitions_optimized.R`
- Make sure Analysis 1 completed first (creates `activity_final` and `bmi_final`)

### "object 'activity_final' not found"
- Run Analysis 1 first: `source("fitbit_bmi_stratification.R")`
- Or run `analysis1_only.R` if the combined script crashes

### Script seems frozen
- Look for progress messages - processing can take 5-10 minutes per step
- With 400k+ participants, some operations take time
- Consider sampling data for testing (50k participants instead of all)
