# METHODS

## Data Source

This retrospective cohort study utilized data from the All of Us Research Program, a large-scale initiative funded by the National Institutes of Health to collect health data from at least one million participants in the United States. Data were accessed through the All of Us Researcher Workbench using the Controlled Tier Curated Data Repository (CDR) version [specify version]. The All of Us Research Program has received Institutional Review Board approval from all participating sites, and all participants provided informed consent.

## Study Population

### Inclusion Criteria

Participants were included if they met all of the following criteria:

1. **GLP-1 Receptor Agonist Initiation**: Patients with at least one prescription for injectable semaglutide (Wegovy, Ozempic) or tirzepatide (Mounjaro, Zepbound) identified through drug exposure records in the OMOP Common Data Model
2. **Obesity**: Body mass index (BMI) ≥30 kg/m² or documented obesity diagnosis (ICD-10 codes E66.x) within 180 days prior to GLP-1 initiation
3. **Fitbit Data Availability**: Linked Fitbit wearable device data with valid activity measurements
4. **Baseline Activity Data**: At least 3 valid Fitbit days during the 180-day baseline period (180 days before to day 0 of GLP-1 initiation)
5. **Early Follow-up Data**: At least 3 valid Fitbit days during the early follow-up period (days 1-30 after GLP-1 initiation)
6. **Treatment Persistence**: At least 2 prescription fills of GLP-1 medications, indicating continued treatment beyond initial prescription

### Exclusion Criteria

Patients were excluded if they had:
1. Baseline BMI <30 kg/m² (as determined by average BMI during the 180-day baseline period)
2. Implausible anthropometric measurements: height <150 cm or >220 cm, weight <30 kg or >300 kg, BMI <18 or >80 kg/m²
3. Implausible activity measurements: <50 or >25,000 steps per day

## Variable Definitions

### GLP-1 Initiation Date

The index date was defined as the date of the first prescription for an injectable GLP-1 receptor agonist (semaglutide or tirzepatide). For patients with multiple initiation events, only the first documented prescription was considered.

### Anthropometric Measurements

**Weight** was obtained from the measurement table (concept_id = 3025315). Height was calculated from historical BMI and weight measurements when direct height measurements were unavailable, using the formula: height (cm) = 100 × √(weight_kg / BMI). For each patient, median height from all available measurements was used. Baseline weight and BMI were calculated as the mean of all measurements during the 180-day baseline period (days -180 to 0).

### Physical Activity Measurements

Fitbit data included daily measurements of:
- **Steps**: Total daily step count
- **Moderate-to-Vigorous Physical Activity (MVPA)**: Sum of fairly active minutes and very active minutes per day
- **Sedentary time**: Minutes per day with minimal movement
- **Activity calories**: Calories expended through physical activity, excluding basal metabolic rate

**Valid day criteria**: A day was considered valid if the device was worn for ≥10 hours. A valid period required ≥3 valid days within the specified time window.

### Time Periods

Outcomes were assessed across the following pre-specified time periods:

1. **Baseline**: Days -180 to 0 (relative to GLP-1 initiation)
2. **Early follow-up**: Days 1-30
3. **Short-term**: Days 31-90
4. **Medium-term**: Days 91-180
5. **Long-term**: Days 181-365
6. **Nadir**: Minimum weight achieved after day 84 (>12 weeks), assessed at any timepoint with available data

For each period, outcomes were calculated as the mean of all valid measurements within that window for each patient.

### Comorbidities

Baseline comorbidities were identified from condition_occurrence records using SNOMED-CT concept codes:
- **Hypertension**: Concept IDs 320128, 201826
- **Diabetes mellitus**: Concept IDs 201826, 443238
- **Dyslipidemia**: Concept IDs 432867, 432571
- **Ischemic heart disease**: Concept IDs 314666, 321318
- **Cerebrovascular accident**: Concept IDs 381591, 372924
- **Osteoarthritis**: Concept IDs 80180, 80004

## Statistical Analysis

### Primary Analysis

Baseline characteristics were summarized using descriptive statistics. Continuous variables with normal distributions were reported as mean ± standard deviation, while skewed distributions (e.g., number of GLP-1 prescriptions) were reported as median [interquartile range]. Categorical variables were reported as frequencies and percentages.

Longitudinal changes in weight and physical activity outcomes were analyzed using **linear mixed-effects models** with random intercepts for each patient to account for within-subject correlation and repeated measurements over time. The models included time as a continuous variable (days from GLP-1 initiation), with time coded at the midpoint of each period (baseline=0, 1-30d=15, 31-90d=60, 91-180d=135, 181-365d=273). These models appropriately handle unbalanced data with different numbers of observations per patient at each timepoint and naturally account for patient attrition over time.

The model specification was:

```
Outcome_ij = β₀ + β₁×Time_j + u_i + ε_ij
```

Where:
- Outcome_ij is the outcome for patient i at time j
- β₀ is the population mean at baseline (intercept)
- β₁ is the average daily change in the outcome (slope)
- u_i is the patient-specific random intercept
- ε_ij is the residual error

The coefficient β₁ represents the average daily change, and its statistical significance was assessed using the Satterthwaite approximation for degrees of freedom. Models were fitted using the lmer() function from the lme4 package in R (version [specify]), with p-values obtained using lmerTest.

### Nadir Weight Analysis

Nadir weight was defined as the minimum weight recorded after 12 weeks (day 84) of GLP-1 therapy. This approach captures maximum weight loss regardless of timing, accounting for individual variability in treatment response trajectories. Patients were classified as "responders" if they achieved ≥5% weight loss from baseline at nadir.

### Sensitivity Analyses

To assess the robustness of our findings, we conducted the following pre-specified sensitivity analyses:

1. **By weight loss response**: Stratification by nadir weight loss (≥5% vs <5% from baseline)
2. **By baseline activity level**: Stratification by median baseline step count (high vs low activity)
3. **By baseline BMI category**: Stratification by WHO obesity class (Class I: 30-34.9, Class II: 35-39.9, Class III: ≥40 kg/m²)
4. **By activity change**: Examination of weight loss according to changes in physical activity (increased >10%, stable ±10%, decreased >10%)

### Software

All analyses were performed using R Statistical Software (version 4.x.x; R Foundation for Statistical Computing, Vienna, Austria). Mixed-effects models utilized the lme4 (version x.x.x) and lmerTest (version x.x.x) packages. Data manipulation employed the tidyverse suite (version x.x.x), and visualizations were created using ggplot2 (version x.x.x). BigQuery API access for data extraction utilized the bigrquery package (version x.x.x).

### Missing Data

The study employed a complete-case analysis approach for each outcome measure. Patients contributed data to periods in which they had valid measurements (≥3 valid Fitbit days). The mixed-effects modeling framework naturally accommodates unbalanced designs where patients contribute different numbers of observations, providing valid statistical inference under the missing at random (MAR) assumption.

For baseline characteristics, missing height measurements were imputed using the geometric mean of historical height calculations derived from paired BMI and weight measurements: height = 100 × √(weight_kg / BMI). This approach increased height data availability from 52% to 99% of the cohort.

### Multiple Comparisons

As this study examined multiple outcomes (weight, steps, MVPA, activity calories), we did not adjust for multiple comparisons in the primary analysis, following recommendations for exploratory epidemiological research. However, we report exact p-values to allow readers to apply their preferred multiple comparison adjustment method.

### Significance Level

Statistical significance was set at a two-tailed α level of 0.05 for all analyses.

## Ethical Considerations

This study was conducted using de-identified data from the All of Us Research Program. The All of Us Research Program operates under a Single IRB model, with ethical oversight provided by the All of Us Institutional Review Board. All participants provided informed consent for the use of their data in research. The analysis plan was reviewed and approved through the All of Us Data Access process prior to accessing the data.

## Data Availability Statement

The data that support the findings of this study are available from the All of Us Research Program. Restrictions apply to the availability of these data, which were used under license for this study. Researchers may access these data by registering for the All of Us Researcher Workbench (https://www.researchallus.org) and completing the required training and data use agreements.

## Code Availability

All analysis code used in this study is available at [GitHub repository URL] to facilitate reproducibility and transparency.

---

**Note**: This methods section should be adapted to match the specific journal requirements and word limits. Version numbers and specific dates should be filled in before publication. Consider adding a CONSORT-style flow diagram showing participant selection and attrition across time periods.
