# Column Name Verification for Analysis Scripts

## Data Sources (from your RData file)

### activity_cleaned
- person_id ✓
- date ✓
- steps ✓
- activity_calories ✓
- sedentary_minutes ✓
- lightly_active_minutes ✓
- fairly_active_minutes ✓
- very_active_minutes ✓
- is_valid_day ✓
- days_from_initiation ✓

### weight_cleaned
- person_id ✓
- measurement_date ✓
- weight_kg ✓
- days_from_initiation ✓

### bmi_data
- person_id ✓
- measurement_date ✓
- bmi ✓
- days_from_initiation ✓

## Column Usage in Scripts

### ✅ CORRECT USAGE

**Both scripts:**
1. ✓ Use `weight_kg` (not just `weight`)
2. ✓ Use `days_from_initiation` (already exists, no calculation needed)
3. ✓ Calculate `active_minutes = fairly_active_minutes + very_active_minutes` (MVPA)
4. ✓ Use `is_valid_day` for filtering
5. ✓ Use `steps`, `activity_calories`, `sedentary_minutes` correctly

**After data preparation:**
- `activity_all` gets: person_id, date, steps, activity_calories, sedentary_minutes, is_valid_day, days_from_initiation, **plus calculated `active_minutes`**
- `weight_all` gets: person_id, measurement_date, weight_kg, days_from_initiation
- `bmi_measured` gets: person_id, measurement_date, bmi, days_from_initiation

### 📊 Derived Columns

**In activity summarization (both scripts):**
```r
summarize(
  steps = mean(steps, na.rm = TRUE),                                          ✓
  active_minutes = mean(fairly_active_minutes + very_active_minutes, ...),   ✓
  sedentary_minutes = mean(sedentary_minutes, na.rm = TRUE),                 ✓
  activity_calories = mean(activity_calories, na.rm = TRUE),                 ✓
  n_days = n(),                                                               ✓
  .groups = "drop"
)
```

This creates `all_activity` with columns:
- person_id
- period
- period_numeric
- steps
- active_minutes (CALCULATED - this is MVPA)
- sedentary_minutes
- activity_calories
- n_days

**Later selections work because `active_minutes` now exists in `all_activity`:**
```r
# This works because all_activity has active_minutes
select(person_id, period, steps, active_minutes, activity_calories)  ✓
```

## ✅ ALL CHECKS PASSED

Both scripts should now work correctly with your data:
- ✓ No references to non-existent columns
- ✓ Correct calculation of MVPA from fairly + very active minutes
- ✓ Proper use of weight_kg, bmi, days_from_initiation
- ✓ All joins use correct column names
