# Install and verify mixed effects modeling packages
# Required for period_analysis_optimized.R

cat("\n##################################################\n")
cat("INSTALLING MIXED EFFECTS MODELING PACKAGES\n")
cat("##################################################\n\n")

# Check and install lme4
cat("Checking lme4 package...\n")
if (!require(lme4, quietly = TRUE)) {
  cat("  Installing lme4...\n")
  install.packages("lme4", repos = "https://cloud.r-project.org")
} else {
  cat("  lme4 already installed\n")
}

# Check and install lmerTest
cat("\nChecking lmerTest package...\n")
if (!require(lmerTest, quietly = TRUE)) {
  cat("  Installing lmerTest...\n")
  install.packages("lmerTest", repos = "https://cloud.r-project.org")
} else {
  cat("  lmerTest already installed\n")
}

# Load and verify
cat("\n##################################################\n")
cat("VERIFICATION TEST\n")
cat("##################################################\n\n")

library(lme4)
library(lmerTest)

cat("Testing mixed effects model functionality...\n\n")

# Create test data
set.seed(123)
test_data <- data.frame(
  person_id = rep(1:20, each = 5),
  timepoint = rep(c("baseline", "period1", "period2", "period3", "period4"), 20),
  value = rnorm(100, mean = 100, sd = 10) + rep(rnorm(20, 0, 5), each = 5)
)

# Fit test model
cat("Fitting test mixed effects model...\n")
test_model <- lmer(value ~ timepoint + (1 | person_id), data = test_data)

cat("\nModel summary:\n")
print(summary(test_model))

cat("\n##################################################\n")
cat("SUCCESS!\n")
cat("##################################################\n\n")

cat("Mixed effects modeling packages are installed and working.\n")
cat("Your period_analysis_optimized.R will now include:\n")
cat("  - Paired t-tests (within-person comparisons)\n")
cat("  - Linear mixed effects models (accounts for repeated measures)\n\n")

cat("You can now run: source('period_analysis_optimized.R')\n")
