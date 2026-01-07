#!/bin/bash
# =============================================================================
# Download Files for Offline Analysis
# =============================================================================
# Run this in All of Us Workbench to download files to your local machine
# =============================================================================

echo "Preparing files for download..."

# Create download directory
mkdir -p files_to_download

# Copy essential data files
echo "Copying data files..."
cp glp1_cleaned_data.RData files_to_download/ 2>/dev/null
cp weight_diagnostics.RData files_to_download/ 2>/dev/null
cp period_analysis_optimized_results.RData files_to_download/ 2>/dev/null

# Copy all analysis scripts
echo "Copying analysis scripts..."
cp diagnose_weight_patterns.R files_to_download/
cp investigate_low_baseline_weights.R files_to_download/
cp sensitivity_analysis_quality_cohort.R files_to_download/
cp period_analysis_optimized.R files_to_download/
cp period_visualizations.R files_to_download/

# Copy data cleaning script (for reference only)
cp comprehensive_data_cleaning.R files_to_download/

echo ""
echo "==================================================="
echo "FILES READY FOR DOWNLOAD"
echo "==================================================="
echo ""
echo "In the All of Us Workbench:"
echo "1. Go to the Files tab (left sidebar)"
echo "2. Navigate to: /home/jupyter/GLP1/files_to_download/"
echo "3. Select all files"
echo "4. Click the download icon (⬇️)"
echo ""
echo "Files prepared:"
ls -lh files_to_download/
echo ""
echo "Total size:"
du -sh files_to_download/
echo ""
echo "==================================================="
