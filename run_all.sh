#!/bin/bash

# Check if Rscript is available
echo "Checking for R..."
if ! command -v Rscript &> /dev/null
then
    echo "Rscript could not be found. Please ensure R is installed and in your PATH."
    echo "On macOS, you can install it via Homebrew: brew install r"
    echo "Or download it from: https://cran.r-project.org/bin/macosx/"
    exit 1
fi

# Run the analysis script
echo "Running analysis..."
Rscript run_analysis.R

# Check if the previous command was successful
if [ $? -ne 0 ]; then
    echo "Failed to run analysis."
    exit 1
fi

echo "Done. Check results.txt and plots/ directory."
