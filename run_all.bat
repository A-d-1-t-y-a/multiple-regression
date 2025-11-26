@echo off
echo Checking for R...
where Rscript
if %errorlevel% neq 0 (
    echo Rscript not found. Please ensure R is installed and in your PATH.
    exit /b 1
)


echo Running analysis...
Rscript run_analysis.R
if %errorlevel% neq 0 (
    echo Failed to run analysis.
    exit /b 1
)

echo Done. Check results.txt and plots/ directory.
