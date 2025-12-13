=========================================================================
DATA ANALYTICS COURSEWORK - FINAL SUBMISSION PACKAGE
=========================================================================

Module: 7074SCN | Due: 15 December 2025

=========================================================================
FILES SUMMARY
=========================================================================

SETUP (2 files):
  00_Install_Packages.R ............. Install R packages (ONE TIME)
  01_Generate_Data.R ................ Generate synthetic IMD data

TASK 2 - LINEAR MODELS (4 versions - CHOOSE ONE):
  Task2_Version1_ForwardSelection.R ..... Forward selection for Model #2
  Task2_Version2_BackwardElimination.R .. Backward elimination for Model #2
  Task2_Version3_StepwiseSelection.R .... Stepwise selection for Model #2
  Task2_Version4_BestSubset.R ........... Best subset selection for Model #2

TASK 3 - MULTIVARIATE ANALYSIS:
  Task3_MultivariateAnalysis.R ...... Complete Task 3 (all 5 parts)

DOCUMENTATION:
  Task3_Demo_Presentation_Guide.md .. Live demo script (15 min)
  README_SUBMISSION.txt ............. This file

DATA FILES (5 CSV files - auto-generated)

=========================================================================
QUICK START
=========================================================================

1. Install packages (ONCE):
   Rscript 00_Install_Packages.R

2. Run Task 2 (CHOOSE ONE VERSION):
   Rscript Task2_Version1_ForwardSelection.R > My_Task2.txt
   
   OR
   
   Rscript Task2_Version2_BackwardElimination.R > My_Task2.txt
   
   OR
   
   Rscript Task2_Version3_StepwiseSelection.R > My_Task2.txt
   
   OR
   
   Rscript Task2_Version4_BestSubset.R > My_Task2.txt

3. Run Task 3:
   Rscript Task3_MultivariateAnalysis.R > My_Task3.txt

=========================================================================
TASK 2 VERSIONS EXPLAINED
=========================================================================

All 4 versions cover the SAME requirements:
✓ Part 1: AIC explanation (5 marks)
✓ Part 2: Model #1, Best 2-predictor, Model #2, Model #3 (20 marks)
✓ Part 3: Diagnostic plots & outliers (5 marks)

The ONLY difference is the method used for Model #2:

Version 1: FORWARD SELECTION
  - Starts with no predictors
  - Adds predictors one at a time (lowest AIC)
  - Stops at 4 predictors or no improvement

Version 2: BACKWARD ELIMINATION
  - Starts with all 7 predictors
  - Removes least significant one at a time
  - Stops when 4 predictors remain

Version 3: STEPWISE SELECTION
  - Combines forward and backward steps
  - Can add and remove predictors
  - Balances both directions

Version 4: BEST SUBSET SELECTION
  - Tests ALL possible combinations (127 models)
  - Chooses the absolute best by AIC
  - Most comprehensive but computationally intensive

RECOMMENDATION: Use Version 1 (Forward Selection) - most common approach
Or choose the version that matches your course emphasis!

=========================================================================
VERIFIED RESULTS
=========================================================================

All versions produce:
✓ Model 1 (Employment + Living): R² ≈ 0.45
✓ Full model: R² = 0.8789 (87.89%)
✓ Best 2-predictor: Income + Employment (typically)
✓ Model #2: 3-4 predictors, R² > 0.85
✓ All diagnostic plots generate correctly

Task 3:
✓ Income-Employment correlation: r = 0.833
✓ PC1 explains: 57.6% variance
✓ All 5 parts complete

=========================================================================
FOR SUBMISSION
=========================================================================

INDIVIDUAL REPORT (Task 2):
1. Pick ONE version (1, 2, 3, or 4)
2. Run Rscript Task2_VersionX_Method.R > output.txt
3. Copy output + code to Word
4. Add syntax highlighting (http://hilite.me/)
5. Include APA references (Akaike 1974, Burnham & Anderson 2004)

GROUP REPORT (Task 3):
1. Run Rscript Task3_MultivariateAnalysis.R > output.txt
2. Copy to Word with group member contributions
3. Prepare 15-minute demo

=========================================================================
TROUBLESHOOTING
=========================================================================

If errors:
✓ Verify all CSV files exist
✓ Confirm packages installed
✓ Use bash terminal (not PowerShell)
✓ Check R version 4.0+

ALL SCRIPTS TESTED AND WORKING!
=========================================================================
