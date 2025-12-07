# Data Analytics Coursework - README

## Project Overview

This repository contains the complete solution for the Data Analytics coursework (Module 7074SCN) at Coventry University. The assignment involves linear regression modeling and multivariate statistical analysis of the Index of Multiple Deprivation (IMD) 2025 dataset.

## Assignment Structure

### Task 1: Markov Chains (Individual) - COMPLETED SEPARATELY
- Python-based analysis of snakes-and-ladders game
- Not included in this repository

### Task 2: Linear Models (Individual)
- **Objective:** Build and compare linear regression models to predict Overall deprivation
- **Dataset:** `imd2025_individual.csv`
- **Key Requirements:**
  - Explain AIC (Akaike Information Criterion)
  - Model #1: Employment + Living predictors
  - Model #2: Best model with ≤4 predictors
  - Model #3: London vs Non-London comparison
  - Diagnostic plots and outlier detection

### Task 3: Multivariate Data Analysis (Group)
- **Objective:** Comprehensive multivariate analysis of deprivation data
- **Dataset:** `imd2025_group.csv`
- **Key Components:**
  1. Data augmentation and correlation analysis
  2. North vs South classification
  3. Principal Component Analysis (PCA)
  4. Hierarchical Cluster Analysis
  5. Choropleth maps (spatial visualization)

## Files in This Repository

### Data Files
- `imd2025_individual.csv` - Individual task dataset (309 districts)
- `imd2025_group.csv` - Group task dataset
- `Local_Authority_District_to_County_(December_2024)_Lookup_in_EN.csv` - Geographic lookup
- `Local_Authority_District_to_Region_(December_2024)_Lookup_in_EN.csv` - Geographic lookup
- `County_to_Region_(December_2024)_Lookup.csv` - County-Region mapping

### R Scripts

#### Setup Scripts
- `install_packages.R` - Installs all required R packages
- `data_generator.R` - Generates synthetic IMD 2025 datasets

#### Task 2 Scripts (4 Versions)
- `task2_version1_forward.R` - **Forward Selection** emphasis
- `task2_version2_backward.R` - **Backward Elimination** emphasis
- `task2_version3_stepwise.R` - **Stepwise Selection** emphasis
- `task2_version4_bestsubset.R` - **Best Subset Selection** emphasis

Each version covers all Task 2 requirements but emphasizes a different model selection approach for Model #2.

#### Task 3 Script
- `task3_complete.R` - Complete group analysis (all 5 parts)

### Documentation
- `demo_presentation_script.md` - 15-minute live demo script for Task 3
- `README.md` - This file

## Getting Started

### Prerequisites
- R version 4.0 or higher
- RStudio (recommended)

### Installation

1. **Install Required Packages:**
   ```R
   source("install_packages.R")
   ```

   Required packages:
   - tidyverse (data manipulation and visualization)
   - olsrr (model selection)
   - ggfortify (diagnostic plots)
   - GGally (scatter matrices)
   - factoextra (PCA visualization)
   - cluster (hierarchical clustering)
   - sf (spatial data - optional)

2. **Generate Data** (if using synthetic data):
   ```R
   source("data_generator.R")
   ```

### Running the Analysis

#### Task 2 (Choose ONE version to submit):
```R
# Version 1 - Forward Selection
source("task2_version1_forward.R")

# OR Version 2 - Backward Elimination
source("task2_version2_backward.R")

# OR Version 3 - Stepwise Selection
source("task2_version3_stepwise.R")

# OR Version 4 - Best Subset Selection
source("task2_version4_bestsubset.R")
```

#### Task 3:
```R
source("task3_complete.R")
```

## Task 2: Four Versions Explained

All four versions of Task 2 cover the same requirements but differ in their approach to Model #2 (best ≤4 predictor model):

| Version | Primary Method | Best For |
|---------|---------------|----------|
| Version 1 | Forward Selection | Building up from simple models |
| Version 2 | Backward Elimination | Starting with full model and simplifying |
| Version 3 | Stepwise Selection | Balanced approach with both directions |
| Version 4 | Best Subset Selection | Exhaustive search of all combinations |

**Recommendation:** Choose the version that best matches your understanding or the approach emphasized in your course materials.

## Task 3: Group Contributions

- **Person 1:** Data augmentation, correlation analysis (Part 1)
- **Person 2:** PCA analysis (Part 3), North/South classification (Part 2)
- **Person 3:** Cluster analysis (Part 4), Choropleth maps (Part 5)

## Key Findings (Expected)

### Task 2
- Employment and Living are significant predictors of Overall deprivation
- Best 2-predictor model likely includes Income and Employment
- Best ≤4 predictor model explains >95% of variance
- London shows different predictor patterns than non-London districts
- Several high-leverage districts identified for further investigation

### Task 3
- Strong correlation (r>0.80) between Income and Employment
- PC1 explains ~70% of variance (general deprivation factor)
- PC2 contrasts economic vs environmental deprivation
- 3-5 distinct district clusters identified
- Domains cluster into economic, social, and environmental groups

## Submission Guidelines

### Individual Report (Tasks 1 & 2)
- Submit ONE Word document
- Include Task 1 (if applicable) and ONE version of Task 2
- Embed code and output directly (no screenshots)
- Use syntax highlighter (http://hilite.me/) for code formatting
- Include APA citations and references
- ~2000 words equivalent

### Group Report (Task 3)
- ONE group member submits
- Include all 5 parts of Task 3
- Clearly state group member contributions
- Embed code and output
- ~1000 words per group member
- Prepare for 15-minute live demo

## Live Demo Preparation

1. Review `demo_presentation_script.md`
2. Practice timing (15 minutes total)
3. Ensure all code runs without errors
4. Prepare backup screenshots
5. Assign sections to group members
6. Practice transitions between speakers

## Troubleshooting

### Common Issues

**Package Installation Fails:**
```R
# Try installing individually
install.packages("tidyverse")
install.packages("olsrr")
# etc.
```

**Data Not Found:**
- Ensure CSV files are in the same directory as R scripts
- Use `getwd()` to check working directory
- Use `setwd("path/to/directory")` if needed

**olsrr Functions Error:**
- Check that all predictors have variation
- Ensure no perfect multicollinearity
- Try different p-value thresholds

**Plots Not Displaying:**
- In RStudio, check Plots pane
- Try `dev.new()` before plotting
- Save plots: `ggsave("plot.png")`

## References

### AIC and Model Selection
- Akaike, H. (1974). A new look at the statistical model identification. *IEEE Transactions on Automatic Control*, 19(6), 716-723.
- Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference: Understanding AIC and BIC in model selection. *Sociological Methods & Research*, 33(2), 261-304.

### IMD 2025
- Ministry of Housing, Communities and Local Government. (2025). *English indices of deprivation 2025*. GOV.UK. https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025

### Statistical Methods
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An introduction to statistical learning*. Springer.

## Contact

For questions about this coursework:
- Module Leader: Dr Mark Johnston (ad4039@coventry.ac.uk)
- Module Code: 7074SCN

## License

This code is for educational purposes only as part of Coventry University coursework.

---

**Last Updated:** December 2025
**Module:** Data Analytics (7074SCN)
**Institution:** Coventry University
