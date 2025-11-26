# Multiple Regression Analysis in R

This project performs a multiple regression analysis and checks assumptions as per the tutorial.

## Files
- `run_analysis.R`: Performs the regression analysis and checks assumptions using the provided dataset `data-sets/mlr_data/mlr0.csv`.
    - Model: `y ~ x1 + x2`
    - Checks: Linearity, Multicollinearity, Independence, Homoscedasticity, Normality, Influential Cases.
- `run_all.bat`: Helper script to run the analysis.

## How to Run
1.  **Install R**: Ensure R is installed and added to your system PATH.
2.  **Run Scripts**:
    - Open a terminal in this directory.
    - Run `run_all.bat`
    - OR run manually:
        ```bash
        Rscript run_analysis.R
        ```

## Output
- `results.txt`: Contains the regression model summary and statistical tests (VIF, Durbin-Watson, etc.).
- `plots/`: Contains diagnostic plots (Linearity, Homoscedasticity, Normality).
- `mlr0_with_diagnostics.csv`: The original data with added Cook's Distance values.

## Note on Data
The analysis uses the provided dataset `data-sets/mlr_data/mlr0.csv`. The variables `y`, `x1`, and `x2` are used as the Dependent Variable and Independent Variables, respectively.
