# Multiple Regression Analysis in R

This project performs a multiple regression analysis and checks assumptions as per the tutorial.

## Files
- `run_analysis.R`: Performs the regression analysis and checks assumptions using the provided dataset `data-sets/mlr_data/mlr0.csv`.
    - Model: `y ~ x1 + x2`
    - Checks: Linearity, Multicollinearity, Independence, Homoscedasticity, Normality, Influential Cases.
- `run_all.bat`: Helper script to run the analysis on Windows.
- `run_all.sh`: Helper script to run the analysis on macOS/Linux.

## How to Run

### Windows
1.  **Install R**: Ensure R is installed and added to your system PATH.
2.  **Run Script**: Double-click `run_all.bat` or run it from the terminal:
    ```cmd
    .\run_all.bat
    ```

### macOS / Linux
1.  **Install R**:
    - **macOS**: Install via Homebrew (`brew install r`) or download from [CRAN](https://cran.r-project.org/bin/macosx/).
    - **Linux**: Use your package manager (e.g., `sudo apt install r-base`).
2.  **Run Script**:
    - Open a terminal in this directory.
    - Make the script executable (first time only):
        ```bash
        chmod +x run_all.sh
        ```
    - Run the script:
        ```bash
        ./run_all.sh
        ```

### Manual Execution (All Platforms)
You can also run the R script directly:
```bash
Rscript run_analysis.R
```

## Output
- `results.txt`: Contains the regression model summary and statistical tests (VIF, Durbin-Watson, etc.).
- `plots/`: Contains diagnostic plots (Linearity, Homoscedasticity, Normality).
- `mlr0_with_diagnostics.csv`: The original data with added Cook's Distance values.

## Note on Data
The analysis uses the provided dataset `data-sets/mlr_data/mlr0.csv`. The variables `y`, `x1`, and `x2` are used as the Dependent Variable and Independent Variables, respectively.
