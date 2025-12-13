# ============================================================================
# Data Analytics Coursework - Package Installation Script
# ============================================================================
# This script installs all required R packages for Tasks 2 and 3
# Run this first before executing any analysis scripts
# ============================================================================

# List of required packages
required_packages <- c(
  "tidyverse",      # Data manipulation and visualization (includes dplyr, ggplot2, readr)
  "olsrr",          # Model selection for linear regression
  "ggfortify",      # Autoplot for diagnostic plots
  "GGally",         # ggpairs for scatter matrix
  "factoextra",     # PCA visualization
  "cluster",        # Cluster analysis (agnes function)
  "sf",             # Spatial data handling for choropleth maps
  "gridExtra",      # Arranging multiple plots
  "corrplot",       # Correlation plots
  "dendextend",     # Enhanced dendrograms
  "ggrepel"         # Better label positioning in plots
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(sprintf("Installing package: %s\n", pkg))
      install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
      
      # Verify installation
      if (require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat(sprintf("✓ Successfully installed: %s\n", pkg))
      } else {
        cat(sprintf("✗ Failed to install: %s\n", pkg))
      }
    } else {
      cat(sprintf("✓ Already installed: %s\n", pkg))
    }
  }
}

# Install packages
cat("============================================================================\n")
cat("Installing Required R Packages for Data Analytics Coursework\n")
cat("============================================================================\n\n")

install_if_missing(required_packages)

cat("\n============================================================================\n")
cat("Package Installation Complete!\n")
cat("============================================================================\n")

# Display session info for verification
cat("\nR Session Information:\n")
sessionInfo()
