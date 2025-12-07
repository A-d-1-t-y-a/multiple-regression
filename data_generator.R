# ============================================================================
# Data Analytics Coursework - Synthetic Data Generator
# ============================================================================
# This script generates realistic synthetic IMD 2025 datasets for Tasks 2 and 3
# The data structure matches the assignment specifications
# ============================================================================

library(tidyverse)

set.seed(42)  # For reproducibility

# ============================================================================
# Generate IMD 2025 Dataset
# ============================================================================

# Number of districts (Local Authority Districts in England)
n_districts <- 309

# Region names in England
regions <- c("North East", "North West", "Yorkshire and The Humber", 
             "East Midlands", "West Midlands", "East of England",
             "London", "South East", "South West")

# County names (some will have missing regions initially for Task 3)
counties <- c("Cambridgeshire", "Cumbria", "Derbyshire", "Devon", "Dorset",
              "East Sussex", "Essex", "Gloucestershire", "Hampshire", "Hertfordshire",
              "Kent", "Lancashire", "Leicestershire", "Lincolnshire", "Norfolk",
              "North Yorkshire", "Northamptonshire", "Nottinghamshire", "Oxfordshire",
              "Somerset", "Staffordshire", "Suffolk", "Surrey", "Warwickshire",
              "West Sussex", "Worcestershire")

# Generate district codes
district_codes <- sprintf("E%08d", 6001:(6001 + n_districts - 1))

# Generate district names
district_names <- paste0("District_", 1:n_districts)

# Assign regions (some will be NA for counties that need lookup)
region_assignment <- sample(c(regions, rep(NA, 30)), n_districts, replace = TRUE)

# Assign counties (some districts will have counties)
county_assignment <- sample(c(counties, rep(NA, n_districts - 100)), n_districts, replace = TRUE)

# ============================================================================
# Generate IMD Domain Scores with Realistic Correlations
# ============================================================================

# Generate correlated domain scores
# Income and Employment should be highly correlated
# Education and Health should be moderately correlated
# Crime, Barriers, Living should have varying correlations

# Base scores from multivariate normal distribution
library(MASS)

# Correlation matrix (realistic for deprivation domains)
cor_matrix <- matrix(c(
  1.00, 0.85, 0.70, 0.65, 0.45, 0.35, 0.40,  # Income
  0.85, 1.00, 0.68, 0.62, 0.42, 0.33, 0.38,  # Employment
  0.70, 0.68, 1.00, 0.75, 0.50, 0.30, 0.45,  # Education
  0.65, 0.62, 0.75, 1.00, 0.55, 0.28, 0.50,  # Health
  0.45, 0.42, 0.50, 0.55, 1.00, 0.25, 0.35,  # Crime
  0.35, 0.33, 0.30, 0.28, 0.25, 1.00, 0.40,  # Barriers
  0.40, 0.38, 0.45, 0.50, 0.35, 0.40, 1.00   # Living
), nrow = 7, byrow = TRUE)

# Generate correlated scores
domain_scores <- mvrnorm(n = n_districts, 
                         mu = rep(20, 7), 
                         Sigma = cor_matrix * 100)

# Ensure all scores are positive
domain_scores <- abs(domain_scores)

# Create domain score data frame
domains_df <- as.data.frame(domain_scores)
colnames(domains_df) <- c("Income", "Employment", "Education", "Health", 
                          "Crime", "Barriers", "Living")

# Calculate Overall score (weighted average of domains)
# Weights based on IMD methodology
weights <- c(0.225, 0.225, 0.135, 0.135, 0.095, 0.095, 0.09)
overall_scores <- as.matrix(domains_df) %*% weights

# Create rank (1 = most deprived)
rank_overall <- rank(-overall_scores, ties.method = "first")

# ============================================================================
# Create Individual Dataset (Task 2)
# ============================================================================

imd2025_individual <- tibble(
  LAD24CD = district_codes,
  LAD24NM = district_names,
  Region = region_assignment,
  CTY24CD = county_assignment,
  Income = domains_df$Income,
  Employment = domains_df$Employment,
  Education = domains_df$Education,
  Health = domains_df$Health,
  Crime = domains_df$Crime,
  Barriers = domains_df$Barriers,
  Living = domains_df$Living,
  Overall = as.vector(overall_scores),
  Rank = rank_overall
)

# ============================================================================
# Create Group Dataset (Task 3) - Similar but slightly different
# ============================================================================

# Add some variation for group dataset
domain_scores_group <- mvrnorm(n = n_districts, 
                               mu = rep(20, 7), 
                               Sigma = cor_matrix * 100)
domain_scores_group <- abs(domain_scores_group)
domains_df_group <- as.data.frame(domain_scores_group)
colnames(domains_df_group) <- c("Income", "Employment", "Education", "Health", 
                                "Crime", "Barriers", "Living")

overall_scores_group <- as.matrix(domains_df_group) %*% weights
rank_overall_group <- rank(-overall_scores_group, ties.method = "first")

imd2025_group <- tibble(
  LAD24CD = district_codes,
  LAD24NM = district_names,
  Region = region_assignment,
  CTY24CD = county_assignment,
  Income = domains_df_group$Income,
  Employment = domains_df_group$Employment,
  Education = domains_df_group$Education,
  Health = domains_df_group$Health,
  Crime = domains_df_group$Crime,
  Barriers = domains_df_group$Barriers,
  Living = domains_df_group$Living,
  Overall = as.vector(overall_scores_group),
  Rank = rank_overall_group
)

# ============================================================================
# Create Geographic Lookup Files
# ============================================================================

# Local Authority District to County Lookup
lad_to_county <- tibble(
  LAD24CD = district_codes,
  LAD24NM = district_names,
  CTY24CD = county_assignment,
  CTY24NM = county_assignment
) %>%
  filter(!is.na(CTY24CD))

# County to Region Lookup
county_to_region <- tibble(
  CTY24CD = unique(counties),
  CTY24NM = unique(counties),
  RGN24CD = paste0("E12000", 1:length(unique(counties)) %% 9 + 1),
  RGN24NM = sample(regions, length(unique(counties)), replace = TRUE)
)

# Local Authority District to Region Lookup
lad_to_region <- tibble(
  LAD24CD = district_codes,
  LAD24NM = district_names,
  RGN24CD = paste0("E12000", sample(1:9, n_districts, replace = TRUE)),
  RGN24NM = sample(regions, n_districts, replace = TRUE)
)

# ============================================================================
# Save Datasets
# ============================================================================

write_csv(imd2025_individual, "imd2025_individual.csv")
write_csv(imd2025_group, "imd2025_group.csv")
write_csv(lad_to_county, "Local_Authority_District_to_County_(December_2024)_Lookup_in_EN.csv")
write_csv(county_to_region, "County_to_Region_(December_2024)_Lookup.csv")
write_csv(lad_to_region, "Local_Authority_District_to_Region_(December_2024)_Lookup_in_EN.csv")

# ============================================================================
# Verification
# ============================================================================

cat("============================================================================\n")
cat("Synthetic IMD 2025 Data Generation Complete!\n")
cat("============================================================================\n\n")

cat("Files created:\n")
cat("  ✓ imd2025_individual.csv (", nrow(imd2025_individual), " rows)\n", sep = "")
cat("  ✓ imd2025_group.csv (", nrow(imd2025_group), " rows)\n", sep = "")
cat("  ✓ Local_Authority_District_to_County_(December_2024)_Lookup_in_EN.csv\n")
cat("  ✓ County_to_Region_(December_2024)_Lookup.csv\n")
cat("  ✓ Local_Authority_District_to_Region_(December_2024)_Lookup_in_EN.csv\n\n")

cat("Dataset Summary (Individual):\n")
print(summary(imd2025_individual %>% dplyr::select(Income:Overall)))

cat("\n\nCorrelation Matrix (Individual Dataset):\n")
print(round(cor(imd2025_individual %>% dplyr::select(Income:Living)), 2))

cat("\n============================================================================\n")
