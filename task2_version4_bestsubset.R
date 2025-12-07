# ============================================================================
# Data Analytics Coursework - Task 2: Linear Models
# Version 4: Best Subset Selection Approach
# ============================================================================
# This version emphasizes best subset selection for Model #2
# ============================================================================

library(tidyverse)
library(olsrr)
library(ggfortify)

data <- read_csv("imd2025_individual.csv", show_col_types = FALSE)

cat("============================================================================\n")
cat("TASK 2 - VERSION 4: BEST SUBSET SELECTION APPROACH\n")
cat("============================================================================\n\n")

# PART 1: AIC
cat("PART 1: AIC EXPLANATION\n")
cat("AIC (Akaike Information Criterion) provides model selection criterion.\n")
cat("Balances fit (likelihood) with complexity (number of parameters).\n")
cat("Formula: AIC = 2k - 2ln(L)\n")
cat("Lower AIC = better model. Prevents overfitting.\n")
cat("References: Akaike (1974), Burnham & Anderson (2004)\n\n")

# PART 2: Models
cat("PART 2: MODEL BUILDING\n\n")

# Model #1
cat("MODEL #1: Employment + Living\n")
model1 <- lm(Overall ~ Employment + Living, data = data)
cat("AIC:", AIC(model1), "| R²:", summary(model1)$r.squared, "\n\n")

# Best 2-predictor using best subset
full_model <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data)
best_subset_all <- ols_step_best_subset(full_model, details = FALSE)

best_2_model <- best_subset_all %>%
  filter(n == 2) %>%
  arrange(aic) %>%
  slice(1)

cat("Best 2-predictor (best subset):", best_2_model$predictors, "\n")
cat("AIC:", best_2_model$aic, "\n\n")

# Model #2: BEST SUBSET (PRIMARY)
cat("MODEL #2: Best ≤4 predictor model - BEST SUBSET SELECTION\n")
cat("Exhaustive search of all possible combinations...\n\n")

best_4_model <- best_subset_all %>%
  filter(n <= 4) %>%
  arrange(aic) %>%
  slice(1)

cat("Optimal model size:", best_4_model$n, "predictors\n")
cat("Selected predictors:", best_4_model$predictors, "\n")
cat("AIC:", best_4_model$aic, "| R²:", best_4_model$rsquare, "\n")
cat("Adj R²:", best_4_model$adjr, "\n\n")

# Extract predictors and fit model
best_preds_subset <- strsplit(best_4_model$predictors, " ")[[1]]
model2 <- lm(as.formula(paste("Overall ~", paste(best_preds_subset, collapse = " + "))), data = data)

# Show top models
cat("Top 5 models (≤4 predictors):\n")
top_models <- best_subset_all %>%
  filter(n <= 4) %>%
  arrange(aic) %>%
  head(5) %>%
  dplyr::select(n, predictors, aic, rsquare, adjr)
print(top_models)
cat("\n")

# Compare with sequential methods
forward_model <- ols_step_forward_p(full_model, penter = 0.05, details = FALSE)
backward_model <- ols_step_backward_p(full_model, prem = 0.05, details = FALSE)
stepwise_model <- ols_step_both_p(full_model, pent = 0.05, prem = 0.05, details = FALSE)

comparison <- data.frame(
  Method = c("Best Subset", "Forward", "Backward", "Stepwise"),
  N_Predictors = c(best_4_model$n, 
                   min(4, length(forward_model$predictors)),
                   min(4, length(backward_model$predictors)),
                   min(4, length(stepwise_model$predictors)))
)
print(comparison)
cat("\n")

# Model #3: London vs Non-London
cat("MODEL #3: London vs Non-London (with Crime)\n")
data_london <- data %>% filter(Region == "London")
data_non_london <- data %>% filter(Region != "London")

# Best subset for London (must include Crime)
full_london <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data_london)
subset_london <- ols_step_best_subset(full_london, details = FALSE)

# Find best model with Crime included
london_with_crime <- subset_london %>%
  filter(grepl("Crime", predictors), n <= 4) %>%
  arrange(aic) %>%
  slice(1)

london_preds <- strsplit(london_with_crime$predictors, " ")[[1]]
model3_london <- lm(as.formula(paste("Overall ~", paste(london_preds, collapse = " + "))), data = data_london)

# Best subset for Non-London
full_non_london <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data_non_london)
subset_non_london <- ols_step_best_subset(full_non_london, details = FALSE)

non_london_with_crime <- subset_non_london %>%
  filter(grepl("Crime", predictors), n <= 4) %>%
  arrange(aic) %>%
  slice(1)

non_london_preds <- strsplit(non_london_with_crime$predictors, " ")[[1]]
model3_non_london <- lm(as.formula(paste("Overall ~", paste(non_london_preds, collapse = " + "))), data = data_non_london)

cat("London:", paste(london_preds, collapse = ", "), "\n")
cat("Non-London:", paste(non_london_preds, collapse = ", "), "\n")
cat("Same predictors?", identical(sort(london_preds), sort(non_london_preds)), "\n\n")

# PART 3: Diagnostics
cat("PART 3: DIAGNOSTIC PLOTS\n")
p1 <- autoplot(model2, which = 1:4, label.size = 2, ncol = 2)
print(p1)

# Comprehensive outlier analysis
std_resid <- rstandard(model2)
cooks_d <- cooks.distance(model2)
leverage <- hatvalues(model2)

cat("\nOutlier Analysis:\n")
cat("High residuals (>3):", sum(abs(std_resid) > 3), "\n")
cat("High Cook's D (>4/n):", sum(cooks_d > 4/nrow(data)), "\n")
cat("High leverage (>2p/n):", sum(leverage > 2*length(coef(model2))/nrow(data)), "\n\n")

all_outliers <- unique(c(
  which(abs(std_resid) > 3),
  which(cooks_d > 4/nrow(data)),
  which(leverage > 2*length(coef(model2))/nrow(data))
))

if (length(all_outliers) > 0) {
  cat("Districts needing investigation:\n")
  print(data[all_outliers, ] %>% dplyr::select(Rank, LAD24NM, Region, Overall) %>% arrange(Rank) %>% head(10))
}

cat("\n============================================================================\n")
cat("TASK 2 VERSION 4 COMPLETE - BEST SUBSET SELECTION EMPHASIS\n")
cat("============================================================================\n")
