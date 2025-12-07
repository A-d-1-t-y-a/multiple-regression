# ============================================================================
# Data Analytics Coursework - Task 2: Linear Models
# Version 2: Backward Elimination Approach
# ============================================================================
# This version emphasizes backward elimination for Model #2
# All other components remain the same as Version 1
# ============================================================================

# Source common functions and setup
library(tidyverse)
library(olsrr)
library(ggfortify)

# Load data
data <- read_csv("imd2025_individual.csv", show_col_types = FALSE)

cat("============================================================================\n")
cat("TASK 2 - VERSION 2: BACKWARD ELIMINATION APPROACH\n")
cat("============================================================================\n\n")

# PART 1: AIC Explanation (same as Version 1)
cat("PART 1: AIC EXPLANATION\n")
cat("AIC (Akaike Information Criterion) balances model fit and complexity.\n")
cat("Formula: AIC = 2k - 2ln(L), where k = parameters, L = likelihood\n")
cat("Lower AIC indicates better model. Penalizes overfitting.\n")
cat("References: Akaike (1974), Burnham & Anderson (2004)\n\n")

# PART 2: Model Building
cat("PART 2: MODEL BUILDING\n\n")

# Model #1: Employment + Living
cat("MODEL #1: Employment + Living\n")
model1 <- lm(Overall ~ Employment + Living, data = data)
cat("AIC:", AIC(model1), "| R²:", summary(model1)$r.squared, "\n\n")

# Find best 2-predictor model
full_model <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data)
backward_2 <- ols_step_backward_p(full_model, prem = 0.05, details = FALSE)
best_2_preds <- backward_2$predictors[1:min(2, length(backward_2$predictors))]
model1_best <- lm(as.formula(paste("Overall ~", paste(best_2_preds, collapse = " + "))), data = data)
cat("Best 2-predictor model:", paste(best_2_preds, collapse = " + "), "\n")
cat("AIC:", AIC(model1_best), "| R²:", summary(model1_best)$r.squared, "\n\n")

# Model #2: BACKWARD ELIMINATION (PRIMARY METHOD)
cat("MODEL #2: Best ≤4 predictor model - BACKWARD ELIMINATION\n")
cat("Starting with all predictors and removing least significant...\n\n")

backward_full <- ols_step_backward_p(full_model, prem = 0.05, details = TRUE)
best_preds_back <- backward_full$predictors[1:min(4, length(backward_full$predictors))]
model2 <- lm(as.formula(paste("Overall ~", paste(best_preds_back, collapse = " + "))), data = data)

cat("Selected predictors:", paste(best_preds_back, collapse = ", "), "\n")
cat("AIC:", AIC(model2), "| R²:", summary(model2)$r.squared, "\n")
cat("Adj R²:", summary(model2)$adj.r.squared, "\n\n")

# Compare with other methods
forward_model <- ols_step_forward_p(full_model, penter = 0.05, details = FALSE)
stepwise_model <- ols_step_both_p(full_model, pent = 0.05, prem = 0.05, details = FALSE)

comparison <- data.frame(
  Method = c("Backward", "Forward", "Stepwise"),
  AIC = c(AIC(model2), 
          AIC(lm(as.formula(paste("Overall ~", paste(forward_model$predictors[1:min(4, length(forward_model$predictors))], collapse = " + "))), data = data)),
          AIC(lm(as.formula(paste("Overall ~", paste(stepwise_model$predictors[1:min(4, length(stepwise_model$predictors))], collapse = " + "))), data = data)))
)
print(comparison)
cat("\n")

# Model #3: London vs Non-London
cat("MODEL #3: London vs Non-London (with Crime)\n")
data_london <- data %>% filter(Region == "London")
data_non_london <- data %>% filter(Region != "London")

# London model with Crime (using backward from full model)
full_london <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data_london)
back_london <- ols_step_backward_p(full_london, prem = 0.05, details = FALSE)
london_preds <- c("Crime", setdiff(back_london$predictors, "Crime")[1:min(3, length(setdiff(back_london$predictors, "Crime")))])
model3_london <- lm(as.formula(paste("Overall ~", paste(london_preds, collapse = " + "))), data = data_london)

# Non-London model
full_non_london <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data_non_london)
back_non_london <- ols_step_backward_p(full_non_london, prem = 0.05, details = FALSE)
non_london_preds <- c("Crime", setdiff(back_non_london$predictors, "Crime")[1:min(3, length(setdiff(back_non_london$predictors, "Crime")))])
model3_non_london <- lm(as.formula(paste("Overall ~", paste(non_london_preds, collapse = " + "))), data = data_non_london)

cat("London predictors:", paste(london_preds, collapse = ", "), "\n")
cat("Non-London predictors:", paste(non_london_preds, collapse = ", "), "\n")
cat("Same predictors?", identical(sort(london_preds), sort(non_london_preds)), "\n\n")

# PART 3: Diagnostic Plots
cat("PART 3: DIAGNOSTIC PLOTS\n")
cat("Examining Model #2 for outliers...\n\n")

# Diagnostic plots
p1 <- autoplot(model2, which = 1:4, label.size = 2, ncol = 2)
print(p1)

# Outlier detection
std_resid <- rstandard(model2)
cooks_d <- cooks.distance(model2)
leverage <- hatvalues(model2)

outliers_resid <- which(abs(std_resid) > 3)
outliers_cooks <- which(cooks_d > 4/nrow(data))
outliers_leverage <- which(leverage > 2*(length(coef(model2)))/nrow(data))

all_outliers <- unique(c(outliers_resid, outliers_cooks, outliers_leverage))

cat("\nDistricts needing investigation:", length(all_outliers), "\n")
if (length(all_outliers) > 0) {
  outlier_data <- data[all_outliers, ] %>%
    dplyr::select(Rank, LAD24NM, Region, Overall) %>%
    arrange(Rank) %>%
    head(10)
  print(outlier_data)
}

cat("\n============================================================================\n")
cat("TASK 2 VERSION 2 COMPLETE - BACKWARD ELIMINATION EMPHASIS\n")
cat("============================================================================\n")
