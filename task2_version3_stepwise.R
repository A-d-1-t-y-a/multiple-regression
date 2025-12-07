# ============================================================================
# Data Analytics Coursework - Task 2: Linear Models
# Version 3: Stepwise Selection Approach
# ============================================================================
# This version emphasizes stepwise selection for Model #2
# ============================================================================

library(tidyverse)
library(olsrr)
library(ggfortify)

data <- read_csv("imd2025_individual.csv", show_col_types = FALSE)

cat("============================================================================\n")
cat("TASK 2 - VERSION 3: STEPWISE SELECTION APPROACH\n")
cat("============================================================================\n\n")

# PART 1: AIC
cat("PART 1: AIC EXPLANATION\n")
cat("AIC balances goodness-of-fit with model parsimony.\n")
cat("Formula: AIC = n*ln(RSS/n) + 2k for linear regression\n")
cat("Lower values preferred. Difference >2 is meaningful.\n")
cat("References: Akaike (1974), Burnham & Anderson (2004)\n\n")

# PART 2: Models
cat("PART 2: MODEL BUILDING\n\n")

# Model #1
cat("MODEL #1: Employment + Living\n")
model1 <- lm(Overall ~ Employment + Living, data = data)
cat("AIC:", AIC(model1), "| R²:", summary(model1)$r.squared, "\n\n")

# Best 2-predictor using stepwise
full_model <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data)
stepwise_2 <- ols_step_both_p(full_model, pent = 0.05, prem = 0.05, details = FALSE)
best_2 <- stepwise_2$predictors[1:min(2, length(stepwise_2$predictors))]
model1_best <- lm(as.formula(paste("Overall ~", paste(best_2, collapse = " + "))), data = data)
cat("Best 2-predictor (stepwise):", paste(best_2, collapse = " + "), "\n")
cat("AIC:", AIC(model1_best), "\n\n")

# Model #2: STEPWISE (PRIMARY)
cat("MODEL #2: Best ≤4 predictor model - STEPWISE SELECTION\n")
cat("Combining forward and backward steps...\n\n")

stepwise_full <- ols_step_both_p(full_model, pent = 0.05, prem = 0.05, details = TRUE)
best_preds_step <- stepwise_full$predictors[1:min(4, length(stepwise_full$predictors))]
model2 <- lm(as.formula(paste("Overall ~", paste(best_preds_step, collapse = " + "))), data = data)

cat("Selected predictors:", paste(best_preds_step, collapse = ", "), "\n")
cat("AIC:", AIC(model2), "| R²:", summary(model2)$r.squared, "\n")
cat("Adj R²:", summary(model2)$adj.r.squared, "\n\n")

# Compare methods
forward_model <- ols_step_forward_p(full_model, penter = 0.05, details = FALSE)
backward_model <- ols_step_backward_p(full_model, prem = 0.05, details = FALSE)

comparison <- data.frame(
  Method = c("Stepwise", "Forward", "Backward"),
  Predictors = c(paste(best_preds_step, collapse = ", "),
                 paste(forward_model$predictors[1:min(4, length(forward_model$predictors))], collapse = ", "),
                 paste(backward_model$predictors[1:min(4, length(backward_model$predictors))], collapse = ", "))
)
print(comparison)
cat("\n")

# Model #3: London vs Non-London
cat("MODEL #3: London vs Non-London (with Crime)\n")
data_london <- data %>% filter(Region == "London")
data_non_london <- data %>% filter(Region != "London")

# Stepwise for London
full_london <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data_london)
step_london <- ols_step_both_p(full_london, pent = 0.05, prem = 0.05, details = FALSE)
london_preds <- unique(c("Crime", step_london$predictors))[1:min(4, length(unique(c("Crime", step_london$predictors))))]
model3_london <- lm(as.formula(paste("Overall ~", paste(london_preds, collapse = " + "))), data = data_london)

# Stepwise for Non-London
full_non_london <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data_non_london)
step_non_london <- ols_step_both_p(full_non_london, pent = 0.05, prem = 0.05, details = FALSE)
non_london_preds <- unique(c("Crime", step_non_london$predictors))[1:min(4, length(unique(c("Crime", step_non_london$predictors))))]
model3_non_london <- lm(as.formula(paste("Overall ~", paste(non_london_preds, collapse = " + "))), data = data_non_london)

cat("London:", paste(london_preds, collapse = ", "), "\n")
cat("Non-London:", paste(non_london_preds, collapse = ", "), "\n")
cat("Same?", identical(sort(london_preds), sort(non_london_preds)), "\n\n")

# PART 3: Diagnostics
cat("PART 3: DIAGNOSTIC PLOTS\n")
p1 <- autoplot(model2, which = 1:4, label.size = 2, ncol = 2)
print(p1)

std_resid <- rstandard(model2)
outliers <- which(abs(std_resid) > 3)
cat("\nOutliers (|std resid| > 3):", length(outliers), "\n")
if (length(outliers) > 0) {
  print(data[outliers, ] %>% dplyr::select(Rank, LAD24NM, Region) %>% head(5))
}

cat("\n============================================================================\n")
cat("TASK 2 VERSION 3 COMPLETE - STEPWISE SELECTION EMPHASIS\n")
cat("============================================================================\n")
