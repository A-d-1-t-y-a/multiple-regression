# Quick test script for Task 2
library(tidyverse)
library(olsrr)

data <- read_csv("imd2025_individual.csv", show_col_types = FALSE)

cat("Data loaded:", nrow(data), "rows\n")
cat("Columns:", paste(names(data), collapse = ", "), "\n\n")

# Test Model #1
model1 <- lm(Overall ~ Employment + Living, data = data)
cat("Model #1 AIC:", AIC(model1), "\n")
cat("Model #1 R²:", summary(model1)$r.squared, "\n\n")

# Test full model
full_model <- lm(Overall ~ Income + Employment + Education + Health + Crime + Barriers + Living, data = data)
cat("Full model AIC:", AIC(full_model), "\n")
cat("Full model R²:", summary(full_model)$r.squared, "\n\n")

# Test forward selection
cat("Testing forward selection...\n")
forward_result <- ols_step_forward_p(full_model, penter = 0.05, details = FALSE)
cat("Forward selected:", length(forward_result$predictors), "predictors\n")
cat("Predictors:", paste(forward_result$predictors, collapse = ", "), "\n\n")

# Test best subset
cat("Testing best subset...\n")
best_subset <- ols_step_best_subset(full_model, details = FALSE)
best_4 <- best_subset %>% filter(n <= 4) %>% arrange(aic) %>% slice(1)
cat("Best ≤4 predictor model has", best_4$n, "predictors\n")
cat("AIC:", best_4$aic, "\n\n")

cat("All tests passed!\n")
