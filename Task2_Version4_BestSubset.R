# ============================================================================
# Data Analytics Coursework - Task 2: Linear Models
# VERSION 4: BEST SUBSET SELECTION APPROACH
# ============================================================================
# This version emphasizes BEST SUBSET SELECTION for Model #2
# All other components identical to Version 1
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggfortify)
})

cat("============================================================================\n")
cat("TASK 2 - VERSION 4: BEST SUBSET SELECTION\n")
cat("Student: [Your Name] | Module: 7074SCN\n")
cat("============================================================================\n\n")

data <- read_csv("imd2025_individual.csv", show_col_types = FALSE)
cat("Dataset loaded:", nrow(data), "districts\n\n")

# ============================================================================
# PART 1: AIC Explanation (Same for all versions)
# ============================================================================

cat("PART 1: AKAIKE INFORMATION CRITERION (AIC)\n\n")
cat("AIC balances model fit with complexity. Formula: AIC = 2k - 2ln(L)\n")
cat("Lower AIC = better model. Penalizes overfitting.\n")
cat("References: Akaike (1974), Burnham & Anderson (2004)\n\n")

# ============================================================================
# PART 2: Model Building
# ============================================================================

cat("PART 2: MODEL BUILDING\n\n")

# MODEL #1 (Same for all versions)
cat("MODEL #1: Employment + Living\n")
model1 <- lm(Overall ~ Employment + Living, data = data)
cat("R² =", round(summary(model1)$r.squared, 4), "| AIC =", round(AIC(model1), 2), "\n\n")

# Best 2-predictor (Same for all versions)
predictors <- c("Income", "Employment", "Education", "Health", "Crime", "Barriers", "Living")
best_aic_2 <- Inf
best_pair <- NULL
for (i in 1:(length(predictors)-1)) {
  for (j in (i+1):length(predictors)) {
    temp_m <- lm(as.formula(paste("Overall ~", predictors[i], "+", predictors[j])), data = data)
    if (AIC(temp_m) < best_aic_2) {
      best_aic_2 <- AIC(temp_m)
      best_pair <- c(predictors[i], predictors[j])
    }
  }
}
cat("Best 2-predictor:", paste(best_pair, collapse = " + "), "| AIC =", round(best_aic_2, 2), "\n\n")

# MODEL #2: BEST SUBSET SELECTION (VERSION-SPECIFIC)
cat("MODEL #2: Best ≤4 predictor model - BEST SUBSET SELECTION\n")
cat("Method: Exhaustive search of all possible combinations\n\n")

# Test all possible combinations up to 4 predictors
all_combs <- list()

for (n_preds in 1:4) {
  combs <- combn(predictors, n_preds, simplify = FALSE)
  for (comb in combs) {
    formula_str <- paste("Overall ~", paste(comb, collapse = " + "))
    temp_model <- lm(as.formula(formula_str), data = data)
    all_combs[[length(all_combs) + 1]] <- list(
      n = n_preds,
      predictors = comb,
      aic = AIC(temp_model),
      r2 = summary(temp_model)$r.squared
    )
  }
}

# Find best by AIC
best_idx <- which.min(sapply(all_combs, function(x) x$aic))
best_comb <- all_combs[[best_idx]]

cat("Evaluated", length(all_combs), "models total\n")
cat("Best model has", best_comb$n, "predictors\n")
cat("Predictors:", paste(best_comb$predictors, collapse = ", "), "\n")
cat("AIC =", round(best_comb$aic, 2), "| R² =", round(best_comb$r2, 4), "\n\n")

# Show top 5 models
cat("Top 5 models by AIC:\n")
top5 <- order(sapply(all_combs, function(x) x$aic))[1:5]
for (i in 1:5) {
  m <- all_combs[[top5[i]]]
  cat(sprintf("%d. %s | AIC = %.2f | R² = %.4f\n",
              i, paste(m$predictors, collapse = " + "), m$aic, m$r2))
}
cat("\n")

selected <- best_comb$predictors
model2 <- lm(as.formula(paste("Overall ~", paste(selected, collapse = " + "))), data = data)

print(summary(model2))
cat("\n")

# MODEL #3: London vs Non-London (Same for all versions)
cat("MODEL #3: London vs Non-London (with Crime)\n")
data_london <- data %>% filter(Region == "London")
data_non_london <- data %>% filter(Region != "London", !is.na(Region))

london_sel <- c("Crime")
nl_sel <- c("Crime")

if (nrow(data_london) >= 20) {
  remaining_l <- setdiff(c("Income", "Employment", "Education", "Health", "Barriers", "Living"), "Crime")
  for (i in 1:3) {
    best_aic_l <- Inf
    best_pred_l <- NULL
    for (pred in remaining_l) {
      test_m <- lm(as.formula(paste("Overall ~", paste(c(london_sel, pred), collapse = " + "))), data = data_london)
      if (AIC(test_m) < best_aic_l) {
        best_aic_l <- AIC(test_m)
        best_pred_l <- pred
      }
    }
    if (!is.null(best_pred_l)) {
      london_sel <- c(london_sel, best_pred_l)
      remaining_l <- setdiff(remaining_l, best_pred_l)
    }
  }
}

remaining_nl <- setdiff(c("Income", "Employment", "Education", "Health", "Barriers", "Living"), "Crime")
for (i in 1:3) {
  best_aic_nl <- Inf
  best_pred_nl <- NULL
  for (pred in remaining_nl) {
    test_m <- lm(as.formula(paste("Overall ~", paste(c(nl_sel, pred), collapse = " + "))), data = data_non_london)
    if (AIC(test_m) < best_aic_nl) {
      best_aic_nl <- AIC(test_m)
      best_pred_nl <- pred
    }
  }
  if (!is.null(best_pred_nl)) {
    nl_sel <- c(nl_sel, best_pred_nl)
    remaining_nl <- setdiff(remaining_nl, best_pred_nl)
  }
}

cat("London:", paste(london_sel, collapse = ", "), "\n")
cat("Non-London:", paste(nl_sel, collapse = ", "), "\n")
cat("Same?", identical(sort(london_sel), sort(nl_sel)), "\n\n")

# ============================================================================
# PART 3: Diagnostic Plots
# ============================================================================

cat("PART 3: DIAGNOSTIC PLOTS AND OUTLIER DETECTION\n\n")

p <- autoplot(model2, which = 1:4, label.size = 2, ncol = 2)
print(p)

model_data <- data[complete.cases(data[, c("Overall", selected)]), ]
std_resid <- rstandard(model2)
cooks_d <- cooks.distance(model2)
leverage <- hatvalues(model2)

outliers_resid <- which(abs(std_resid) > 3)
outliers_cooks <- which(cooks_d > 4/nrow(model_data))
outliers_lev <- which(leverage > 2*length(coef(model2))/nrow(model_data))
all_outliers <- unique(c(outliers_resid, outliers_cooks, outliers_lev))

cat("\nOutliers flagged:", length(all_outliers), "\n")
if (length(all_outliers) > 0) {
  print(model_data[all_outliers, ] %>% dplyr::select(Rank, LAD24NM, Region, Overall) %>% arrange(Rank) %>% head(10))
}

cat("\n============================================================================\n")
cat("VERSION 4 COMPLETE - BEST SUBSET SELECTION EMPHASIS\n")
cat("============================================================================\n")
