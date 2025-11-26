# Load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("car")) install.packages("car", repos = "http://cran.us.r-project.org")
if (!require("lmtest")) install.packages("lmtest", repos = "http://cran.us.r-project.org")
if (!require("broom")) install.packages("broom", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(car)
library(lmtest)
library(broom)

# Create plots directory
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Load data
# We use the provided dataset mlr0.csv
# Path is relative to the project root
data_path <- file.path("data-sets", "mlr_data", "mlr0.csv")
if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}
data <- read.csv(data_path)

# 1. Run Multiple Regression
# Model: y ~ x1 + x2
# We use x1 and x2 as the independent variables (IVs) and y as the dependent variable (DV).
model <- lm(y ~ x1 + x2, data = data)

# Save summary to results file
sink("results.txt")
cat("Multiple Regression Results (Dataset: mlr0.csv)\n")
cat("=============================================\n\n")
print(summary(model))
cat("\n")

# Assumption Checking

# Assumption #1: Linearity
# Scatterplots of IVs vs DV
p1 <- ggplot(data, aes(x = x1, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Linearity Check: x1 vs y")
ggsave("plots/linearity_x1.png", plot = p1)

p2 <- ggplot(data, aes(x = x2, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Linearity Check: x2 vs y")
ggsave("plots/linearity_x2.png", plot = p2)

cat("Assumption #1: Linearity checked via scatterplots (saved in plots/).\n\n")

# Assumption #2: Multicollinearity
cat("Assumption #2: Multicollinearity (VIF and Tolerance)\n")
vif_vals <- vif(model)
tolerance <- 1/vif_vals
print(vif_vals)
cat("Tolerance:\n")
print(tolerance)
cat("\n")

# Assumption #3: Independence of Residuals
cat("Assumption #3: Independence of Residuals (Durbin-Watson)\n")
dw <- dwtest(model)
print(dw)
cat("\n")

# Assumption #4: Homoscedasticity
# Plot of Fitted vs Residuals
p3 <- ggplot(augment(model), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Homoscedasticity Check: Fitted vs Residuals")
ggsave("plots/homoscedasticity.png", plot = p3)

cat("Assumption #4: Homoscedasticity checked via plot (saved in plots/).\n\n")

# Assumption #5: Normality of Residuals
# Q-Q Plot
p4 <- ggplot(augment(model), aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normality Check: Q-Q Plot of Residuals")
ggsave("plots/normality_qq.png", plot = p4)

cat("Assumption #5: Normality checked via Q-Q plot (saved in plots/).\n\n")

# Assumption #6: Influential Cases
# Cook's Distance
cat("Assumption #6: Influential Cases (Cook's Distance)\n")
cooksD <- cooks.distance(model)
influential <- cooksD[(cooksD > (4/nobs(model)))]
if (length(influential) > 0) {
  cat("Potential influential cases (Cook's D > 4/n):\n")
  print(influential)
} else {
  cat("No influential cases detected (Cook's D <= 4/n).\n")
}

# Add Cook's Distance to data and save
data$cooks_distance <- cooksD
write.csv(data, "mlr0_with_diagnostics.csv", row.names = FALSE)

sink()
cat("Analysis complete. Results saved to results.txt and plots in plots/ directory.\n")
