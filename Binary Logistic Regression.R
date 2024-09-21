# the Logistic Regression predicts the outcome of something happening or not happening and produces a probability value between 0 and 1.

# H0: there is no significant relationship between age and the likelihood of having diabetes.
# HA: there is a significant relationship between age and the likelihood of having diabetes.
# H0: there is no statistically significant relationship between BMI and the likelihood of having diabetes.
# HA: there is a statistically significant relationship between BMI and the likelihood of having diabetes.
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary libraries
library(ggplot2)

# Create a sample dataset
set.seed(123)  # for reproducibility
n <- 100  # Number of samples
age <- rnorm(n, mean = 50, sd = 10)  # Age in years
bmi <- rnorm(n, mean = 25, sd = 5)  # BMI

# Simulate a binary outcome (0 = no diabetes, 1 = diabetes)
diabetes <- rbinom(n, 1, prob = 1 / (1 + exp(-(0.1 * age + 0.15 * bmi - 8))))

# Combine into a data frame
dat <- data.frame(age, bmi, diabetes)

# Fit logistic regression model
model <- glm(diabetes ~ age + bmi, data = dat, family = "binomial")

# Print model summary
summary(model)

# Print test results with interpretation for logistic regression
cat("\nLogistic Regression Test Results:\n")

# Extract p-values for age and bmi
p_value_age <- summary(model)$coefficients["age", "Pr(>|z|)"]
p_value_bmi <- summary(model)$coefficients["bmi", "Pr(>|z|)"]

# Define significance level
alpha <- 0.05

cat("Test Statistic (Age Coefficient):", summary(model)$coefficients["age", "z value"], "\n")
cat("p-value (Age):", p_value_age, "\n")

if(p_value_age < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0) for Age. There is a statistically significant relationship between Age and the likelihood of having diabetes.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0) for Age. There is no statistically significant relationship between Age and the likelihood of having diabetes.\n")
}

cat("\nTest Statistic (BMI Coefficient):", summary(model)$coefficients["bmi", "z value"], "\n")
cat("p-value (BMI):", p_value_bmi, "\n")

if(p_value_bmi < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0) for BMI. There is a statistically significant relationship between BMI and the likelihood of having diabetes.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0) for BMI. There is no statistically significant relationship between BMI and the likelihood of having diabetes.\n")
}

# Odds ratios
odds_ratios <- exp(coef(model))
cat("\nOdds Ratios:\n")
print(odds_ratios)
cat("Interpretation: The odds ratios show how much the odds of having diabetes change with a one-unit increase in age or BMI.\n")

# Confidence intervals for the odds ratios
conf_int <- exp(confint(model))
cat("\n95% Confidence Intervals for Odds Ratios:\n")
print(conf_int)
cat("If the 95% confidence interval for an odds ratio does not include 1, it suggests a significant association.\n")

# Visualization using ggplot2
ggplot(dat, aes(x = age, y = diabetes)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.02), alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue", se = TRUE) +
  labs(title = "Logistic Regression of Diabetes by Age",
       x = "Age",
       y = "Probability of Having Diabetes") +
  theme_minimal()

# Add another plot for BMI
ggplot(dat, aes(x = bmi, y = diabetes)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.02), alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "red", se = TRUE) +
  labs(title = "Logistic Regression of Diabetes by BMI",
       x = "BMI",
       y = "Probability of Having Diabetes") +
  theme_minimal()
