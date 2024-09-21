# the Multiple Linear Regression is used to compare the relationship of one dependendent variable to multiple independent variables.

# H0: there is no significant relationship between age and income.
# HA: there is a significant relationship between age and income.
# H0: there is no statistically significant relationship between education and income.
# HA: there is a statistically significant relationship between education and income.
# H0: there is no statistically significant relationship between experience and income.
# HA: there is a statistically significant relationship between experience and income.
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary libraries
library(ggplot2)

# Create a sample dataset
set.seed(123)  # for reproducibility
n <- 100  # Number of samples
age <- rnorm(n, mean = 40, sd = 10)  # Age in years
education <- rnorm(n, mean = 16, sd = 2)  # Education in years
experience <- rnorm(n, mean = 15, sd = 5)  # Work experience in years

# Simulate income (dependent variable) based on age, education, and experience
income <- 20000 + 500 * age + 3000 * education + 1000 * experience + rnorm(n, mean = 0, sd = 10000)

# Combine into a data frame
dat <- data.frame(age, education, experience, income)

# Print the first few rows of the dataset
head(dat)

# Hypothesis Statements
cat("Hypotheses:\n")
cat("Null Hypothesis (H0): There is no significant relationship between age, education, or work experience and income.\n")
cat("Alternative Hypothesis (H1): There is a significant relationship between at least one of the predictors (age, education, or work experience) and income.\n")

# Fit multiple linear regression model
model <- lm(income ~ age + education + experience, data = dat)

# Print model summary
summary(model)

# Print interpretation statements
cat("\nInterpretation:\n")
cat("1. The coefficients represent the change in the dependent variable (income) for a one-unit increase in each predictor variable, holding other variables constant.\n")
cat("2. A positive coefficient indicates that as the predictor variable increases, the dependent variable also increases.\n")
cat("3. The p-values indicate whether the relationship between each predictor and the dependent variable is statistically significant.\n")

# Extract p-values for age, education, and experience
p_value_age <- summary(model)$coefficients["age", "Pr(>|t|)"]
p_value_education <- summary(model)$coefficients["education", "Pr(>|t|)"]
p_value_experience <- summary(model)$coefficients["experience", "Pr(>|t|)"]

# Define significance level
alpha <- 0.05

cat("\nTest Statistic (Age Coefficient):", summary(model)$coefficients["age", "t value"], "\n")
cat("p-value (Age):", p_value_age, "\n")

if(p_value_age < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0) for Age. There is a statistically significant relationship between Age and Income.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0) for Age. There is no statistically significant relationship between Age and Income.\n")
}

cat("\nTest Statistic (Education Coefficient):", summary(model)$coefficients["education", "t value"], "\n")
cat("p-value (Education):", p_value_education, "\n")

if(p_value_education < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0) for Education. There is a statistically significant relationship between Education and Income.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0) for Education. There is no statistically significant relationship between Education and Income.\n")
}

cat("\nTest Statistic (Experience Coefficient):", summary(model)$coefficients["experience", "t value"], "\n")
cat("p-value (Experience):", p_value_experience, "\n")

if(p_value_experience < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0) for Experience. There is a statistically significant relationship between Experience and Income.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0) for Experience. There is no statistically significant relationship between Experience and Income.\n")
}

# Visualization using ggplot2 for each predictor

# Plot for Age vs Income
ggplot(dat, aes(x = age, y = income)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
  labs(title = "Linear Regression of Income by Age",
       x = "Age",
       y = "Income") +
  theme_minimal()

# Plot for Education vs Income
ggplot(dat, aes(x = education, y = income)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
  labs(title = "Linear Regression of Income by Education",
       x = "Education (Years)",
       y = "Income") +
  theme_minimal()

# Plot for Experience vs Income
ggplot(dat, aes(x = experience, y = income)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
  labs(title = "Linear Regression of Income by Experience",
       x = "Experience (Years)",
       y = "Income") +
  theme_minimal()
