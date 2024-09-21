# the Simple Linear Regression is used to compare the relationship between a dependendent variable and an independent variable

# H0: there is no statistically significant relationship between hours studied and exam score.
# HA: there is a statistically significant relationship between hours studied and exam score.
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary library
library(ggplot2)

# Create a sample dataset
n <- 100  # Number of samples
hours_studied <- rnorm(n, mean = 10, sd = 3)  # Hours studied

# Simulate exam scores (dependent variable) based on hours studied
exam_score <- 50 + 5 * hours_studied + rnorm(n, mean = 0, sd = 10)

# Combine into a data frame
dat <- data.frame(hours_studied, exam_score)

# Print the first few rows of the dataset
head(dat)

# Hypothesis Statements
cat("Hypotheses:\n")
cat("Null Hypothesis (H0): There is no significant linear relationship between hours studied and exam score.\n")
cat("Alternative Hypothesis (H1): There is a significant linear relationship between hours studied and exam score.\n")

# Fit simple linear regression model
model <- lm(exam_score ~ hours_studied, data = dat)

# Print model summary
summary(model)

# Extract coefficient and p-value for hours_studied
p_value_hours <- summary(model)$coefficients["hours_studied", "Pr(>|t|)"]
coef_hours <- summary(model)$coefficients["hours_studied", "Estimate"]
intercept <- summary(model)$coefficients["(Intercept)", "Estimate"]

# Define significance level
alpha <- 0.05

# Print interpretation statements
cat("\nInterpretation:\n")
cat("1. The coefficient for hours studied is", coef_hours, ", which represents the estimated change in exam score for each additional hour studied.\n")
cat("2. The p-value for the hours studied coefficient is", p_value_hours, ".\n")

if (p_value_hours < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a statistically significant linear relationship between hours studied and exam score.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no statistically significant linear relationship between hours studied and exam score.\n")
}

# Print the regression equation
cat("\nRegression Equation:\n")
cat("Exam Score = ", round(intercept, 2), " + ", round(coef_hours, 2), " * Hours Studied\n")

# Visualize the data and the regression line using ggplot2
ggplot(dat, aes(x = hours_studied, y = exam_score)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of data points
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +  # Regression line with confidence interval
  labs(title = "Simple Linear Regression of Exam Score by Hours Studied",
       x = "Hours Studied",
       y = "Exam Score") +
  theme_minimal()
