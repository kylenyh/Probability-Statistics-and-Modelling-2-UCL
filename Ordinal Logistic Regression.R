# the Ordinal Logistic Regression is used to determine the relationship between one or more independent factors and the likelihood that an ordinal outcome will fall into a certain category or a higher category

# H0: hours of training does not significantly affect the probability of higher satisfaction levels
# HA: hours of training does significantly affect the probability of higher satisfaction levels
# p-val < sig level reject H0
# p-val > sig level accept H0


# Load necessary libraries
library(MASS)    # For ordinal logistic regression
library(ggplot2) # For visualization
library(tidyr)   # For data manipulation

# Set seed for reproducibility
set.seed(123)

# Create a sample dataset
data <- data.frame(
  satisfaction = factor(sample(1:3, 200, replace = TRUE), levels = 1:3, labels = c("Low", "Medium", "High")),
  hours_training = rnorm(200, mean = 10, sd = 5)
)

# Print the first few rows of the dataset
cat("Sample Dataset:\n")
print(head(data))

# Fit the ordinal logistic regression model
model <- polr(satisfaction ~ hours_training, data = data, Hess = TRUE)

# Summary of the model
cat("\nOrdinal Logistic Regression Model Summary:\n")
summary(model)

# Create a contingency table of observed satisfaction levels
contingency_table <- table(data$satisfaction)

# Print the contingency table
cat("\nContingency Table of Observed Satisfaction Levels:\n")
print(contingency_table)

# Create a new data frame for predictions
pred_data <- data.frame(
  hours_training = seq(min(data$hours_training), max(data$hours_training), length.out = 100)
)
pred_data$predicted_probs <- predict(model, newdata = pred_data, type = "probs")

# Convert to long format for ggplot
pred_data_long <- pivot_longer(pred_data, cols = starts_with("predicted_probs"), names_to = "Satisfaction", values_to = "Probability")

# Plot the predicted probabilities
ggplot(pred_data_long, aes(x = hours_training, y = Probability, color = Satisfaction)) +
  geom_line(size = 1) +
  labs(title = "Predicted Probabilities of Satisfaction Levels",
       x = "Hours of Training",
       y = "Probability",
       color = "Satisfaction Level") +
  theme_minimal()

# Hypothesis Statements
cat("\nHypothesis Statements:\n")
cat("Null Hypothesis (H0): Hours of training does not significantly affect the probability of higher satisfaction levels.\n")
cat("Alternative Hypothesis (HA): Hours of training significantly affects the probability of higher satisfaction levels.\n")

# Print interpretation statements
coef_summary <- summary(model)
coef <- coef_summary$coefficients
p_values <- coef_summary$z

cat("\nOrdinal Logistic Regression Results:\n")
cat("Coefficients:\n")
print(coef)
cat("P-values:\n")
print(p_values)

# Determine significance
alpha <- 0.05
cat("\nHypothesis Testing:\n")
if (any(p_values < alpha)) {
  cat("Conclusion: Reject the null hypothesis (H0). Hours of training significantly affects satisfaction levels.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). Hours of training does not significantly affect satisfaction levels.\n")
}
