# the Nominall Logistic Regression predicts the probabilities of each possible outcome category as a function of predictor variables

# H0: age does not significantly affect the likelihood of choosing a product (A, B, or C)
# HA: age does significantly affect the likelihood of choosing a product (A, B, or C)
# p-val < sig level reject H0
# p-val > sig level accept H0


# Load necessary libraries
library(nnet)    # For multinomial logistic regression
library(ggplot2) # For visualization
library(tidyr)   # For data manipulation

# Create a sample dataset
data <- data.frame(
  product_choice = factor(sample(c("A", "B", "C"), 300, replace = TRUE)),
  age = rnorm(300, mean = 45, sd = 10)
)

# Print the first few rows of the dataset
cat("Sample Dataset:\n")
print(head(data))

# Fit the multinomial logistic regression model
model <- multinom(product_choice ~ age, data = data)

# Summary of the model
cat("\nNominal Logistic Regression Model Summary:\n")
summary(model)

# Create a contingency table of observed product choices
contingency_table <- table(data$product_choice)

# Print the contingency table
cat("\nContingency Table of Observed Product Choices:\n")
print(contingency_table)

# Create a new data frame for predictions
pred_data <- data.frame(
  age = seq(min(data$age), max(data$age), length.out = 100)
)
pred_data <- cbind(pred_data, predict(model, newdata = pred_data, type = "probs"))

# Convert to long format for ggplot
pred_data_long <- pivot_longer(pred_data, cols = starts_with("product_choice"), names_to = "Product", values_to = "Probability")

# Plot the predicted probabilities
ggplot(pred_data_long, aes(x = age, y = Probability, color = Product)) +
  geom_line(size = 1) +
  labs(title = "Predicted Probabilities of Product Choices by Age",
       x = "Age",
       y = "Probability",
       color = "Product Choice") +
  theme_minimal()

# Hypothesis Statements
cat("\nHypothesis Statements:\n")
cat("Null Hypothesis (H0): Age does not significantly affect the likelihood of choosing a product (A, B, or C).\n")
cat("Alternative Hypothesis (HA): Age significantly affects the likelihood of choosing a product (A, B, or C).\n")

# Print interpretation statements
coef_summary <- summary(model)
coef <- coef_summary$coefficients
z_values <- coef_summary$z

cat("\nNominal Logistic Regression Results:\n")
cat("Coefficients:\n")
print(coef)
cat("Z-values:\n")
print(z_values)

# Determine significance (manually or with additional steps if p-values are needed)
alpha <- 0.05
cat("\nHypothesis Testing:\n")
if (any(abs(z_values) > qnorm(1 - alpha/2))) { # Approximate significance based on z-values
  cat("Conclusion: Reject the null hypothesis (H0). Age significantly affects the likelihood of choosing a product.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). Age does not significantly affect the likelihood of choosing a product.\n")
}
