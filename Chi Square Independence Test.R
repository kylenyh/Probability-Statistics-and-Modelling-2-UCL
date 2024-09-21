# the Chi-Square Independence Test is used to assesses whether there is a significant association or relationship between two categorical variables

# H0: There is no statistically significant association between gender and product preferences
# HA: there is a statistically significant association between gender and product preferences
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create a sample dataset
n <- 200  # Number of samples
gender <- sample(c("Male", "Female"), n, replace = TRUE)
preferences <- sample(c("Product A", "Product B", "Product C"), n, replace = TRUE)

# Combine into a data frame
dat <- data.frame(gender, preferences)

# Print the first few rows of the dataset
head(dat)

# Create a contingency table
contingency_table <- table(dat$gender, dat$preferences)

# Print the contingency table
print(contingency_table)

# Hypothesis Statements
cat("Hypotheses:\n")
cat("Null Hypothesis (H0): There is no statistically significant association between gender and product preferences.\n")
cat("Alternative Hypothesis (H1): There is a statistically significant association between gender and product preferences.\n")

# Perform the Chi-Square Test
chi_square_test <- chisq.test(contingency_table)

# Print test results
cat("\nChi-Square Test Results:\n")
cat("Chi-Square Statistic:", chi_square_test$statistic, "\n")
cat("Degrees of Freedom:", chi_square_test$parameter, "\n")
cat("p-value:", chi_square_test$p.value, "\n")

# Interpretation
alpha <- 0.05

if (chi_square_test$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a statistically significant association between gender and product preferences.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no statistically significant association between gender and product preferences.\n")
}

# Convert data to long format for visualization
dat_long <- dat |>
  group_by(gender, preferences) |>
  summarise(count = n()) |>
  ungroup()

# Create a bar plot to visualize the data
ggplot(dat_long, aes(x = preferences, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Product Preferences by Gender",
       x = "Product Preferences",
       y = "Count") +
  theme_minimal()
