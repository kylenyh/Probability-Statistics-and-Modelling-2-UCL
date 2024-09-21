# the Chi-Square Goodness of Fitness Test is used to assesses whether an observed distribution of data matches the expected distribution of the data

# H0: the observed distribution of grades does not fit the expected uniform distribution
# HA: the observed distribution of grades does fit the expected uniform distribution
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary libraries
library(ggplot2)

# Create a sample dataset
grades <- sample(c("A", "B", "C", "D", "F"), 200, replace = TRUE)

# Create a frequency table of observed data
observed_counts <- table(grades)

# Print observed counts
cat("Observed Counts:\n")
print(observed_counts)

# Define expected proportions for a uniform distribution
# For a uniform distribution, each category is equally likely
expected_proportions <- rep(1/length(observed_counts), length(observed_counts))

# Calculate expected counts based on the total number of observations
total_observations <- sum(observed_counts)
expected_counts <- expected_proportions * total_observations

# Perform the Chi-Square Goodness-of-Fit Test
chi_square_test <- chisq.test(observed_counts, p = expected_proportions)

# Print test results
cat("\nChi-Square Goodness-of-Fit Test Results:\n")
cat("Chi-Square Statistic:", chi_square_test$statistic, "\n")
cat("Degrees of Freedom:", chi_square_test$parameter, "\n")
cat("p-value:", chi_square_test$p.value, "\n")

# Hypothesis Statements
cat("\nHypotheses:\n")
cat("Null Hypothesis (H0): The observed distribution of grades fits the expected uniform distribution.\n")
cat("Alternative Hypothesis (H1): The observed distribution of grades does not fit the expected uniform distribution.\n")

# Create a data frame for the contingency table
contingency_table <- data.frame(
  Grade = names(observed_counts),
  Observed = observed_counts,
  Expected = expected_counts
)

# Print the contingency table
cat("Contingency Table:\n")
print(contingency_table)

# Interpretation
alpha <- 0.05

if (chi_square_test$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). The observed distribution of grades does not fit the expected uniform distribution.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). The observed distribution of grades fits the expected uniform distribution.\n")
}

# Prepare data for visualization
dat <- as.data.frame(observed_counts)
names(dat) <- c("Grade", "Count")
dat$Expected <- expected_counts

# Create a bar plot to visualize the observed vs. expected counts
ggplot(dat, aes(x = Grade)) +
  geom_bar(aes(y = Count, fill = "Observed"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Expected, fill = "Expected"), stat = "identity", position = "dodge", alpha = 0.5) +
  labs(title = "Observed vs. Expected Counts of Grades",
       x = "Grade",
       y = "Count") +
  scale_fill_manual(values = c("Observed" = "blue", "Expected" = "green")) +
  theme_minimal()
