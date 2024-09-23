# the One-Sample Wilcoxon Test/Wilcoxon Signed-Rank Test is used to determine whether the median of a sample is equal to a specified median value.
# H0: there is no significant difference between the actual median value of weight loss and the hypothesized median value of weight loss
# HA: there is a significant difference between the actual median value of weight loss and the hypothesized median value of weight loss
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary libraries
library(ggplot2)

# Create a sample dataset
set.seed(123)  # for reproducibility
n <- 30  # Number of samples
weight_loss <- rnorm(n, mean = -2, sd = 2)  # Simulate weight loss data

# Specify the hypothesized median value (e.g., 0)
hypothesized_median <- 0

# Perform the One-Sample Wilcoxon Test
wilcox_test_result <- wilcox.test(weight_loss, mu = hypothesized_median, alternative = "two.sided")

# Print test results with interpretation for the One-Sample Wilcoxon Test
cat("\nOne-Sample Wilcoxon Test Results:\n")
cat("Test Statistic (V):", wilcox_test_result$statistic, "\n")
cat("p-value:", wilcox_test_result$p.value, "\n")

# Define significance level
alpha <- 0.05

if(wilcox_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). The median weight loss is significantly different from 0.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference between the median weight loss and 0.\n")
}

# Visualization using ggplot2
ggplot(data = data.frame(weight_loss), aes(x = weight_loss)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = median(weight_loss), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = hypothesized_median, color = "green", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Weight Loss",
       x = "Weight Loss",
       y = "Frequency") +
  theme_minimal() +
  annotate("text", x = median(weight_loss), y = 4, label = paste("Median =", round(median(weight_loss), 2)), color = "red", hjust = 1.1) +
  annotate("text", x = hypothesized_median, y = 4, label = "Hypothesized Median = 0", color = "green", hjust = -0.1)
