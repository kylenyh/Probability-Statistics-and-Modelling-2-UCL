# the Two-Sample Wilcoxon Test/Wilcoxon Rank-Sum Test/Mann Whitney U Test is used to determine whether there is a significant difference between the distributions of two independent samples.
# H0: there is a significant difference in weight loss between Group A and Group B.
# HA: there is a significant difference in weight loss between Group A and Group B.
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary libraries
library(ggplot2)

# Create a sample dataset for two independent groups
set.seed(123)  # for reproducibility
n <- 30  # Number of samples per group

# Simulate weight loss data for two groups (e.g., Treatment A and Treatment B)
group_A <- rnorm(n, mean = -2, sd = 2)  # Group A (e.g., Treatment A)
group_B <- rnorm(n, mean = -1, sd = 2)  # Group B (e.g., Treatment B)

# Combine into a data frame
weight_loss_data <- data.frame(
  weight_loss = c(group_A, group_B),
  group = rep(c("Group A", "Group B"), each = n)
)

# Perform the Two-Sample Wilcoxon Test
wilcox_test_result <- wilcox.test(weight_loss ~ group, data = weight_loss_data, alternative = "two.sided")

# Print test results with interpretation for the Two-Sample Wilcoxon Test
cat("\nTwo-Sample Wilcoxon Test Results:\n")
cat("Test Statistic (W):", wilcox_test_result$statistic, "\n")
cat("p-value:", wilcox_test_result$p.value, "\n")

# Define significance level
alpha <- 0.05

if(wilcox_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant difference in weight loss between Group A and Group B.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference in weight loss between Group A and Group B.\n")
}

# Visualization using ggplot2
ggplot(weight_loss_data, aes(x = weight_loss, fill = group)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~group, scales = "free_y") +
  geom_vline(data = aggregate(weight_loss ~ group, data = weight_loss_data, median), 
             aes(xintercept = weight_loss, color = group), linetype = "dashed", size = 1) +
  labs(title = "Histogram of Weight Loss by Group",
       x = "Weight Loss",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("Group A" = "blue", "Group B" = "green")) +
  scale_color_manual(values = c("Group A" = "blue", "Group B" = "green")) +
  theme(legend.position = "none") +
  geom_text(data = aggregate(weight_loss ~ group, data = weight_loss_data, median), 
            aes(x = weight_loss, y = 4, label = paste("Median =", round(weight_loss, 2))), 
            color = "black", hjust = 1.1)
