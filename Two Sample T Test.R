# the Two-Sample T-Test is used to determine whether there is a significant difference between the means of two independent groups.

# H0: the mean weight of the apples in orchard A is equal to the mean weight of the apples in orchard B
# H1: the mean weight of the apples in orchard A is not equal to the mean weight of the apples in orchard B
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load required libraries
library(ggplot2)

# Sample data comparing weights of apples from two different orchards
apple_data <- data.frame(
  orchard = rep(c("Orchard A", "Orchard B"), each = 15),
  weight = c(152, 157, 149, 158, 160, 155, 156, 151, 153, 159,
             154, 157, 156, 158, 151, 162, 165, 161, 168, 164,
             159, 166, 162, 163, 160, 167, 158, 169, 161, 170)
)

# Display the dataset
print(apple_data)

# Perform two-sample t-test
t_test_result <- t.test(weight ~ orchard, data = apple_data)

# Print the result of the t-test
print(t_test_result)

# Create a boxplot using ggplot2
ggplot(apple_data, aes(x = orchard, y = weight, fill = orchard)) +
  geom_boxplot() +
  labs(title = "Boxplot of Apple Weights from Two Orchards", x = "Orchard", y = "Weight (grams)") +
  theme_minimal()

# Print test result with an interpretation
cat("Two-Sample T-Test:\n")
cat("Test Statistic:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")

if (t_test_result$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant difference in the mean weight of apples from Orchard A and Orchard B.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference in the mean weight of apples from Orchard A and Orchard B.\n")
}
