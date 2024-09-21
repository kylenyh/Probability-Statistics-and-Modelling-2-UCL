# the One-Sample T-Test is used to test the statistical difference between a sample mean and a known or assumed/hypothesized value of the mean in the population

# H0: the mean weight of the apples in the sample is equal to the hypothesized population mean
# H1: the mean weight of the apples in the sample is not equal to the hypothesized population mean (two-tailed test)
# p-val < sig level reject H0
# p-val > sig level accept H0

# a sample dataset that compares the weights of a group of apples against a hypothesized mean weight.

# Sample dataset: weights of 20 apples in grams
# actual mean weight of an apple is 154.25
apple_weights <- c(152, 148, 155, 160, 149, 158, 162, 157, 154, 150, 
                   148, 161, 159, 153, 156, 149, 151, 158, 150, 155)

# Conduct a one-sample t-test
# hypothesized mean weight of an apple is 155 grams
t_test_result <- t.test(apple_weights, mu = 155)

# Display the results
print(t_test_result)

# Boxplot
boxplot(apple_weights, 
        horizontal = TRUE, 
        col = "lightgreen", 
        main = "Boxplot of Apple Weights",
        xlab = "Apple Weight (grams)")

# Add a reference line for the hypothesized mean
abline(v = 155, col = "red", lwd = 2, lty = 2)

# Print test result with an interpretation
cat("One-Sample T-Test:\n")
cat("Test Statistic:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")

if (t_test_result$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant difference in the mean weight of the apples compared to the hypothesized mean of 155 grams.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference in the mean weight of the apples compared to the hypothesized mean of 155 grams.\n")
}
