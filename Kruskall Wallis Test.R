# the Kruskall Wallis Test is used to compare the medians of three or more independent groups to determine if they come from the same distribution.

# H0: there is no statistically significant difference in the median values among groups A, B, C
# HA: there is a statistically significant difference in the median values among groups A, B, C
# p-val < sig level reject H0
# p-val > sig level accept H0

# load library package
library(ggplot2)

# Sample data of Test times for three different groups of students
group_A <- c(30, 35, 40, 45, 50)
group_B <- c(20, 25, 30, 35, 40)
group_C <- c(40, 45, 50, 55, 60)

# Combine data into a single data frame
test_data <- data.frame(
  group = rep(c("A", "B", "C"), each = 5),
  time = c(group_A, group_B, group_C)
)

# Display the dataset
print(test_data)

# Set significance level
alpha <- 0.05

# Perform the Kruskal-Wallis test
kruskal_test_result <- kruskal.test(time ~ group, data = test_data)

# Print the result
print(kruskal_test_result)

# Create a boxplot to visualize the differences
ggplot(test_data, aes(x = group, y = time, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Test Times by Group",
       x = "Group",
       y = "Test Time (minutes)") +
  theme_minimal()

# Print test result with an interpretation
cat("Kruskal-Wallis Test:\n")
cat("Test Statistic:", kruskal_test_result$statistic, "\n")
cat("p-value:", kruskal_test_result$p.value, "\n")

if(kruskal_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a statistically significant difference in the median values among groups A, B, C.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no statistically significant difference in the median values among groups A, B, C.\n")
}
