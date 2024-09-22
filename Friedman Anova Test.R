# the Friedman Anova Test is used to detect differences in ranks across multiple test attempts.
# H0: there is no significant difference in the median test scores across the teaching methods.
# HA: there is a significant difference in the median test scores across the teaching methods.
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary library packages
library(ggplot2)
library(reshape2)

# Sample data for related groups (test scores of students across three different teaching methods)
test_scores_data <- data.frame(
  student = 1:15,
  method_A = c(78, 85, 82, 76, 88, 81, 80, 77, 84, 79, 86, 75, 82, 87, 80),
  method_B = c(80, 83, 84, 79, 85, 83, 81, 79, 86, 81, 87, 77, 83, 88, 81),
  method_C = c(85, 89, 88, 84, 90, 86, 85, 83, 88, 86, 89, 82, 87, 91, 85)
)

# Display the dataset
print(test_scores_data)

# Convert the data to long format for the Friedman test
test_scores_long <- melt(test_scores_data, id.vars = 'student', variable.name = 'Method', value.name = 'Score')

# Perform Friedman ANOVA test
friedman_test_result <- friedman.test(Score ~ Method | student, data = test_scores_long)

# Print the result
print(friedman_test_result)

# Visualization: Boxplot of Scores by Teaching Method
boxplot(Score ~ Method, data = test_scores_long, 
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Test Scores by Teaching Method",
        xlab = "Teaching Method", ylab = "Test Score")

# Visualization: ggplot2 Boxplot of Scores by Teaching Method
ggplot(test_scores_long, aes(x = Method, y = Score, fill = Method)) +
  geom_boxplot() +
  labs(title = "Test Scores by Teaching Method", x = "Teaching Method", y = "Test Score") +
  theme_minimal() +
  scale_fill_manual(values = c("method_A" = "lightblue", "method_B" = "lightgreen", "method_C" = "lightpink")) +
  theme(legend.position = "none")

# Define significance level
alpha <- 0.05

# Print test result with an interpretation
cat("Friedman ANOVA Test Results:\n")
cat("Test Statistic:", friedman_test_result$statistic, "\n")
cat("p-value:", friedman_test_result$p.value, "\n")

if(friedman_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant difference in the median test scores across the teaching methods.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference in the median test scores across the teaching methods.\n")
}
